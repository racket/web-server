#lang racket/base

(require racket/async-channel
         racket/contract
         racket/match)

;; Timeout plan
;; ============
;;
;; For each connection:
;;   * start timeout on connection startup.
;;
;; For each request on a connection:
;;   * reset timeout when request data comes in,
;;   * reset timeout before the response is handled by the application,
;;   * reset timeout after the application hands the response back,
;;   * reset timeout for every chunk of data sent to the client.
;;
;; The application may further adjust its allotted timeout for handling
;; requests by using the dispatcher in the dispatch-timeout module.

(define-logger web-server/timer)

(struct timer-manager (thd ch))
(struct timer (tm [deadline #:mutable] [action #:mutable])
  #:extra-constructor-name make-timer)

;; start-timer-manager : -> timer-manager?
;; The timer manager thread
(define (start-timer-manager)
  (define ch (make-async-channel))
  (define thd
    (thread
     (lambda ()
       (define timers (make-hash))
       (let loop ([first-deadline +inf.0]
                  [deadline-evt never-evt])
         (log-web-server/timer-debug "timers: ~a, next deadline in ~.sms" (hash-count timers) (- first-deadline (current-inexact-milliseconds)))
         (sync
          (handle-evt
           ch
           (match-lambda
             [(cons 'add t)
              (define deadline (timer-deadline t))
              (define-values (first-deadline* deadline-evt*)
                (if (< deadline first-deadline)
                    (values deadline (alarm-evt deadline))
                    (values first-deadline deadline-evt)))
              (hash-set! timers t #t)
              (loop first-deadline* deadline-evt*)]

             [(cons 'remove t)
              (hash-remove! timers t)
              (loop first-deadline deadline-evt)]))

          (handle-evt
           deadline-evt
           (lambda (_)
             (define now (current-inexact-milliseconds))
             (define-values (n-expired first-deadline)
               (for/fold ([n-expired 0]
                          [first-deadline +inf.0])
                         ([t (in-hash-keys timers)])
                 (define deadline (timer-deadline t))
                 (cond
                   [(<= deadline now)
                    ((timer-action t))
                    (hash-remove! timers t)
                    (values (add1 n-expired) first-deadline)]

                   [else
                    (values n-expired (if (< deadline first-deadline)
                                          deadline
                                          first-deadline))])))
             (log-web-server/timer-debug "expired ~a timers" n-expired)
             (loop first-deadline (alarm-evt first-deadline)))))))))

  (timer-manager thd ch))

;; add-timer : timer-manager number (-> void) -> timer
(define (add-timer tm msecs action)
  (define now (current-inexact-milliseconds))
  (define t (timer tm (+ now msecs) action))
  (begin0 t
    (async-channel-put
     (timer-manager-ch tm)
     (cons 'add t))))

;; revise-timer! : timer msecs (-> void) -> timer
;; revise the timer to ring msecs from now
(define (revise-timer! t msecs [action (timer-action t)])
  (set-timer-deadline! t (+ (current-inexact-milliseconds) msecs))
  (set-timer-action! t action))

;; cancel-timer! : timer -> void
(define (cancel-timer! t)
  (async-channel-put
   (timer-manager-ch (timer-tm t))
   (cons 'remove t)))

;; start-timer : timer-manager num (-> void) -> timer
;; to make a timer that calls to-do after sec from make-timer's application
(define (start-timer tm secs to-do)
  (add-timer tm (* 1000 secs) to-do))

;; reset-timer : timer num -> void
;; to cause timer to expire after sec from the adjust-msec-to-live's application
(define (reset-timer! t secs)
  (revise-timer! t (* 1000 secs)))

;; increment-timer! : timer num -> void
;; add secs to the timer, rather than replace
(define (increment-timer! t secs)
  (revise-timer! t
                 (+ (- (timer-deadline t) (current-inexact-milliseconds))
                    (* 1000 secs))))


(provide/contract
 [timer-manager? (-> any/c boolean?)]
 [struct timer ([tm timer-manager?]
                [deadline number?]
                [action (-> any/c)])]
 [start-timer-manager (-> timer-manager?)]
 [start-timer (timer-manager? number? (-> any/c) . -> . timer?)]
 [reset-timer! (timer? number? . -> . any/c)]
 [increment-timer! (timer? number? . -> . any/c)]
 [revise-timer! (timer? number? (-> any/c) . -> . void?)]
 [cancel-timer! (timer? . -> . any/c)])
