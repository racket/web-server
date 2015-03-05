#lang racket/base
(require racket/unit
         net/tcp-sig
         web-server/web-server-sig
         web-server/web-config-sig
         web-server/private/dispatch-server-with-connect-unit
         web-server/private/dispatch-server-sig
         web-server/private/raw-dispatch-server-connect-unit)

(provide dispatch-server@
         dispatch-server-with-connect@)

(define-compound-unit/infer dispatch-server@
  (import tcp^ dispatch-server-config^)
  (export dispatch-server^)
  (link [([ws : web-server^]) dispatch-server-with-connect@]
        [([dsp : dispatch-server-connect^]) raw:dispatch-server-connect@]))
