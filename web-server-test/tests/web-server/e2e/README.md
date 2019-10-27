# web-server end-to-end tests

These tests spin up real web servers in order to ensure that the
system works end-to-end.  Each subfolder is expected to contain two
files: `server.rkt` and `tests.rkt`.

Each `server.rkt` module must provide a function called `start` that
starts a web server and returns a function that can be used to stop
that server.

Each `tests.rkt` module must provide a rackunit `test-suite` called
`tests`.
