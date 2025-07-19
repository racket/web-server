# web-server end-to-end tests

These tests spin up real web servers in order to ensure that the
system works end-to-end.  Each subfolder is expected to contain two
files: `server.rkt` and `tests.rkt`.

Each `server.rkt` module must provide a function called `start` starts a
web server on an open port and returns a procedure that can be used to
stop the server and the port the server is listening on.

Each `tests.rkt` module must provide a function called `make-tests`
that takes a port and a procedure that stops the server when called and
returns a rackunit `test-suite`.
