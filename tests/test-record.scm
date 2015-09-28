(import (except (scheme base) define-record-type)
	(aeolus modes parameters)
	(aeolus misc record)
	(aeolus-test))

(test-begin "Simplified R6RS record")

(let ()
  (define-record-type (<bar> make-bar bar?))
  (define-record-type (<foo> make-foo foo?)
    (parent <bar>)
    (protocol (lambda (p) (lambda (v) ((p) v))))
    (fields (immutable v foo-v)))
  (test-assert "bar?" (bar? (make-bar)))
  (test-assert "foo?" (foo? (make-foo 'a)))
  (test-assert "bar?(2)" (bar? (make-foo 'a)))
  (test-equal "foo-v" 'a (foo-v (make-foo 'a))))

(test-end)
