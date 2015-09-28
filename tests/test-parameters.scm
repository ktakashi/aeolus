(import (except (scheme base) define-record-type)
	(aeolus modes parameters)
	(aeolus misc record)
	(aeolus-test))

(test-begin "Mode parameters")

(let ()
  (define-record-type (<bar> make-bar bar?))
  (define-record-type (<foo> make-foo foo?)
    (parent <bar>)
    (protocol (lambda (p) (lambda (v) (p v))))
    (fields (immutable v foo-v)))
  (test-assert "foo?" (foo? (make-foo 'a))))

(let ()
  (define-mode-parameter foo-parameter make-foo-parameter foo-parameter?
    (foo parameter-foo))
  (test-assert "foo-parameter?" (foo-parameter? (make-foo-parameter "s")))
  ;; (test-error  "field type" (make-foo-parameter #u8(1 2 3)))
  (let ((foo (make-foo-parameter "s")))
    (test-assert "mode-parameter?" (mode-parameter? foo))
    (test-equal "parameter-foo" "s" (parameter-foo foo))
    (test-error "parameter-iv on foo" (parameter-iv foo))
    (let ((composite (make-composite-parameter foo (make-iv-paramater #u8(1)))))
      (test-assert "mode-parameter? (composite)" composite)
      (test-equal "parameter-foo (2)" "s" (parameter-foo composite))
      (test-equal "parameter-iv" #u8(1) (parameter-iv composite)))))

(test-assert "ctr parameter (1)" 
	     (counter-parameter? (make-counter-parameter 'big)))
(test-assert "ctr parameter (2)" 
	     (counter-parameter? (make-counter-parameter '(big rfc3686))))
(test-error "ctr parameter (error)"  (make-counter-parameter 'unknown))


(test-end)
