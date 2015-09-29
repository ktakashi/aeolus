(import (except (scheme base) define-record-type)
	(aeolus modes parameters)
	(aeolus misc record)
	(aeolus-test))

(test-begin "Mode parameters")

(let ()
  (define-mode-parameter foo-parameter make-foo-parameter foo-parameter?
    (foo parameter-foo))
  (test-assert "foo-parameter?" (foo-parameter? (make-foo-parameter "s")))

  (let ((foo (make-foo-parameter "s")))
    (test-assert "mode-parameter?" (mode-parameter? foo))
    (test-equal "parameter-foo" "s" (parameter-foo foo))
    (test-error "parameter-iv on foo" (parameter-iv foo))
    (let ((composite (make-composite-parameter foo (make-iv-paramater #u8(1)))))
      (test-assert "mode-parameter? (composite)" composite)
      (test-equal "parameter-foo (2)" "s" (parameter-foo composite))
      (test-equal "parameter-iv" #u8(1) (parameter-iv composite)))))

(test-assert "ctr parameter (1)" 
	     (counter-parameter? (make-counter-parameter #u8() 'big)))
(test-assert "ctr parameter (2)" 
	     (counter-parameter? (make-counter-parameter #u8() 'big)))
(test-assert "ctr parameter (3)" 
	     (counter-parameter? (make-counter-parameter #u8() 'big 0)))

(test-error "ctr parameter (error)"  (make-counter-parameter #u8() 'unknown))


(test-end)
