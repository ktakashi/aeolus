(import (scheme base)
	(aeolus modes parameters)
	(aeolus-test))

(test-begin "Mode parameters")

(let ()
  (define-mode-parameter foo-parameter (make-foo-parameter foo) foo-parameter?
    (foo parameter-foo string?))
  (test-assert "foo-parameter?" (foo-parameter? (make-foo-parameter "s")))
  (test-error  "field type" (make-foo-parameter #u8(1 2 3)))
  (let ((foo (make-foo-parameter "s")))
    (test-assert "mode-parameter?" (mode-parameter? foo))
    (test-equal "parameter-foo" "s" (parameter-foo foo))
    (test-error "parameter-iv on foo" (parameter-iv foo))
    (let ((composite (make-composite-parameter foo (make-iv-paramater #u8(1)))))
      (test-assert "mode-parameter? (composite)" composite)
      (test-equal "parameter-foo (2)" "s" (parameter-foo composite))
      (test-equal "parameter-iv" #u8(1) (parameter-iv composite)))))

(test-end)