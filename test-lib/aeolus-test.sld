(define-library (aeolus-test)
  (export test-begin
	  test-end
	  test-assert
	  test-equal
	  test-error)
  ;; sign...
  (cond-expand
   ((library (srfi 64))
    (import (srfi 64)))
   ((library (chibi test))
    ;; test-equal in (chibi test) contains a bug
    ;; (the macro can't be expanded properly)
    ;; so make thin wrapper
    (import (except (chibi test) test-equal) (scheme base))
    (begin
      (define-syntax test-equal
	(syntax-rules ()
	  ((_ name expect expr)
	   (test name expect expr))
	  ((_ expect expr)
	   (test-equal 'expr expect expr))))))
   ((library (gauche test))
    (import (scheme base)
	    (rename (gauche test)
		    (test-error %test-error)
		    (test-start test-begin)
		    (test-section test-group)))
    (begin
      (define-syntax test-equal
	(syntax-rules ()
	  ((_ expect expr)
	   (test-equal 'expr expect expr))
	  ((_ name expect expr)
	   (test* name expect expr))))
      (define-syntax test-assert
	(syntax-rules ()
	  ((_ expr)
	   (test-assert 'expr expr))
	  ((_ name expr)
	   (test* name #t (and expr #t)))))
      (define (test-error-compare? e r) (test-error? r))
      (define-syntax test-error
	(syntax-rules ()
	  ((_ expr)
	   (test-error 'expr expr))
	  ((_ name expr)
	   (test* name 'dummy expr test-error-compare?))))))
   (else
    (import (scheme base) (scheme write))
    (begin
      (define (test-begin . o) #f)
      (define (test-end . o) #f)
      (define-syntax test-assert
	(syntax-rules ()
	  ((_ expr) (test-assert 'expr expr))
	  ((_ name expr)
	   (guard (e (else (display "FAIL: ") (write name) (newline)))
	     (or expr)))))
      (define-syntax test-equal
	(syntax-rules ()
	  ((_ expected expr) (test-equal 'expr expected expr))
	  ((_ name expected expr)
	   (let ((res expr))
	     (cond
	      ((not (equal? res expected))
	       (display "FAIL: ") (display name) (newline)
	       (display "   expected ") (write expected) (newline)
	       (display "   but got ")  (write res) (newline)))))))))))