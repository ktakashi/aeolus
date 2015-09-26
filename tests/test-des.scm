(import (scheme base)
	(aeolus cipher)
	(aeolus cipher des)
	(aeolus modes ecb))

(cond-expand
 ((library (rnrs))    (import (rename (rnrs) (bitwise-arithmetic-shift arithmetic-shift))))
 ((library (srfi 60)) (import (srfi 60)))
 ((library (srfi 33)) (import (srfi 33)))
 (else                (error "bitwise library not found")))

;; sign...
(cond-expand
 ((library (srfi 64))
  (import (srfi 64)))
 ((library (chibi test))
  ;; test-equal in (chibi test) contains a bug
  ;; (the macro can't be expanded properly)
  ;; so make thin wrapper
  (import (except (chibi test) test-equal))
  (begin
    (define-syntax test-equal
      (syntax-rules ()
	((_ name expect expr)
	 (test name expect expr))
	((_ expect expr)
	 (test-equal 'expr expect expr))))))
 ((library (gauche test))
  (import (rename (gauche test)
		  (test-start test-begin)
		  (test-section test-group)))
  (begin
    (define-syntax test-equal
      (syntax-rules ()
	((_ name expect expr)
	 (test* name expect expr))
	((_ expect expr)
	 (test-equal 'expr expect expr))))
    (define-syntax test-assert
      (syntax-rules ()
	((_ name expr)
	 (test* name #t expr))
	((_ expect expr)
	 (test-assert 'expr expr))))))
 (else
  (import (scheme write))
  (begin
    (define (test-begin . o) #f)
    (define (test-end . o) #f)
    (define-syntax test
      (syntax-rules ()
	((test expected expr)
	 (let ((res expr))
	   (cond
	    ((not (equal? expr expected))
	     (display "FAIL: ")
	     (write 'expr)
	     (display ": expected ")
	     (write expected)
	     (display " but got ")
	     (write res)
	     (newline))))))))))

(test-begin "DES")

(test-assert "cipher?"
	     (cipher? (make-cipher DES #u8(1 2 3 4 5 6 7 8) *mode-ecb*)))

(define test-vectors
  '(#(#x0000000000000000 #x0000000000000000 #x8CA64DE9C1B123A7)
    #(#xFFFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFF #x7359B2163E4EDC58)
    #(#x3000000000000000 #x1000000000000001 #x958E6E627A05557B)
    #(#x1111111111111111 #x1111111111111111 #xF40379AB9E0EC533)
    #(#x0123456789ABCDEF #x1111111111111111 #x17668DFC7292532D)
    #(#x1111111111111111 #x0123456789ABCDEF #x8A5AE1F81AB8F2DD)
    #(#x0000000000000000 #x0000000000000000 #x8CA64DE9C1B123A7)
    #(#xFEDCBA9876543210 #x0123456789ABCDEF #xED39D950FA74BCC4)
    #(#x7CA110454A1A6E57 #x01A1D6D039776742 #x690F5B0D9A26939B)
    #(#x0131D9619DC1376E #x5CD54CA83DEF57DA #x7A389D10354BD271)
    #(#x07A1133E4A0B2686 #x0248D43806F67172 #x868EBB51CAB4599A)
    #(#x3849674C2602319E #x51454B582DDF440A #x7178876E01F19B2A)
    #(#x04B915BA43FEB5B6 #x42FD443059577FA2 #xAF37FB421F8C4095)
    #(#x0113B970FD34F2CE #x059B5E0851CF143A #x86A560F10EC6D85B)
    #(#x0170F175468FB5E6 #x0756D8E0774761D2 #x0CD3DA020021DC09)
    #(#x43297FAD38E373FE #x762514B829BF486A #xEA676B2CB7DB2B7A)
    #(#x07A7137045DA2A16 #x3BDD119049372802 #xDFD64A815CAF1A0F)
    #(#x04689104C2FD3B2F #x26955F6835AF609A #x5C513C9C4886C088)
    #(#x37D06BB516CB7546 #x164D5E404F275232 #x0A2AEEAE3FF4AB77)
    #(#x1F08260D1AC2465E #x6B056E18759F5CCA #xEF1BF03E5DFA575A)
    #(#x584023641ABA6176 #x004BD6EF09176062 #x88BF0DB6D70DEE56)
    #(#x025816164629B007 #x480D39006EE762F2 #xA1F9915541020B56)
    #(#x49793EBC79B3258F #x437540C8698F3CFA #x6FBF1CAFCFFD0556)
    #(#x4FB05E1515AB73A7 #x072D43A077075292 #x2F22E49BAB7CA1AC)
    #(#x49E95D6D4CA229BF #x02FE55778117F12A #x5A6B612CC26CCE4A)
    #(#x018310DC409B26D6 #x1D9D5C5018F728C2 #x5F4C038ED12B2E41)
    #(#x1C587F1C13924FEF #x305532286D6F295A #x63FAC0D034D9F793)
    #(#x0101010101010101 #x0123456789ABCDEF #x617B3A0CE8F07100)
    #(#x1F1F1F1F0E0E0E0E #x0123456789ABCDEF #xDB958605F8C8C606)
    #(#xE0FEE0FEF1FEF1FE #x0123456789ABCDEF #xEDBFD1C66C29CCC7)
    #(#x0000000000000000 #xFFFFFFFFFFFFFFFF #x355550B2150E2451)
    #(#xFFFFFFFFFFFFFFFF #x0000000000000000 #xCAAAAF4DEAF1DBAE)
    #(#x0123456789ABCDEF #x0000000000000000 #xD5D44FF720683D0D)
    #(#xFEDCBA9876543210 #xFFFFFFFFFFFFFFFF #x2A2BB008DF97C2F2)
    #(#x7CA110454A1A6E57 #x01A1D6D039776742 #x690F5B0D9A26939B)
    #(#x0131D9619DC1376E #x5CD54CA83DEF57DA #x7A389D10354BD271)
    #(#x07A1133E4A0B2686 #x0248D43806F67172 #x868EBB51CAB4599A)
    #(#x3849674C2602319E #x51454B582DDF440A #x7178876E01F19B2A)
    #(#x04B915BA43FEB5B6 #x42FD443059577FA2 #xAF37FB421F8C4095)
    #(#x0113B970FD34F2CE #x059B5E0851CF143A #x86A560F10EC6D85B)
    #(#x0170F175468FB5E6 #x0756D8E0774761D2 #x0CD3DA020021DC09)
    #(#x43297FAD38E373FE #x762514B829BF486A #xEA676B2CB7DB2B7A)
    #(#x07A7137045DA2A16 #x3BDD119049372802 #xDFD64A815CAF1A0F)
    #(#x04689104C2FD3B2F #x26955F6835AF609A #x5C513C9C4886C088)
    #(#x37D06BB516CB7546 #x164D5E404F275232 #x0A2AEEAE3FF4AB77)
    #(#x1F08260D1AC2465E #x6B056E18759F5CCA #xEF1BF03E5DFA575A)
    #(#x584023641ABA6176 #x004BD6EF09176062 #x88BF0DB6D70DEE56)
    #(#x025816164629B007 #x480D39006EE762F2 #xA1F9915541020B56)
    #(#x49793EBC79B3258F #x437540C8698F3CFA #x6FBF1CAFCFFD0556)
    #(#x4FB05E1515AB73A7 #x072D43A077075292 #x2F22E49BAB7CA1AC)
    #(#x49E95D6D4CA229BF #x02FE55778117F12A #x5A6B612CC26CCE4A)
    #(#x018310DC409B26D6 #x1D9D5C5018F728C2 #x5F4C038ED12B2E41)
    #(#x1C587F1C13924FEF #x305532286D6F295A #x63FAC0D034D9F793)))

(define (integer->bytevector integer size)
  (let ((bv (make-bytevector size 0)))
    (do ((i 0 (+ i 1)))
	((= i size) bv)
      (let ((n (bitwise-and (arithmetic-shift integer (* i -8)) #xFF)))
	(bytevector-u8-set! bv (- size i 1) n)))))

(define (des-ecb-test cipher-maker)
  (lambda (vec)
    (let ((key (integer->bytevector (vector-ref vec 0) 8))
	  (plain (integer->bytevector (vector-ref vec 1) 8))
	  (ct (integer->bytevector (vector-ref vec 2) 8)))
      ;; it's ECB so we can reuse it
      (define cipher (cipher-maker key))
      
      (test-equal (string-append "encrypt: " 
				 (number->string (vector-ref vec 1) 16)
				 " -> "
				 (number->string (vector-ref vec 2) 16))
		  ct
		  (cipher-encrypt cipher plain))
      (test-equal (string-append "decrypt: " 
				 (number->string (vector-ref vec 2) 16)
				 " -> "
				 (number->string (vector-ref vec 1) 16))
		  plain
		  (cipher-decrypt cipher ct)))))

(for-each (des-ecb-test (lambda (key) (make-cipher DES key *mode-ecb*)))
	  test-vectors)
(for-each (des-ecb-test (lambda (key)
			  ;; do kinda double des
			  (make-cipher DES3 (bytevector-append key key)
				       *mode-ecb*))) 
			test-vectors)
(for-each (des-ecb-test (lambda (key)
			  ;; do kinda triple des
			  (make-cipher DES3 (bytevector-append key key key)
				       *mode-ecb*))) 
			test-vectors)

(test-end)
