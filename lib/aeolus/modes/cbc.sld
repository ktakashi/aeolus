;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; aeolus/modes/cbc.sld - CBC mode
;;;
;;;  Copyright (c) 2015  Takashi Kato  <ktakashi@ymail.com>
;;;  
;;;  Redistribution and use in source and binary forms, with or without
;;;  modification, are permitted provided that the following conditions
;;;  are met:
;;;  
;;;  1. Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;  
;;;  2. Redistributions in binary form must reproduce the above copyright
;;;     notice, this list of conditions and the following disclaimer in the
;;;     documentation and/or other materials provided with the distribution.
;;;  
;;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(define-library (aeolus modes cbc)
  (export mode-cbc)
  (import (scheme base)
	  (aeolus modes descriptor)
	  (aeolus modes parameters)
	  (aeolus cipher descriptor)
	  (aeolus misc bitwise))
  (begin
    (define-record-type <symmetric-cbc> (make-cbc spec iv key blocklen)
      symmetric-cbc?
      (spec cbc-cipher-spec)
      (iv   cbc-cipher-iv)
      (key  cbc-cipher-key)
      (blocklen cbc-cipher-blocklen)) ;; well not needed but for convenience

    ;; spec is a vector
    (define (cbc-start spec key param)
      (unless (iv-parameter? param) (error "cbc-start: CBC requires IV"))
      (let ((skey ((cipher-descriptor-setup spec) key 
		   (cipher-descriptor-default-round spec)))
	    (blocklen (cipher-descriptor-block-size spec)))
	(make-cbc spec (parameter-iv param) skey blocklen)))

    (define (cbc-setiv cbc iv)
      (unless (= (bytevector-length iv) (cbc-cipher-blocklen cbc))
	(error 'cbc-setiv "Invalid argument"))
      (bytevector-copy! (cbc-cipher-iv cbc) 0 iv))

    (define (cbc-getiv cbc) (bytevector-copy (cbc-cipher-iv cbc)))

    (define (cbc-encrypt cbc pt)
      (define blocklen (cbc-cipher-blocklen cbc))
      (define pt-len (bytevector-length pt))
      (define iv (cbc-cipher-iv cbc))
      (define (bv-xor pt start iv)
	(do ((i 0 (+ i 1)))
	    ((= i blocklen) iv)
	  (let ((v (bitwise-xor (bytevector-u8-ref iv i)
				(bytevector-u8-ref pt (+ start i)))))
	    (bytevector-u8-set! iv i v))))
      (unless (zero? (modulo pt-len blocklen))
	(error "cbc-encrypt: invalid argument"))
      (let ((ct (make-bytevector (bytevector-length pt)))
	    (encrypt (cipher-descriptor-encrypt (cbc-cipher-spec cbc)))
	    (key (cbc-cipher-key cbc)))
	(let loop ((i 0))
	  (if (= i pt-len)
	      ct
	      (let ((b (encrypt (bv-xor pt i iv) i ct i key)))
		(unless (= b blocklen) 
		  (error "cbc-encrypt: invalid encryption"))
		(do ((j 0 (+ j 1)))
		    ((= j blocklen))
		  (bytevector-u8-set! iv i (bytevector-u8-ref ct (+ i j))))
		(loop (+ i blocklen)))))))
	     
    (define (cbc-decrypt cbc ct)
      (define blocklen (cbc-cipher-blocklen cbc))
      (define ct-len (bytevector-length ct))
      (define iv (cbc-cipher-iv cbc))
      (unless (zero? (modulo ct-len blocklen))
	(error "cbc-decrypt: invalid argument"))
      (let ((pt (make-bytevector (bytevector-length ct)))
	    (decrypt (cipher-descriptor-decrypt (cbc-cipher-spec cbc)))
	    (key (cbc-cipher-key cbc))
	    (tmp (make-bytevector blocklen)))
	(let loop ((i 0))
	  (if (= i ct-len)
	      pt
	      (let ((b (decrypt ct i tmp 0 key)))
		(unless (= b blocklen)
		  (error "cbc-decrypt: invalid encryption"))
		(do ((j 0 (+ j 1)))
		    ((= j blocklen))
		  (let ((t (bitwise-xor (bytevector-u8-ref tmp j)
					(bytevector-u8-ref iv j))))
		    (bytevector-u8-set! iv i (bytevector-u8-ref ct (+ i j)))
		    (bytevector-u8-set! pt (+ i j) t)))
		(loop (+ i blocklen)))))))

    (define (cbc-done cbc)
      ((cipher-descriptor-done (cbc-cipher-spec cbc)) (cbc-cipher-key cbc)))

    (define (mode-cbc) 
      (make-mode-descriptor cbc-start cbc-encrypt cbc-decrypt
			    cbc-setiv cbc-getiv #f cbc-done))
    )
)
