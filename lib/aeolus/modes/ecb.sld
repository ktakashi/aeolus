;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; aeolus/modes/ecb.sld - ECB mode
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

(define-library (aeolus modes ecb)
  (export mode-ecb)
  (import (scheme base)
	  (aeolus cipher descriptor)
	  (aeolus modes descriptor))
  (begin
    (define-record-type <symmetric-ecb> (make-ecb spec key blocklen)
      symmetric-ecb?
      (spec ecb-cipher-spec)
      (key  ecb-cipher-key)
      (blocklen ecb-cipher-blocklen)) ;; well not needed but for convenience

    ;; spec is a vector
    (define (ecb-start spec key param)
      (let ((skey ((cipher-descriptor-setup spec) key 
		   (cipher-descriptor-default-round spec)))
	    (blocklen (cipher-descriptor-block-size spec)))
	(make-ecb spec skey blocklen)))

    (define (ecb-encrypt ecb pt)
      (define blocklen (ecb-cipher-blocklen ecb))
      (define pt-len (bytevector-length pt))
      (unless (zero? (modulo pt-len blocklen))
	(error "ecb-encrypt: invalid argument"))
      (let ((ct (make-bytevector (bytevector-length pt)))
	    (encrypt (cipher-descriptor-encrypt (ecb-cipher-spec ecb)))
	    (key (ecb-cipher-key ecb)))
	(let loop ((i 0))
	  (if (= i pt-len)
	      ct
	      (let ((b (encrypt pt i ct i key)))
		(unless (= b blocklen) 
		  (error "ecb-encrypt: invalid encryption"))
		(loop (+ i blocklen)))))))
	     
    (define (ecb-decrypt ecb ct)
      (define blocklen (ecb-cipher-blocklen ecb))
      (define ct-len (bytevector-length ct))
      (unless (zero? (modulo ct-len blocklen))
	(error "ecb-decrypt: invalid argument"))
      (let ((pt (make-bytevector (bytevector-length ct)))
	    (decrypt (cipher-descriptor-decrypt (ecb-cipher-spec ecb)))
	    (key (ecb-cipher-key ecb)))
	(let loop ((i 0))
	  (if (= i ct-len)
	      pt
	      (let ((b (decrypt ct i pt i key)))
		(unless (= b blocklen)
		  (error "ecb-decrypt: invalid encryption"))
		(loop (+ i blocklen)))))))

    (define (ecb-done ecb)
      ((cipher-descriptor-done (ecb-cipher-spec ecb)) (ecb-cipher-key ecb)))

    (define (mode-ecb)
      (make-mode-descriptor ecb-start ecb-encrypt ecb-decrypt 
			    #f #f #f ecb-done))
    )
)
