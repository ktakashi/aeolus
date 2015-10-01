;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; aeolus/modes/ctr.sld - CTR mode
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

(define-library (aeolus modes ctr)
  (export mode-ctr)
  (import (scheme base)
	  (aeolus modes descriptor)
	  (aeolus modes parameters)
	  (aeolus cipher descriptor)
	  (aeolus misc bitwise))
  (begin
    (define-record-type <symmetric-ctr> 
      (make-ctr spec ctr pad index key big? blocklen)
      symmetric-ctr?
      (spec ctr-cipher-spec)
      (ctr  ctr-cipher-ctr)
      (pad  ctr-cipher-pad)
      ;; index of current padding vector
      (index  ctr-cipher-index ctr-cipher-index-set!)
      (key  ctr-cipher-key)
      (big? ctr-cipher-big?)	      ;; big endian (should we support little?)
      (blocklen ctr-cipher-blocklen)) ;; well not needed but for convenience

    ;; spec is a vector
    (define (ctr-start spec key param)
      (unless (iv-parameter? param) (error "ctr-start: CTR requires IV"))
      (let ((skey ((cipher-descriptor-setup spec) key 
		   (parameter-round param 0)))
	    (blocklen (cipher-descriptor-block-size spec))
	    (iv (parameter-iv param)))
	(unless (= (bytevector-length iv) blocklen)
	  (error "ctr-start: invalid IV size"))
	(let (;; counter size is always blocklen
	      ;; NOTE: if RFC 3686 is specified with block size less than
	      ;; 16, this will be truncated. (that's for AES CTR so it
	      ;; must be users' responsitility.)
	      (ctr (bytevector-copy iv 0 blocklen))
	      (big? (eq? (parameter-endian param 'big) 'big))
	      (encrypt (cipher-descriptor-encrypt spec))
	      (pad (make-bytevector blocklen)))
	  ;; initialise pad
	  (unless (= (encrypt ctr 0 pad 0 skey) blocklen)
	    (error "ctr-start: failed to set IV"))
	  (make-ctr spec ctr pad 0 skey big? blocklen))))

    (define (ctr-setiv ctr iv)
      (define blocklen (ctr-cipher-blocklen ctr))
      (unless (= (bytevector-length iv) blocklen)
	(error 'ctr-setiv "Invalid argument"))
      ;; copy it
      (bytevector-copy! (ctr-cipher-ctr ctr) 0 iv 0)
      (let ((encrypt (cipher-descriptor-encrypt (ctr-cipher-spec ctr))))
	(unless (= (encrypt iv 0 (ctr-cipher-pad ctr) 0 (ctr-cipher-key ctr)) 
		   blocklen)
	  (error "ctr-setiv: failed to set IV"))
	;; forse resetting index
	(ctr-cipher-index-set! ctr 0)))

    (define (ctr-getiv ctr) (bytevector-copy (ctr-cipher-ctr ctr)))

    (define (ctr-encrypt ctr pt)
      (define blocklen (ctr-cipher-blocklen ctr))
      (define pt-len (bytevector-length pt))
      (define ctr-bv (ctr-cipher-ctr ctr))
      (define ctr-len (bytevector-length ctr-bv))
      (define pad    (ctr-cipher-pad ctr))
      (define big? (ctr-cipher-big? ctr))

      (define (ctr-xor pt ct start pad index)
	(do ((i 0 (+ i 1)) (index index (+ index 1)))
	    ((or (= (+ i start) pt-len) (= index blocklen)) (values i index))
	  (let ((ptc (bytevector-u8-ref pt (+ i start)))
		(pac (bytevector-u8-ref pad index)))
	    (bytevector-u8-set! ct (+ i start) (bitwise-xor ptc pac)))))

      (let ((ct (make-bytevector (bytevector-length pt)))
	    (encrypt (cipher-descriptor-encrypt (ctr-cipher-spec ctr)))
	    (key (ctr-cipher-key ctr)))
	(let loop ((i 0))
	  (cond ((= i pt-len) ct)
		(else
		 (let-values (((t next-index)
			       (ctr-xor pt ct i pad (ctr-cipher-index ctr))))
		   (cond ((= next-index blocklen)
			  ;; ok increament it
			  (if big?
			      (let loop ((i (- blocklen 1)))
				(unless (< i 0)
				  (let ((x (bytevector-u8-ref ctr-bv i)))
				    (bytevector-u8-set! ctr-bv i
				      (bitwise-and (+ x 1) #xFF))
				    (when (zero? (bytevector-u8-ref ctr-bv i))
				      (loop (- i 1))))))
			      (let loop ((i 0))
				(when (< i blocklen)
				  (let ((x (bytevector-u8-ref ctr-bv i)))
				    (bytevector-u8-set! ctr-bv i
				      (bitwise-and (+ x 1) #xFF))
				    (when (zero? (bytevector-u8-ref ctr-bv i))
				      (loop (+ i 1)))))))
			  (ctr-cipher-index-set! ctr 0))
			 (else (ctr-cipher-index-set! ctr next-index)))
		     (unless (= (encrypt ctr-bv 0 pad 0 key) blocklen)
		       (error "ctr-encrypt: invalid encryption"))
		   (loop (+ i t))))))))
    
    (define (ctr-done ctr)
      ((cipher-descriptor-done (ctr-cipher-spec ctr)) (ctr-cipher-key ctr)))

    (define (mode-ctr) 
      (make-mode-descriptor ctr-start ctr-encrypt ctr-encrypt
			    ctr-setiv ctr-getiv #f ctr-done))
    )
)
