;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; aeolus/cipher/des.sld - DES cipher implementation
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

;; the exporting cipher implementation must be an vector
;; which contains the followings
;;  - min key length
;;  - max key length
;;  - block size
;;  - default number of rounds
;;  - setup procedure
;;  - encryption procedure (ECB)
;;  - decryption procedure (ECB)
;;  - done procedure

(define-library (aeolus cipher des)
  (export DES DES3
	  (rename DES3 DESede))
  (import (scheme base))
  ;; TODO create bitwise library to handle this
  (cond-expand
   ((library (srfi 60)) (import (srfi 60)))
   ((library (srfi 33)) (import (srfi 33)))
   ((library (rnrs))
    (import (rename (rnrs) (bitwise-arithmetic-shift arithmetic-shift))))
   (else                (error "bitwise library not found")))
  (include "schedule.scm")
  (include "operations.scm")
  (begin
    (define-record-type <des-key> (%make-des-key ek dk) des-key?
      (ek des-key-ek)
      (dk des-key-dk))

    (define (make-des-key)
      (%make-des-key (make-vector 32) (make-vector 32)))
	  

    ;; the code is based on libtomcrypt
    (define (des-setup key round)
      (unless (or (zero? round) (= round 16))
	;; TODO use specific condition if implementation supports
	;;      (rnrs) or SRFI-35
	(error "des-setup: invalid round count" round))
      (unless (= (bytevector-length key) 8)
	(error "des-setup: invalid key size" (bytevector-length key)))
      (let ((symmetric-key (make-des-key)))
	(deskey key 0 #f (des-key-ek symmetric-key))
	(deskey key 0 #t (des-key-dk symmetric-key))
	symmetric-key))

    (define (des-encrypt pt ps ct cs key)
      (define work (make-vector 2))
      (vector-set! work 0 (load32h pt (+ ps 0)))
      (vector-set! work 1 (load32h pt (+ ps 4)))
      (desfunc work (des-key-ek key))
      (store32h ct (+ cs 0) (vector-ref work 0))
      (store32h ct (+ cs 4) (vector-ref work 1))
      8)
    (define (des-decrypt ct cs pt ps key)
      (define work (make-vector 2))
      (vector-set! work 0 (load32h ct (+ cs 0)))
      (vector-set! work 1 (load32h ct (+ cs 4)))
      (desfunc work (des-key-dk key))
      (store32h pt (+ ps 0) (vector-ref work 0))
      (store32h pt (+ ps 4) (vector-ref work 1))
      8)

    ;; nothing to be done
    (define (des-done key) #t)

    (define (DES)
      (vector 8 8 8 16
	      des-setup
	      des-encrypt
	      des-decrypt
	      des-done))

    (define-record-type <des3-key> 
      (%make-des3-key ek0 ek1 ek2 dk0 dk1 dk2) des3-key?
      (ek0 des3-key-ek0)
      (ek1 des3-key-ek1)
      (ek2 des3-key-ek2)
      (dk0 des3-key-dk0)
      (dk1 des3-key-dk1)
      (dk2 des3-key-dk2))

    (define (make-des3-key)
      (%make-des3-key (make-vector 32) (make-vector 32) (make-vector 32)
		      (make-vector 32) (make-vector 32) (make-vector 32)))

    (define (des3-setup key round)
      (define keylen (bytevector-length key))
      (unless (or (zero? round) (= round 16))
	;; TODO use specific condition if implementation supports
	;;      (rnrs) or SRFI-35
	(error "des-setup: invalid round count" round))
      (unless (or (= keylen 24) (= keylen 16))
	(error "des-setup: invalid key size" keylen))
      (let ((symmetric-key (make-des3-key)))
	(deskey key 0 #f (des3-key-ek0 symmetric-key)) ;; en
	(deskey key 8 #t (des3-key-ek1 symmetric-key)) ;; de
	(if (= keylen 24)
	    (deskey key 16 #f (des3-key-ek2 symmetric-key))  ;; en
	    ;; 2 key 3DES, k3=k1
	    (deskey key 0  #f (des3-key-ek2 symmetric-key))) ;; en
	(deskey key 0 #t (des3-key-dk0 symmetric-key)) ;; en
	(deskey key 8 #f (des3-key-dk1 symmetric-key)) ;; de
	(if (= keylen 24)
	    (deskey key 16 #t (des3-key-dk2 symmetric-key))  ;; en
	    ;; 2 key 3DES, k3=k1
	    (deskey key 0  #t (des3-key-dk2 symmetric-key))) ;; en
	symmetric-key))

    (define (des3-encrypt pt ps ct cs key)
      (define work (make-vector 2))
      (vector-set! work 0 (load32h pt (+ ps 0)))
      (vector-set! work 1 (load32h pt (+ ps 4)))
      (desfunc work (des3-key-ek0 key))
      (desfunc work (des3-key-ek1 key))
      (desfunc work (des3-key-ek2 key))
      (store32h ct (+ cs 0) (vector-ref work 0))
      (store32h ct (+ cs 4) (vector-ref work 1))
      8)
    (define (des3-decrypt pt ps ct cs key)
      (define work (make-vector 2))
      (vector-set! work 0 (load32h pt (+ ps 0)))
      (vector-set! work 1 (load32h pt (+ ps 4)))
      (desfunc work (des3-key-dk0 key))
      (desfunc work (des3-key-dk1 key))
      (desfunc work (des3-key-dk2 key))
      (store32h ct (+ cs 0) (vector-ref work 0))
      (store32h ct (+ cs 4) (vector-ref work 1))
      8)
    
    
    (define (DES3)
      (vector 24 24 8 16 
	      des3-setup
	      des3-encrypt
	      des3-decrypt
	      des-done))
      
    )
)

