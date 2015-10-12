;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; aeolus/misc/common.sld - Common operations
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

(define-library (aeolus misc common)
  (export byte
	  rol ror rolc rorc
	  load32h 
	  store32h store64h)
  (import (scheme base)
	  (aeolus misc bitwise))
  (begin
    ;; common thing, TODO move to somewhere
    (define-syntax byte
      (syntax-rules ()
	((_ x i)
	 (let ((n (* 8 i)))
	   (bitwise-and #xff (arithmetic-shift x (- n)))))))

    (define (rol x y)
      (define y31 (bitwise-and y 31))
      (let ((n1 (arithmetic-shift x y31))
	    (n2 (bitwise-and (arithmetic-shift (bitwise-and x #xFFFFFFFF) 
					       (- y31 32))
			     #xFFFFFFFF)))
	(bitwise-and (bitwise-ior n1 n2) #xFFFFFFFF)))
    (define (ror x y)
      (define y31 (bitwise-and y 31))
      (let ((n1 (arithmetic-shift (bitwise-and x #xFFFFFFFF) (- y31)))
	    (n2 (arithmetic-shift x (- 32 y31))))
	(bitwise-and (bitwise-ior n1 n2) #xFFFFFFFF)))
    (define rolc rol)
    (define rorc ror)
    )
  ;; load32h
  (cond-expand
   ((library (rnrs))
    (import (only (rnrs) bytevector-u32-ref bytevector-u32-set!
		  bytevector-u64-set!))
    (begin
      (define (load32h bv start)
	(bytevector-u32-ref bv start 'big))
      (define (store32h bv start v)
	(bytevector-u32-set! bv start v 'big))
      (define (store64h bv start v)
	(bytevector-u64-set! bv start v 'big))))
   (else
    (begin
      (define (load32h bv start)
	(bitwise-ior (arithmetic-shift (bytevector-u8-ref bv start) 24)
		     (arithmetic-shift (bytevector-u8-ref bv (+ start 1)) 16)
		     (arithmetic-shift (bytevector-u8-ref bv (+ start 2)) 8)
		     (bytevector-u8-ref bv (+ start 3))))
      (define (store32h bv start v)
	(bytevector-u8-set! bv start
			    (bitwise-and (arithmetic-shift v -24) #xFF))
	(bytevector-u8-set! bv (+ start 1)
			    (bitwise-and (arithmetic-shift v -16) #xFF))
	(bytevector-u8-set! bv (+ start 2)
			    (bitwise-and (arithmetic-shift v -8) #xFF))
	(bytevector-u8-set! bv (+ start 3)
			    (bitwise-and v #xFF)))
      (define (store64h bv start v)
	(bytevector-u8-set! bv start
			    (bitwise-and (arithmetic-shift v -56) #xFF))
	(bytevector-u8-set! bv (+ start 1)
			    (bitwise-and (arithmetic-shift v -48) #xFF))
	(bytevector-u8-set! bv (+ start 2)
			    (bitwise-and (arithmetic-shift v -40) #xFF))
	(bytevector-u8-set! bv (+ start 3)
			    (bitwise-and (arithmetic-shift v -32) #xFF))
	(bytevector-u8-set! bv (+ start 4)
			    (bitwise-and (arithmetic-shift v -24) #xFF))
	(bytevector-u8-set! bv (+ start 5)
			    (bitwise-and (arithmetic-shift v -16) #xFF))
	(bytevector-u8-set! bv (+ start 6)
			    (bitwise-and (arithmetic-shift v -8) #xFF))
	(bytevector-u8-set! bv (+ start 7)
			    (bitwise-and v #xFF))))))
)
	  
