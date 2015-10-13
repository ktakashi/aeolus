;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; aeolus/digest/sha1.sld - SHA1 hash implementation
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

(define-library (aeolus digest sha1)
  (export SHA1)
  (import (except (scheme base) define-record-type)
	  (aeolus digest descriptor)
	  (aeolus misc record)
	  (aeolus misc bitwise)
	  (aeolus misc common))
  (begin
    (define-record-type (<sha> make-sha1-state sha1-state?)
      (parent <digest-base>)
      (fields (mutable s0 sha1-s0 sha1-s0-set!)
	      (mutable s1 sha1-s1 sha1-s1-set!)
	      (mutable s2 sha1-s2 sha1-s2-set!)
	      (mutable s3 sha1-s3 sha1-s3-set!)
	      (mutable s4 sha1-s4 sha1-s4-set!))
      (protocol (lambda (p)
		  (lambda ()
		    ((p (make-bytevector 64 0))
		     #x67452301
		     #xefcdab89
		     #x98badcfe 
		     #x10325476
		     #xc3d2e1f0)))))

    (define (sha1-compress sha1 buffer start)
      (define W (make-vector 80 0))
      ;; copy the state into 512 bits into W[0...15]
      (do ((i 0 (+ i 1))) ((= i 16))
	(vector-set! W i (load32h buffer (+ start (* i 4)))))
      ;; expand
      (do ((i 16 (+ i 1))) ((= i 80))
	(vector-set! W i (rol (bitwise-xor (vector-ref W (- i 3))
					   (vector-ref W (- i 8))
					   (vector-ref W (- i 14))
					   (vector-ref W (- i 16)))
			      1)))

      (let ((a (sha1-s0 sha1))
	    (b (sha1-s1 sha1))
	    (c (sha1-s2 sha1))
	    (d (sha1-s3 sha1))
	    (e (sha1-s4 sha1)))
	(define (f0 x y z) (bitwise-xor z (bitwise-and x (bitwise-xor y z))))
	(define (f1 x y z) (bitwise-xor x y z))
	(define (f2 x y z) 
	  (let ((xy (bitwise-and x y)))
	    (bitwise-ior xy (bitwise-and z (bitwise-ior x y)))))
	(define f3 f1)
	(define (make-ff fn magic)
	  (lambda (a b c d e i)
	    (let ((e (+ (rolc a 5) (fn b c d) e (vector-ref W i) magic))
		  (b (rolc b 30)))
	      (values e b))))
	;; returns next e and b (position)
	(define ff0 (make-ff f0 #x5a827999))
	(define ff1 (make-ff f1 #x6ed9eba1))
	(define ff2 (make-ff f2 #x8f1bbcdc))
	(define ff3 (make-ff f3 #xca62c1d6))
	(define (do-compress ff a b c d e start end)
	  (let loop ((i start) (a a) (b b) (c c) (d d) (e e))
	    (if (= i end)
		(values a b c d e)
		(let*-values (((e b) (ff a b c d e    i   ))
			      ((d a) (ff e a b c d (+ i 1)))
			      ((c e) (ff d e a b c (+ i 2)))
			      ((b d) (ff c d e a b (+ i 3)))
			      ((a c) (ff b c d e a (+ i 4))))
		  (loop (+ i 5) a b c d e)))))
	(let*-values (((a b c d e) (do-compress ff0 a b c d e  0 20))
		      ((a b c d e) (do-compress ff1 a b c d e 20 40))
		      ((a b c d e) (do-compress ff2 a b c d e 40 60))
		      ((a b c d e) (do-compress ff3 a b c d e 60 80)))
	  (sha1-s0-set! sha1 (bitwise-and (+ (sha1-s0 sha1) a) #xFFFFFFFF))
	  (sha1-s1-set! sha1 (bitwise-and (+ (sha1-s1 sha1) b) #xFFFFFFFF))
	  (sha1-s2-set! sha1 (bitwise-and (+ (sha1-s2 sha1) c) #xFFFFFFFF))
	  (sha1-s3-set! sha1 (bitwise-and (+ (sha1-s3 sha1) d) #xFFFFFFFF))
	  (sha1-s4-set! sha1 (bitwise-and (+ (sha1-s4 sha1) e) #xFFFFFFFF)))))

    (define-digest-process sha1-process! sha1-compress 64)

    (define (sha1-done! sha1 out . optional)
      ;; FIXME we should move this somewhere
      (define start (if (null? optional) 0 (car optional)))
      (define buffer (digest-base-buffer sha1))

      (when (>= (digest-base-current sha1) (bytevector-length buffer))
	(error "sha1-done: invalid argument"))
      (digest-base-length-set! sha1 
			       (+ (digest-base-length sha1)
				  (* (digest-base-current sha1) 8)))

      (let ((c (digest-base-current sha1)))
	(define (next-c c)
	  (if (> c 56)
	      (do ((c c (+ c 1)))
		  ((= c 64)
		   (digest-base-current-set! sha1 64) ;; blocksize
		   (sha1-compress sha1 buffer 0)
		   (digest-base-current-set! sha1 0)
		   0)
		(bytevector-u8-set! buffer c 0))
	      c))
	(define (do-final sha1 c)
	  (store64h buffer 56 (digest-base-length sha1))
	  (sha1-compress sha1 buffer 0)
	  (store32h out    start     (sha1-s0 sha1))
	  (store32h out (+ start 4 ) (sha1-s1 sha1))
	  (store32h out (+ start 8 ) (sha1-s2 sha1))
	  (store32h out (+ start 12) (sha1-s3 sha1))
	  (store32h out (+ start 16) (sha1-s4 sha1))
	  out)
	;; append the '1' bit
	(bytevector-u8-set! buffer c #x80)
	(let loop ((c (next-c (+ c 1))))
	  (if (= c 56)
	      (do-final sha1 c)
	      (begin
		(bytevector-u8-set! buffer c 0)
		(loop (+ c 1)))))))
    (define (SHA1)
      (make-digest-descriptor 20 64 "1.3.14.3.2.26" 
			      make-sha1-state
			      sha1-process!
			      sha1-done!))


    )
  )
