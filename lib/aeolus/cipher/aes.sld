;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; aeolus/cipher/aes.sld - AES cipher implementation
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

(define-library (aeolus cipher aes)
  (export AES)
  (import (scheme base) 
	  (aeolus cipher descriptor)
	  (aeolus misc bitwise)
	  (aeolus misc common))
  (include "aes/table.scm")
  (begin
    (define-record-type <aes-key> (%make-aes-key ek dk nr) aes-key?
      (ek aes-key-ek) ;; ulong 32
      (dk aes-key-dk) ;; ulong 32
      (nr aes-key-nr))

    (define (make-aes-key nr) 
      (%make-aes-key (make-vector 60) (make-vector 60) nr))

    (define (setup-mix temp)
      (bitwise-xor (vector-ref Te4_3 (byte temp 2))
		   (vector-ref Te4_2 (byte temp 1))
		   (vector-ref Te4_1 (byte temp 0))
		   (vector-ref Te4_0 (byte temp 3))))

    (define (aes-setup key round)
      (define keylen (bytevector-length key))
      (define (create-aes-key key round)
	(case keylen
	  ((16 24 32) => 
	   (lambda (len)
	     (let ((nr (+ 10 (* (- (quotient len 8) 2) 2))))
	       (unless (or (zero? round) (= round nr))
		 (error "aes-setup: invalid round" round nr))
	       (make-aes-key nr))))
	  (else (error "aes-setup: invalid key size" keylen))))
      (define vset! vector-set!)
      (define vref vector-ref)
      (define bxor bitwise-xor)
      (define (setup-ek skey)
	(let ((rk (aes-key-ek skey)))
	  (vset! rk 0 (load32h key 0))
	  (vset! rk 1 (load32h key 4))
	  (vset! rk 2 (load32h key 8))
	  (vset! rk 3 (load32h key 12))
	  (case keylen
	    ((16)
	     (let loop ((i 0) (ri 0))
	       (let ((rk0 (vref rk ri))
		     (rk1 (vref rk (+ ri 1)))
		     (rk2 (vref rk (+ ri 2)))
		     (rk3 (vref rk (+ ri 3))))
		 (vset! rk (+ ri 4) (bxor rk0 (setup-mix rk3) (vref rcon i)))
		 (vset! rk (+ ri 5) (bxor rk1 (vref rk (+ ri 4))))
		 (vset! rk (+ ri 6) (bxor rk2 (vref rk (+ ri 5))))
		 (vset! rk (+ ri 7) (bxor rk3 (vref rk (+ ri 6))))
		 (unless (= (+ i 1) 10)
		   (loop (+ i 1) (+ ri 4))))))
	    ((24) 
	     (vset! rk 4 (load32h key 16))
	     (vset! rk 5 (load32h key 20))
	     (let loop ((i 0) (ri 0))
	       (let ((rk0 (vref rk ri))
		     (rk1 (vref rk (+ ri 1)))
		     (rk2 (vref rk (+ ri 2)))
		     (rk3 (vref rk (+ ri 3)))
		     (rk5 (vref rk (+ ri 5))))
		 (vset! rk (+ ri 6) (bxor rk0 (setup-mix rk5) (vref rcon i)))
		 (vset! rk (+ ri 7) (bxor rk1 (vref rk (+ ri 6))))
		 (vset! rk (+ ri 8) (bxor rk2 (vref rk (+ ri 7))))
		 (vset! rk (+ ri 9) (bxor rk3 (vref rk (+ ri 8))))
		 (unless (= (+ i 1) 8)
		   (let ((rk4 (vref rk (+ ri 4))))
		     (vset! rk (+ ri 10) (bxor rk4 (vref rk (+ ri 9))))
		     (vset! rk (+ ri 11) (bxor rk5 (vref rk (+ ri 10))))
		     (loop (+ i 1) (+ ri 6)))))))
	    ((32) 
	     (vset! rk 4 (load32h key 16))
	     (vset! rk 5 (load32h key 20))
	     (vset! rk 6 (load32h key 24))
	     (vset! rk 7 (load32h key 28))
	     (let loop ((i 0) (ri 0))
	       (let ((rk0 (vref rk ri))
		     (rk1 (vref rk (+ ri 1)))
		     (rk2 (vref rk (+ ri 2)))
		     (rk3 (vref rk (+ ri 3)))
		     (rk7 (vref rk (+ ri 7))))
		 (vset! rk (+ ri 8)  (bxor rk0 (setup-mix rk7) (vref rcon i)))
		 (vset! rk (+ ri 9)  (bxor rk1 (vref rk (+ ri 8))))
		 (vset! rk (+ ri 10) (bxor rk2 (vref rk (+ ri 9))))
		 (vset! rk (+ ri 11) (bxor rk3 (vref rk (+ ri 10))))
		 (unless (= (+ i 1) 7)
		   (let ((rk4 (vref rk (+ ri 4)))
			 (rk5 (vref rk (+ ri 5)))
			 (rk6 (vref rk (+ ri 6)))
			 (rk11 (vref rk (+ ri 11))))
		     (vset! rk (+ ri 12) (bxor rk4 (setup-mix (rorc rk11 8))))
		     (vset! rk (+ ri 13) (bxor rk5 (vref rk (+ ri 12))))
		     (vset! rk (+ ri 14) (bxor rk6 (vref rk (+ ri 13))))
		     (vset! rk (+ ri 15) (bxor rk7 (vref rk (+ ri 14))))
		     (loop (+ i 1) (+ ri 8))))))))))

      (define (setup-dk skey)
	(define (copy4 rk rrk rki rrki)
	  (do ((i 0 (+ i 1)))
	      ((= i 4))
	    (vset! rk (+ i rki) (vref rrk (+ i rrki)))))
	(let ((rk (aes-key-dk skey))
	      (rrk (aes-key-ek skey))
	      (rrki (- (+ 28 keylen) 4)))
	  (define (set-it rrk rki rrki index)
	    (let ((temp (vref rrk (+ rrki index))))
	      (vset! rk (+ rki index)
		     (bxor (vref Tks0 (byte temp 3))
			   (vref Tks1 (byte temp 2))
			   (vref Tks2 (byte temp 1))
			   (vref Tks3 (byte temp 0))))))
	  (copy4 rk rrk 0 rrki)
	  (do ((i 1 (+ i 1)) 
	       (rrki (- rrki 4) (- rrki 4)) 
	       (rki 4 (+ rki 4))
	       (nr (aes-key-nr skey)))
	      ((= i nr)
	       ;; copy last
	       (copy4 rk rrk rki rrki)
	       skey)
	    (set-it rrk rki rrki 0)
	    (set-it rrk rki rrki 1)
	    (set-it rrk rki rrki 2)
	    (set-it rrk rki rrki 3))))
      
      (let ((skey (create-aes-key key round)))
	(setup-ek skey)
	(setup-dk skey)))
    
    (define (aes-encrypt pt ps ct cs key)
      (define nr (aes-key-nr key))
      (define rk (aes-key-ek key))
      (define (round nr rk)
	(define (compute-t rk rki s0 s1 s2 s3)
	  (bitwise-xor (Te0 (byte s0 3))
		       (Te1 (byte s1 2))
		       (Te2 (byte s2 1))
		       (Te3 (byte s3 0))
		       (vector-ref rk rki)))

	(let loop ((r (quotient nr 2))
		   (rki 0)
		   (s0 (bitwise-xor (load32h pt ps)        (vector-ref rk 0)))
		   (s1 (bitwise-xor (load32h pt (+ ps 4))  (vector-ref rk 1)))
		   (s2 (bitwise-xor (load32h pt (+ ps 8))  (vector-ref rk 2)))
		   (s3 (bitwise-xor (load32h pt (+ ps 12)) (vector-ref rk 3))))
	  (let ((t0 (compute-t rk (+ rki 4) s0 s1 s2 s3))
		(t1 (compute-t rk (+ rki 5) s1 s2 s3 s0))
		(t2 (compute-t rk (+ rki 6) s2 s3 s0 s1))
		(t3 (compute-t rk (+ rki 7) s3 s0 s1 s2))
		(rki (+ rki 8))
		(r (- r 1)))
	    (if (zero? r)
		(values rki t0 t1 t2 t3)
		(loop r rki 
		      (compute-t rk rki       t0 t1 t2 t3)
		      (compute-t rk (+ rki 1) t1 t2 t3 t0)
		      (compute-t rk (+ rki 2) t2 t3 t0 t1)
		      (compute-t rk (+ rki 3) t3 t0 t1 t2))))))
      ;; should we check size of pt?
      (let-values (((rki t0 t1 t2 t3) (round nr rk)))
	(define (compute-s rk rki t0 t1 t2 t3)
	  (bitwise-xor (vector-ref Te4_3 (byte t0 3))
		       (vector-ref Te4_2 (byte t1 2))
		       (vector-ref Te4_1 (byte t2 1))
		       (vector-ref Te4_0 (byte t3 0))
		       (vector-ref rk rki)))
	(store32h ct cs        (compute-s rk rki       t0 t1 t2 t3))
	(store32h ct (+ cs 4)  (compute-s rk (+ rki 1) t1 t2 t3 t0))
	(store32h ct (+ cs 8)  (compute-s rk (+ rki 2) t2 t3 t0 t1))
	(store32h ct (+ cs 12) (compute-s rk (+ rki 3) t3 t0 t1 t2))
	16))

    (define (aes-decrypt ct cs pt ps key)
      (define nr (aes-key-nr key))
      (define rk (aes-key-dk key))
      (define (round nr rk)
	(let loop ((r (quotient nr 2))
		   (rki 0)
		   (s0 (bitwise-xor (load32h ct (+ cs 0))  (vector-ref rk 0)))
		   (s1 (bitwise-xor (load32h ct (+ cs 4))  (vector-ref rk 1)))
		   (s2 (bitwise-xor (load32h ct (+ cs 8))  (vector-ref rk 2)))
		   (s3 (bitwise-xor (load32h ct (+ cs 12)) (vector-ref rk 3))))
	  (define (compute-t rk rki s0 s1 s2 s3)
	    (bitwise-xor (Td0 (byte s0 3))
			 (Td1 (byte s1 2))
			 (Td2 (byte s2 1))
			 (Td3 (byte s3 0))
			 (vector-ref rk rki)))
	  
	  (let ((t0 (compute-t rk (+ rki 4) s0 s3 s2 s1))
		(t1 (compute-t rk (+ rki 5) s1 s0 s3 s2))
		(t2 (compute-t rk (+ rki 6) s2 s1 s0 s3))
		(t3 (compute-t rk (+ rki 7) s3 s2 s1 s0))
		(rki (+ rki 8))
		(r (- r 1)))
	    (if (zero? r)
		(values rki t0 t1 t2 t3)
		(loop r rki 
		      (compute-t rk (+ rki 0) t0 t3 t2 t1)
		      (compute-t rk (+ rki 1) t1 t0 t3 t2)
		      (compute-t rk (+ rki 2) t2 t1 t0 t3)
		      (compute-t rk (+ rki 3) t3 t2 t1 t0))))))
      ;; should we check size of pt?
      (let-values (((rki t0 t1 t2 t3) (round nr rk)))
	(define (compute-s rk rki t0 t1 t2 t3)
	  (bitwise-xor (bitwise-and (vector-ref Td4 (byte t0 3)) #xff000000)
		       (bitwise-and (vector-ref Td4 (byte t1 2)) #x00ff0000)
		       (bitwise-and (vector-ref Td4 (byte t2 1)) #x0000ff00)
		       (bitwise-and (vector-ref Td4 (byte t3 0)) #x000000ff)
		       (vector-ref rk rki)))
	(store32h pt (+ ps 0) (compute-s rk (+ rki 0) t0 t3 t2 t1))
	(store32h pt (+ ps 4) (compute-s rk (+ rki 1) t1 t0 t3 t2))
	(store32h pt (+ ps 8) (compute-s rk (+ rki 2) t2 t1 t0 t3))
	(store32h pt (+ ps 12) (compute-s rk (+ rki 3) t3 t2 t1 t0))
	16))

    (define (aes-done key) #t)
      
    (define (AES)
      (make-cipher-descriptor 16 32 16 10
			      aes-setup
			      aes-encrypt
			      aes-decrypt
			      aes-done))
    )
)
