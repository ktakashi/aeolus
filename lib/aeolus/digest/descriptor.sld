;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; aeolus/digest/descriptor.sld - Digest descriptor
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

(define-library (aeolus digest descriptor)
  (export make-digest-descriptor
	  digest-descriptor-hashsize
	  digest-descriptor-blocksize
	  digest-descriptor-oid
	  digest-descriptor-init
	  digest-descriptor-process
	  digest-descriptor-done

	  define-digest-process
	  <digest-base>
	  digest-base-current digest-base-current-set!
	  digest-base-length digest-base-length-set!
	  digest-base-buffer
	  )
  (import (except (scheme base) define-record-type)
	  (aeolus misc record))
  (begin
    ;; the same as other descriptors, it's vector
    ;;  - hashsize: size of digest in octets
    ;;  - blocksize: input block size in octets
    ;;  - oid: ASN.1 OID in string or #f. (e.g. SHA-1 oid "1.3.14.3.2.26")
    ;;  - init: initialisation procedure
    ;;  - process: hash process
    ;;  - done: retriever
    (define (make-digest-descriptor hashsize blocksize oid init process done)
      (vector hashsize blocksize oid init process done))

    (define (digest-descriptor-hashsize  d) (vector-ref d 0))
    (define (digest-descriptor-blocksize d) (vector-ref d 1))
    (define (digest-descriptor-oid	  d) (vector-ref d 2))
    (define (digest-descriptor-init	  d) (vector-ref d 3))
    (define (digest-descriptor-process	  d) (vector-ref d 4))
    (define (digest-descriptor-done      d) (vector-ref d 5))


    (define-record-type (<digest-base> make-digest-base digest-base?)
      (fields (immutable buffer digest-base-buffer)
	      ;; current length
	      (mutable current digest-base-current digest-base-current-set!)
	      ;; input length
	      (mutable length  digest-base-length digest-base-length-set!))
      (protocol (lambda (p)
		  (lambda (buf)
		    (p buf 0 0)))))

    (define-syntax define-digest-process
      (syntax-rules ()
	((_ name compress block)
	 (define (name digest in . optional)
	   (define start (if (null? optional) 0 (car optional)))
	   (define end (if (or (null? optional) (null? (cdr optional)))
			   (bytevector-length in)
			   (cadr optional)))
	   (define block-size block)
	   (define comp compress)
	   
	   (define buffer (digest-base-buffer digest))
	   (define (cur! v) (digest-base-current-set! digest v))
	   (define (cur) (digest-base-current digest))
	   (define (len! v) 
	     (let ((l (digest-base-length digest)))
	       (digest-base-length-set! digest (+ l v))))
	   (when (> (digest-base-current digest) (bytevector-length buffer))
	     (error "digest-process: invalid argument"))
	   (let loop  ((inlen (- end start)) (start start))
	     (unless (<= inlen 0)
	       (let ((c (cur)))
		 (if (and (zero? c) (>= inlen block-size))
		     (begin
		       (comp digest in start)
		       (len! (* block-size 8))
		       (loop (- inlen block-size) (+ start block-size)))
		     (let ((n (min inlen (- block-size c))))
		       (bytevector-copy! buffer c in start n)
		       (cur! (+ c n))
		       (when (= c block-size)
			 (comp digest buffer)
			 (len! (* 8 block-size))
			 (cur! 0))
		       (loop (- inlen n) (+ start n)))))))))))
    ))
