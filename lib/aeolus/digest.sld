;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; aeolus/digest.sld - Hash interface
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

(define-library (aeolus digest)
  (export make-digest
	  digest-process!
	  digest-done!
	  digest-blocksize
	  digest-size
	  digest-oid
	  bytevector->digest)
  (import (scheme base)
	  (aeolus digest descriptor))
  (begin
    (define-record-type <digester> 
      (make-digester digester size blocksize oid process done) digest?
      (digester  digest-digester)
      (size      digest-size)
      (blocksize digest-blocksize)
      (oid       digest-oid)
      (process   digest-process)
      (done      digest-done))

    (define (make-digest digest)
      (let ((descriptor (digest)))
	(make-digester ((digest-descriptor-init descriptor))
		       (digest-descriptor-hashsize descriptor)
		       (digest-descriptor-blocksize descriptor)
		       (digest-descriptor-oid descriptor)
		       (digest-descriptor-process descriptor)
		       (digest-descriptor-done descriptor))))

    (define (digest-process! digest bv . optional)
      (apply (digest-process digest) (digest-digester digest) bv optional))

    (define (digest-done! digest out . optional)
      (apply (digest-done digest) (digest-digester digest) out optional))


    (define (bytevector->digest digest bv . optional)
      (if (digest? digest)
	  (let ((out (make-bytevector (digest-size digest))))
	    (apply digest-process! digest bv optional)
	    (digest-done! digest out))
	  (apply bytevector->digest (make-digest digest) bv optional)))
    ))
