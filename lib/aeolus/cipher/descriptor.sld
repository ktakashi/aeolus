;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; aeolus/cipher/descriptor.sld - Cipher descriptor
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

;; the cipher descriptor is an vector (for now) but not depend on it
;; which contains the followings
;;  - min key length
;;  - max key length
;;  - block size
;;  - default number of rounds
;;  - setup procedure
;;  - encryption procedure (ECB)
;;  - decryption procedure (ECB)
;;  - done procedure

(define-library (aeolus cipher descriptor)
  (export make-cipher-descriptor
	  cipher-descriptor-min-key-length
	  cipher-descriptor-max-key-length
	  cipher-descriptor-block-size
	  cipher-descriptor-default-round
	  cipher-descriptor-setup
	  cipher-descriptor-encrypt
	  cipher-descriptor-decrypt
	  cipher-descriptor-done)
  (import (scheme base))
  (begin
    (define (make-cipher-descriptor min-key max-key block-size default-round
				    setup encrypt decrypt done)
      (vector min-key max-key block-size default-round
	      setup encrypt decrypt done))
    

    (define (cipher-descriptor-min-key-length desc) (vector-ref desc 0))
    (define (cipher-descriptor-max-key-length desc) (vector-ref desc 1))
    (define (cipher-descriptor-block-size     desc) (vector-ref desc 2))
    (define (cipher-descriptor-default-round  desc) (vector-ref desc 3))
    (define (cipher-descriptor-setup          desc) (vector-ref desc 4))
    (define (cipher-descriptor-encrypt        desc) (vector-ref desc 5))
    (define (cipher-descriptor-decrypt        desc) (vector-ref desc 6))
    (define (cipher-descriptor-done           desc) (vector-ref desc 7))
    )
)