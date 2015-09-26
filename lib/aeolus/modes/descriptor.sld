;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; aeolus/modes/descriptor.sld - Mode descriptor
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

;; the mode descriptor is an vector (for now) but not depend on it
;; which contains the followings
;;  - min key length
;;  - max key length
;;  - block size
;;  - default number of rounds
;;  - setup procedure
;;  - encryption procedure (ECB)
;;  - decryption procedure (ECB)
;;  - done procedure

(define-library (aeolus modes descriptor)
  (export make-mode-descriptor
	  mode-descriptor-start
	  mode-descriptor-encrypt
	  mode-descriptor-decrypt
	  mode-descriptor-setiv
	  mode-descriptor-getiv
	  mode-descriptor-update-aad
	  mode-descriptor-done)
  (import (scheme base))
  (begin
    (define (make-mode-descriptor start encrypt decrypt setiv getiv 
				  update-aad done)
      (vector start encrypt decrypt setiv getiv update-aad done))
    
    (define (mode-descriptor-start      desc) (vector-ref desc 0))
    (define (mode-descriptor-encrypt    desc) (vector-ref desc 1))
    (define (mode-descriptor-decrypt    desc) (vector-ref desc 2))
    (define (mode-descriptor-setiv      desc) (vector-ref desc 3))
    (define (mode-descriptor-getiv      desc) (vector-ref desc 4))
    (define (mode-descriptor-update-aad desc) (vector-ref desc 5))
    (define (mode-descriptor-done       desc) (vector-ref desc 6))
    )
)