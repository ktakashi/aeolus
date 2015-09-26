;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; aeolus/cipher.sld - ciphers interface
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

(define-library (aeolus cipher)
  (export make-cipher cipher?
	  cipher-encrypt
	  cipher-decrypt
	  cipher-done)
  (import (scheme base) (aeolus modes descriptor))
  (begin
    ;; TODO padding
    (define-record-type <cipher> (%make-cipher mode key) cipher?
      (mode cipher-mode)
      (key  cipher-key) ;; mode key
      )
      
    (define (make-cipher spec key  mode . maybe-param)
      (let* ((param (if (null? maybe-param)
			#f 
			(car maybe-param)))
	     (modev (mode))
	     (setup (mode-descriptor-start modev)))
	;; setup it with mode
	(%make-cipher modev (setup (spec) key param))))

    (define (cipher-encrypt cipher pt)
      ((mode-descriptor-encrypt (cipher-mode cipher)) (cipher-key cipher) pt))

    (define (cipher-decrypt cipher ct)
      ((mode-descriptor-decrypt (cipher-mode cipher)) (cipher-key cipher) ct))

    (define (cipher-done cipher)
      ((mode-descriptor-decrypt (cipher-mode cipher)) (cipher-key cipher)))
    ))
