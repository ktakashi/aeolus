;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; aeolus/padding/pkcs.sld - PKCS padding
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

(define-library (aeolus padding pkcs)
  (export pkcs5-paddings)
  (import (scheme base))
  (begin
    ;; PKCS #5 padding.
    ;; reference http://www.rsa.com/rsalabs/node.asp?id=2127
    (define (pkcs5-paddings)
      (define (pad bv block-size)
	(let* ((len (bytevector-length bv))
	       (mod (modulo len block-size))
	       (t (- block-size mod))
	       (padding (if (zero? t) 8 t)))
	  (let ((new (make-bytevector (+ len padding) padding)))
	    (bytevector-copy! new 0 bv 0 len)
	    new)))
      (define (unpad bv block-size)
	(let* ((len (bytevector-length bv))
	       (pad (bytevector-u8-ref bv (- len 1)))
	       (new (make-bytevector (- len pad) 0)))
	  (bytevector-copy! new  0 bv 0 (- len pad))
	  new))
      (values pad unpad))

    ;; TODO more
    ))
