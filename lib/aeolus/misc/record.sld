;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; aeolus/misc/record.sld - Simplified R6RS record
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

;; maybe we can also use SRFI-99 or (rnrs)
(define-library (aeolus misc record)
  (export define-record-type
	  fields mutable immutable parent protocol sealed opaque nongenerative)
  (cond-expand
   ((library (rnrs)) (import (rnrs)))
   (else
    (import (rename (scheme base)
		    (define-record-type scheme:define-record-type)))
    (begin 
      (define (find-tail pred list)
	(let lp ((list list))
	  (and (not (null? list))
	       (if (pred (car list)) list
		   (lp (cdr list))))))
      (define (find proc lis)
	(cond ((find-tail proc lis) => car)
	      (else #f)))
      ;; this is enough
      (define (for-all proc lis1 lis2)
	(cond ((and (null? lis1) (null? lis2)))
	      ((and (pair? lis1) (pair? lis2))
	       (and (proc (car lis1) (car lis2))
		    (for-all (cdr lis1) (cdr lis2))))
	      (else (error "incorrect list is given (maybe not the same length)"
			   lis1 lis2)))))
    (include "record/opaque-cell.scm")
    (include "record/vector-types.scm")
    (include "record/core.scm")
    (include "record/explicit.scm")))
  )
