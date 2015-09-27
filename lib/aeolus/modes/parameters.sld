;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; aeolus/modes/parameters.sld - Mode parameter
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

(define-library (aeolus modes parameters)
  (export mode-parameter? make-composite-parameter 
	  define-mode-parameter
	  make-iv-paramater iv-parameter? parameter-iv
	  make-counter-parameter counter-parameter?
	  parameter-endian parameter-round
	  )
  (import (scheme base) (scheme case-lambda))
  (begin
    (define-record-type <composite-parameter> 
      (%make-composite-parameter parameters) mode-parameter?
      (parameters composite-mode-parameters))

    (define (make-composite-parameter . params)
      (define (check params)
	(or (null? params)
	    (and (mode-parameter? (car params))
		 (check (cdr params)))))
      (unless (check params)
	(error "make-composite-parameter: mode-parameter is required" params))
      (%make-composite-parameter 
       ;; Should we use append-map in SRFI-1?
       ;; but not sure if I should add more dependency...
       (apply append (map composite-mode-parameters params))))

    (define (find-parameter pred composite)
      (let loop ((parameters (composite-mode-parameters composite)))
	(cond ((null? parameters) #f)
	      ((pred (car parameters)) (car parameters))
	      (else (loop (cdr parameters))))))

    (define-syntax check-field
      (syntax-rules ()
	((_ who (pred?) o)
	 (unless (pred? o)
	   (error (string-append (symbol->string 'who) ": invalid field type")
		  o)))
	;; do nothing
	((_ who () o) (begin))))
    ;; mode parameter is immutable so not setter
    (define-syntax define-mode-parameter
      (syntax-rules (lambda protocol)
	((_ name (ctr params ...) pred (field accessor ...) ...)
	 (define-mode-parameter "field" name %ctr %pred
	   (ctr params ...) pred ((field accessor ...) ...) ()))
	((_ "field" name %ctr %pred (ctr params ...) pred 
	    ((field accessor ...) rest ...) (fields ...))
	 (define-mode-parameter "field" name %ctr %pred
	   (ctr params ...) pred (rest ...)
	   (fields ... (field real-accessor accessor ...))))
	((_ "field" name %ctr %pred (ctr params ...) pred () (fields ...))
	 (define-mode-parameter "ctr" name %ctr %pred
	   (ctr params ...) pred fields ... ))
	;; constructor
	;; TODO should we add protocol keyword as well?
	((_ "ctr" name %ctr %pred (ctr (lambda (p) expr ...)) pred 
	    (field rest ...) ...)
	 (begin
	   ;; kinda R6RS protocol looks like thing
	   (define-mode-parameter "make" name %real %pred
	     (%ctr field ...) pred (field rest ...) ...)
	   (define ctr
	     (let ((this (lambda (p) expr ...)))
	       (lambda args
		 (apply (this %ctr) args))))))
	((_ "ctr" name %ctr %pred (ctr params ...) pred fields ...)
	 (define-mode-parameter "make" name %ctr %pred
	   (ctr params ...) pred fields ... ))
	((_ "make" name %ctr %pred (ctr params ...) pred
	    (field real-accessor accessor field-pred? ...) ...)
	 (begin
	   (define-record-type name (%ctr params ...) %pred
	     (field real-accessor) ...)
	   (define (pred o)
	     (and (mode-parameter? o) ;; always mode-parameter
		  (find-parameter %pred o)))
	   (define (ctr params ...)
	     (check-field field (field-pred? ...) params) ...
	     (%make-composite-parameter (list (%ctr params ...))))
	   (define (accessor p)
	     (let ((this (find-parameter %pred p)))
	       (real-accessor this)))
	   ...))))

    (define-mode-parameter <iv-parameter> (make-iv-paramater iv) iv-parameter?
      (iv parameter-iv bytevector?))

    (define (counter-type? o)
      (or (and (symbol? o) (memq o '(little big)))
	  (and (list? o) (counter-type? (car o)) (eq? (cadr o) 'rfc3686))))
    (define-mode-parameter <counter-parameter> 
      (make-counter-parameter 
       (lambda (p) 
	 (case-lambda
	  ((type) (p type 0))
	  ((type round) (p type round)))))
      counter-parameter?
      (endian parameter-endian counter-type?)
      (rount  parameter-round  integer?))

    )
)