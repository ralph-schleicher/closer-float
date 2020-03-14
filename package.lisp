;;; package.lisp --- closer to floating-point arithmetic.

;; Copyright (C) 2020 Ralph Schleicher

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in
;;      the documentation and/or other materials provided with the
;;      distribution.
;;
;;    * Neither the name of the copyright holder nor the names of its
;;      contributors may be used to endorse or promote products derived
;;      from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :common-lisp-user)

(defpackage :closer-float
  (:use :common-lisp)
  (:documentation "Closer to floating-point arithmetic.

Provide a common interface to IEEE 754 (a.k.a. ISO/IEC/IEEE 60559)
and ISO/IEC 10967 features.

If a symbol is not bound, then the corresponding feature is not
available."))

(in-package :closer-float)

(defun fix-me (&optional symbol type)
  "Issue a feature improvement request."
  (error "~A for ~A on ~A ~A.~%~
Please send patches to <~A>."
	 (if (null symbol)
	     "This functionality is not implemented"
	   (format nil "The ~A ‘~A’ is not defined"
		   (or type (if (fboundp symbol)
				"function"
			      "symbol"))
		   (symbol-name symbol)))
	 (lisp-implementation-type)
	 (software-type)
	 (machine-type)
	 ;; Mail address or URL for reporting bugs.
	 (let ((sys (asdf:find-system :closer-float)))
	   (or (ignore-errors
		(asdf:system-maintainer sys))
	       (ignore-errors
		(asdf:system-author sys))))))

(defmacro defconst (name value &optional doc)
  "Define a constant variable.

This is like ‘defconstant’ except that the initially set value
is reused when the ‘defconst’ form is evaluated again."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defmacro defsubst (name arg-list &body body)
  "Define an inline function.

This is like ‘defun’ except that the function is globally marked
for inline expansion by the compiler."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,arg-list
       ,@body)))

;;; package.lisp ends here
