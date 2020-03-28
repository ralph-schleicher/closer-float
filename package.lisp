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
  (:use :common-lisp :iterate)
  (:documentation "Closer to floating-point arithmetic.

Provide a common interface to IEEE 754 (a.k.a. ISO/IEC/IEEE 60559)
and ISO/IEC 10967 features.

The general rule is: If a symbol is not bound, then the corresponding
feature is not supported by the implementation.  Therefore, you can
use ‘boundp’ and ‘fboundp’ to check for a feature at run-time.

As an alternative, the Closer Float library also defines some feature
test macros to check for features at compile-time.  Below is a list of
all feature test macros together with their meaning.

closer-float-signed-zero
     True if there are distinct representations for positive and
     negative floating-point zero.  If so, the predicate functions
     ‘float-positive-zero-p’ and ‘float-negative-zero-p’ are defined
     for all float types.

closer-float-sign-symmetry
     True if there is sign symmetry in the range of floating-point
     numbers.  That means, for any positive floating-point number
     there is a corresponding negative floating-point number.  This
     includes positive and negative infinity if infinite numbers are
     defined.

closer-float-infinity
     True if the positive and negative infinity constants and the
     respective predicate functions ‘float-positive-infinity-p’ and
     ‘float-negative-infinity-p’ are defined for all float types.  If
     sign symmetry is defined, the sign symmetric infinity constants
     and the predicate function ‘float-infinity-p’ are defined, too.

closer-float-not-a-number
     True if a default not-a-number constant and the predicate
     function ‘float-not-a-number-p’ is defined for all float types.

closer-float-rounding-mode
     True if the floating-point rounding mode is accessible."))

(in-package :closer-float)

(define-condition feature-error (program-error)
  ((symbol
    :initarg :symbol
    :initform nil)
   (type
    :initarg :type
    :initform nil))
  (:report
   (lambda (c stream)
     (let ((symbol (slot-value c 'symbol))
	   (type (slot-value c 'type)))
       (format stream "~A for ~A on ~A ~A.~%~
Please send patches to ~A."
	       (if (null symbol)
		   "This functionality is not implemented"
		 (format nil "The ~(~A~) ‘~(~A~)’ is not defined"
			 (or type (if (fboundp symbol)
				      "function"
				    "symbol"))
			 symbol))
	       (lisp-implementation-type)
	       (software-type)
	       (machine-type)
	       ;; Mail address or URL for reporting bugs.
	       (let ((sys (asdf:find-system :closer-float)))
		 (or (ignore-errors
		      (asdf:system-maintainer sys))
		     (ignore-errors
		      (asdf:system-author sys)))))))))

(defun fix-me (&optional symbol type)
  "Issue a feature improvement request."
  (error 'feature-error :symbol symbol :type type))

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

(defconst n/a 'n/a
  "The representation of ‘not applicable’ or ‘not available’.
The symbol ‘n/a’ is a constant variable bound to itself.")

(defsubst getval (key alist)
  "Return the ‘cdr’ of the first cons in ALIST whose ‘car’ is equal to KEY,
or KEY if no such cons is found."
  (alexandria:if-let ((cell (assoc key alist)))
      (cdr cell)
    key))

(defsubst getkey (value alist)
  "Return the ‘car’ of the first cons in ALIST whose ‘cdr’ is equal to VALUE,
or VALUE if no such cons is found."
  (alexandria:if-let ((cell (rassoc value alist)))
      (car cell)
    value))

(defconst all-rounding-mode-keywords
  '(:nearest-even :nearest-away :up :down :zero)
  "The list of Closer Float rounding mode keywords.")

;;; package.lisp ends here
