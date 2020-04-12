;;; closer-float.lisp --- closer to floating-point arithmetic.

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

(in-package :closer-float)

;;;; Infinity

;; Positive infinity.
(export 'short-float-positive-infinity)
(setf (documentation 'short-float-positive-infinity 'variable)
      "Value of positive infinity.")

(export 'single-float-positive-infinity)
(setf (documentation 'single-float-positive-infinity 'variable)
      "Value of positive infinity.")

(export 'double-float-positive-infinity)
(setf (documentation 'double-float-positive-infinity 'variable)
      "Value of positive infinity.")

(export 'long-float-positive-infinity)
(setf (documentation 'long-float-positive-infinity 'variable)
      "Value of positive infinity.")

;; Negative infinity.
(export 'short-float-negative-infinity)
(setf (documentation 'short-float-negative-infinity 'variable)
      "Value of negative infinity.")

(export 'single-float-negative-infinity)
(setf (documentation 'single-float-negative-infinity 'variable)
      "Value of negative infinity.")

(export 'double-float-negative-infinity)
(setf (documentation 'double-float-negative-infinity 'variable)
      "Value of negative infinity.")

(export 'long-float-negative-infinity)
(setf (documentation 'long-float-negative-infinity 'variable)
      "Value of negative infinity.")

;; Predicates.
(export 'float-positive-infinity-p)
(setf (documentation 'float-positive-infinity-p 'function)
      "True if the floating-point number argument is equal to positive infinity.")

(export 'float-negative-infinity-p)
(setf (documentation 'float-negative-infinity-p 'function)
      "True if the floating-point number argument is equal to negative infinity.")

(export 'float-infinity-p)
(setf (documentation 'float-infinity-p 'function)
      "True if the floating-point number argument is infinite.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (boundp 'short-float-positive-infinity)
	     (boundp 'short-float-negative-infinity)
	     (boundp 'single-float-positive-infinity)
	     (boundp 'single-float-negative-infinity)
	     (boundp 'double-float-positive-infinity)
	     (boundp 'double-float-negative-infinity)
	     (boundp 'long-float-positive-infinity)
	     (boundp 'long-float-negative-infinity)
	     (fboundp 'float-positive-infinity-p)
	     (fboundp 'float-negative-infinity-p))
    (pushnew :closer-float-infinity *features*)))

;; Sign symmetry in the range of floating-point numbers.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (= (- most-positive-short-float)
		most-negative-short-float)
	     (= (- most-positive-single-float)
		most-negative-single-float)
	     (= (- most-positive-double-float)
		most-negative-double-float)
	     (= (- most-positive-long-float)
		most-negative-long-float)
	     #+closer-float-infinity
	     (ignore-errors
	      (and (= (- short-float-positive-infinity)
		      short-float-negative-infinity)
		   (= (- single-float-positive-infinity)
		      single-float-negative-infinity)
		   (= (- double-float-positive-infinity)
		      double-float-negative-infinity)
		   (= (- long-float-positive-infinity)
		      long-float-negative-infinity)
		   (fboundp 'float-infinity-p))))
    (pushnew :closer-float-sign-symmetry *features*)))

(export 'short-float-infinity)
#+(and closer-float-infinity closer-float-sign-symmetry)
(defconst short-float-infinity short-float-positive-infinity)
(setf (documentation 'short-float-infinity 'variable)
      "Value of positive infinity.
The following identities hold:

     (= short-float-infinity short-float-positive-infinity)
and
     (= (- short-float-infinity) short-float-negative-infinity)")

(export 'single-float-infinity)
#+(and closer-float-infinity closer-float-sign-symmetry)
(defconst single-float-infinity single-float-positive-infinity)
(setf (documentation 'single-float-infinity 'variable)
      "Value of positive infinity.
The following identities hold:

     (= single-float-infinity single-float-positive-infinity)
and
     (= (- single-float-infinity) single-float-negative-infinity)")

(export 'double-float-infinity)
#+(and closer-float-infinity closer-float-sign-symmetry)
(defconst double-float-infinity double-float-positive-infinity)
(setf (documentation 'double-float-infinity 'variable)
      "Value of positive infinity.
The following identities hold:

     (= double-float-infinity double-float-positive-infinity)
and
     (= (- double-float-infinity) double-float-negative-infinity)")

(export 'long-float-infinity)
#+(and closer-float-infinity closer-float-sign-symmetry)
(defconst long-float-infinity long-float-positive-infinity)
(setf (documentation 'long-float-infinity 'variable)
      "Value of positive infinity.
The following identities hold:

     (= long-float-infinity long-float-positive-infinity)
and
     (= (- long-float-infinity) long-float-negative-infinity)")

;;;; Not-a-Number

;; Quiet not-a-number.
(export 'short-float-quiet-not-a-number)
(setf (documentation 'short-float-quiet-not-a-number 'variable)
      "Value of a quiet not-a-number.")

(export 'single-float-quiet-not-a-number)
(setf (documentation 'single-float-quiet-not-a-number 'variable)
      "Value of a quiet not-a-number.")

(export 'double-float-quiet-not-a-number)
(setf (documentation 'double-float-quiet-not-a-number 'variable)
      "Value of a quiet not-a-number.")

(export 'long-float-quiet-not-a-number)
(setf (documentation 'long-float-quiet-not-a-number 'variable)
      "Value of a quiet not-a-number.")

;; Signaling not-a-number.
(export 'short-float-signaling-not-a-number)
(setf (documentation 'short-float-signaling-not-a-number 'variable)
      "Value of a signaling not-a-number.")

(export 'single-float-signaling-not-a-number)
(setf (documentation 'single-float-signaling-not-a-number 'variable)
      "Value of a signaling not-a-number.")

(export 'double-float-signaling-not-a-number)
(setf (documentation 'double-float-signaling-not-a-number 'variable)
      "Value of a signaling not-a-number.")

(export 'long-float-signaling-not-a-number)
(setf (documentation 'long-float-signaling-not-a-number 'variable)
      "Value of a signaling not-a-number.")

;; Default not-a-number.
(export 'short-float-not-a-number)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (not (boundp 'short-float-not-a-number))
	     (boundp 'short-float-quiet-not-a-number))
    (defconst short-float-not-a-number short-float-quiet-not-a-number))
  (when (and (not (boundp 'short-float-not-a-number))
	     (boundp 'short-float-signaling-not-a-number))
    (defconst short-float-not-a-number short-float-signaling-not-a-number)))
(setf (documentation 'short-float-not-a-number 'variable)
      "Value of the default not-a-number.")

(export 'single-float-not-a-number)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (not (boundp 'single-float-not-a-number))
	     (boundp 'single-float-quiet-not-a-number))
    (defconst single-float-not-a-number single-float-quiet-not-a-number))
  (when (and (not (boundp 'single-float-not-a-number))
	     (boundp 'single-float-signaling-not-a-number))
    (defconst single-float-not-a-number single-float-signaling-not-a-number)))
(setf (documentation 'single-float-not-a-number 'variable)
      "Value of the default not-a-number.")

(export 'double-float-not-a-number)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (not (boundp 'double-float-not-a-number))
	     (boundp 'double-float-quiet-not-a-number))
    (defconst double-float-not-a-number double-float-quiet-not-a-number))
  (when (and (not (boundp 'double-float-not-a-number))
	     (boundp 'double-float-signaling-not-a-number))
    (defconst double-float-not-a-number double-float-signaling-not-a-number)))
(setf (documentation 'double-float-not-a-number 'variable)
      "Value of the default not-a-number.")

(export 'long-float-not-a-number)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (not (boundp 'long-float-not-a-number))
	     (boundp 'long-float-quiet-not-a-number))
    (defconst long-float-not-a-number long-float-quiet-not-a-number))
  (when (and (not (boundp 'long-float-not-a-number))
	     (boundp 'long-float-signaling-not-a-number))
    (defconst long-float-not-a-number long-float-signaling-not-a-number)))
(setf (documentation 'long-float-not-a-number 'variable)
      "Value of the default not-a-number.")

;; Predicates.
(export 'float-quiet-not-a-number-p)
(setf (documentation 'float-quiet-not-a-number-p 'function)
      "True if the floating-point number argument is a quiet not-a-number.")

(export 'float-signaling-not-a-number-p)
(setf (documentation 'float-signaling-not-a-number-p 'function)
      "True if the floating-point number argument is a signaling not-a-number.")

(export 'float-not-a-number-p)
(setf (documentation 'float-not-a-number-p 'function)
      "True if the floating-point number argument is not-a-number.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (boundp 'short-float-not-a-number)
	     (boundp 'single-float-not-a-number)
	     (boundp 'double-float-not-a-number)
	     (boundp 'long-float-not-a-number)
	     (fboundp 'float-not-a-number-p))
    (pushnew :closer-float-not-a-number *features*)))

;;;; Zero

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (= (float-sign -0S0) (float-sign (- 0S0)) -1S0)
	     (= (float-sign -0F0) (float-sign (- 0F0)) -1F0)
	     (= (float-sign -0D0) (float-sign (- 0D0)) -1D0)
	     (= (float-sign -0L0) (float-sign (- 0L0)) -1L0))
    (pushnew :closer-float-signed-zero *features*)))

(export 'float-zero-p)
(defsubst float-zero-p (x)
  (declare (type float x))
  "True if the argument is zero.

Argument X is any floating-point number, including infinity and
 not-a-number.

If signed zero is used in the implementation, return true if the
absolute value of X is zero."
  #-closer-float-not-a-number
  (zerop x)
  #+closer-float-not-a-number
  (unless (float-not-a-number-p x)
    (zerop x)))

(export 'float-positive-zero-p)
(defsubst float-positive-zero-p (x)
  "True if the argument is positive zero.

Argument X is any floating-point number, including infinity and
 not-a-number.

If signed zero is not used in the implementation, return true if
X is zero."
  (declare (type float x))
  (and (float-zero-p x)
       (plusp (float-sign x))))

(export 'float-negative-zero-p)
(defsubst float-negative-zero-p (x)
  "True if the argument is negative zero.

Argument X is any floating-point number, including infinity and
 not-a-number.

If signed zero is not used in the implementation, always return
false."
  (declare (type float x))
  (and (float-zero-p x)
       (minusp (float-sign x))))

;;;; Rounding Mode

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (fboundp 'get-rounding-mode)
	     (fboundp 'set-rounding-mode))
    (pushnew :closer-float-rounding-mode *features*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp 'rounding-mode-alist)
    (defconst rounding-mode-alist ())))
(setf (documentation 'rounding-mode-alist 'variable)
      "Mapping of Closer Float rounding mode keywords.

Value is an alist with elements of the form ‘(KEY . VALUE)’
where KEY is the Closer Float rounding mode keyword and VALUE
is the corresponding value in the implementation, or ‘n/a’ if
the rounding mode is not available.  No mapping is required
if VALUE is equal to KEY.")

(export 'rounding-mode-keywords)
(defconst rounding-mode-keywords
  (progn
    #+closer-float-rounding-mode
    (iter (for key :in all-rounding-mode-keywords)
	  (for value = (getval key rounding-mode-alist))
	  (unless (eq value n/a)
	    (collect key))))
  "The list of rounding mode keywords used in the implementation.

Below is a table of all rounding mode keywords together with their
meaning.

     :nearest-even
          Round to nearest, ties to even.

     :nearest-away
          Round to nearest, ties away from zero.

     :up
          Direct rounding towards positive infinity.

     :down
          Direct rounding towards negative infinity.

     :zero
          Direct rounding towards zero.

The ‘:nearest-away’ rounding mode is defined by IEEE 754 as
an option for the decimal floating-point formats.  It is not
defined, for example, in the C floating-point environment
‘fenv.h’.  Thus, chances are low that this rounding mode is
used in the implementation.")

(export 'rounding-mode)
(defsubst rounding-mode ()
  "Accessor for the floating-point rounding mode.

Value is a rounding mode keyword.

When setting the rounding mode, ‘:nearest’ is a synonym for
‘:nearest-even’.

The ‘rounding-mode-keywords’ variable lists the rounding mode
keywords used in the implementation.

A ‘program-error’ is signaled if you attempt to set a rounding
mode not used in the implementation."
  #-closer-float-rounding-mode
  (fix-me 'rounding-mode 'function)
  #+closer-float-rounding-mode
  (getkey (get-rounding-mode) rounding-mode-alist))

(defun (setf rounding-mode) (rounding-mode)
  (check-type rounding-mode #.(list* 'member :nearest all-rounding-mode-keywords))
  #-closer-float-rounding-mode
  (fix-me '(setf rounding-mode) 'function)
  #+closer-float-rounding-mode
  (let* ((key (if (eq rounding-mode :nearest) :nearest-even rounding-mode))
	 (value (getval key rounding-mode-alist)))
    (when (eq value n/a)
      (fix-me `(setf (rounding-mode) ,rounding-mode) :operation))
    (set-rounding-mode value))
  rounding-mode)
(setf (documentation '(setf rounding-mode) 'function)
      (documentation 'rounding-mode 'function))

(export 'with-rounding-mode)
(defmacro with-rounding-mode (rounding-mode &body body)
  "Establish a lexical environment with the current rounding mode set
to ROUNDING-MODE.  If argument ROUNDING-MODE is ‘nil’, don't change
the current rounding mode.  When BODY returns, the previous rounding
mode is restored."
  (check-type rounding-mode #.(list* 'member nil :nearest all-rounding-mode-keywords))
  #-closer-float-rounding-mode
  `(fix-me 'with-rounding-mode 'macro)
  #+closer-float-rounding-mode
  (let ((saved (gensym "SAVED"))
	(mode (gensym "MODE")))
    `(let ((,saved (get-rounding-mode)))
       (unwind-protect
	    (progn
	      (alexandria:when-let ((,mode ,rounding-mode))
		(setf (rounding-mode) ,mode))
	      ,@body)
	 (set-rounding-mode ,saved)))))

;;;; Exceptions

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (fboundp 'get-unmasked-traps)
	     (fboundp 'set-unmasked-traps))
    (pushnew :closer-float-handle-traps *features*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp 'exception-alist)
    (defconst exception-alist ())))
(setf (documentation 'exception-alist 'variable)
      "Mapping of Closer Float exception keywords.

Value is an alist with elements of the form ‘(KEY . VALUE)’
where KEY is the Closer Float exception keyword and VALUE is
the corresponding value in the implementation, or ‘n/a’ if
the exception is not available.  No mapping is required
if VALUE is equal to KEY.")

(export 'exception-keywords)
(defconst exception-keywords
  (let (traps)
    #+closer-float-handle-traps
    (progn
      ;; Closer Float keywords.
      (iter (for key :in all-exception-keywords)
	    (for value = (getval key exception-alist))
	    (unless (eq value n/a)
	      (push key traps)))
      ;; Implementation keywords.
      (iter (for (key . value) :in exception-alist)
	    (when (eq key n/a)
	      (pushnew value traps))))
    (nreverse traps))
  "The list of exception keywords used in the implementation.

Below is a table of all exception keywords together with their
meaning.

     :invalid-operation
          Invalid operation; ‘floating-point-invalid-operation’
          condition.

     :division-by-zero
          Division by zero; ‘division-by-zero’ condition.

     :overflow
          Floating-point overflow; ‘floating-point-overflow’
          condition.

     :underflow
          Floating-point underflow; ‘floating-point-underflow’
          condition.

     :inexact
          Inexact result; ‘floating-point-inexact’ condition.

The value and meaning of other floating-point exceptions used in
the implementation is not standardized.  The only known exception
of this kind is the denormalized operand exception of the Intel
x86 architecture.  Whether or not non-standard exceptions are
exposed to the user is implementation dependent.")

(export 'unmasked-traps)
(defsubst unmasked-traps ()
  "Accessor for the enabled floating-point exceptions.

Value is a list of exception keywords.

When enabling floating-point exceptions, a value of ‘t’ is a
synonym for all exceptions.

The ‘exception-keywords’ variable lists the exception keywords
used in the implementation.

A ‘program-error’ is signaled if you attempt to unmask a trap,
i.e. enable an exception, not used in the implementation."
  #-closer-float-handle-traps
  (fix-me 'unmasked-traps 'function)
  #+closer-float-handle-traps
  (let ((traps (get-unmasked-traps)))
    (iter (for list :on traps)
	  (for value = (car list))
	  (for key = (getkey value exception-alist))
	  (setf (car list) key))
    traps))

(defsubst (setf unmasked-traps) (traps)
  #-closer-float-handle-traps
  (fix-me '(setf unmasked-traps) 'function)
  #+closer-float-handle-traps
  (set-unmasked-traps
   (if (eq traps t)
       (iter (for key :in exception-keywords)
	     (collect (getval key exception-alist)))
     (iter (for key :in (alexandria:ensure-list traps))
	   (unless (member key exception-keywords)
	     (fix-me `(setf (unmasked-traps) ,traps) :operation))
	   (adjoining (getval key exception-alist)))))
  traps)
(setf (documentation '(setf unmasked-traps) 'function)
      (documentation 'unmasked-traps 'function))

(export 'masked-traps)
(defsubst masked-traps ()
  "Accessor for the disabled floating-point exceptions.

Value is a list of exception keywords.

The ‘exception-keywords’ variable lists the exception keywords
used in the implementation.

A ‘program-error’ is signaled if you attempt to mask a trap not
used in the implementation."
  (set-difference exception-keywords (unmasked-traps)))

(defsubst (setf masked-traps) (traps)
  (setf (unmasked-traps)
	(if (eq traps t)
	    ()
	  (set-difference exception-keywords (alexandria:ensure-list traps)))))
(setf (documentation '(setf masked-traps) 'function)
      (documentation 'masked-traps 'function))

(export 'with-unmasked-traps)
(defmacro with-unmasked-traps (traps &body body)
  "Establish a lexical environment where the floating-point exceptions
listed in TRAPS are enabled.  If argument TRAPS is ‘t’, enable all
exceptions.  When BODY returns, the previous signaling exceptions are
restored."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (get-unmasked-traps)))
       (unwind-protect
	    (progn
	      (setf (unmasked-traps) ,traps)
	      ,@body)
	 (set-unmasked-traps ,saved)))))

(export 'with-masked-traps)
(defmacro with-masked-traps (traps &body body)
  "Establish a lexical environment where the floating-point exceptions
listed in TRAPS are disabled.  If argument TRAPS is ‘t’, disable all
exceptions.  When BODY returns, the previous signaling exceptions are
restored."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (get-unmasked-traps)))
       (unwind-protect
	    (progn
	      (setf (masked-traps) ,traps)
	      ,@body)
	 (set-unmasked-traps ,saved)))))

(defun %merge-traps (traps place)
  (dolist (trap (if (eq traps t)
		    exception-keywords
		  (alexandria:ensure-list traps)))
    (pushnew trap place))
  place)

(export 'with-unmasked-traps*)
(defmacro with-unmasked-traps* (traps &body body)
  "Like ‘with-unmasked-traps’ except that traps accumulate."
  `(with-unmasked-traps (%merge-traps ,traps (unmasked-traps))
     ,@body))

(export 'with-masked-traps*)
(defmacro with-masked-traps* (traps &body body)
  "Like ‘with-masked-traps’ except that traps accumulate."
  `(with-masked-traps (%merge-traps ,traps (masked-traps))
     ,@body))

;;; closer-float.lisp ends here
