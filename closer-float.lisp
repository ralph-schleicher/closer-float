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

;; Plus-minus infinity.
(export 'short-float-infinity)
(when (and (boundp 'short-float-positive-infinity)
	   (boundp 'short-float-negative-infinity)
	   (= (- short-float-positive-infinity) short-float-negative-infinity))
  (defconst short-float-infinity short-float-positive-infinity))
(setf (documentation 'short-float-infinity 'variable)
      "Value of positive infinity.
The following identities hold:

     (= short-float-infinity short-float-positive-infinity)
     (= (- short-float-infinity) short-float-negative-infinity)")

(export 'single-float-infinity)
(when (and (boundp 'single-float-positive-infinity)
	   (boundp 'single-float-negative-infinity)
	   (= (- single-float-positive-infinity) single-float-negative-infinity))
  (defconst single-float-infinity single-float-positive-infinity))
(setf (documentation 'single-float-infinity 'variable)
      "Value of positive infinity.
The following identities hold:

     (= single-float-infinity single-float-positive-infinity)
     (= (- single-float-infinity) single-float-negative-infinity)")

(export 'double-float-infinity)
(when (and (boundp 'double-float-positive-infinity)
	   (boundp 'double-float-negative-infinity)
	   (= (- double-float-positive-infinity) double-float-negative-infinity))
  (defconst double-float-infinity double-float-positive-infinity))
(setf (documentation 'double-float-infinity 'variable)
      "Value of positive infinity.
The following identities hold:

     (= double-float-infinity double-float-positive-infinity)
     (= (- double-float-infinity) double-float-negative-infinity)")

(export 'long-float-infinity)
(when (and (boundp 'long-float-positive-infinity)
	   (boundp 'long-float-negative-infinity)
	   (= (- long-float-positive-infinity) long-float-negative-infinity))
  (defconst long-float-infinity long-float-positive-infinity))
(setf (documentation 'long-float-infinity 'variable)
      "Value of positive infinity.
The following identities hold:

     (= long-float-infinity long-float-positive-infinity)
     (= (- long-float-infinity) long-float-negative-infinity)")

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
(when (and (not (boundp 'short-float-not-a-number))
	   (boundp 'short-float-quiet-not-a-number))
  (defconst short-float-not-a-number short-float-quiet-not-a-number))
(when (and (not (boundp 'short-float-not-a-number))
	   (boundp 'short-float-signaling-not-a-number))
  (defconst short-float-not-a-number short-float-signaling-not-a-number))
(setf (documentation 'short-float-not-a-number 'variable)
      "Value of the default not-a-number.")

(export 'single-float-not-a-number)
(when (and (not (boundp 'single-float-not-a-number))
	   (boundp 'single-float-quiet-not-a-number))
  (defconst single-float-not-a-number single-float-quiet-not-a-number))
(when (and (not (boundp 'single-float-not-a-number))
	   (boundp 'single-float-signaling-not-a-number))
  (defconst single-float-not-a-number single-float-signaling-not-a-number))
(setf (documentation 'single-float-not-a-number 'variable)
      "Value of the default not-a-number.")

(export 'double-float-not-a-number)
(when (and (not (boundp 'double-float-not-a-number))
	   (boundp 'double-float-quiet-not-a-number))
  (defconst double-float-not-a-number double-float-quiet-not-a-number))
(when (and (not (boundp 'double-float-not-a-number))
	   (boundp 'double-float-signaling-not-a-number))
  (defconst double-float-not-a-number double-float-signaling-not-a-number))
(setf (documentation 'double-float-not-a-number 'variable)
      "Value of the default not-a-number.")

(export 'long-float-not-a-number)
(when (and (not (boundp 'long-float-not-a-number))
	   (boundp 'long-float-quiet-not-a-number))
  (defconst long-float-not-a-number long-float-quiet-not-a-number))
(when (and (not (boundp 'long-float-not-a-number))
	   (boundp 'long-float-signaling-not-a-number))
  (defconst long-float-not-a-number long-float-signaling-not-a-number))
(setf (documentation 'long-float-not-a-number 'variable)
      "Value of the default not-a-number.")

;; Predicates.
(export 'float-infinity-p)
(setf (documentation 'float-infinity-p 'function)
      "True if the floating-point number argument is infinite.")

(export 'float-positive-infinity-p)
(setf (documentation 'float-positive-infinity-p 'function)
      "True if the floating-point number argument is equal to positive infinity.")

(export 'float-negative-infinity-p)
(setf (documentation 'float-negative-infinity-p 'function)
      "True if the floating-point number argument is equal to negative infinity.")

(export 'float-not-a-number-p)
(setf (documentation 'float-not-a-number-p 'function)
      "True if the floating-point number argument is not-a-number.")

(export 'float-quiet-not-a-number-p)
(setf (documentation 'float-quiet-not-a-number-p 'function)
      "True if the floating-point number argument is a quiet not-a-number.")

(export 'float-signaling-not-a-number-p)
(setf (documentation 'float-signaling-not-a-number-p 'function)
      "True if the floating-point number argument is a signaling not-a-number.")

;; Rounding.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (fboundp 'get-rounding-mode)
	     (fboundp 'set-rounding-mode))
    (pushnew :closer-float-rounding-mode *features*)))

(defvar *rounding-mode* (progn #+closer-float-rounding-mode (get-rounding-mode))
  "The current rounding mode.")
(proclaim '(type (member :nearest-even :nearest-away :up :down :zero nil) *rounding-mode*))

;; Always query the actual value in case the rounding mode is
;; modified behind our back.
(export 'rounding-mode)
(defsubst rounding-mode ()
  "Accessor for the floating-point rounding mode.
Value is one of the following.

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

     nil
         Rounding mode is undefined.

When setting the rounding mode, ‘:nearest’ is a synonym for
‘:nearest-even’.

The ‘:nearest-away’ rounding mode is defined by IEEE 754 but
not, for example, in the C floating-point environment ‘fenv.h’.
Thus, chances are low that this rounding mode is supported."
  #-closer-float-rounding-mode
  (fix-me 'rounding-mode)
  (get-rounding-mode))

(defun (setf rounding-mode) (value)
  #-closer-float-rounding-mode
  (fix-me 'rounding-mode)
  (check-type value (member :nearest :nearest-even :nearest-away :up :down :zero))
  (let ((rounding-mode (if (eq value :nearest) :nearest-even value)))
    (set-rounding-mode rounding-mode)
    (setf *rounding-mode* rounding-mode)))
(setf (documentation '(setf rounding-mode) 'function)
      (documentation 'rounding-mode 'function))

(export 'with-rounding-mode)
(defmacro with-rounding-mode (rounding-mode &rest body)
  "Establish a lexical environment with the current rounding mode set
to ROUNDING-MODE.  If argument ROUNDING-MODE is ‘nil’, don't change
the current rounding mode.  When BODY returns, the previous rounding
mode is restored. "
  (alexandria:once-only (rounding-mode)
    `(let ((*rounding-mode* (rounding-mode)))
       (unwind-protect
	    (let ((*rounding-mode* *rounding-mode*))
	      (when ,rounding-mode
		(setf (rounding-mode) ,rounding-mode))
	      ,@body)
	 (when (not (null *rounding-mode*))
	   (setf (rounding-mode) *rounding-mode*))))))

;;; closer-float.lisp ends here
