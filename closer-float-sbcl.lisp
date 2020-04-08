;;; closer-float-sbcl.lisp --- closer to floating-point arithmetic.

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
(import '(sb-ext:short-float-positive-infinity
	  sb-ext:single-float-positive-infinity
	  sb-ext:double-float-positive-infinity
	  sb-ext:long-float-positive-infinity))

;; Negative infinity.
(import '(sb-ext:short-float-negative-infinity
	  sb-ext:single-float-negative-infinity
	  sb-ext:double-float-negative-infinity
	  sb-ext:long-float-negative-infinity))

;; Quiet not-a-number.
(defconst single-float-quiet-not-a-number
  (sb-kernel:make-single-float #2r01111111110000000000000000000000))
(defconst short-float-quiet-not-a-number single-float-quiet-not-a-number)

(defconst double-float-quiet-not-a-number
  (sb-kernel:make-double-float #2r01111111111110000000000000000000
			       #2r00000000000000000000000000000000))
(defconst long-float-quiet-not-a-number double-float-quiet-not-a-number)

;; Signaling not-a-number.
(defconst single-float-signaling-not-a-number
  (sb-kernel:make-single-float #2r01111111101111111111111111111111))
(defconst short-float-signaling-not-a-number single-float-signaling-not-a-number)

(defconst double-float-signaling-not-a-number
  (sb-kernel:make-double-float #2r01111111111101111111111111111111
			       #2r11111111111111111111111111111111))
(defconst long-float-signaling-not-a-number double-float-signaling-not-a-number)

;; Predicates.
(import 'sb-ext:float-infinity-p)

(defsubst float-positive-infinity-p (x)
  (declare (type float x))
  (and (sb-ext:float-infinity-p x)
       (plusp x)))

(defsubst float-negative-infinity-p (x)
  (declare (type float x))
  (and (sb-ext:float-infinity-p x)
       (minusp x)))

(defsubst float-not-a-number-p (x)
  (declare (type float x))
  (sb-ext:float-nan-p x))

(defsubst float-quiet-not-a-number-p (x)
  (declare (type float x))
  (and (sb-ext:float-nan-p x)
       (not (sb-ext:float-trapping-nan-p x))))

;; Note: ‘sb-ext:float-trapping-nan-p’ only checks the quiet/signaling
;; bit of the significand.
(defsubst float-signaling-not-a-number-p (x)
  (declare (type float x))
  (and (sb-ext:float-nan-p x)
       (sb-ext:float-trapping-nan-p x)))

;; Rounding.
;;
;; See ‘sb-vm:+rounding-mode-alist+’.
(defconst rounding-mode-alist '((:nearest-even . :nearest)
				(:nearest-away . n/a)
				(:up . :positive-infinity)
				(:down . :negative-infinity))
  "Mapping of Closer Float rounding mode keywords.")

(defsubst get-rounding-mode ()
  (getf (sb-int:get-floating-point-modes) :rounding-mode))

(defsubst set-rounding-mode (value)
  (sb-int:set-floating-point-modes :rounding-mode value))

;; Exceptions.
;;
;; See ‘sb-vm:+float-trap-alist+’.
(defconst exception-alist '((:invalid-operation . :invalid)
			    (:division-by-zero . :divide-by-zero)
			    #+x86
			    (n/a . :denormalized-operand))
  "Mapping of Closer Float exception keywords.")

(defsubst get-unmasked-traps ()
  (getf (sb-int:get-floating-point-modes) :traps))

(defsubst set-unmasked-traps (traps)
  (sb-int:set-floating-point-modes :traps traps))

;;; closer-float-sbcl.lisp ends here
