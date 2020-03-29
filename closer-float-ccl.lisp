;;; closer-float-ccl.lisp --- closer to floating-point arithmetic.

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
(defconst short-float-positive-infinity 1S++0)
(defconst single-float-positive-infinity 1F++0)
(defconst double-float-positive-infinity 1D++0)
(defconst long-float-positive-infinity 1L++0)

;; Negative infinity.
(defconst short-float-negative-infinity -1S++0)
(defconst single-float-negative-infinity -1F++0)
(defconst double-float-negative-infinity -1D++0)
(defconst long-float-negative-infinity -1L++0)

;; Not-a-number.
(defvar short-float-quiet-not-a-number)
(defvar single-float-quiet-not-a-number)
(defvar double-float-quiet-not-a-number)
(defvar long-float-quiet-not-a-number)

(defvar short-float-signaling-not-a-number)
(defvar single-float-signaling-not-a-number)
(defvar double-float-signaling-not-a-number)
(defvar long-float-signaling-not-a-number)

(defconst short-float-not-a-number 1S+-0)
(defconst single-float-not-a-number 1F+-0)
(defconst double-float-not-a-number 1D+-0)
(defconst long-float-not-a-number 1L+-0)

;; Predicates.
(defsubst float-positive-infinity-p (x)
  (declare (type float x))
  (and (ccl::infinity-p x)
       (plusp x)))

(defsubst float-negative-infinity-p (x)
  (declare (type float x))
  (and (ccl::infinity-p x)
       (minusp x)))

(defsubst float-infinity-p (x)
  (declare (type float x))
  (ccl::infinity-p x))

(defsubst float-not-a-number-p (x)
  (declare (type float x))
  (and (ccl::nan-or-infinity-p x)
       (not (ccl::infinity-p x))))

;; Rounding.
(defconst rounding-mode-alist '((:nearest-even . :nearest)
				(:nearest-away . n/a)
				(:up . :positive)
				(:down . :negative))
  "Mapping of Closer Float rounding mode keywords.")

(defsubst get-rounding-mode ()
  (ccl:get-fpu-mode :rounding-mode))

(defsubst set-rounding-mode (value)
  (ccl:set-fpu-mode :rounding-mode value))

;; Exceptions.
(defconst exception-alist '((:invalid-operation . :invalid))
  "Mapping of Closer Float exception keywords.")

(defconst ccl-exception-keywords
  '(:invalid :division-by-zero :overflow :underflow :inexact))

(defsubst get-unmasked-traps ()
  (let (traps)
    (dolist (trap ccl-exception-keywords)
      (when (ccl:get-fpu-mode trap)
	(push trap traps)))
    (nreverse traps)))

(defsubst set-unmasked-traps (traps)
  (dolist (trap ccl-exception-keywords)
    (ccl:set-fpu-mode trap (if (member trap traps) t))))

;;; closer-float-ccl.lisp ends here
