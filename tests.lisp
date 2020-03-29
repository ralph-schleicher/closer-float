;;; tests.lisp --- closer to floating-point arithmetic.

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

(ql:quickload :iterate)
(ql:quickload :lisp-unit)
(ql:quickload :closer-float)

(defpackage :closer-float-tests
  (:use :common-lisp
	:iterate
	:lisp-unit
	:closer-float))

(in-package :closer-float-tests)

(defvar float-sizes '("SHORT" "SINGLE" "DOUBLE" "LONG"))

(defun ~ (&rest strings)
  (apply #'concatenate 'string (mapcar #'string strings)))

(defparameter +zero
  (iter (for size :in float-sizes)
	(for name = (~ size "-FLOAT"))
	(for symbol = (find-symbol name :common-lisp))
	(adjoining (coerce 0 symbol))))

(defparameter -zero
  (progn
    #+closer-float-signed-zero
    (mapcar #'- +zero)))

(defparameter zero
  (concatenate 'list +zero -zero))

(defparameter +inf
  (iter (for size :in float-sizes)
	(for name = (~ size "-FLOAT-POSITIVE-INFINITY"))
	(for symbol = (find-symbol name :closer-float))
	(when (and symbol (boundp symbol))
	  (adjoining (symbol-value symbol)))))

(defparameter -inf
  (iter (for size :in float-sizes)
	(for name = (~ size "-FLOAT-NEGATIVE-INFINITY"))
	(for symbol = (find-symbol name :closer-float))
	(when (and symbol (boundp symbol))
	  (adjoining (symbol-value symbol)))))

(defparameter inf
  (concatenate 'list +inf -inf))

(defparameter qnan
  (iter (for size :in float-sizes)
	(for name = (~ size "-FLOAT-QUIET-NOT-A-NUMBER"))
	(for symbol = (find-symbol name :closer-float))
	(when (and symbol (boundp symbol))
	  (adjoining (symbol-value symbol)))))

(defparameter snan
  (iter (for size :in float-sizes)
	(for name = (~ size "-FLOAT-SIGNALING-NOT-A-NUMBER"))
	(for symbol = (find-symbol name :closer-float))
	(when (and symbol (boundp symbol))
	  (adjoining (symbol-value symbol)))))

(defparameter nan
  (iter (for size :in float-sizes)
	(for name = (~ size "-FLOAT-NOT-A-NUMBER"))
	(for symbol = (find-symbol name :closer-float))
	(when (and symbol (boundp symbol))
	  (adjoining (symbol-value symbol)))))

(defparameter finite
  (let (name symbol)
    (iter (for size :in float-sizes)
	  (setf name (~ "MOST-POSITIVE-" size "-FLOAT"))
	  (setf symbol (find-symbol name :common-lisp))
	  (adjoining (symbol-value symbol))
	  (setf name (~ "MOST-NEGATIVE-" size "-FLOAT"))
	  (setf symbol (find-symbol name :common-lisp))
	  (adjoining (symbol-value symbol))
	  (setf name (~ size "-FLOAT"))
	  (setf symbol (find-symbol name :common-lisp))
	  (adjoining (coerce  1 symbol))
	  (adjoining (coerce -1 symbol)))))

(defparameter numbers
  (iter (for item :in (concatenate 'list finite zero inf qnan snan nan))
	(adjoining item)))

(macrolet ((def (name (fun valid))
	     (let ((item (gensym "ITEM"))
		   (list (gensym "LIST")))
	       `(define-test ,name
		  (when (fboundp ',fun)
		    (let ((,list ,valid))
		      (dolist (,item ,list)
			(assert-true (,fun ,item)))
		      (dolist (,item (set-difference numbers ,list))
			(assert-false (,fun ,item)))))))))
  (def positive-zero
      (float-positive-zero-p +zero))
  (def negative-zero
      (float-negative-zero-p -zero))
  (def zero
      (float-zero-p zero))
  (def positive-infinity
      (float-positive-infinity-p +inf))
  (def negative-infinity
      (float-negative-infinity-p -inf))
  (def infinity
      (float-infinity-p inf))
  (def quiet-not-a-number
      (float-quiet-not-a-number-p qnan))
  (def signaling-not-a-number
      (float-signaling-not-a-number-p snan))
  (def not-a-number
      (float-not-a-number-p (union (union qnan snan) nan))))

(define-test rounding-mode
  #+closer-float-rounding-mode
  (let ((saved (rounding-mode)))
    (with-rounding-mode nil
      (assert-true (eq (rounding-mode) saved))
      (dolist (mode rounding-mode-keywords)
	(setf (rounding-mode) mode)
	(expt pi (/ (1+ (random 10)) (1+ (random 10))))
	(assert-true (eq (rounding-mode) mode))))
    (when (member :nearest-even rounding-mode-keywords)
      (with-rounding-mode :nearest
	(assert-true (eq (rounding-mode) :nearest-even))))
    (when (member :nearest-away rounding-mode-keywords)
      (with-rounding-mode :nearest-away
	(assert-true (eq (rounding-mode) :nearest-away))))
    (when (member :up rounding-mode-keywords)
      (with-rounding-mode :up
	(assert-true (eq (rounding-mode) :up))))
    (when (member :down rounding-mode-keywords)
      (with-rounding-mode :down
	(assert-true (eq (rounding-mode) :down))))
    (when (member :zero rounding-mode-keywords)
      (with-rounding-mode :zero
	(assert-true (eq (rounding-mode) :zero))))
    (assert-true (eq (rounding-mode) saved))))

(let ((lisp-unit:*print-errors* t)
      (lisp-unit:*print-failures* t)
      (lisp-unit:*print-summary* t))
  (run-tests))

;;; tests.lisp ends here
