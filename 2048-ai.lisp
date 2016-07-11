;;; 2048-ai.lisp ---
;;
;; Filename: 2048-ai.lisp
;; Description:
;; Author: enzo liu
;; Maintainer:
;; Created: Sun Jul 10 12:19:45 2016 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Tue Jul 12 01:07:55 2016 (+0800)
;;           By: enzo liu
;;     Update #: 582
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(load "./2048.lisp")
(defpackage #:ai-2048
  (:use #:cl #:common-2048 #:game-2048)
  (:export #:next-direction
           #:dumb-ai
           #:max-ai))

(in-package #:ai-2048)

(defparameter *actions* '(:left :right :up :down))

(defun available-direction (board direction)
  (move-board direction board))

(defun available-directions (board)
  (remove-if-not (lambda (d) (available-direction board d)) *actions*))

(defun empty-cells (board)
  (length (empty-pos board)))


(defclass ai () () (:documentation "an ai interface"))

(defgeneric next-direction (ai board)
  (:documentation "the ai interface to provide next-direction based by current board."))


(defclass dumb-ai (ai) () (:documentation "an dumb ai implementation"))

(defmethod next-direction ((ai dumb-ai) board)
  (random-action board))

(defun random-action (board)
  (let ((choices (available-directions board)))
    (unless (null choices) (nth (random (length choices)) choices))))

(defclass hurs-ai (ai) () (:documentation "an ai based by expectation of heuristic value of the board"))

(defgeneric perf (hurs-ai) (:documentation "the hurs function"))
(defgeneric depth (hurs-ai) (:documentation "the calculate depth"))

(defclass max-ai (hurs-ai) () (:documentation "an ai based by expectation of max value generated of the board"))

(defun max-score (board) (apply #'max (flatten board)))
(defmethod perf ((ai max-ai)) #'max-score)
(defmethod depth ((ai max-ai)) 2)

(defmethod next-direction ((ai hurs-ai) board)
  (let* ((ordered (most-score board (depth ai) 1.0 (perf ai)))
         (best (car ordered)))
    (if (= 0 (car best))
        (random-action board)
        (cdr best))))

(defun most-score (board depth posi perf)
  (sort (mapcar (lambda (dire) (score board dire depth posi perf)) *actions*)
        #'> :key #'car))

(defun score (board d depth posi perf)
  (let ((next (move-board d board)))
    (if next
        (cons (score-board next (1- depth) posi perf) d)
        (cons 0 d))))

(defparameter *posi-4* (/ *limit-4* *base*))
(defparameter *posi-2* (/ (- *base* *limit-4*) *base*))

(defun score-board (board depth posi perf)
  (apply #'+
         (mapcar (lambda (pos)
                   (+ (* *posi-2*
                         (score-real (add-random-at-pos board
                                                        (car pos)
                                                        (cdr pos)
                                                        2)
                                     depth
                                     (* *posi-2* posi)
                                     perf))
                      (* *posi-4*
                         (score-real (add-random-at-pos board
                                                        (car pos)
                                                        (cdr pos)
                                                        4)
                                     depth
                                     (* *posi-4* posi)
                                     perf))))
                 (empty-pos board))))

(defun score-real (board depth posi perf)
  (let ((score (funcall perf board)))
    (if (or (= 0 depth) (< posi 0.005))
        score
        (let ((best (car (most-score board depth posi perf))))
          (if (= 0 (car best))
              0
              (/ (+ score (car best)) 2))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2048.lisp ends here
