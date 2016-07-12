;;; 2048-ai.lisp ---
;;
;; Filename: 2048-ai.lisp
;; Description:
;; Author: enzo liu
;; Maintainer:
;; Created: Sun Jul 10 12:19:45 2016 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Tue Jul 12 18:49:00 2016 (+0800)
;;           By: enzo liu
;;     Update #: 715
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
           #:max-ai
           #:max-depth-ai
           #:hurs-score-ai
           #:hurs-depth-ai))

(in-package #:ai-2048)

(defparameter *actions* '(:left :right :up :down))

(defun available-direction (board direction)
  (move-board direction board))

(defun available-directions (board)
  (remove-if-not (lambda (d) (available-direction board d)) *actions*))

(defun non-empty-cells (board)
  (length (remove 0 (flatten board))))

(defun empty-cells (board)
  (if (null board)
      0
      (length (empty-pos board))))

(defun merge-cells (board)
  (- (apply #'max (mapcar (lambda (d) (empty-cells (move-board d board))) *actions*))
     (length (empty-pos board))))

(defun random-action (board)
  (let ((choices (available-directions board)))
    (unless (null choices) (nth (random (length choices)) choices))))

(defun most-score (board perf)
  (sort (mapcar (lambda (dire) (score board dire perf)) *actions*)
        #'> :key #'car))

(defun score (board d perf)
  (let ((next (move-board d board)))
    (if next
        (cons (funcall perf next) d)
        (cons 0 d))))

(defparameter *posi-4* (/ *limit-4* *base*))
(defparameter *posi-2* (/ (- *base* *limit-4*) *base*))


(defclass ai () () (:documentation "an ai interface"))

(defgeneric next-direction (ai board)
  (:documentation "the ai interface to provide next-direction based by current board."))


(defclass dumb-ai (ai) () (:documentation "an dumb ai implementation"))

(defmethod next-direction ((ai dumb-ai) board)
  (random-action board))

(defclass hurs-ai (ai) () (:documentation "an ai based by expectation of heuristic value of the board"))

(defmethod next-direction ((ai hurs-ai) board)
  (let* ((ordered (most-score board (perf ai)))
         (best (car ordered)))
    (if (= 0 (car best))
        (random-action board)
        (cdr best))))

(defgeneric perf (hurs-ai) (:documentation "the hurs function"))

(defclass max-ai (hurs-ai) () (:documentation "an ai based by expectation of max value generated of the board"))
(defun max-score (board) (apply #'max (flatten board)))
(defmethod perf ((ai max-ai)) #'max-score)

(defclass max-depth-ai (hurs-ai)
  ((depth :initform 3 :initarg :depth :accessor depth))
  (:documentation "an ai based by expectation of max value generated of the board"))

(defmethod perf ((ai max-depth-ai))
  (lambda (board)
    (max-depth-score (depth ai) board 1.0 #'max-score)))

(defclass hurs-score-ai (hurs-ai) () (:documentation "an ai based by expectation of max value generated of the board"))

(defun hurs-all-score (board)
  (+ (hurs-all-score-1 board)
     (hurs-all-score-1 (rotate-270 board))))

(defun dist (board)
  (apply #'+ (mapcar (lambda (ls) (apply #'+ (maplist (lambda (l)
                                                   (if (< (length l) 2)
                                                       0
                                                       (abs (- (car l) (cadr l)))))
                                                 ls)))
                     board)))

(defun hurs-all-score-1 (board)
  (let* ((order (apply #'+
                       (mapcar
                        (lambda (l) (if (equal l (sort (copy-list l) #'>))
                                   0 (apply #'max l)))
                        board)))
         (empty (empty-cells board))
         (max (apply #'max (flatten board)))
         (merges (merge-cells board))
         (dist (dist board))
         (score (+ 10000 (* 512 merges)
                   (* 2 (if (= max 0) 1  max))
                   (* -2 dist)
                   (* 512 empty)
                   (* -2 order))))
    ;; (format T
    ;;         "empty: ~a, order: ~a, dist: ~a  max: ~a, merges: ~a, score: ~a ~%"
    ;;         empty order dist max merges score)
    score))

(defmethod perf ((ai hurs-score-ai)) #'hurs-all-score)

(defclass hurs-depth-ai (hurs-ai)
  ((depth :initform 3 :initarg :depth :accessor depth))
  (:documentation "an ai based by expectation of max value generated of the board"))

(defmethod perf ((ai hurs-depth-ai))
  (lambda (board)
    (max-depth-score (depth ai) board 1.0 #'hurs-all-score)))

(defun add-random-at-pos-1 (pos value board)
  (add-random-at-pos board (car pos) (cdr pos) value))

(defun max-depth-score (depth board posi hurs-score)
  (let ((score (funcall hurs-score board)))
    (if (or (= depth 0) (< posi 0.0001) (< (non-empty-cells board)
                                           (floor (* *row* *col*) 2)))
        score
        (let* ((choices (empty-pos board))
               (scores  (loop for pos in choices sum
                             (+
                              (* *posi-4* (max-depth-score (1- depth)
                                                           (add-random-at-pos-1 pos 4 board)
                                                           (* posi *posi-4*)
                                                           hurs-score))
                              (* *posi-2* (max-depth-score (1- depth)
                                                           (add-random-at-pos-1 pos 2 board)
                                                           (* posi *posi-2*)
                                                           hurs-score)))))
               (n (length choices)))
          (if (= 0 n) 0 (/ (+ score (floor scores n)) 2))))))

;; (defun memoize (fn)
;;   (let ((cache (make-hash-table :test #'equal)))
;;     #'(lambda (&rest args)
;;         (multiple-value-bind
;;               (result exists)
;;             (gethash args cache)
;;           (if exists
;;               result
;;               (setf (gethash args cache)
;;                     (apply fn args)))))))

;; (setf (fdefinition 'max-depth-score) (memoize #'max-depth-score))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2048.lisp ends here
