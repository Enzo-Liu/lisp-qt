;;; 2048-ai.lisp ---
;;
;; Filename: 2048-ai.lisp
;; Description:
;; Author: enzo liu
;; Maintainer:
;; Created: Sun Jul 10 12:19:45 2016 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Mon Jul 11 21:11:05 2016 (+0800)
;;           By: enzo liu
;;     Update #: 480
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
           #:hurs-ai))

(in-package #:ai-2048)

(defparameter *actions* '(:left :right :up :down))

(defun available-direction (board direction)
  (move-board direction board))

(defun empty-cells (board)
  (length (empty-pos board)))


(defclass ai () () (:documentation "an ai interface"))

(defgeneric next-direction (ai board)
  (:documentation "the ai interface to provide next-direction based by current board."))


(defclass dumb-ai (ai) () (:documentation "an dumb ai implementation"))

(defmethod next-direction ((ai dumb-ai) board)
  (let ((choices (remove-if-not (lambda (d) (move-board d board)) *actions*)))
    (unless (null choices)
      (nth (random (length choices)) choices))))


(defclass hurs-ai (ai) () (:documentation "an ai based by expectation of heuristic score of the board"))

(defmethod next-direction ((ai hurs-ai) board)
  (let ))

(defun next-direction-dumb (board)
  (caar (sort (availables 2 board)
              #'<
              :key
              (lambda (ds) (avg-next-performance (move-boards ds board))))))

(defun availables (deepth board)
  (remove-if-not (lambda (as) (move-boards as board)) (actions deepth)))

(defun actions (deepth)
  (if (= deepth 0)
      (mapcar (lambda (a) (cons a nil)) *actions*)
      (mapcan (lambda (as) (mapcar (lambda (a) (cons a as)) *actions*))
              (actions (1- deepth)))))

(defun move-boards (ds b)
  (if (null b) nil
      (if (null ds)
          b
          (move-boards (cdr ds) (move-board (car ds) b)))))


;; 数量少
;; 距离近
;;
(defun average (&rest args)
  (when args
    (floor (apply #'+ args) (length args))))

(defun avg-next-performance (board)
  (let ((choices (empty-pos board)))
    (apply #'average (mapcar
                      (lambda (p) (min (performance (add-random-at-pos board (car p) (cdr p) 2))
                                  (performance (add-random-at-pos board (car p) (cdr p) 4))
                                  4))
                      choices))))

(defun performance (board)
  (let ((num (apply #'+ (mapcar (lambda (l) (length (remove 0 l))) board)))
        (dist (distance board))
        (order (order board)))
    (+  (* order dist) (expt 2 num))))

(defun order (board)
  (+ (sorted (rotate board)) (sorted board)))

(defun sorted (board)
  (apply #'+ (mapcar (lambda (ls) (if (equal ls (sort (copy-list ls) #'>)) 0 (apply #'+ ls))) board)))

(defun distance (board)
  (board-loop row col sum (distance-v row col (board-value row col board) board)))

(defun distance-v (r c v board)
  (let ((weight (+ r c)))
    (* weight (if (= v 0) 1 v)
       (+
        (distance-2 v (board-value (1+ r) c board))
        (distance-2 v (board-value (1- r) c board))
        (distance-2 v (board-value r (1- c) board))
        (distance-2 v (board-value r (1+ c) board))))))

(defun distance-2 (v1 v2)
  (cond ((= v1 0) 4096)
        ((= v2 0) 0)
        (T (* (log v1 2) (abs (- (log v1 2) (log v2 2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2048.lisp ends here
