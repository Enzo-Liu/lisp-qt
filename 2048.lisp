;;; 2048.lisp ---
;;
;; Filename: 2048.lisp
;; Description:
;; Author: enzo liu
;; Maintainer:
;; Created: Sun Jul 10 12:19:45 2016 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Mon Jul 11 23:31:02 2016 (+0800)
;;           By: enzo liu
;;     Update #: 474
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

(defpackage #:common-2048
  (:use #:cl)
  (:export #:rotate
           #:rotate-180
           #:rotate-270
           #:squeeze
           #:board-value
           #:left-board
           #:up-board
           #:*row*
           #:*col*
           #:empty-pos
           #:down-board
           #:board-loop
           #:right-board
           #:flatten
           #:*base*
           #:*limit-4*))
(in-package #:common-2048)

(defmacro board-loop (row col form &body body)
  `(loop for ,row from 0 to (1- *row*) ,form
        (loop for ,col from 0 to (1- *col*) ,form
             (progn ,@body))))

(defparameter *base* 10)
(defparameter *limit-4* 1)
(defparameter *col* 4)
(defparameter *row* 4)

(defun flatten (l)
  (loop for a in l appending a))

(defun empty-pos (board)
  (remove nil (flatten (board-loop row col collect (when (= 0 (board-value row col board)) (cons row col))))))

(defun board-value (row col board)
  (cond ((< row 0) 0)
        ((>= row *row*) 0)
        ((>= col *col*) 0)
        ((< col 0) 0)
        (T (nth col (nth row board)))))

(defun rotate (board) (mapcar #'reverse (apply #'mapcar (cons #'list board))))

(defun rotate-180 (board) (rotate (rotate board)))

(defun rotate-270 (board) (rotate (rotate-180 board)))

(defun merge1 (ls)
  (cond ((<= (length ls) 1) ls)
        ((= (car ls) (cadr ls)) (cons (* 2 (car ls)) (merge1 (cddr ls))))
        (T (cons (car ls) (merge1 (cdr ls))))))

;; filter all 0 , left merge , and then padding the 0
(defun squeeze (ls)
  (let* ((len (length ls))
         (toMerge (remove 0 ls))
         (merged (merge1 toMerge)))
    (append merged (make-list (- len (length merged)) :initial-element 0))))

(defun left-board (board)
  (mapcar #'squeeze board))

(defun right-board (board)
  (rotate-180 (left-board (rotate-180 board))))

(defun up-board (board)
  (rotate (left-board (rotate-270 board))))

(defun down-board (board)
  (rotate-270 (left-board (rotate board))))

(defpackage #:game-2048
  (:use #:cl #:common-2048)
  (:export #:add-random
           #:add-random-at-pos
           #:move-board
           #:ended
           #:board-loop
           #:board-value
           #:succeeded
           #:*row*
           #:*col*))

(in-package #:game-2048)

(defun any (ls)
  (cond ((= 0 (length ls)) nil)
        ((car ls) T)
        (T (any (cdr ls)))))

(defun fullp (board) (not (any (mapcar (lambda (l) (member 0 l)) board))))

(defun add-random-at-pos (board r c v)
  (board-loop row col collect (if (and (= row r) (= col c))
                                  v
                                  (board-value row col board))))

(defun add-random (board num)
  (let ((choices (empty-pos board)))
    (if (or (null choices) (= num 0))
        board
        (add-random
         (let* ((ri (random (length choices)))
                (choice (nth ri choices))
                (r (car choice))
                (c (cdr choice)))
           (add-random-at-pos board r c (if (< *limit-4* (random *base*)) 4 2)))
         (1- num)))))

;; left merge

(defun move-board (direction board)
  (let ((new-board
         (case direction
           (:left (left-board board))
           (:up (up-board board))
           (:down (down-board board))
           (:right (right-board board)))))
    (if (equal new-board board) nil new-board)))

(defun ended (board)
  (not (any (mapcar (lambda (dire) (move-board dire board)) '(:left :up :down :right)))))

(defun succeeded (board)
  (any (mapcar (lambda (ls) (some (lambda (x) (>= x 2048)) ls)) board)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2048.lisp ends here
