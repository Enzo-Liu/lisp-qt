;;; 2048.lisp ---
;;
;; Filename: 2048.lisp
;; Description:
;; Author: enzo liu
;; Maintainer:
;; Created: Sun Jul 10 12:19:45 2016 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Sun Jul 10 22:05:42 2016 (+0800)
;;           By: enzo liu
;;     Update #: 384
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
           #:value
           #:left-board
           #:up-board
           #:*row*
           #:*col*
           #:empty-pos
           #:down-board
           #:right-board))
(in-package #:common-2048)

(defparameter *col* 4)
(defparameter *row* 4)

(defun empty-pos (board)
  (loop for row from 0 to (1- *row*) append
       (loop for col from 0 to (1- *col*)
          when (= 0 (value row col board))
          collect (cons row col))))

(defun value (row col board)
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
  (loop for row from 0 to (1- *row*) collect
       (loop for col from 0 to (1- *col*) collect
            (if (and (= row r) (= col c))
                v
                (value row col board)))))

(defun add-random (board num)
  (let ((choices (empty-pos board)))
    (if (or (null choices) (= num 0))
        board
        (add-random
         (let* ((ri (random (length choices)))
                (r (car (nth ri choices)))
                (c (cdr (nth ri choices))))
           (add-random-at-pos board r c (if (= 0 (random 3)) 4 2)))
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



(defpackage #:ai-2048
  (:use #:cl #:common-2048 #:game-2048)
  (:export #:next-direction))
(in-package #:ai-2048)

(defun next-direction (board)
  (caar (sort (availables 1 board)
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

(defparameter *actions* '(:left :right :up :down))

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
    (+  (* 1024 order) dist
        (if (> num (* 3 (floor (* *row* *col*) 4)))
            (expt 2 num)
            (* 32 num)))
    order))

(defun order (board)
  (+ (sorted (rotate-270  board)) (sorted board)))

(defun sorted (board)
  (apply #'+ (mapcar (lambda (ls) (if (equal ls (sort (copy-list ls) #'>)) 0 (apply #'+ ls))) board)))

(defun distance (board)
  (loop for row from 0 to (1- *row*) sum
       (loop for col from 0 to (1- *col*) sum
            (distance-v row col (value row col board) board))))

(defun distance-v (r c v board)
  (let ((weight (+ r c)))
    (* weight
       (+
        (distance-2 v (value (1+ r) c board))
        (distance-2 v (value (1- r) c board))
        (distance-2 v (value r (1- c) board))
        (distance-2 v (value r (1+ c) board))))))

(defun distance-2 (v1 v2)
  (cond ((= v1 0) 0)
        ((= v2 0) 0)
        (T (* (log v1 2) (abs (- (log v1 2) (log v2 2)))))))

(defun available-direction (board direction)
  (move-board direction board))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2048.lisp ends here
