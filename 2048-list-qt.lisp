;;; 2048-list-qt.lisp ---
;;
;; Filename: 2048-list-qt.lisp
;; Description:
;; Author: enzo liu
;; Maintainer:
;; Created: Sat Jul  9 07:09:09 2016 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Sat Jul  9 17:57:29 2016 (+0800)
;;           By: enzo liu
;;     Update #: 248
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

(ql:quickload '(qtools qtcore qtgui))

(defpackage #:qtools-intro
  (:use #:cl+qt  :trivial-main-thread)
  (:export #:main))

(in-package #:qtools-intro)
(in-readtable :qtools)


(define-widget game (QWidget)
  ((board :initarg :board
          :accessor board)))

(define-widget tile (QLabel)
  ((value :initarg :value
          :accessor value )))

(defparameter *style-list*
  '((0 . "background: rgb(204,192,179); border-radius: 10px;")
    (2 . "background: rgb(238,228,218); color: rgb(119,110,101); font: bold; border-radius: 10px; font: 40pt;")
    (4 . "background: rgb(237,224,200); color: rgb(119,110,101); font: bold; border-radius: 10px; font: 40pt; ")
    (8 . "background: rgb(242,177,121); color: rgb(255,255,255); font: bold; border-radius: 10px; font: 40pt;")
    (16 . "background: rgb(245,150,100); color: rgb(255,255,255); font: bold; border-radius: 10px; font: 40pt;")
    (32 . "background: rgb(245,125,95); color: rgb(255,255,255); font: bold; border-radius: 10px; font: 40pt;")
    (64 . "background: rgb(245,95,60); color: rgb(255,255,255); font: bold; border-radius: 10px; font: 40pt;")
    (128 . "background: rgb(237,207,114); color: rgb(255,255,255); font: bold; border-radius: 10px; font: 40pt;")
    (256 . "background: rgb(237,204,97); color: rgb(255,255,255); font: bold; border-radius: 10px; font: 40pt;")
    (512 .  "background: rgb(237,200,80); color: rgb(255,255,255); font: bold; border-radius: 10px; font: 40pt;")
    (1024 . "background: rgb(210,161,68); color: rgb(255,255,255); font: bold; border-radius: 10px; font: 40pt;")
    (2048 . "background: rgb(237.194.46); color: rgb(255,255,255); font: bold; border-radius: 10px; font: 40pt;")))

(defun blur (score)
  (let ((res (cdr (assoc score *blur-list*))))
    (if (null res) 0 res)))

(defun style (score)
  (let ((res (cdr (assoc score *style-list*))))
    (if (null res)
        "QLabel {background: rgb(47 43 37); color: rgb(255,255,255); font: bold; border-radius: 10px; font: 40pt;}"
        (format nil "QLabel { ~a }" res))))

(defun index (l row col)
  (nth col (nth row l)))

;; a macro for looping over the board
(defmacro board-loop ((row a b) (col c d) form &body body)
  `(loop for row from ,a to ,b ,form
                                (loop for col from ,c to ,d ,form
                                                             (progn ,@body))))
;; a macro for getting a board value
(defmacro bval (b r c)
  `(value (index (board ,b) ,r ,c)))

;; this method checks if the board is full
(defmethod fullp ((this game))
  (let ((full t))
    (board-loop (row 0 3) (col 0 3) do
      (when (= (bval this row col) 0)
        (setf full nil)))
    full))

(defmethod left ((this game))
  ;; assume no change, set to true on change
  (let ((change nil))
    ;; define a local function for shifting left once
    (flet ((shift-left (row)
             ;; for each column
             (loop for col from 0 to 3 do
               ;; when this cell is not zero
               (when (/= (bval this row col) 0)
                 ;; for each cell from here to 1
                 (loop for k from col downto 1 do
                   ;; if it can move to the right, move it
                   (when (= (bval this row (- col k)) 0)
                     (setf (bval this row (- col k)) (bval this row col))
                     (setf (bval this row col) 0)
                     (setf change t)))))))
      ;; for each row
      (loop for row from 0 to 3 do
        (shift-left row)
        ;; now we need to compact the adjacent ones
        (loop for col from 0 to 3 do
          ;; if it can be combined
          (when (and (< (+ col 1) 4) (= (bval this row col) (bval this row (+ col 1))))
            ;; combine it
            (setf (bval this row col) (* (bval this row col) 2))
            (setf (bval this row (+ col 1)) 0)
            (when (/= (bval this row col) 0)
              (setf change t))))
        ;; check again to see if we can move things to the left
        (shift-left row))
      ;; whether there was any change
      change)))

;; this function rotates the board by 90 degrees counter-clockwise
(defmethod rotate ((this game))
  (setf (board this) (reverse (apply #'mapcar #'list (board this)))))

;; to move right, rotate twice, left, rotate twice
(defmethod right ((this game))
  (let ((change nil))
    (dotimes (i 2) (rotate this))
    (setf change (left this))
    (dotimes (i 2) (rotate this))
    change))

;; to move up, rotate once, left, rotate 3 times
(defmethod up ((this game))
  (let ((change nil))
    (rotate this)
    (setf change (left this))
    (dotimes (i 3) (rotate this))
    change))

;; to move down rotate three times, left, rotate once
(defmethod down ((this game))
  (let ((change nil))
    (dotimes (i 3) (rotate this))
    (setf change (left this))
    (rotate this)
    change))

;; this method adds a random 2 or 4 cell into an empty space
(defmethod add-random ((this game) &optional (num 1))
  (when (not (fullp this))
    (let ((row (random 4)) (col (random 4)))
      (if (= (bval this row col) 0)
          (setf (value (nth col (nth row (board this)))) (if (= (random 2) 0) 2 4))
          (add-random this))))
  (when (> num 1)
    (add-random this (- num 1))))

(defmethod initialize-instance :after ((tile tile) &key)
  (q+ set-alignment tile (q+ qt.align-center)))

(defmethod update-board ((this game))
  (board-loop (row 0 3) (col 0 3) do
    (update (index (board this) row col))))

;; the constructor for the board-widget
(defmethod initialize-instance :after ((game game) &key)
  (q+ set-geometry game 100 100 500 355)
  (q+ set-style-sheet game "QWidget{background-color: rgb(187,173,160)}")
  (q+ set-window-title game "2048 in sbcl by qtools")
  (add-random game 2)
  (update-board game))

(define-override (game key-press-event) (event)
  (when (cond ((= (#_key event) (enum-value (#_Qt::Key_Left))) (left game))
              ((= (#_key event) (enum-value (#_Qt::Key_Up))) (up game))
              ((= (#_key event) (enum-value (#_Qt::Key_Right))) (right game))
              ((= (#_key event) (enum-value (#_Qt::Key_Down))) (down game))
              (t nil))
    (add-random game 1)
    (update-board game)
    (check-end game)))

(defmethod update ((tile tile))
  (let ((score (value tile)))
    (q+ set-text tile (if (eql score 0) "" (format nil "~a" (value tile))))
    (q+ set-style-sheet tile (style score))))

(define-subwidget (game layout) (q+:make-qgridlayout game)
  (board-loop (row 0 3) (col 0 3) do
    (q+ add-widget layout (index (board game) row col) row col)))

;; this method returns whether the game is lost
(defmethod lostp ((this game))
  ;; assume that we did lose
  (let ((lost t))
    ;; check the verticals
    (board-loop (row 0 3) (col 0 2) do
      (when (= (bval this row col) (bval this row (+ col 1)))
        (setf lost nil)))
    ;; check the horizontals
    (board-loop (row 0 2) (col 0 3) do
      (when (= (bval this row col) (bval this (+ row 1) col))
        (setf lost nil)))
    ;; check for zeroes
    (when (not (fullp this))
      (setf lost nil))
    lost))

(defun ask-question (title text)
  (let ((dialog (#_new QMessageBox)))
    (#_setWindowTitle dialog title)
    (#_setText dialog text)
    (#_addButton dialog (#_QMessageBox::No))
    (#_addButton dialog (#_QMessageBox::Yes))
    (= (#_exec dialog) (enum-value (#_QMessageBox::Yes)))))

;; this method returns whether the game is won
(defmethod wonp ((this game))
  (let ((won nil))
    (board-loop (row 0 3) (col 0 3) do
      (when (= (bval this row col) 2048)
        (setf won t)))
    won))

(defmethod check-end ((this game))
  (let ((quit nil) (reset nil))
    ;; if the game is won or lost, ask to reset
    (when (lostp this)
      (if (ask-question "Sorry" "You lost! Play again?")
          (setf reset t)
          (setf quit t)))
    (when (wonp this)
      (if (ask-question "Congratulations" "You won! Play again?")
          (setf reset t)
          (setf quit t)))
    (when reset
      (board-loop (row 0 3) (col 0 3) do
        (setf (bval this row col) 0))
      (add-random this 2)
      (update-board this))
    (when quit
      (#_QCoreApplication::exit 0))))

(defun main ()
  (with-main-window
      (window
       (make-instance 'game
                      :board (board-loop (row 0 3) (col 0 3) collect
                               (make-instance 'tile :value 0))))))

(defun restart-game ()
  (#_QCoreApplication::exit 0)
  (with-body-in-main-thread ()
    (main)))

(restart-game)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2048-list-qt.lisp ends here
