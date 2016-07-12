;;; 2048-list-qt.lisp ---
;;
;; Filename: 2048-list-qt.lisp
;; Description:
;; Author: enzo liu
;; Maintainer:
;; Created: Sat Jul  9 07:09:09 2016 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Tue Jul 12 18:42:59 2016 (+0800)
;;           By: enzo liu
;;     Update #: 637
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
(load "./2048.lisp")
(load "./2048-ai.lisp")

(defpackage #:qt-2048
  (:use #:cl+qt  :trivial-main-thread #:game-2048 #:ai-2048)
  (:export #:main))

(in-package #:qt-2048)
(in-readtable :qtools)

(defun make-board (r c)
  (board-loop row col collect 0))

(define-widget game (QWidget)
  ((board :initarg :board :accessor board)
   (tiles :initarg :tiles :accessor tiles)
   (ai :initarg :ai :accessor ai)))

;; the constructor for the board-widget
(defmethod initialize-instance :after ((game game) &key)
  (q+ set-geometry game 100 100 500 555)
  (q+ set-style-sheet game "QWidget{background-color: rgb(187,173,160)}")
  (q+ set-window-title game "2048 in sbcl by qtools")
  (setf (board game) (add-random (board game) 2))
  (notify-board-update game))

(define-signal (game board-update) ())

(define-slot (game board-update) ()
  (declare (connected game (board-update)))
  (update-board game))

(define-signal (game fail) ())

(define-slot (game fail) ()
  (declare (connected game (fail)))
  (if (ask-question "Sorry" "You lost! Play again?")
      (reset game)
      (quit game)))

(define-signal (game success) ())

(define-slot (game success) ()
  (declare (connected game (success)))
  (if (ask-question "Congratulations" "You won! Play again?")
      (reset game)
      (quit game)))

(defmethod reset ((game game))
  (setf (board game) (add-random (make-board *row* *col*) 2))
  (notify-board-update game))

(defmethod quit ((game game)) (#_QCoreApplication::exit 0))

(defmethod move ((game game) direction)
  (let ((board (move-board direction (board game))))
    (when board
      (setf (board game) (add-random board 1))
      (notify-board-update game)
      (check-end game))))

(defmethod check-end ((game game))
  (cond ((ended (board game))
         (notify-game-fail game))
        ((succeeded (board game))
         ;;(notify-game-success game)
         )
        (T nil)))

(defun ask-question (title text)
  (let ((dialog (#_new QMessageBox)))
    (#_setWindowTitle dialog title)
    (#_setText dialog text)
    (#_addButton dialog (#_QMessageBox::No))
    (#_addButton dialog (#_QMessageBox::Yes))
    (= (#_exec dialog) (enum-value (#_QMessageBox::Yes)))))

(defmethod notify-board-update ((game game))
  (signal! game (board-update)))

(defmethod notify-game-fail ((game game))
  (signal! game (fail)))

(defmethod notify-game-success ((game game))
  (signal! game (success)))

(defmethod update-board ((game game))
  (let ((board (board game))
        (tiles (tiles game)))
    (mapcar (lambda (bl tl) (mapcar #'update tl bl)) board tiles)))

(define-widget tile (QLabel) ())

(defmethod initialize-instance :after ((tile tile) &key)
  (q+ set-alignment tile (q+ qt.align-center)))

(defun make-tiles (r c)
  (board-loop row col collect (make-instance 'tile)))

(defparameter *style-list*
  '((0 . "background: rgb(204,192,179); border-radius: 10px;")
    (2 . "background: rgb(238,228,218); color: rgb(119,110,101); font: bold; border-radius: 10px; font: 40pt;")
    (4 . "background: rgb(237,224,200); color: rgb(119,110,101); font: bold; border-radius: 10px; font: 40pt; ")
    (8 . "background: rgb(242,177,121); color: rgb(255,255,255); font: bold; border-radius: 10px; font: 40pt;")
    (16 . "background: rgb(245,150,100); color: rgb(255,255,255); font: bold; border-radius: 10px; font: 40pt;")
    (32 . "background: rgb(245,125,95); color: rgb(255,255,255); font: bold; border-radius: 10px; font: 40pt;")
    (64 . "background: rgb(245,95,60); color: rgb(255,255,255); font: bold; border-radius: 10px; font: 40pt;")
    (128 . "background: rgb(237,207,114); color: rgb(255,255,255); font: bold; border-radius: 10px; font: 32pt;")
    (256 . "background: rgb(237,204,97); color: rgb(255,255,255); font: bold; border-radius: 10px; font: 32pt;")
    (512 .  "background: rgb(237,200,80); color: rgb(255,255,255); font: bold; border-radius: 10px; font: 32pt;")
    (1024 . "background: rgb(210,161,68); color: rgb(255,255,255); font: bold; border-radius: 10px; font: 24pt;")
    (2048 . "background: rgb(237.194.46); color: rgb(255,255,255); font: bold; border-radius: 10px; font: 24pt;")))

(defun style (num)
  (let ((res (cdr (assoc num *style-list*))))
    (if (null res)
        "QLabel {background: rgb(47 43 37); color: rgb(255,255,255); font: bold; border-radius: 10px; font: 24pt;}"
        (format nil "QLabel { ~a }" res))))

(defun direction (event)
  (cond ((= (#_key event) (enum-value (#_Qt::Key_Left))) :left)
        ((= (#_key event) (enum-value (#_Qt::Key_Up))) :up)
        ((= (#_key event) (enum-value (#_Qt::Key_Right))) :right)
        ((= (#_key event) (enum-value (#_Qt::Key_Down))) :down)
        (t nil)))

(define-override (game key-press-event) (event)
  (move game (direction event)))

(defun update (tile num)
  (q+ set-text tile (if (eql num 0) "" (format nil "~a" num)))
  (q+ set-style-sheet tile (style num)))

(define-subwidget (game layout) (q+:make-qvboxlayout game))

(define-subwidget (game ai) (q+:make-qhboxlayout)
  (q+ add-layout layout ai))

(define-subwidget (game buttons) (q+:make-qvboxlayout)
  (q+ add-layout ai buttons))

(define-subwidget (game grid) (q+:make-qgridlayout)
  (q+ insert-layout layout 0 grid)
  (board-loop row col do
    (q+ add-widget grid (board-value row col (tiles game)) row col)))

(unless (boundp '*game*)
  (defparameter *game* nil))

(defun main ()
  (with-main-window
      (window
       (setf *game* (make-instance 'game
                                   :board (make-board *row* *col*)
                                   :tiles (make-tiles *row* *col*)
                                   :ai 'dumb-ai)))))

(defun restart-game ()
  (when *game* (quit *game*))
  (call-in-main-thread #'main))

(define-subwidget (game run) (q+:make-qpushbutton "run" game)
  (q+ add-widget ai run))

(define-slot (game change-ai) ((ai string))
  (setf (ai game) ai))

(define-slot (game run-with-ai) ()
  (declare (connected run (pressed)))
  (sb-thread:make-thread
   (lambda () (try-ai (make-instance (ai game))))))

(defmacro new-ai-slot (ai-name text)
  `(progn
     (define-subwidget (game ,ai-name) (q+:make-qradiobutton ,text game)
       (q+ add-widget buttons ,ai-name))
     (define-slot (game ,(make-symbol (format nil "change-to-~a"  ai-name))) ()
       (declare (connected ,ai-name (pressed)))
       (setf (ai game) ',ai-name))))

(new-ai-slot max-ai "choose one direction by max value generated")
(new-ai-slot dumb-ai "choose one direction randomly")
(new-ai-slot max-depth-ai "choose by max value generated in depth ")
(new-ai-slot hurs-score-ai "choose by hurs score")
(new-ai-slot hurs-depth-ai "choose by hurs score in depth ")

(defun try-ai (ai)
  (let ((direction (next-direction ai (board *game*))))
    (when direction
      (move *game* direction)
      (sleep 0.1)
      (try-ai ai))))

(restart-game)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2048-list-qt.lisp ends here
