;;; qtools.lisp ---
;;
;; Filename: qtools.lisp
;; Description:
;; Author: enzo liu
;; Maintainer:
;; Created: Sat Jul  9 00:15:27 2016 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Sat Jul  9 00:21:59 2016 (+0800)
;;           By: enzo liu
;;     Update #: 11
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

(define-widget main-window (QWidget)
  ())

(define-subwidget (main-window name) (q+:make-qlineedit main-window)
  (setf (q+:placeholder-text name) "Your name please."))

(define-subwidget (main-window go) (q+:make-qpushbutton "Go!" main-window))

(define-subwidget (main-window layout) (q+:make-qhboxlayout main-window)
  (q+:add-widget layout name)
  (q+:add-widget layout go))

(define-signal (main-window name-set) (string))

(define-slot (main-window go) ()
  (declare (connected go (pressed)))
  (declare (connected name (return-pressed)))
  (signal! main-window (name-set string) (q+:text name)))

(define-slot (main-window name-set) ((new-name string))
  (declare (connected main-window (name-set string)))
  (q+:qmessagebox-information main-window "Greetings" (format NIL "Good day to you, ~a!" new-name)))

(define-override (main-window paint-event) (event)
  (declare (ignore event))
  (with-finalizing ((painter (q+:make-qpainter main-window)))
    (q+:fill-rect painter (q+:rect main-window) (q+:qt.white))))

(defun main ()
  (with-main-window (window (make-instance 'main-window))
    (#_setGeometry window 100 100 500 355)))

(defun test ()
  (with-body-in-main-thread ()
    (main)))

(test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; qtools.lisp ends here
