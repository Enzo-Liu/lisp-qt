;;; 2048-ai.lisp ---
;;
;; Filename: 2048-ai.lisp
;; Description:
;; Author: enzo liu
;; Maintainer:
;; Created: Sun Jul 10 12:19:45 2016 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Thu Jul 14 17:18:43 2016 (+0800)
;;           By: enzo liu
;;     Update #: 886
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
           #:hurs-new-ai
           #:max-depth-ai
           #:hurs-score-ai
           #:expt-max-ai
           #:hurs-depth-ai))

(in-package #:ai-2048)

(defparameter *actions* '(:left :right :up :down))

(defun available-direction (board direction)
  (move-board direction board))

(defun distinct-tiles (board)
  (length (remove-duplicates (flatten board))))

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
                                   0 (apply #'+ l)))
                        board)))
         (empty (empty-cells board))
         (praise (apply #'+ (mapcar (lambda (v) (if (= v 0) 0
                                               (expt (log v 2) 4)))
                                    (flatten board))))
         (merges (merge-cells board))
         (fail (if (null (available-directions board)) -100000 0))
         (dist (dist board))
         (score (+ 10000
                   fail
                   praise
                   (* -2 dist)
                   (* 512 empty)
                   (* -2 order))))
    ;; (format T
    ;;         "empty: ~a, order: ~a, dist: ~a  max: ~a, merges: ~a, score: ~a ~%"
    ;;         empty order dist max merges score)
    score))

(defmethod perf ((ai hurs-score-ai)) #'hurs-all-score)

(defclass hurs-depth-ai (hurs-ai)
  ((depth :initform 6 :initarg :depth :accessor depth))
  (:documentation "an ai based by expectation of max value generated of the board"))

(defmethod perf ((ai hurs-depth-ai))
  (lambda (board)
    (max-depth-score (depth ai) board 1.0 #'hurs-all-score)))

(defun add-random-at-pos-1 (pos value board)
  (add-random-at-pos board (car pos) (cdr pos) value))

(defun max-depth-score (depth board posi hurs-score)
  (let ((score (funcall hurs-score board)))
    (if (or (= depth 0) (< posi 0.0001)
            (and (< (apply #'max (flatten board)) 1024)
                 (< (non-empty-cells board)
                    (* *row* *col* 0.70))))
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
          (if (= 0 n) 0 (+ score (floor scores n)))))))

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

(defclass expt-max-ai (ai) () (:documentation "an expt ai implementation copy by nneonneo see: https://github.com/nneonneo/2048-ai"))

(defmethod next-direction ((ai expt-max-ai) board)
  (cdr (n-best-move board)))

(defun n-best-move (board)
  (let ((choices (mapcar (lambda (d) (cons (n-score-toplevel-move board d) d))
                         (available-directions board))))
    (car (sort choices #'> :key #'car))))

(defclass eval-state ()
  ((trans-table :initform (make-hash-table :test #'equal) :accessor trans-table)
   (max-depth :initform 0 :accessor max-depth)
   (cur-depth :initform 0 :accessor cur-depth)
   (depth-limit :initarg :depth-limit :accessor depth-limit)
   (moves-evaled :initform 0 :accessor moves-evaled)))

(defun n-encode (board)
  (apply #'+ (mapcar (lambda (v i) (ceiling (* (expt 65536 i) (n-encode-row v))))
                     (reverse board) *numlist*))
  )

(defmethod board-evaled ((state eval-state) board)
  (gethash (n-encode board) (trans-table state)))

(defmethod evaled-board ((state eval-state) board score)
  (let ((b (n-encode board)))
    (setf (gethash b (trans-table state)) (cons b score))))

(defun n-score-toplevel-move (board move)
  (let ((state (make-instance 'eval-state
                              :depth-limit (max 3  (- (distinct-tiles board) 2)))))
    (%n-score-toplevel-move state board move)))

(defun %n-score-toplevel-move (state board move)
  (let ((new-board (move-board move board)))
    (if (or (eql new-board board) (null new-board))
        0
        (n-score-tilechoose-node state new-board 1.0))))

(defun n-score-helper (board)
  (apply #'+ (mapcar #'n-score-heur-row board)))

(defun n-score-heur-row (row)
  (elt  *heur-score-table* (n-encode-row row)))


(defun range (max &key (min 0) (step 1))
  (loop for n from min below max by step
     collect n))

(defun heur-score-table ()
  (apply #'vector (mapcar #'row-score (range 65536))))

(defparameter *numlist* (range 100))

(defun n-encode-row (row)
  (apply #'+ (mapcar (lambda (v i)
                       (if (= v 0)
                           0
                           (ceiling (* (expt 16 i) (+ 1 (log v 2))))))
                     (reverse row) *numlist*)))

(defun n-decode-row (v)
  (labels ((decode (value len res)
             (if (= len 0)
                 (reverse res)
                 (multiple-value-bind (q r) (floor value (expt 16 (1- len)))
                   (decode r (1- len) (cons q res))))))
    (decode v *row* nil)))

(defun row-score (v)
  (let* ((row (n-decode-row v))
         (sum (apply #'+ (mapcar (lambda (v1) (expt 3.5 (if (= v1 0) 0 (log v1 2)))) row)))
         (empty (length (remove 0 row)))
         (merges (- (length (remove-if-not (lambda (v) (= v 0)) (squeeze row))) empty))
         (ml (apply #'+ (maplist (lambda (ls) ()
                                    (if (< (length ls) 2)
                                        0
                                        (if (< (car ls) (cadr ls))
                                            (abs (- (expt (car ls) 2) (expt (cadr ls) 2)))
                                            0)))
                                 row)))
         (mr (apply #'+ (maplist (lambda (ls) ()
                                    (if (< (length ls) 2)
                                        0
                                        (if (< (car ls) (cadr ls))
                                            (abs (- (expt (car ls) 2) (expt (cadr ls) 2)))
                                            0)))
                                 (reverse row)))))
    (+ 200000
       (* -11 sum)
       (* 1400 merges)
       (* 270 empty)
       (* -47 (min ml mr)))))

(defparameter *heur-score-table* (heur-score-table))
(defun n-score-heur-board (board)
  (+ (n-score-helper board) (n-score-helper (rotate board)))
  ;;(hurs-all-score board)
  )

(defun n-score-tilechoose-node (state board prob)
  (let ((cache (board-evaled state board))
        (choices (empty-pos board)))
    (cond ((or (null choices) (< prob 0.0001) (>= (cur-depth state) (depth-limit state)))
           (setf (max-depth state) (max (cur-depth state) (max-depth state)))
           (n-score-heur-board board))
          ((and (< (cur-depth state) 15) cache ;(< (car cache) (cur-depth state))
                )
           (cdr cache))
          (T
           (let* ((len (length choices))
                  (n-prob (float (/ prob len)))
                  (score (float
                          (/
                           (apply #'+
                                  (mapcar (lambda (pos)
                                            (n-random-pos-score state board pos n-prob))
                                          choices))
                           len))))
             (if (< (cur-depth state) 15)
                 (evaled-board state board score))
             score)))))

(defun n-random-pos-score (state board pos prob)
  (+ (* *posi-4* (n-score-move-node state
                                    (add-random-at-pos-1 pos 4 board) (* prob *posi-4*)))
     (* *posi-2* (n-score-move-node state
                                    (add-random-at-pos-1 pos 2 board) (* prob *posi-2*)))))

(defun n-score-move-node (state board prob)
  (let ((choices (available-directions board))
        (score 0))
    (when choices
      (incf (cur-depth state))
      (setf score (apply #'max (mapcar (lambda (d)
                                         (n-score-tilechoose-node state
                                                                  (move-board d board)
                                                                  prob))
                                       (available-directions board))))
      (decf (cur-depth state)))
    score))

(defclass hurs-new-ai (ai) () (:documentation "an ai based by last"))

(defmethod next-direction ((ai hurs-new-ai) board)
  (let* ((ops (mapcar (lambda (d) (cons d (n-score-heur-board (move-board d board))))
                      (available-directions board)))
         (ordered (sort ops #'> :key #'cdr) )
         (best (car ordered)))
    (car best)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2048.lisp ends here
