;;; plong.lisp --- a simple remixable ball-and-paddle example for Xelf

;; Copyright (C) 2014  David O'Toole

;; Author: David O'Toole <dto@blocky.io>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defpackage :plong
  (:use :cl :xelf)
  (:export plong))

(in-package :plong)

(defparameter *unit* 16)

(defun units (n) (* *unit* n))

(defparameter *width* 640)
(defparameter *height* 480)

;;; A bouncing ball

(defclass ball (xelf:node)
  ((height :initform (units 1))
   (width :initform (units 1))
   (color :initform "white")
   (speed :initform 6)
   (heading :initform (direction-heading :downright))))

(defun ball () (slot-value (current-buffer) 'ball))

(defmethod update ((self ball))
  (with-slots (heading speed) self
    (move self heading speed)))

;;; Walls for the ball to bounce off of

(defclass wall (node)
  ((color :initform "gray50")))
   
(defmethod collide ((ball ball) (wall wall))
  (with-slots (heading speed x y) ball
    ;; back away from wall
    (move ball (opposite-heading heading) speed)
    (aim ball (heading-between ball (paddle)))
    (percent-of-time 10 (incf heading (radian-angle 90)))))

(defresource "bip.wav" :volume 20)

(defmethod collide :after ((ball ball) (node node))
  (play-sample "bip.wav"))

;;; Bricks to bash 
    
(defparameter *brick-width* (units 2))
(defparameter *brick-height* (units 1.2))

(defclass brick (node)
  ((color :initform "gray60")
   (height :initform *brick-height*)
   (width :initform *brick-width*)))

(defmethod initialize-instance :after ((brick brick) &key color)
  (when color
    (setf (slot-value brick 'color) color)))

(defmethod collide ((self ball) (brick brick))
  (with-slots (heading) self
    (destroy brick)
    (incf heading (radian-angle 90))))

;;; The player's paddle control

(defclass paddle (node)
  ((direction :initform nil)
   (height :initform (units 1))
   (width :initform (units 8))
   (color :initform "white")))

(defun paddle () (slot-value (current-buffer) 'paddle))

(defparameter *paddle-speed* 3)

(defun holding-left-arrow ()
  (or (joystick-button-pressed-p :left)
      (keyboard-down-p :kp4)
      (keyboard-down-p :left)))

(defun holding-right-arrow ()
  (or (joystick-button-pressed-p :right)
      (keyboard-down-p :kp6)
      (keyboard-down-p :right)))

(defmethod update ((paddle paddle))
  (with-slots (direction) paddle
    (setf direction
	  (cond ((holding-left-arrow) :left)
		((holding-right-arrow) :right)))
    (when direction
      (move paddle (direction-heading direction) *paddle-speed*))))

(defmethod collide ((paddle paddle) (wall wall))
  (with-slots (heading) paddle
    (setf heading (opposite-heading heading))
    (move paddle heading (* *paddle-speed* 2))))

(defmethod english ((paddle paddle))
  (with-slots (direction) paddle
    (case direction
      (:left (direction-heading :upleft))
      (:right (direction-heading :upright))
      (otherwise (+ (slot-value (ball) 'heading)
		    (radian-angle 90))))))

(defmethod collide ((ball ball) (paddle paddle))
  (with-slots (heading speed) ball
    (setf heading (english paddle))
    (move ball heading speed)))

;;; Parts of the game board

(defun make-wall (x y width height)
  (let ((wall (make-instance 'wall)))
    (resize wall width height)
    (move-to wall x y)
    wall))

(defun make-border (x y width height)
  (let ((left x)
	(top y)
	(right (+ x width))
	(bottom (+ y height)))
    (with-new-buffer
      ;; top wall
      (insert (make-wall left top (- right left) (units 1)))
      ;; bottom wall
      (insert (make-wall left bottom (- right left (units -1)) (units 1)))
      ;; left wall
      (insert (make-wall left top (units 1) (- bottom top)))
      ;; right wall
      (insert (make-wall right top (units 1) (- bottom top (units -1))))
      ;; send it all back
      (current-buffer))))

(defparameter *row-colors* 
  '("dark orchid" "medium orchid" "orchid" "dark orange" "orange" "gold"))

(defun row-color (row)
  (nth (mod row (length *row-colors*))
       *row-colors*))

(defun make-puzzle ()
  (with-new-buffer
    (dotimes (row 6)
      (dotimes (column 17)
	(add-node (current-buffer)
		  (make-instance 'brick :color (row-color row))
		  (+ 50 (* column *brick-width*))
		  (+ 50 (* row *brick-height*)))))))

;;; The buffer class for the game

(defclass plong (xelf:buffer)
  ((paddle :initform (make-instance 'paddle))
   (ball :initform (make-instance 'ball))
   (background-color :initform "black")
   (width :initform *width*)
   (height :initform *height*)))

(defmethod reset-game ((plong plong))
  (with-slots (ball paddle) plong
    (with-buffer plong
      (bind-event plong '(:r :control) 'reset-game)
      (insert ball)
      (insert paddle)
      (move-to ball 80 280)
      (move-to paddle 110 400)
      (paste plong (make-border 0 0 (- *width* (units 1)) (- *height* (units 1))))
      (paste plong (make-puzzle)))))

(defun plong ()
  (setf *resizable* t)
  (setf *scale-output-to-window* t)
  (with-session
    (open-project :plong)
    (index-all-images)
    (index-pending-resources)
    (preload-resources)
    (let ((plong (make-instance 'plong)))
      (switch-to-buffer plong)
      (reset-game plong))))

;;; plong.lisp ends here
