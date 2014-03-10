;;; plong.lisp --- a simple ball-and-paddle example for Xelf
;;                     ... with a mondrian twist

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

(define ball
  :height (units 1)
  :width (units 1)
  :color "white"
  :speed 4
  :heading (direction-heading :downright))

(defun ball () (field-value :ball (current-buffer)))

(defmethod update ((self ball))
  (with-fields (heading speed) self
    (move self heading speed)))

(define wall
  :color "gray50")
   
(defmethod collide ((self ball) (wall wall))
  (with-fields (heading speed) self
    ;; back away from wall
    (move self (opposite-heading heading) speed)
    (incf heading (radian-angle 90))))

(defparameter *brick-width* (units 3))
(defparameter *brick-height* (units 2))

(define brick
  :color (random-choose '("red" "orange" "yellow"))
  :height *brick-height*
  :width *brick-width*)

(defmethod collide ((self ball) (brick brick))
  (with-fields (heading) self
    (destroy brick)
    (incf heading (radian-angle 90))))

(define paddle 
  :direction nil
  :height (units 1)
  :width (units 8)
  :color "white")

(defun paddle () (field-value :paddle (current-buffer)))

(defparameter *paddle-speed* 3)

(defun holding-left-arrow ()
  (or (joystick-button-pressed-p :left)
      (keyboard-down-p :kp4)
      (keyboard-down-p :left)))

(defun holding-right-arrow ()
  (or (joystick-button-pressed-p :right)
      (keyboard-down-p :kp6)
      (keyboard-down-p :right)))

(defmethod update ((self paddle))
  (with-fields (direction) self
    (setf direction
	  (cond ((holding-left-arrow) :left)
		((holding-right-arrow) :right)))
    (when direction
      (move self (direction-heading direction) *paddle-speed*))))

(defmethod collide ((self paddle) (wall wall))
  (with-fields (heading) self
    (setf heading (opposite-heading heading))
    (move self heading *paddle-speed*)))

(defmethod english ((self paddle))
  (with-fields (direction) self
    (case direction
      (:left (direction-heading :upleft))
      (:right (direction-heading :upright))
      (otherwise (+ (field-value :heading (ball))
		    (radian-angle 90))))))

(defmethod collide ((self ball) (paddle paddle))
  (with-fields (heading speed) self
    (setf heading (english paddle))
    (move self heading speed)))

(defun make-wall (x y width height)
  (let ((wall (new 'wall)))
    (xelf:resize wall width height)
    (xelf:move-to wall x y)
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
      ;; send it back
      (current-buffer))))

(defun make-puzzle ()
  (with-new-buffer
    (dotimes (row 5)
      (dotimes (column 8)
	(insert (new 'brick) 
		(* column *brick-width*)
		(* row *brick-height*))))
    (current-buffer)))

(define (plong buffer)
  :paddle (new 'paddle)
  :ball (new 'ball)
  :background-color "black"
  :width *width*
  :height *height*)

(defmethod reset ((self plong))
  (with-fields (ball paddle) self
    (with-buffer self
      (insert ball)
      (insert paddle)
      (move-to ball 80 280)
      (move-to paddle 110 400)
      (paste-from self (make-border 0 0 620 460))
      (paste-from self (make-puzzle) 110 110))))

(defun plong ()
  (with-session
      (open-project :plong 
		    :path #P"/home/dto/plong/"
		    :width *width* :height *height*)
    (index-all-images)
    (preload-resources)
    ;;    (index-pending-resources)
    (let ((plong (new 'plong)))
      (switch-to-buffer plong)
      (reset plong)
      (play)
      (start-session))))

;;; plong.lisp ends here
