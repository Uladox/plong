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

(defparameter *width* 640)
(defparameter *height* 480)

(defparameter *unit* 16)

(defun units (n) (* *unit* n))

(xelf:define-project plong
  :width *width*
  :height *height*)

(in-package :plong)

(define ball
  :image "ball.png"
  :speed 4
  :heading (direction-heading :up))

(defmethod run ((self ball))
  (with-fields (heading speed) self
    (move self heading speed)))

(define wall
  :color "gray50")
   
(defmethod collide ((self ball) (wall wall))
  (with-fields (heading speed) self
    ;; back away from wall
    (move self (opposite-heading heading) speed)
    (incf heading (radian-angle 90))
    (play-sample "bounce.wav")))

(define brick
  :color "red"
  :height 40
  :width 100)

(defmethod collide ((self ball) (brick brick))
  (with-fields (heading) self
    (destroy brick)
    (incf heading (radian-angle 90))))

(defresource "bip.wav" :volume 30)

(defmethod destroy :after ((self brick))
  (play-sample "bip.wav"))
   
(define paddle 
  :direction nil
  :image "paddle.png")

(defmethod english ((self paddle))
  (with-fields (direction heading) self
    (case direction
      (:left (direction-heading :upleft))
      (:right (direction-heading :upright))
      (otherwise (+ heading (radian-angle 90))))))
    
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

(define (plong buffer)
  :paddle (new 'paddle)
  :ball (new 'ball)
  :background-color "black"
  :width *width*
  :height *height*)

(defun paddle () (field-value :paddle (current-buffer)))
(defun ball () (field-value :ball (current-buffer)))



;;; plong.lisp ends here
