;;; plong.lisp --- a simple remixable ball-and-paddle example for Xelf

;; Copyright (C) 2014  David O'Toole

;; Author: David O'Toole <dto@blocky.io>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code: 

;; First we must define a new package for our game, and import the
;; Xelf symbols. In this case the only exported function is PLONG,
;; which starts the game when called. (We'll show the definition of
;; PLONG later.)

(defpackage :plong
  (:use :cl :xelf)
  (:export plong))

(in-package :plong)

;; Here we define an arbitrary measurement unit used throughout, and
;; set up some variables to hold the height and width of the game
;; world.

(defparameter *unit* 16)
(defun units (n) (* *unit* n))
(defparameter *width* 640)
(defparameter *height* 480)

;; Now it's time to define some game objects. Xelf game objects are
;; called "nodes", and they can interact by being grouped into
;; "buffers" of different kinds. Naturally there are base classes
;; called NODE and BUFFER. These classes define the basic behaviors of
;; the game engine.  Nodes are endowed with such properties as an (X Y
;; Z) position, width, height, and image to be displayed, and so on. The
;; default node behaviors also hook all game objects into buffer
;; features, such as collision detection, pathfinding, and
;; serialization.

;; To define nodes of your own, use DEFCLASS and give NODE as a
;; superclass. You can override the default values of NODE slots, as
;; well as add your own.

(defclass ball (node)
  ((height :initform (units 1))
   (width :initform (units 1))
   (color :initform "white")
   (speed :initform 6)
   (heading :initform (direction-heading :downright))))

;; The generic function UPDATE is called on each object once during
;; each game loop.

(defmethod update ((ball ball))
  (with-slots (heading speed) ball
    (move ball heading speed)))

;; Now we need walls around the game world in order to contain the
;; ball.

(defclass wall (node)
  ((color :initform "gray50")))

;; We want the ball to bounce off of the walls. The COLLIDE method is
;; called for every frame on all pairs of objects whose bounding boxes
;; collide during that frame.
   
(defmethod collide ((ball ball) (wall wall))
  (with-slots (heading speed x y) ball
    ;; back away from wall
    (move ball (opposite-heading heading) speed)
    ;; point toward player. (The function PADDLE is defined later.)
    (aim ball (heading-between ball (paddle)))
    ;; sometimes choose another direction to prevent getting stuck
    (percent-of-time 10 (incf heading (radian-angle 90)))))

;; The ball should emit a retro beep when colliding with any node. We
;; use DEFRESOURCE to let Xelf know about the sound file. (The Xelf
;; reference gives more information about DEFRESOURCE.)

(defresource "bip.wav" :volume 20)

(defmethod collide :after ((ball ball) (node node))
  (play-sample "bip.wav"))

;; Now it's time to bash some bricks! First we define the dimensions
;; of a brick and create a class.
    
(defparameter *brick-width* (units 2))
(defparameter *brick-height* (units 1.2))

(defclass brick (node)
  ((color :initform "gray60")
   (height :initform *brick-height*)
   (width :initform *brick-width*)))

;; Here's how we can add color to bricks when they're being created.

(defmethod initialize-instance :after ((brick brick) &key color)
  (when color
    (setf (slot-value brick 'color) color)))

;; Finally, the ball should bounce off the bricks and break them.

(defmethod collide ((ball ball) (brick brick))
  (with-slots (heading) ball
    (destroy brick)
    (incf heading (radian-angle 90))))

;; Now we define some useful shorthand functions to refer to the ball and
;; paddle.

(defun ball () (slot-value (current-buffer) 'ball))
(defun paddle () (slot-value (current-buffer) 'paddle))

;; (We'll set up the CURRENT-BUFFER later so that its SLOT-VALUEs 
;; are indeed set to right objects.)

;; The player controls a rectangular paddle which can move left or
;; right within the buffer.

(defclass paddle (node)
  ((direction :initform nil)
   (height :initform (units 1))
   (width :initform (units 8))
   (color :initform "white")))

(defparameter *paddle-speed* 3)

;; Now we define some handy functions to check whether the player is
;; pressing left or right on the keyboard. Numeric keypad is also
;; supported---it's a good idea to check both when using arrows to
;; control your game.

(defun holding-left-arrow ()
  (or (keyboard-down-p :kp4)
      (keyboard-down-p :left)))

(defun holding-right-arrow ()
  (or (keyboard-down-p :kp6)
      (keyboard-down-p :right)))

(defun find-joystick-direction ()
  (let ((heading (when (left-analog-stick-pressed-p)
		   (left-analog-stick-heading))))
    (when heading 
      (if (and (> heading (/ pi 2))
	       (< heading (* 3 (/ pi 2))))
	  :left 
	  :right))))

(defun find-direction ()
  (or (when (plusp (number-of-joysticks))
	(find-joystick-direction))
      (cond ((holding-left-arrow) :left)
	    ((holding-right-arrow) :right))))

;; In the paddle's UPDATE method, we read the inputs and move the
;; paddle accordingly.

(defmethod update ((paddle paddle))
  (with-slots (direction) paddle
    (setf direction (find-direction))
    (when direction
      (move paddle (direction-heading direction) *paddle-speed*))))

;; The paddle should bounce back from the walls, too.

(defmethod collide ((paddle paddle) (wall wall))
  (with-slots (heading) paddle
    (setf heading (opposite-heading heading))
    (move paddle heading (* *paddle-speed* 2))))

;; The "english" is the directional force applied to the ball because
;; of the player's moving the paddle to the left or right at the
;; moment of collision.

(defmethod english ((paddle paddle))
  (with-slots (direction) paddle
    (case direction
      (:left (direction-heading :upleft))
      (:right (direction-heading :upright))
      (otherwise (+ (slot-value (ball) 'heading)
		    (radian-angle 90))))))

;; In the BALL,PADDLE collision method, the english is applied and the
;; ball is bounced away.

(defmethod collide ((ball ball) (paddle paddle))
  (with-slots (heading speed) ball
    (setf heading (english paddle))
    (move ball heading speed)))

;; Now that we have all the pieces of our game world, it's time to put
;; them all together in a buffer. First we have a function to make a
;; wall of a specified height, width, and position.

(defun make-wall (x y width height)
  (let ((wall (make-instance 'wall)))
    (resize wall width height)
    (move-to wall x y)
    wall))

;; This function MAKE-BORDER returns a buffer with four walls.

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

;; Now it's time for pretty rows of colored bricks.

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

;; You can see that MAKE-PUZZLE also returns a new buffer. We'll put
;; together these component buffers into the final game board below
;; with a function called PASTE.

;; But first, we need a Buffer subclass for the game board.

(defclass plong (buffer)
  ((paddle :initform (make-instance 'paddle))
   (ball :initform (make-instance 'ball))
   (background-color :initform "black")
   (width :initform *width*)
   (height :initform *height*)))

;; After initializing a new Plong buffer, we set things up so that
;; pressing Control-R causes the game to reset.

(defmethod initialize :after ((plong plong) &key)
  (bind-event plong '(:r :control) 'start-game))

;; The START-GAME function builds the game board by inserting the
;; ball and paddle objects, then pasting in the bricks and border.

(defmethod start-game ((plong plong))
  (with-slots (ball paddle) plong
    (with-buffer plong
      (insert ball)
      (insert paddle)
      (move-to ball 80 280)
      (move-to paddle 110 400)
      (paste plong (make-border 0 0 (- *width* (units 1)) (- *height* (units 1))))
      (paste plong (make-puzzle)))))

;; Now we define the main entry point for the game, the function
;; PLONG. We set up our variables and then invoke WITH-SESSION to
;; start Xelf going.

(defun plong ()
  ;; Configure the screen dimensions
  (setf *screen-height* *height*)
  (setf *screen-width* *width*)
  ;; Allow resizing of window and scaling
  (setf *resizable* t)
  (setf *scale-output-to-window* t)
  (with-session
    (open-project :plong)
    ;; this indexes everything defined with DEFRESOURCE
    (index-pending-resources) 
    (let ((plong (make-instance 'plong)))
      ;; start the buffer running
      (switch-to-buffer plong)
      (start-game plong))))

;;; plong.lisp ends here
