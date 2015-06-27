(defun start ()
  (ql:quickload  
   '(:lispbuilder-sdl-mixer :lispbuilder-sdl-ttf  
     :lispbuilder-sdl-image :uuid :cl-opengl :cl-fad)) 
  (push #P"~/src/xelf/" asdf:*central-registry*) 
  (asdf:load-system :xelf) 
  (push #P"~/src/plong/" asdf:*central-registry*)  
  (asdf:load-system :plong)
  (set (intern "*user-projects-directory*" (find-package :xelf)) #P"~/src/")
  (funcall (find-symbol "PLONG" (find-package :plong)))) 
