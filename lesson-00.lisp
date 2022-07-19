#|
In emacs, to load this file, either:

select buffer: C-x h
eval selection: C-c C-r

or modify pathname as needed and eval this expression:

(load "~/Development/kaveh-common-lisp-lessons/lesson-00.lisp")
|#

(in-package "CL-USER")
(require "COCOA")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (objc:load-framework "OpenGL" :gl))

;;;; graphics ===========================================================

;;; view class that draws a square
(defclass my-opengl-view (ns:ns-opengl-view)
  ()
  (:metaclass ns:+ns-object))

;;; draw a square outline in OpenGL
(defun draw-square ()
  (#_glColor3f 1.0 1.0 1.0)
  (#_glLineWidth 3.0)
  (#_glBegin #$GL_LINE_LOOP)
  (#_glVertex3f  0.5  0.5 0.0)
  (#_glVertex3f  0.5 -0.5 0.0)
  (#_glVertex3f -0.5 -0.5 0.0)
  (#_glVertex3f -0.5  0.5 0.0)
  (#_glEnd))

;;; display the view
(objc:defmethod (#/drawRect: :void) ((self my-opengl-view) (rect :<NSR>ect))
  (#_glClearColor 0.0 0.0 0.0 0.0)
  (#_glClear #$GL_COLOR_BUFFER_BIT)
  (draw-square)
  (#_glFlush))

;;; respond to first click in window
(objc:defmethod (#/acceptsFirstMouse: :<BOOL>) ((self my-opengl-view) event)
  (declare (ignore event))
  t)

;;; request a view refresh when mouse click
(objc:defmethod (#/mouseDown: :void) ((self my-opengl-view) event)
  (declare (ignore event))
  (#/setNeedsDisplay: self t))

;;; create and display a window containing an OpeGL view
(defun show-window ()
  (ns:with-ns-rect (frame 0 0 512 512)
    (let* ((w (make-instance 'ns:ns-window
			     :with-content-rect frame
			     :style-mask (logior #$NSTitledWindowMask
						 #$NSClosableWindowMask
						 #$NSMiniaturizableWindowMask)
			     :backing #$NSBackingStoreBuffered
			     :defer t))
	   (v (make-instance 'my-opengl-view)))
      (#/setContentView: w v)
      (#/setNeedsDisplay: v t)
      (#/release v)
      (#/center w)
      (#/orderFront: w nil)
      w)))

;;; execute code on main thread -- necessary for interacting with UI elements
(defmacro on-main-thread (&rest actions)
  `(ccl::call-in-event-process
     #'(lambda ()
         ,@actions)))

;;; run function
(defun run ()
  (on-main-thread (show-window)))

(run)

