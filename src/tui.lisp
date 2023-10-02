(defpackage :cl-tui
  (:use :cl :cl-tui/terminal :cl-interpol))

(in-package :cl-tui)

#+production
(declaim (optimize (safety 1) (debug 0) (speed 3)))

#-production
(declaim (optimize (safety 3) (debug 3) (speed 1)))

(defgeneric init (model)
  (:documentation "Initialize the model and optionally emit commands"))

(defgeneric update (model message)
  (:documentation "Update the model with the given message, optionally emitting commands"))

(defgeneric to-string (obj)
  (:documentation "Return a string representatio of obj."))

(defmethod to-string ((s string))
  (declare (values string))
  s)

(defgeneric view (model)
  (:documentation
   "Render the model and return something that implements (to-string) representing the current view for that model."))

(defclass program ()
  ((model :accessor program-model
          :initarg :initial-model
          :documentation "The initial model for this program to act on.")))

(defun run-program (model)
  (let ((program (make-instance 'program :initial-model model)))
    (run-event-loop program)))


(defun run-event-loop (program)
  ;; read messages from queue
  ;; update the model
  ;; dispatch any commands
  ;; do it all over again
  (cl-tui/terminal:with-alternate-screen
      (cl-tui/terminal:with-raw-term
          (as:with-event-loop (:catch-app-errors t)
            ;; exit the event loop on sig term
            (as:signal-handler as:+sigint+ (lambda (sig)
                                             (declare (ignore sig))
                                             (as:exit-event-loop)))
            (as:with-interval (2)
              (format t "Next ...~%")))))
  ;; make sure terminal gets back into normal mode

  )

;; let's try this immediatly
(defclass counter ()
  ((value :accessor value :initarg :value)))

(defstruct increment-msg amount)
(defstruct decrement-msg amount)

;; multiple dispatch is helping us here quite a bit
(defmethod update ((model counter) (msg increment-msg))
  (incf (value model) (increment-msg-amount msg)))

(defmethod update ((model counter) (msg decrement-msg))
  (defc (value model) (decrement-msg-amount msg)))

(defvar main-program (make-instance 'program :initial-model (make-instance 'counter :value 5)))

(defun run-example ()
  (let ((program main-program))
    (run-event-loop program)))
