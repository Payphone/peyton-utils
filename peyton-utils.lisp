;;;; peyton-utils.lisp

(in-package #:peyton-utils)

(defmacro aif (condition then &optional else)
  "Euphoric if, provides the 'it' variable storing the condition's value."
  `(let ((it ,condition))
     (if it
         ,then
         ,else)))

(defmacro asetf (value new-value)
  "Similar to setf, except it only takes a single pair and binds the symbol
   value to 'it'."
  `(let ((it ,value))
     (setf ,value ,new-value)))

(defmacro cat (&rest strings)
  "Concatenates strings."
  `(concatenate 'string ,@strings))

(defun compose (&rest fns)
  "Returns a new function by applying functions to each other."
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
        (reduce #'(lambda (v f) (funcall f v))
                rest
                :initial-value (apply fn1 args)))))

(defun read-until (separator stream &key (read #'read-char) (test #'char=))
  "Builds a list of characters until the separator character is reached."
  (let ((character (funcall read stream nil)))
    (if (or (not character) (funcall test separator character))
        (reverse acc)
        (read-until separator stream (cons character acc)))))

(defun read-until-not (separator stream &key (read #'read-char) (test #'char=))
  "Builds a list of characters until the characters are not the same."
  (read-until separater stream :read read :test (compose #'not test)))

(defun octets->integer (octets)
  "Given a list of octets, converts them to a decimal value."
  (loop for octet in octets
     summing (+ octet (ash total 8)) into total
     finally (return total)))
