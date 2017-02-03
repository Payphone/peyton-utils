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

(defmacro awhen (condition &body body)
  `(let ((it ,condition))
     (when it ,@body)))

(defmacro cat (&rest strings)
  "Concatenates strings."
  `(concatenate 'string ,@strings))

(defun compose (fn &rest fns)
  "Returns a new function by applying functions to each other."
  (reduce (lambda (fn1 fn2)
            (lambda (&rest arguments)
              (funcall fn1 (apply fn2 arguments))))
          fns :initial-value fn))

(defun flatten (lst &optional acc)
  (when lst
    (if (listp lst)
        (flatten (car lst) (flatten (cdr lst) acc))
        (cons lst acc))))

(defun read-until (separator stream &key (read #'read-char) (test #'eq) acc)
  "Builds a list of characters until the separator character is reached.
  Does not include the separator in the output."
  (let ((character (funcall read stream nil)))
    (if (or (null character) (funcall test separator character))
        (reverse acc)
        (read-until separator stream
                    :read read :test test :acc (cons character acc)))))

(defun read-until-not (separator stream &key (read #'read-char) (test #'eq))
  "Builds a list of characters until the characters are not the same."
  (let ((values (read-until separator stream :read read
                            :test (compose #'not test))))
    (file-position stream (1- (file-position stream)))
    values))

(defun read-file (file)
  (coerce (with-open-file (in file) (read-until :EOF in)) 'string))

(defun octets->integer (octets &optional (acc 0))
  "Given a list of octets, converts them to a decimal value."
  (cond ((null octets) acc)
        ((atom octets) octets)
        (t (octets->integer (cdr octets) (+ (car octets) (ash acc 8))))))

(defun octets->string (octets)
  (coerce (mapcar (compose #'code-char #'octets->integer) octets) 'string))

(defmacro with-gensyms (syms &body body)
  `(let ,(loop for s in syms collect `(,s (gensym)))
     ,@body))

(defun snoc (item lst)
  "Conses an item to the end of a list."
  (reverse (cons item (reverse lst))))
