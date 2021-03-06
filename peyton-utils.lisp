;;;; peyton-utils.lisp

(in-package #:peyton-utils)


;; Macros

(defmacro aif (condition then &optional else)
  "Anaphoric if, provides the 'it' variable storing the condition's evaluated
  value."
  `(let ((it ,condition))
     (if it
         ,then
         ,else)))

(defmacro asetf (value new-value)
  "Anaphoric setf, takes a single pair and binds the evaluated
   value to 'it'."
  `(let ((it ,value))
     (setf ,value ,new-value)))

(defmacro awhen (condition &body body)
  "Anaphoric when, provides the 'it' variable storing the condition's evaluated
  value."
  `(let ((it ,condition))
     (when it ,@body)))

;; Read and file functions

(defun unread-1 (stream)
  "Moves the file back one element. You probably shouldn't use this."
  (file-position stream (1- (file-position stream))))

(defun read-until (separator stream &key (read #'read-char) (test #'eq) acc)
  "Builds a list of characters until the separator character is reached.
  Does not include the separator in the output, but removes the separator from
  the stream."
  (let ((character (funcall read stream nil)))
    (if (or (not character) (funcall test separator character))
        (if acc (coerce (reverse acc) 'string))
        (read-until separator stream
                    :read read :test test :acc (cons character acc)))))

(defun read-until-not (separator stream &key (read #'read-char) (test #'eq))
  "Builds a list of characters until the characters are not equal to the
  separator."
  (let ((values (read-until separator stream :read read
                            :test (compose #'not test))))
    (unread-1 stream)
    values))

(defun read-between (from to stream &key (read #'read-char) (test #'eq))
  "Reads all the values between two separators."
  (read-until from stream :read read :test test)
  (read-until to stream :read read :test test))

;; List functions

(defun collect-n (n lst &optional acc)
  (if (< n 1)
      (reverse acc)
      (collect-n (1- n) (cdr lst) (cons (car lst) acc))))

(defun collect-to (separators lst &key (test #'eq) acc)
  (when lst
    (let* ((move-by (length separators))
           (elements (collect-n move-by lst)))
      (if (funcall test separators elements)
          (reverse acc)
          (collect-to separators (nthcdr move-by lst) :test test
                      :acc (append elements acc))))))

(defun collect (fn lst &optional acc)
  (if lst
      (if (funcall fn (car lst))
          (collect fn (cdr lst) (cons (car lst) acc))
          (collect fn (cdr lst) acc))
      (reverse acc)))

(defun collect-until (separator lst &key (test #'eq) acc)
  "Collects elements of a list until the separator is reached."
  (if lst
    (let ((element (car lst)))
      (if (funcall test separator element)
          (reverse acc)
          (collect-until separator (cdr lst) :test test :acc (cons element acc))))
    (reverse acc)))

(defun collect-around (separator lst &key (test #'eq))
  (append (collect-until separator lst :test test)
          (remove-until separator lst :test test)))

(defun remove-until (separator lst &key (test #'eq))
  "Removes elements of a list until the separator is reached, returning the new
  list."
  (when lst
    (let ((element (car lst)))
      (if (funcall test separator element)
          (cdr lst)
          (remove-until separator (cdr lst) :test test)))))

(defun collect-between (from to lst &key (test #' eq))
  "Collects all the elements lying between two separators."
  (collect-until to (remove-until from lst :test test) :test test))

(defun snoc (item lst)
  "Conses an item to the end of a list."
  (reverse (cons item (reverse lst))))


;; Conversions

(defun octets->integer (octets &optional (acc 0))
  "Given a list of octets, converts them to a decimal value."
  (cond ((null octets) acc)
        ((atom octets) octets)
        (t (octets->integer (cdr octets) (+ (car octets) (ash acc 8))))))

(defun octets->string (octets)
  "Given a list of octets, converts them to a string."
  (coerce (mapcar (compose #'code-char #'octets->integer) octets) 'string))
