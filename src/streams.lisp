(in-package :fiasco)

(defclass column-counting-output-stream
    (trivial-gray-streams:fundamental-character-output-stream)
  ((column :initform 0 :accessor output-column)
   (understream :initarg :understream :initform (error "required!"))))

(defmethod trivial-gray-streams:stream-write-sequence
    ((s column-counting-output-stream)
     seq start end &key)
  "Write SEQ to stream S."
  (let ((newline-pos (position #\Newline seq :from-end t)))
    (when newline-pos
      (setf (output-column s) (- (length seq) newline-pos 1))))
  (write-sequence seq (slot-value s 'understream) :start start :end end))

(defmethod trivial-gray-streams:stream-start-line-p
    ((s column-counting-output-stream))
  "Tell if stream S is already at start of fresh new line."
  (eql (output-column s) 0))

(defmethod trivial-gray-streams:stream-write-char
    ((s column-counting-output-stream) char)
  "Write CHAR to stream S."
  (if (eq char #\Newline)
      (setf (output-column s) 0)
      (incf (output-column s)))
  (write-char char (slot-value s 'understream)))
