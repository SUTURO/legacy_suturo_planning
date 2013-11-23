(in-package :suturo-planning-process-module)

(defun parse-perceived-objects-to-string (objs)
  (let ((result "["))
    (loop for i from 0 below (length objs)
          for obj = (elt objs i)
          do
            (setq result (concatenate 'string
                                      result 
                                      (parse-perceived-object-to-string obj)
                                      ", ")))
    (concatenate 'string (subseq result 0 (- (length result) 2)) "]")))

(defun parse-perceived-object-to-string (obj)
; uint32 (float64 float64 float64) float32
  (roslisp:with-fields (c_id c_volume recognition_label_2d shape) obj 
    (concatenate 'string 
                 "["
                 (write-to-string c_id)
                 ", "
                 "\""
                 recognition_label_2d
                 "\""
                 ", "
                 (write-to-string c_volume)
                 ", "
                 (write-to-string shape)
                 "]")))

(defun parse-object-ids-from-string (str)
  (let ((result '()))
    (ppcre:do-matches (s e "[0-9]{1,}" str nil :start 0 :end (length str))
                      (push (read-from-string (subseq str s e)) result))
    result))
