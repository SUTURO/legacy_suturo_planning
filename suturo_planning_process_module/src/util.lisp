(in-package :suturo-planning-process-module)

(defun parse-perceived-objects-to-string (objs)
  "Parses a list of objects of type PerceivedObject to string
  for the use with a json_prolog service"
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
  "Parses the given object of type PerceivedObject to string
  for the use with a json_prolog service"
  (roslisp:with-fields (c_id c_volume recognition_label_2d c_shape) obj 
    (concatenate 'string 
                 "["
                 (write-to-string c_id)
                 ", "
                 "'"
                 recognition_label_2d
                 "'"
                 ", '"
                 (write-to-string c_volume)
                 "', "
                 (write-to-string c_shape)
                 "]")))

(defun parse-object-ids-from-string (str)
  "Parses a comma seperated list of numbers to a lisp-list of numbers"
  (let ((result '()))
    (ppcre:do-matches (s e "[0-9]{1,}" str nil :start 0 :end (length str))
                      (push (read-from-string (subseq str s e)) result))
    result))

(defun filter-objects-by-ids (objs ids)
  "Filters a list of objects of type PerceivedObject by a list of given ids"
  (let ((results ()))
    (format t "~a~%" ids)
    (loop for obj across objs
          do
             (roslisp:with-fields 
                 (c_id) 
               obj
               (format t "~a~%" c_id)
               (if (find c_id ids)
                   (push obj results))))
    (format t "~a~%" results)
    results))
