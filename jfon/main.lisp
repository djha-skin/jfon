(in-package #:cl-user)
(defpackage
  #:skin.djha.jfon (:use #:cl)
  (:documentation
    "
    JZON for FSets
    ")

    (:export
      parse
      stringify)
    (:import-from
      #:fset)
    (:package-local-nicknames (#:jzon #:com.inuoe.jzon)))
(in-package #:skin.djha.jfon)

; Write a parser like jzon's `parse` except it outputs FSet maps and seqs.
(defun parse-value (parser event value)
  (declare (type jzon:parser parser))
        (ecase event
          (:value value)
          (:begin-array (parse-array parser))
          (:begin-object (parse-object parser))))

(defun parse-array (parser)
  (declare (type jzon:parser parser))
  (loop with result = (fset:empty-seq)
        for event and value
        do (setf (values event value) = (jzon:parse-next parser)
        while (not (eq event :end-array))
        do
        (fset:adjoinf result (parse-value parser event value))
        finally
        (return result))))

(defun parse-object (parser)
  (declare (type jzon:parser parser))
  (loop with result = (fset:empty-seq)
        for event and value
        do (setf (values event value) (jzon:parse-next parser))
        while (not (eq event :end-array))
        do
        ; The event should *always* be `:object-key` at the start of the loop.
        (unless (eq event :object-key)
          (error "Unexpected event: ~A" event))
        (let ((domain value))
          (multiple-value-bind (ev val) (jzon:parse-next parser)
            (fset:adjoinf result domain (parse-value parser ev val))))
        finally
        (return result)))

(defun parse (strm)
  "
    Parse an input stream into FSet collections.
    "
    (jzon:with-parser (parser strm)
      (multiple-value-bind
        (ev val)
        (jzon:parse-next parser)
      (parse-value parser ev val))))

; Write a stringifier like jzon's `stringify` except it takes FSet maps and
; seqs. Basically taken right out of jzon's documentation.
(defun stringify (&key (strm t) (pretty nil))
  (labels ((helper (thing)
             (etypecase thing
               (jzon:json-atom
                 (jzon:write-value* thing))
               (fset:seq
                 (jzon:with-array*
                   (map nil #'recurse thing)))
               (fset:map
                 (jzon:with-object*
                   (maphash (lambda (k v)
                              (jzon:write-key* k)
                              (helper v))
                            thing))))))
      (jzon:with-writer* (:stream strm) (:pretty pretty)
        (helper thing))))
