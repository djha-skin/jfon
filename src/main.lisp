(in-package #:cl-user)
(defpackage
  #:skin.djha.jfon
  (:use #:cl)
  (:documentation
    "
    JZON for FSets
    ")

    (:export
      parse
      stringify)
    (:import-from #:fset)
    (:local-nicknames (#:jzon #:com.inuoe.jzon)))
(in-package #:skin.djha.jfon)

; Write a parser like jzon's `parse` except it outputs FSet maps and seqs.
(defun parse-value (parser event value &key capture-order)
        (ecase event
          (:value value)
          (:begin-array (parse-array parser))
          (:begin-object (parse-object parser
                                       :capture-order capture-order))))

(defun parse-array (parser)
  (loop with result = (fset:empty-seq)
        and event
        and value
        do (setf (values event value) (jzon:parse-next parser))
        while (not (eq event :end-array))
        do
        (fset:push-last result (parse-value parser event value))
        finally
        (return result)))

(defun parse-object (parser &key capture-order)
  ;(declare (type jzon:parser parser))
  (loop with result = (if capture-order
                          (fset:empty-map)
                          (fset:empty-replay-map))
        and event
        and value
        do (setf (values event value) (jzon:parse-next parser))
        while (not (eq event :end-object))
        do
        ; The event should *always* be `:object-key` at the start of the loop.
        (unless (eq event :object-key)
          (error "Unexpected event: ~A" event))
        (let ((domain value))
          (multiple-value-bind (ev val) (jzon:parse-next parser)
            (fset:adjoinf result domain (parse-value parser ev val))))
        finally
        (return result)))

(defun parse (strm &key capture-order)
  "
  Parse an input stream into FSet collections.
  "
  (jzon:with-parser (parser strm)
                    (let ((value (multiple-value-bind
                      (ev val)
                      (jzon:parse-next parser)
                      (parse-value parser ev val
                                   :capture-order capture-order))))
                    (let ((ev (jzon:parse-next parser)))
                      (unless (null ev)
                        (error "Unexpected event: ~A" ev)))
                    value)))


; Write a stringifier like jzon's `stringify` except it takes FSet maps and
; seqs. Basically taken right out of jzon's documentation.
(defun stringify (thing &key (strm t) (pretty nil))
  (labels ((helper (thing)
             (etypecase thing
               (jzon:json-atom
                 (jzon:write-value* thing))
               (fset:seq
                 (jzon:with-array*
                   (fset:do-seq (v thing)
                           (helper v))))
               (fset:map
                 (jzon:with-object*
                   (fset:do-map (k v thing)
                           (jzon:write-key* k)
                              (helper v)))))))
      (jzon:with-writer* (:stream strm :pretty pretty)
        (helper thing))))
