(declaim (optimize (speed 0) (space 0) (debug 3)))
(in-package #:cl-user)
(defpackage #:skin.djha.jfon/tests
  (:use #:cl
        #:rove)
  (:local-nicknames
    (#:jfon #:skin.djha.jfon)
    (#:jzon #:com.inuoe.jzon)))
(in-package #:skin.djha.jfon/tests)
(fset:fset-setup-readtable *readtable*)

(defun nested-to-alist
  (value)
  "
  Recursively changes value, converting all hash tables within the tree to an
  alist. Makes testing easier.
  "
  (cond
    ((stringp value) value)
    ((or (vectorp value) (listp value))
     (map 'list #'nested-to-alist value))
    ((hash-table-p value)
     (let ((coll
       (loop for k being the hash-key of value
           using (hash-value v)
           collect (cons k (nested-to-alist v)))))
       (stable-sort coll #'string< :key (lambda (thing)
                                          (format nil "~A" (car thing))))))
    (t
      value)))

(deftest
  parse-tests
  (testing "empty"
           (ok (signals (with-input-from-string (strm "")
                      (jfon:parse strm)))))
  (testing "simple case"
           (ok (fset:equal?
                 #[ "a" ]
                 (with-input-from-string (strm "[\"a\"]")
                   (jfon:parse strm))
                 )))

  (testing "more general case"
           (ok (fset:equal?
                   (with-input-from-string
                     (strm
                       "
                       {
                        \"the-wind\": \"bullseye\",
                        \"the-trees\": false,
                        \"the-sparrows\": \"his-eye\",
                        \"poem\": \"His eye is on the sparrow\",
                        \"this-should-still-work\": 15.0,
                        \"other\": \"And I know He's watching over me\",
                        \"force_push\": \"I sing because I'm happy\",
                        \"i am mordac\": true,
                        \"you are so wrong\": null,
                        \"wendover\": [
                            {
                                \"so\": 1,
                                \"much\": -10,
                                \"gambling\": 100,
                                \"but\": 1000,
                                \"also\": -1000,
                                \"apparently\": 10000,
                                \"paramedics\": -10000,
                                \"and\": 1.01
                            },
                            {
                                \"die\": \"in\",
                                \"a\": \"fire\"
                            },
                            15,
                            \"this that\"
                         ]
                       }
                       ")
                     (jfon:parse strm))

                     #{|
                     ("poem" "His eye is on the sparrow")
                     ("other" "And I know He's watching over me")
                     ("the-wind" "bullseye")
                     ("wendover"
                      #[
                        #{|
                        ("so" 1)
                        ("and" 1.01d0)
                        ("but" 1000)
                        ("also" -1000)
                        ("much" -10)
                        ("gambling" 100)
                        ("apparently" 10000)
                        ("paramedics" -10000) |}
                        #{| ("a" "fire") ("die" "in") |}
                        15
                        "this that" ])
                     ("the-trees" NIL)
                     ("force_push" "I sing because I'm happy")
                     ("i am mordac" T)
                     ("the-sparrows" "his-eye")
                     ("you are so wrong" 'CL:NULL)
                     ("this-should-still-work" 15.0d0) |}))))


(deftest
  stringify-tests
  (testing "empty"
           (ok (equal
                 "false"
                 (with-output-to-string (strm)
                   (jfon:stringify nil :strm strm)))))
  (testing "general case"
           (ok (equal
                "{
  \"poem\": \"His eye is on the sparrow\",
  \"other\": \"And I know He's watching over me\",
  \"the-wind\": \"bullseye\",
  \"wendover\": [
    {
      \"so\": 1,
      \"and\": 1.01,
      \"but\": 1000,
      \"also\": -1000,
      \"much\": -10,
      \"gambling\": 100,
      \"apparently\": 10000,
      \"paramedics\": -10000
    },
    {
      \"a\": \"fire\",
      \"die\": \"in\"
    },
    15,
    \"this that\"
  ],
  \"the-trees\": false,
  \"force_push\": \"I sing because I'm happy\",
  \"i am mordac\": true,
  \"the-sparrows\": \"his-eye\",
  \"you are so wrong\": null,
  \"this-should-still-work\": 15.0
}"
                 (with-output-to-string (strm)
                   (jfon:stringify 
                     #{|
                     ("poem" "His eye is on the sparrow")
                     ("other" "And I know He's watching over me")
                     ("the-wind" "bullseye")
                     ("wendover"
                      #[
                        #{|
                        ("so" 1)
                        ("and" 1.01d0)
                        ("but" 1000)
                        ("also" -1000)
                        ("much" -10)
                        ("gambling" 100)
                        ("apparently" 10000)
                        ("paramedics" -10000) |}
                        #{| ("a" "fire") ("die" "in") |}
                        15
                        "this that" ])
                     ("the-trees" NIL)
                     ("force_push" "I sing because I'm happy")
                     ("i am mordac" T)
                     ("the-sparrows" "his-eye")
                     ("you are so wrong" 'CL:NULL)
                     ("this-should-still-work" 15.0d0) |}
                     :strm strm :pretty t))))))


