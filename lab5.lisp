(defstruct specialization
  (id 0 :type integer)
  (name "" :type string)
  (field "" :type string))

(defstruct scientific-article
  (id 0 :type integer)
  (title "" :type string)
  (specialization-id 0 :type integer)
  (year 0 :type integer)
  (author "" :type string))

(defun convert-values (values record-type)
  "Aux function for (read-csv-file). Converts each row's values into their expected types."
  (case record-type
      (:specialization (list (parse-integer (first values)) (second values) (third values)))
      (:scientific-article (list (parse-integer (first values))
                              (second values)
                              (parse-integer (third values))
                              (parse-integer (fourth values))
                              (fifth values)))))

(defun read-csv-file (file-path record-type)
  "Reads a csv file. Returns a list of structured data for all rows in the file."
  (with-open-file (stream file-path :direction :input)
      (let* ((result '()))
        (do ((line (read-line stream) (read-line stream nil 'eof)))
              ((eq line 'eof) nil (reverse result))
              (push (convert-values (uiop:split-string (string-right-trim '(#\Return) line) :separator '(#\,)) record-type) result)))))

(defun attribute-to-index (attribute record-type)
  "Aux function for (filter-records). A table to get indexes for specified attributes."
  (case record-type
    (:specialization (case attribute
                   (:ID 0)
                   (:NAME 1)
                   (:FIELD 2)))
    (:scientific-article (case attribute
                        (:ID 0)
                        (:TITLE 1)
                        (:SPECIALIZATION-ID 2)
                        (:YEAR 3)
                        (:AUTHOR 4)))))

(defun group-parameters (parameters)
  "Aux function for (filter-records). Groups &rest arguments into pairs."
  (loop for (attribute value) on parameters by #'cddr
        collect (list attribute value)))

(defun select (file-path record-type)
  "Return a lambda function that filters data from the csv file when called. Can be provided structure attributes and values to filter the data."
  (let ((rows (read-csv-file file-path record-type)))
    (lambda (&rest filter-params)
      (let ((filtered-rows
              (if (null filter-params)
                  rows
                  (remove-if-not 
                   (lambda (row)
                       (every 
                        (lambda (param-pair)
                            (let* ((attribute-index (attribute-to-index (first param-pair) record-type))
                                   (value (second param-pair)))
                              (equal (nth attribute-index row) value)))
                        (group-parameters filter-params)))
                   rows))))
        (mapcar 
         (lambda (row)
             (case record-type
               (:specialization
                (make-specialization 
                 :id (first row)
                 :name (second row)
                 :field (third row)))
               (:scientific-article
                (make-scientific-article
                 :id (first row)
                 :title (second row)
                 :specialization-id (third row)
                 :year (fourth row)
                 :author (fifth row)))))
         filtered-rows)))))

(defun save-records-to-csv (records file-path)
  "Writes records to a file."
  (with-open-file (stream file-path 
                          :direction :output 
                          :if-exists :supersede)
    (dolist (record records)
      (format stream "~{~a~^,~}~%" 
              (typecase record
                (specialization 
                 (list (specialization-id record)
                       (specialization-name record)
                       (specialization-field record)))
                (scientific-article
                 (list (scientific-article-id record)
                       (scientific-article-title record)
                       (scientific-article-specialization-id record)
                       (scientific-article-year record)
                       (scientific-article-author record))))))))

(defun records-to-map (records)
  "Converts records into a hash table."
  (mapcar 
   (lambda (record)
       (let ((hash (make-hash-table :test 'equal)))
         (typecase record
           (specialization 
            (setf (gethash :id hash) (specialization-id record)
                  (gethash :name hash) (specialization-name record)
                  (gethash :field hash) (specialization-field record)))
           (scientific-article
            (setf (gethash :id hash) (scientific-article-id record)
                  (gethash :title hash) (scientific-article-title record)
                  (gethash :specialization-id hash) (scientific-article-specialization-id record)
                  (gethash :year hash) (scientific-article-year record)
                  (gethash :author hash) (scientific-article-author record))))
         hash))
   records))
(defun display-records (records)
  "Pretty print for records."
  (dolist (record records)
    (format t "~&~A~%" 
            (typecase record
              (specialization 
               (format nil "Specialization: ID=~D, Name=~A, Field=~A" 
                       (specialization-id record)
                       (specialization-name record)
                       (specialization-field record)))
              (scientific-article
               (format nil "Scientific Article: ID=~D, Title=~A, Specialization ID=~D, Year=~D, Author=~A"
                       (scientific-article-id record)
                       (scientific-article-title record)
                       (scientific-article-specialization-id record)
                       (scientific-article-year record)
                       (scientific-article-author record)))))))
(defun check-read-test (test-name file-path record-type expected)
  "Enhanced test function for read-csv-file with better output."
  (let ((result (equal (read-csv-file file-path record-type) expected)))
    (format t "Test: ~A - ~:[FAILED~;PASSED~]~%" test-name result)))

(defun check-filter-test (test-name filter-output expected)
  "Enhanced test function for select with better output."
  (let ((result (every #'equalp filter-output expected)))
    (format t "Test: ~A - ~:[FAILED~;PASSED~]~%" test-name result)))

 (defun run-examples ()
  (format t "Examples:~%")
  (check-read-test "Example 1" "./FP_lab5/tests/test_read.csv" :scientific-article '((1 "TitleTest1" 1 2022 "AuthorTest1")))
  (check-filter-test "Example 2" (funcall (select "./FP_lab5/tests/test_select_article.csv" :scientific-article) :id 1) 
                     (list (make-scientific-article :id 1 :title "DeepLearning" :specialization-id 1 :year 2020 :author "JohnDoe")))
  (check-filter-test "Example 3" (funcall (select "./FP_lab5/tests/test_select_article.csv" :scientific-article) :year 2020 :author "JohnDoe") 
                     (list (make-scientific-article :id 1 :title "DeepLearning" :specialization-id 1 :year 2020 :author "JohnDoe")))
  (format t "Full table:~%")
  (display-records (funcall (select "./FP_lab5/tests/test_select_article.csv" :scientific-article))))
