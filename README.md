<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Давидюк Микола Юрійович КВ-12</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом
(п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV
файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним
типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.

1. Визначити структури або утиліти для створення записів з таблиць (в залежності від
типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а
також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або
структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і
т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз,
який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було
передано у select . При цьому лямбда-вираз в якості ключових параметрів може
отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку
лише заданими значеннями (виконати фільтрування). Вибірка повертається у
вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від
варіанту):
структури у геш-таблиці
геш-таблиці у асоціативні списки
асоціативні списки у геш-таблиці

6. Написати функцію(-ї) для "красивого" виводу записів таблиці.
   
## Варіант 7
База даних: Наукові статті

Тип записів: Структура

Таблиці: Спеціальності, Наукові статті
  
## Лістинг реалізації завдання
```lisp
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
```
  
### Тестові набори та утиліти
```lisp
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
```

### Тестування
```lisp
CL-USER>  (run-examples)
Examples:
Test: Example 1 - PASSED
Test: Example 2 - PASSED
Test: Example 3 - PASSED
Full table:
Scientific Article: ID=1, Title=DeepLearning, Specialization ID=1, Year=2020, Author=JohnDoe
Scientific Article: ID=2, Title=MachineLearning, Specialization ID=2, Year=2021, Author=AliceSmith
Scientific Article: ID=3, Title=DataScience, Specialization ID=1, Year=2022, Author=BobJohnson
Scientific Article: ID=4, Title=QuantumComputing, Specialization ID=3, Year=2023, Author=CharlieBrown
```
