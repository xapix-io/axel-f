(ns axel-f.date-test
  (:require [axel-f.excel :as af]
            [axel-f.excel.date :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest now
  (t/testing "current date time"
    (let [[t m] (sut/NOW)]
      (t/is (= "LocalDateTime" t))
      (t/is (integer? m)))

    (let [[t m] ((af/compile "NOW()"))]
      (t/is (= "LocalDateTime" t))
      (t/is (integer? m)))))

(t/deftest today
  (t/testing "current date"
    (t/testing "current date time"
      (let [[t m] (sut/TODAY)]
        (t/is (= "LocalDate" t))
        (t/is (integer? m)))

      (let [[t m] ((af/compile "TODAY()"))]
        (t/is (= "LocalDate" t))
        (t/is (integer? m))))))

(t/deftest date
  (t/testing "desired date"
    (let [[t m] (sut/DATE 1988 8 21)]
      (t/is (= t "LocalDate"))
      (t/is (integer? m)))

    (let [[t m] ((af/compile "DATE(1988, 8, 21)"))]
      (t/is (= t "LocalDate"))
      (t/is (integer? m)))

    (let [[t m] ((af/compile "DATE('1988', '8', '21')"))]
      (t/is (= t "LocalDate"))
      (t/is (integer? m)))))

(t/deftest day
  (t/testing "get day"
    (t/is (= 21
             (sut/DAY (sut/DATE 1988 8 21))
             ((af/compile "DAY(DATE(1988, 8, 21))"))))))

(t/deftest month
  (t/testing "get month"
    (t/is (= 8
             (sut/MONTH (sut/DATE 1988 8 21))
             ((af/compile "MONTH(DATE(1988, 8, 21))"))))))

(t/deftest year
  (t/testing "get year"
    (t/is (= 1988
             (sut/YEAR (sut/DATE 1988 8 21))
             ((af/compile "YEAR(DATE(1988, 8, 21))"))))))

(t/deftest comparable
  (t/testing "local date"
    (t/is ((af/compile "DATE(1988, 8, 21) > DATE(1988, 7, 21)")))
    (t/is ((af/compile "DATE(1988, 8, 21) = DATE(1988, 8, 21)")))
    (t/is ((af/compile "DATE(1988, 8, 21) < DATE(1988, 9, 21)")))
    (t/is ((af/compile "TODAY() > DATE(1988, 8, 21)")))
    (t/is ((af/compile "TODAY() > 0"))))

  (t/testing "local date time"
    (t/is ((af/compile "NOW() > 0")))
    (t/is ((af/compile "NOW() > DATE(1988, 8, 21)")))))

(t/deftest format-test
  (t/is (= "1988-08-21" ((af/compile "coerce.to-string(DATE(1988, 8, 21))"))))
  (t/is (= "Aug-88-21" ((af/compile "coerce.to-string(DATE(1988, 8, 21), 'MMM-yy-dd')")))))
