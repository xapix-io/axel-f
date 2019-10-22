(ns axel-f.date-test
  (:require [axel-f.excel :as af]
            [axel-f.excel.date :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest now
  (t/testing "current date time"
    (t/is (instance? #?(:clj java.time.LocalDateTime
                        :cljs js/Date)
                     (sut/NOW)))

    (t/is (instance? #?(:clj java.time.LocalDateTime
                        :cljs js/Date)
                     ((af/compile "NOW()"))))))

(t/deftest today
  (t/testing "current date"
    (t/is (instance? #?(:clj java.time.LocalDate
                        :cljs js/Date)
                     (sut/TODAY)))

    (t/is (instance? #?(:clj java.time.LocalDate
                        :cljs js/Date)
                     ((af/compile "TODAY()"))))))

(t/deftest date
  (t/testing "desired date"
    (t/is (instance? #?(:clj java.time.LocalDate
                        :cljs js/Date)
                     (sut/DATE 1988 8 21)))

    (t/is (instance? #?(:clj java.time.LocalDate
                        :cljs js/Date)
                     ((af/compile "DATE(1988, 8, 21)"))))

    (t/is (instance? #?(:clj java.time.LocalDate
                        :cljs js/Date)
                     ((af/compile "DATE('1988', '8', '21')"))))))

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
