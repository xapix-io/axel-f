(ns axel-f.excel.date
  (:refer-clojure :exclude [format])
  (:require #?@(:clj [[java-time :as jt]
                      [java-time.format :as jtf]]
                :cljs [[goog.i18n.DateTimeFormat]
                       [goog.i18n.DateTimeParse]])
            [axel-f.excel.coerce :as coerce])
  #?(:clj (:import java.time.ZoneOffset
                   java.time.ZoneId)))

(defn epoch-milli [date]
  #?(:clj (.toEpochMilli (.toInstant (.atZone date (ZoneId/ofOffset "UTC" (ZoneOffset/ofHours 0)))))
     :cljs (.getTime date)))

(defn local-date-time []
  #?(:clj (jt/local-date-time)
     :cljs (let [n (js/Date.)]
             (js/Date. (js/Date.UTC (.getUTCFullYear n)
                                    (.getUTCMonth n)
                                    (.getUTCDate n)
                                    (.getUTCHours n)
                                    (.getUTCMinutes n)
                                    (.getUTCSeconds n)
                                    (.getUTCMilliseconds n))))))

(defn local-date []
  #?(:clj (jt/local-date)
     :cljs (let [n (js/Date.)]
             (js/Date. (js/Date.UTC (.getUTCFullYear n)
                                    (.getUTCMonth n)
                                    (.getUTCDate n))))))

(defn format [date fmt]
  #?(:clj (jt/format fmt (coerce/inst date))
     :cljs (let [formatter (goog.i18n.DateTimeFormat. fmt)]
             (.format formatter (coerce/inst date)))))

(defn parse [type date-string pattern]
  [(case type
     :local-date "LocalDate"
     :local-date-time "LocalDateTime")
   (epoch-milli #?(:clj
                   (let [pattern (jtf/formatter pattern)]
                     ((case type
                        :local-date jt/local-date
                        :local-date-time jt/local-date-time)
                      pattern date-string))
                   :cljs
                   (let [date (case type
                                :local-date-time (local-date-time)
                                :local-date (local-date))]
                     (.strictParse (goog.i18n.DateTimeParse. pattern)
                                   date-string
                                   date)
                     date)))])

(defmethod coerce/inst "LocalDate" [[_ millis]]
  #?(:clj (.. (java.time.Instant/ofEpochMilli millis) (atZone (java.time.ZoneId/systemDefault)) toLocalDate)
     :cljs (js/Date. millis)))

(defmethod coerce/inst "LocalDateTime" [[_ millis]]
  #?(:clj (.. (java.time.Instant/ofEpochMilli millis) (atZone (java.time.ZoneId/systemDefault)) toLocalDateTime)
     :cljs (js/Date. millis)))

(defmethod coerce/excel-number "LocalDate" [[_ millis]]
  (Math/round (double (/ millis 1000))))

(defmethod coerce/excel-number "LocalDateTime" [[_ millis]]
  (Math/round (double (/ millis 1000))))

(defmethod coerce/excel-string "LocalDate"
  ([d] (format d "YYYY-MM-dd"))
  ([d fmt] (format d fmt)))

(defmethod coerce/excel-string "LocalDateTime"
  ([d] (format d "YYYY-MM-dd'T'HH:mm:ss.SSS"))
  ([d fmt] (format d fmt)))

(defn NOW*
  "Returns the current date and time as a date value."
  []
  ["LocalDateTime" (epoch-milli (local-date-time))])

(def NOW #'NOW*)

(defn TODAY*
  "Returns the current date as a date value."
  []
  ["LocalDate"
   #?(:clj (.toEpochMilli (.toInstant (.atStartOfDay (jt/local-date) (ZoneId/ofOffset "UTC" (ZoneOffset/ofHours 0)))))
      :cljs (epoch-milli (local-date)))])

(def TODAY #'TODAY*)

(defn DATE*
  "Converts a year, month, and day into a date."
  [^{:doc "The year component of the date."} year
   ^{:doc "The month component of the date."} month
   ^{:doc "The day component of the date."} day]
  ["LocalDate"
   #?(:clj (.toEpochMilli (.toInstant (.atStartOfDay (jt/local-date (coerce/excel-number year)
                                                                    (coerce/excel-number month)
                                                                    (coerce/excel-number day))
                                                     (ZoneId/ofOffset "UTC" (ZoneOffset/ofHours 0)))))
      :cljs (let [d (js/Date. (js/Date.UTC (coerce/excel-number year)
                                           (dec (coerce/excel-number month))
                                           (coerce/excel-number day)))]
              (.getTime d)))])

(def DATE #'DATE*)

(defn DAY*
  "Returns the day of the month that a specific date falls on, in numeric format."
  [^{:doc "The date from which to extract the day. Must be a reference containing a date, or a function returning a date type.
"} date]
  #?(:clj (jt/as (coerce/inst date) :day-of-month)
     :cljs (.getUTCDate (coerce/inst date))))

(def DAY #'DAY*)

(defn MONTH*
  "Returns the month of the year a specific date falls in, in numeric format."
  [^{:doc "The date from which to extract the month. Must be a reference containing a date, or a function returning a date type"} date]
  #?(:clj (jt/as (coerce/inst date) :month-of-year)
     :cljs (inc (.getUTCMonth (coerce/inst date)))))

(def MONTH #'MONTH*)

(defn YEAR*
  "Returns the year specified by a given date."
  [^{:doc "The date from which to calculate the year. Must be a reference containing a date, or a function returning a date type."} date]
  #?(:clj (jt/as (coerce/inst date) :year)
     :cljs (.getUTCFullYear (coerce/inst date))))

(def YEAR #'YEAR*)

(defn PARSE-DATE*
  "Returns the date of the date and time if parsed from first argument using pattern as a second argument."
  [^{:doc "A string"} date-string
   ^{:doc "A pattern sutable to parse date/time strings. Should follow java.time.format.DateTimeFormatter specification"} pattern]
  (parse :local-date date-string pattern))

(def PARSE-DATE #'PARSE-DATE*)

(defn PARSE-DATE-TIME*
  "Returns the date of the date and time if parsed from first argument using pattern as a second argument."
  [^{:doc "A string"} date-string
   ^{:doc "A pattern sutable to parse date/time strings. Should follow java.time.format.DateTimeFormatter specification"} pattern]
  (parse :local-date-time date-string pattern))

(def PARSE-DATE-TIME #'PARSE-DATE-TIME*)

(def env
  {"YEAR" YEAR
   "MONTH" MONTH
   "NOW" NOW
   "DATE" DATE
   "DAY" DAY
   "TODAY" TODAY
   "PARSE" {"DATE" PARSE-DATE
            "DATETIME" PARSE-DATE-TIME}})
