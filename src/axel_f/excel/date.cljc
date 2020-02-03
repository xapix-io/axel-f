(ns axel-f.excel.date
  (:require #?(:clj [java-time :as jt])
            [axel-f.excel.coerce :as coerce])
  #?(:clj (:import java.time.ZoneOffset
                   java.time.ZoneId)))

#?(:clj (defmethod coerce/excel-number java.time.LocalDate [ld]
          (.toEpochSecond (.atStartOfDay ld (ZoneId/ofOffset "UTC" (ZoneOffset/ofHours 0))))))

#?(:clj (defmethod coerce/excel-number java.time.LocalDateTime [ldt]
          (.toEpochSecond ldt ZoneOffset/UTC)))

#?(:cljs (defmethod coerce/excel-number js/Date [jsd]
           (Math/round (/ (.getTime jsd) 1000))))

(defn NOW*
  "Returns the current date and time as a date value."
  []
  #?(:clj (jt/local-date-time)
     :cljs (js/Date.)))

(def NOW #'NOW*)

(defn TODAY*
  "Returns the current date as a date value."
  []
  #?(:clj (jt/local-date)
     :cljs (let [n (js/Date.)]
             (js/Date. (js/Date.UTC (.getUTCFullYear n)
                                    (.getUTCMonth n)
                                    (.getUTCDate n))))))

(def TODAY #'TODAY*)

(defn DATE*
  "Converts a year, month, and day into a date."
  [^{:doc "The year component of the date."} year
   ^{:doc "The month component of the date."} month
   ^{:doc "The day component of the date."} day]
  #?(:clj (jt/local-date (coerce/excel-number year)
                         (coerce/excel-number month)
                         (coerce/excel-number day))
     :cljs (js/Date. (js/Date.UTC (coerce/excel-number year)
                                  (dec (coerce/excel-number month))
                                  (coerce/excel-number day)))))

(def DATE #'DATE*)

(defn DAY*
  "Returns the day of the month that a specific date falls on, in numeric format."
  [^{:doc "The date from which to extract the day. Must be a reference containing a date, or a function returning a date type.
"} date]
  #?(:clj (jt/as date :day-of-month)
     :cljs (.getUTCDate date)))

(def DAY #'DAY*)

(defn MONTH*
  "Returns the month of the year a specific date falls in, in numeric format."
  [^{:doc "The date from which to extract the month. Must be a reference containing a date, or a function returning a date type"} date]
  #?(:clj (jt/as date :month-of-year)
     :cljs (inc (.getUTCMonth date))))

(def MONTH #'MONTH*)

(defn YEAR*
  "Returns the year specified by a given date."
  [^{:doc "The date from which to calculate the year. Must be a reference containing a date, or a function returning a date type."} date]
  #?(:clj (jt/as date :year)
     :cljs (.getUTCFullYear date)))

(def YEAR #'YEAR*)

(def env
  {"YEAR" YEAR
   "MONTH" MONTH
   "NOW" NOW
   ;; "NETWORKDAYS" NETWORKDAYS
   ;; "DAYS" DAYS
   "DATE" DATE
   ;; "DAYS360" DAYS360
   ;; "YEARFRAC" YEARFRAC
   ;; "WEEKDAY" WEEKDAY
   ;; "TIME" TIME
   ;; "WORKDAY" WORKDAY
   ;; "EDATE" EDATE
   ;; "WEEKNUM" WEEKNUM
   ;; "EOMONTH" EOMONTH
   ;; "ISOWEEKNUM" ISOWEEKNUM
   "DAY" DAY
   ;; "MINUTE" MINUTE
   ;; "WORKDAY.INTL" WORKDAY.INTL
   ;; "NETWORKDAYS.INTL" NETWORKDAYS.INTL
   ;; "DATEDIF" DATEDIF
   ;; "HOUR" HOUR
   ;; "TIMEVALUE" TIMEVALUE
   ;; "DATEVALUE" DATEVALUE
   ;; "SECOND" SECOND
   "TODAY" TODAY
   })
