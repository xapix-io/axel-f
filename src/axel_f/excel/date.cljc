(ns axel-f.excel.date
  (:refer-clojure :exclude (format))
  (:require #?@(:cljs [["@js-joda/core"]
                       ["@js-joda/timezone"]
                       ["@js-joda/locale_en-us" :as js-joda-locale]
                       [java.time.format :refer [DateTimeFormatter]]])
            [cljc.java-time.zone-id :as zone-id]
            [cljc.java-time.zone-offset :as zone-offset]
            [cljc.java-time.local-date :as local-date]
            [cljc.java-time.local-date-time :as local-date-time]
            [axel-f.excel.coerce :as coerce])
  #?(:clj
     (:import [java.time.format DateTimeFormatter]
              [java.util Locale])))

(defmethod coerce/excel-number java.time.LocalDate [ld]
  (.toEpochSecond (.atStartOfDay ld (zone-id/of-offset "UTC" (zone-offset/of-hours 0)))))

(defmethod coerce/excel-number java.time.LocalDateTime [ldt]
  (.toEpochSecond ldt zone-offset/utc))

(defn ^DateTimeFormatter formatter
  "Constructs a DateTimeFormatter out of either a
  * format string - \"YYYY/mm/DD\" \"YYY HH:MM\" etc."
  [fmt]
  (let [locale #?(:clj Locale/US
                  :cljs (.. js-joda-locale -Locale -US))]
    (.. DateTimeFormatter
        (ofPattern fmt)
        (withLocale locale))))

(defn format [dt fmt]
  (if fmt
    (.format (formatter fmt) dt)
    (str dt)))

(defmethod coerce/excel-str java.time.LocalDate [ld & [fmt]]
  (format ld fmt))

(defmethod coerce/excel-str java.time.LocalDateTime [ldt & [fmt]]
  (format ldt fmt))

(defn NOW*
  "Returns the current date and time as a date value."
  []
  (local-date-time/now))

(def NOW #'NOW*)

(defn TODAY*
  "Returns the current date as a date value."
  []
  (local-date/now))

(def TODAY #'TODAY*)

(defn DATE*
  "Converts a year, month, and day into a date."
  [^{:doc "The year component of the date."} year
   ^{:doc "The month component of the date."} month
   ^{:doc "The day component of the date."} day]
  (local-date/of (coerce/excel-number year)
                 (coerce/excel-number month)
                 (coerce/excel-number day)))

(def DATE #'DATE*)

(defn DAY*
  "Returns the day of the month that a specific date falls on, in numeric format."
  [^{:doc "The date from which to extract the day. Must be a reference containing a date, or a function returning a date type.
"} date]
  (cond
    (instance? java.time.LocalDate date)
    (local-date/get-day-of-month date)

    (instance? java.time.LocalDateTime date)
    (local-date-time/get-day-of-month date)))

(def DAY #'DAY*)

(defn MONTH*
  "Returns the month of the year a specific date falls in, in numeric format."
  [^{:doc "The date from which to extract the month. Must be a reference containing a date, or a function returning a date type"} date]
  (cond
    (instance? java.time.LocalDate date)
    (local-date/get-month-value date)

    (instance? java.time.LocalDateTime date)
    (local-date-time/get-month-value date)))

(def MONTH #'MONTH*)

(defn YEAR*
  "Returns the year specified by a given date."
  [^{:doc "The date from which to calculate the year. Must be a reference containing a date, or a function returning a date type."} date]
  (cond
    (instance? java.time.LocalDate date)
    (local-date/get-year date)

    (instance? java.time.LocalDateTime date)
    (local-date-time/get-year date)))

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
