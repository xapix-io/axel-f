(ns axel-f.excel.date
  (:require [tick.alpha.api :as t]
            [tick.locale-en-us]
            [cljc.java-time.zone-id :as zone-id]
            [cljc.java-time.zone-offset :as zone-offset]
            [axel-f.excel.coerce :as coerce]))

(defmethod coerce/excel-number java.time.LocalDate [ld]
  (.toEpochSecond (.atStartOfDay ld (zone-id/of-offset "UTC" (zone-offset/of-hours 0)))))

(defmethod coerce/excel-number java.time.LocalDateTime [ldt]
  (.toEpochSecond ldt zone-offset/utc))

(defn -format
  ([d] (t/format d))
  ([d fmt]
   (t/format fmt d)))

(defmethod coerce/excel-str java.time.LocalDate [ld & [fmt]]
  (apply (partial -format ld) (when fmt [fmt])))

(defmethod coerce/excel-str java.time.LocalDateTime [ldt & [fmt]]
  (apply (partial -format ldt) (when fmt [fmt])))

(defn NOW*
  "Returns the current date and time as a date value."
  []
  (t/date-time))

(def NOW #'NOW*)

(defn TODAY*
  "Returns the current date as a date value."
  []
  (t/date))

(def TODAY #'TODAY*)

(defn DATE*
  "Converts a year, month, and day into a date."
  [^{:doc "The year component of the date."} year
   ^{:doc "The month component of the date."} month
   ^{:doc "The day component of the date."} day]
  (t/new-date (coerce/excel-number year)
              (coerce/excel-number month)
              (coerce/excel-number day)))

(def DATE #'DATE*)

(defn DAY*
  "Returns the day of the month that a specific date falls on, in numeric format."
  [^{:doc "The date from which to extract the day. Must be a reference containing a date, or a function returning a date type.
"} date]
  (t/day-of-month date))

(def DAY #'DAY*)

(defn MONTH*
  "Returns the month of the year a specific date falls in, in numeric format."
  [^{:doc "The date from which to extract the month. Must be a reference containing a date, or a function returning a date type"} date]
  (t/int (t/month date)))

(def MONTH #'MONTH*)

(defn YEAR*
  "Returns the year specified by a given date."
  [^{:doc "The date from which to calculate the year. Must be a reference containing a date, or a function returning a date type."} date]
  (t/int (t/year date)))

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
