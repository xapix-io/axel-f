(ns axel-f.functions.text
  (:require #?(:clj [axel-f.macros :refer [def-excel-fn find-impl]]
               :cljs [axel-f.macros :refer [find-impl] :refer-macros [def-excel-fn]])
            [axel-f.error :as error]
            [axel-f.functions.coercion :as coercion]
            axel-f.functions.math
            [clojure.string :as string]
            #?(:cljs [goog.string :as gstring])))

(defn- regex-escape [pattern]
  (let [cmap {\. "\\."
              \+ "\\+"
              \| "\\|"
              \* "\\*"
              \^ "\\^"
              \[ "\\["
              \] "\\]"
              \( "\\("
              \) "\\)"}]
    (if (char? pattern)
      (or (get cmap pattern) pattern)
      (string/escape pattern cmap))))

(def roman-numerals
  {\I   1   \V   5   \X   10   \L   50
   \C   100 \D   500 \M   1000 "IV" 4
   "IX" 9   "XL" 40  "XC" 90   "CD" 400
   "CM" 900})

(def-excel-fn arabic [s]
  (let [sign (if (string/starts-with? s "-")
               -1 1)
        s (partition-all 2 (vec (string/replace-first s #"-" "")))]
    (loop [acc 0 current (first s) other (rest s)]
      (if current
        (let [is-pair? (get roman-numerals (apply str current))
              value (or is-pair?
                        (get roman-numerals (first current)))]
          (if is-pair?
            (recur (+ acc value) (first other) (rest other))
            (let [new-other (->> (concat current other)
                                flatten
                                rest
                                (partition-all 2))]
              (recur (+ acc value) (first new-other) (rest new-other)))))
        (* sign acc)))))

;; TODO
;; (defn asc-fn [])

(def-excel-fn char [number]
  (if number
    (if-let [number (some-> number coercion/excel-number int)]
      (if (< 0 number 65536)
        #?(:clj (-> number char str)
           :cljs (js/String.fromCharCode number))
        (throw (error/error "#NUM!" (str "Function CHAR parameter 1 value " number " is out of range."))))
      (throw (error/error "#VALUE!" (str "Function CHAR parameter 1 expects number values. But '" number "' is a text."))))
    (throw (error/error "#N/A" "Wrong number of args (0) passed to: CHAR"))))

(def-excel-fn code [text]
  (if-let [text (coercion/excel-str text)]
    #?(:clj (some-> text first int)
       :cljs (let [res (.charCodeAt text 0)]
               (when-not (js/isNaN res) res)))))

(def-excel-fn concatenate [st1 & stx]
  (apply (find-impl "TEXTJOIN") "" false st1 stx))

(def-excel-fn dollar [number & [number-of-places]]
  (let [number-of-places (or number-of-places 2)]
    (if-let [number (coercion/excel-number number)]
      (if-let [number-of-places (coercion/excel-number number-of-places)]
        (let [number (double ((find-impl "ROUND") number number-of-places))
              fmt (if (< number-of-places 0)
                    "%.0f"
                    (str "%." number-of-places "f"))]
          (str "$" #?(:clj (format fmt number)
                      :cljs (gstring/format fmt number))))
        (throw (error/error "#VALUE!" "Function DOLLAR parameter 2 expects number values.")))
      (throw (error/error "#VALUE!" "Function DOLLAR parameter 1 expects number values.")))))

(def-excel-fn exact [str1 str2]
  (= (coercion/excel-str str1)
     (coercion/excel-str str2)))

(def-excel-fn find [substr str & [from-index]]
  (let [from-index (or from-index 0)]
    (some-> str
            (string/index-of substr from-index)
            inc)))

(def-excel-fn join [delimeter arg & args]
  (if (and delimeter arg)
    (apply (find-impl "TEXTJOIN") delimeter false arg args)
    (throw (error/error "#N/A" (str "Wrong number of args (" (count (filter identity [delimeter arg]))
                                    ") passed to: JOIN")))))

;; TODO
;; (defn fixed-fn [])

(def-excel-fn left [text & [number]]
  (let [number (or number 1)]
    (if (> number (count text))
      text
      (subs text 0 number))))

(def-excel-fn len [text]
  (let [text (cond
               (string? text) text
               (seqable? text) (first text))]
    (count (coercion/excel-str text))))

(def-excel-fn lower [text]
  (string/lower-case (coercion/excel-str text)))

(def-excel-fn mid [text start number]
  (if-let [start (coercion/excel-number start)]
    (if-let [number (coercion/excel-number number)]
      (let [text (coercion/excel-str text)
            text-end (count text)
            params-start (dec start)
            params-end (+ (dec start) number)
            start (if (> params-start text-end)
                    text-end
                    params-start)
            end (if (> params-end text-end)
                  text-end
                  params-end)]
        (subs text start end))
      (throw (error/error "#VALUE!"
                          (error/format-not-a-number-error "MID" 3 number))))
    (throw (error/error "#VALUE!"
                        (error/format-not-a-number-error "MID" 2 start)))))

(def-excel-fn proper [text]
  (string/replace (coercion/excel-str text) #"\w*" string/capitalize))

(def-excel-fn regexextract [text regular-expression]
  (cond
    (not (string? text))
    (throw (error/error "#VALUE!"
                        (error/format-not-a-string-error "REGEXEXTRACT" 1 text)))

    (not (string? regular-expression))
    (throw (error/error "#VALUE!"
                        (error/format-not-a-string-error "REGEXEXTRACT" 2 regular-expression)))

    :otherwise
    (let [res (re-find (re-pattern regular-expression)
                       text)]
      (cond
        (string? res) res
        (vector? res) (second res)
        :otherwise res))))

(def-excel-fn regexmatch [text regular-expression]
  (cond
    (not (string? text))
    (throw (error/error "#VALUE!"
                        (error/format-not-a-string-error "REGEXMATCH" 1 text)))

    (not (string? regular-expression))
    (throw (error/error "#VALUE!"
                        (error/format-not-a-string-error "REGEXMATCH" 2 regular-expression)))

    :otherwise
    (boolean ((find-impl "REGEXEXTRACT") text regular-expression))))

(def-excel-fn regexreplace [text regular-expression replacement]
  (cond
    (not (string? text))
    (throw (error/error "#VALUE!"
                        (error/format-not-a-string-error "REGEXREPLACE" 1 text)))

    (not (string? regular-expression))
    (throw (error/error "#VALUE!"
                        (error/format-not-a-string-error "REGEXREPLACE" 2 regular-expression)))

    (not (string? replacement))
    (throw (error/error "#VALUE!"
                        (error/format-not-a-string-error "REGEXREPLACE" 3 replacement)))

    :otherwise
    (string/replace text (re-pattern regular-expression) replacement)))

(def-excel-fn replace [text position length new-text]
  (if-let [position (coercion/excel-number position)]
    (if-let [length (coercion/excel-number length)]
      (str (subs text 0 (dec position))
           (coercion/excel-str new-text)
           (subs text (+ (dec position) length)))
      (throw (error/error "#VALUE!"
                          (error/format-not-a-number-error "REPLACE" 3 length))))
    (throw (error/error "#VALUE!"
                        (error/format-not-a-number-error "REPLACE" 2 position)))))

(def-excel-fn rept [text number]
  (if-let [number (coercion/excel-number number)]
    (->> (constantly text)
        (repeatedly number)
        (apply str))
    (throw (error/error "#VALUE!"
                        (error/format-not-a-number-error "REPT" 2 number)))))

(def-excel-fn right [text & [number]]
  (let [number (or number 1)]
    (if (<= (count text) number)
      text
      (subs text (- (count text)
                    number)))))

(def-excel-fn roman [n]
  (if-let [n (coercion/excel-number n)]
    (if (<= 0 n 3999)
      (let [n (int n)
            alphabet (sort-by val > roman-numerals)]
        (loop [res "" n n]
          (if (zero? n) res
              (let [[rom arab] (some #(when (<= (val %) n) %) alphabet)]
                (recur (str res rom) (- n arab))))))
      (throw (error/error "#VALUE!" (str "Function ROMAN parameter 1 value is " n ". Valid values are between 1 and 3999 inclusive."))))
    (throw (error/error "#VALUE!"
                        (error/format-not-a-number-error "ROMAN" 1 n)))))

(def-excel-fn search [find-text within-text & [position]]
  (let [position (or position 0)]
    (inc
     (string/index-of (string/lower-case within-text)
                      (string/lower-case find-text)
                      position))))

(def-excel-fn split [text delimeter & [split-by-each & [remove-empty-text]]]
  (keep
   (if (or (nil? remove-empty-text)
           remove-empty-text)
     not-empty
     identity)
   (string/split (coercion/excel-str text)
                 (re-pattern (if split-by-each
                               (string/join "|" (map regex-escape
                                                     (vec (coercion/excel-str delimeter))))
                               (regex-escape (coercion/excel-str delimeter))))
                 -1)))

(defn- substitute-fn* [text old-text new-text occurrence]
  (if (empty? old-text)
    text
    (if (= occurrence :all)
      (string/replace text (re-pattern old-text) new-text)
      (let [occurrence (int occurrence)]
        (loop [i 1 index (string/index-of text old-text)]
          (if index
            (if (= i occurrence)
              (str (subs text 0 index)
                   new-text
                   (subs text (+ index (count old-text))))
              (recur (inc i) (string/index-of text old-text (inc index))))
            text))))))

(def-excel-fn substitute [text old-text new-text & [occurrence]]
  (if-let [occurrence (cond
                        (nil? occurrence) :all
                        :otherwise (coercion/excel-number occurrence))]
    (substitute-fn* (coercion/excel-str text)
                    (coercion/excel-str old-text)
                    (coercion/excel-str new-text)
                    occurrence)
    (throw (error/error "#VALUE!"
                        (error/format-not-a-number-error "SUBSTITUTE" 4 occurrence)))))

(def-excel-fn t [value]
  (when (string? value)
    value))

;; TODO
;; (defn text-fn [])

(def-excel-fn trim [& args]
  (string/trim
   (string/replace (-> args first coercion/excel-str) #"\ +" " ")))

(def-excel-fn upper [& args]
  (-> args
      first
      coercion/excel-str
      string/upper-case))

(def-excel-fn value [s]
  (or
   (when (and (seqable? s)
              (empty? s))
     0)
   (when-not (boolean? s)
     (coercion/excel-number s))
   (throw (error/error "#VALUE!" (str "VALUE parameter '" (coercion/excel-str s) "' cannot be parsed to number.")))))

(def-excel-fn textjoin [delimeter ignore-empty & items]
  (->> items
      flatten
      (map coercion/excel-str)
      (filter (if ignore-empty
                not-empty
                identity))
      (string/join delimeter)))
