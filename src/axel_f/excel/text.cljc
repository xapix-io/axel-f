(ns axel-f.excel.text
  (:require [axel-f.excel.coerce :as coerce]
            [axel-f.excel.math :as math]
            [clojure.string :as string]))

(declare TEXTJOIN)

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

(defn ARABIC*
  "Computes the value of a Roman numeral."
  [s]
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

(def ARABIC #'ARABIC*)

;; TODO
;; (defn asc-fn [])

(defn CHAR*
  "Convert a number into a character according to the current Unicode table."
  [number]
  (let [number (some-> number coerce/excel-number int)]
    #?(:clj (-> number char str)
       :cljs (js/String.fromCharCode number))))

(def CHAR #'CHAR*)

(defn CODE*
  "Returns the numeric Unicode map value of the first character in the string provided."
  [text]
  (let [text (coerce/excel-string text)
        res #?(:clj (some-> text first int)
               :cljs (.charCodeAt text 0))]
    #?(:clj res
       :cljs (when-not (js/isNaN res) res))))

(def CODE #'CODE*)

(defn CONCATENATE*
  "Appends strings to one another."
  [st1 & stx]
  (apply TEXTJOIN "" true st1 stx))

(def CONCATENATE #'CONCATENATE*)

(defn format-money [number number-of-places]
  (let [number (double (math/ROUND number number-of-places))
        number-of-places (if (< number-of-places 0)
                           0 number-of-places)]
    (str "$"
         #?(:clj
            (format (str "%." number-of-places "f") number)
            :cljs
            (string/replace (.toFixed number number-of-places)
                            (re-pattern "\\d(?=(\\d{3})+\\.)")
                            "$&,")))))

(defn DOLLAR*
  "Formats a number into the locale-specific currency format."
  [number & [number-of-places]]
  (let [number-of-places (or number-of-places 2)
        number (coerce/excel-number number)
        number-of-places (coerce/excel-number number-of-places)]
    (format-money number number-of-places)))

(def DOLLAR #'DOLLAR*)

(defn EXACT*
  "Tests whether two strings are identical."
  [str1 str2]
  (= (coerce/excel-string str1)
     (coerce/excel-string str2)))

(def EXACT #'EXACT*)

(defn FIND*
  "Returns the position at which a string is first found within text where the capitalization of letters matters. Returns #VALUE! if the string is not found."
  [substr str & [from-index]]
  (let [from-index (or from-index 0)]
    (some-> str (string/index-of substr from-index) inc)))

(def FIND #'FIND*)

(defn JOIN*
  "Concatenates the elements of one or more one-dimensional arrays using a specified delimiter."
  [delimeter arg & args]
  (apply TEXTJOIN delimeter false arg args))

(def JOIN #'JOIN*)

;; TODO
;; (defn fixed-fn [])

(defn LEFT*
  "Returns a substring from the beginning of a specified string."
  [text & [number]]
  (let [number (or number 1)]
    (if (> number (count text))
      text
      (subs text 0 number))))

(def LEFT #'LEFT*)

(defn LEN*
  "Returns the length of a string."
  [text]
  (let [text (cond
               (string? text) text
               (seqable? text) (first text))]
    (count (coerce/excel-string text))))

(def LEN #'LEN*)

(defn LOWER*
  "Converts a specified string to lowercase."
  [text]
  (string/lower-case (coerce/excel-string text)))

(def LOWER #'LOWER*)

(defn MID*
  "Returns a segment of a string."
  [text start number]
  (let [start (coerce/excel-number start)
        number (coerce/excel-number number)
        text (coerce/excel-string text)
        text-end (count text)
        params-start (dec start)
        params-end (+ (dec start) number)
        start (if (> params-start text-end)
                text-end
                params-start)
        end (if (> params-end text-end)
              text-end
              params-end)]
    (subs text start end)))

(def MID #'MID*)

(defn PROPER*
  "Capitalizes each word in a specified string."
  [text]
  (string/replace (coerce/excel-string text) #"\w*" string/capitalize))

(def PROPER #'PROPER*)

(defn REGEXEXTRACT*
  "Extracts matching substrings according to a regular expression."
  [text regular-expression]
  (let [res (re-find (re-pattern regular-expression)
                     text)]
    (cond
      (string? res) res
      (vector? res) (second res)
      :else res)))

(def REGEXEXTRACT #'REGEXEXTRACT*)

(defn REGEXMATCH*
  "Whether a piece of text matches a regular expression."
  [text regular-expression]
  (boolean (REGEXEXTRACT text regular-expression)))

(def REGEXMATCH #'REGEXMATCH*)

(defn REGEXREPLACE*
  "Replaces part of a text string with a different text string using regular expressions."
  [text regular-expression replacement]
  (string/replace text (re-pattern regular-expression) replacement))

(def REGEXREPLACE #'REGEXREPLACE*)

(defn REPLACE*
  "Replaces part of a text string with a different text string."
  [text position length new-text]
  (let [position (coerce/excel-number position)
        length (coerce/excel-number length)]
    (str (subs text 0 (dec position))
         (coerce/excel-string new-text)
         (subs text (+ (dec position) length)))))

(def REPLACE #'REPLACE*)

(defn REPT*
  "Returns specified text repeated a number of times."
  [text number]
  (let [number (coerce/excel-number number)]
    (->> (constantly text)
         (repeatedly number)
         (apply str))))

(def REPT #'REPT*)

(defn RIGHT*
  "Returns a substring from the end of a specified string."
  [text & [number]]
  (let [number (or number 1)]
    (if (<= (count text) number)
      text
      (subs text (- (count text)
                    number)))))

(def RIGHT #'RIGHT*)

(defn ROMAN*
  "Formats a number in Roman numerals."
  [n]
  (let [n (int (coerce/excel-number n))
        alphabet (sort-by val > roman-numerals)]
    (loop [res "" n n]
      (if (zero? n) res
          (let [[rom arab] (some #(when (<= (val %) n) %) alphabet)]
            (recur (str res rom) (- n arab)))))))

(def ROMAN #'ROMAN*)

(defn SEARCH*
  "Returns the position at which a string is first found within text and ignores capitalization of letters. Returns #VALUE! if the string is not found."
  [find-text within-text & [position]]
  (let [position (or position 0)]
    (inc
     (string/index-of (string/lower-case within-text)
                      (string/lower-case find-text)
                      position))))

(def SEARCH #'SEARCH*)

(defn SPLIT*
  "Divides text around a specified character or string, and puts each fragment into an array."
  [text delimeter & [split-by-each & [remove-empty-text]]]
  (keep
   (if (or (nil? remove-empty-text)
           remove-empty-text)
     not-empty
     identity)
   (string/split (coerce/excel-string text)
                 (re-pattern (if split-by-each
                               (string/join "|" (map regex-escape
                                                     (vec (coerce/excel-string delimeter))))
                               (regex-escape (coerce/excel-string delimeter))))
                 -1)))

(def SPLIT #'SPLIT*)

(defn- substitute-fn*
  [text old-text new-text occurrence]
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

(defn SUBSTITUTE*
  "Replaces existing text with new text in a string."
  [text old-text new-text & [occurrence]]
  (let [occurrence (cond
                     (nil? occurrence) :all
                     :else (coerce/excel-number occurrence))]
    (substitute-fn* (coerce/excel-string text)
                    (coerce/excel-string old-text)
                    (coerce/excel-string new-text)
                    occurrence)))

(def SUBSTITUTE #'SUBSTITUTE*)

(defn T*
  "Returns string arguments as text."
  [value]
  (when (string? value)
    value))

(def T #'T*)

;; TODO
;; (defn text-fn [])

(defn TRIM*
  "Removes leading, trailing, and repeated spaces in text."
  [& args]
  (string/trim
   (string/replace (-> args first coerce/excel-string) #"\ +" " ")))

(def TRIM #'TRIM*)

(defn UPPER*
  "Converts a specified string to uppercase."
  [& args]
  (-> args
      first
      coerce/excel-string
      string/upper-case))

(def UPPER #'UPPER*)

(defn VALUE*
  "Converts a string in any of the recognizeable date, time or number formats into a number."
  [s]
  (cond
    (and (seqable? s)
         (empty? s))
    0

    (not (boolean? s))
    (coerce/excel-number s)))

(def VALUE #'VALUE*)

(defn TEXTJOIN*
  "Combines the text from multiple strings and/or arrays, with a specifiable delimiter separating the different texts."
  [delimeter ignore-empty & items]
  (let [items (flatten items)
        items (if ignore-empty (keep identity items) items)]
    (->> items
         (map coerce/excel-string)
         (string/join delimeter))))

(def TEXTJOIN #'TEXTJOIN*)

(defn CLEAN*
  "Returns the text with the non-printable ASCII characters removed."
  [text]
  (string/replace text #"[\x00-\x1F]" ""))

(def CLEAN #'CLEAN*)

(def env
  {"ARABIC" ARABIC
   "CHAR" CHAR
   "CODE" CODE
   "CONCATENATE" CONCATENATE
   "CLEAN" CLEAN
   "DOLLAR" DOLLAR
   "EXACT" EXACT
   "FIND" FIND
   "JOIN" JOIN
   "LEFT" LEFT
   "LEN" LEN
   "LOWER" LOWER
   "MID" MID
   "PROPER" PROPER
   "REGEXEXTRACT" REGEXEXTRACT
   "REGEXMATCH" REGEXMATCH
   "REGEXREPLACE" REGEXREPLACE
   "REPLACE" REPLACE
   "REPT" REPT
   "RIGHT" RIGHT
   "ROMAN" ROMAN
   "SEARCH" SEARCH
   "SPLIT" SPLIT
   "SUBSTITUTE" SUBSTITUTE
   "T" T
   "TRIM" TRIM
   "UPPER" UPPER
   "VALUE" VALUE
   "TEXTJOIN" TEXTJOIN})
