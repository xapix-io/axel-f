(ns axel-f.functions.text
  (:require [axel-f.functions.core :refer [def-excel-fn]]
            [axel-f.functions.coercion :as coercion]
            [axel-f.functions.math :as math]
            [clojure.string :as string]))

(declare textjoin)

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

(defn arabic
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

(def arabic-meta
  {:desc "Computes the value of a Roman numeral."
   :args [{:desc "The Roman numeral to format, whose value must be between 1 and 3999, inclusive."}]})

;; TODO
;; (defn asc-fn [])

(defn char*
  [number]
  (let [number (some-> number coercion/excel-number int)]
    #?(:clj (-> number char str)
       :cljs (js/String.fromCharCode number))))

(def char*-meta
  {:desc "Convert a number into a character according to the current Unicode table."
   :args [{:desc "The number of the character to look up from the current Unicode table in decimal format."}]})

(defn code
  [text]
  (let [text (coercion/excel-str text)]
    #?(:clj (some-> text first int)
       :cljs (let [res (.charCodeAt text 0)]
               (when-not (js/isNaN res) res)))))

(def code-meta
  {:desc "Returns the numeric Unicode map value of the first character in the string provided."
   :args [{:desc "The string whose first character's Unicode map value will be returned."}]})

(defn concatenate
  [st1 & stx]
  (apply textjoin "" false st1 stx))

(def concatenate-meta
  {:desc "Appends strings to one another."
   :args [{:desc "The initial string."}
          {:desc "More strings to append in sequence."
           :opt true
           :repeatable true}]})

(defn format-money [number number-of-places]
  (let [number (double (math/round number number-of-places))
        number-of-places (if (< number-of-places 0)
                           0 number-of-places)]
    (str "$"
         #?(:clj
            (format (str "%." number-of-places "f") number)
            :cljs
            (string/replace (.toFixed number number-of-places)
                            (re-pattern "\\d(?=(\\d{3})+\\.)")
                            "$&,")))))

(defn dollar
  [number & [number-of-places]]
  (let [number-of-places (or number-of-places 2)
        number (coercion/excel-number number)
        number-of-places (coercion/excel-number number-of-places)]
    (format-money number number-of-places)))

(def dollar-meta
  {:desc "Formats a number into the locale-specific currency format."
   :args [{:desc "The value to be formatted."}
          {:desc "The number of decimal places to display."
           :opt true}]})

(defn exact
  [str1 str2]
  (= (coercion/excel-str str1)
     (coercion/excel-str str2)))

(def exact-meta
  {:desc "Tests whether two strings are identical."
   :args [{:desc "The first string to compare"}
          {:desc "The second string to compare"}]})

(defn find*
  [substr str & [from-index]]
  (let [from-index (or from-index 0)]
    (some-> str (string/index-of substr from-index) inc)))

(def find*-meta
  {:desc "Returns the position at which a string is first found within text where the capitalization of letters matters. Returns #VALUE! if the string is not found."
   :args [{:desc "The string to look for within arg2."}
          {:desc "The text to search for the first occurrence of arg1."}
          {:desc "The character within arg2 at which to start the search."
           :opt true}]})

(defn join
  [delimeter arg & args]
  (apply textjoin delimeter false arg args))

(def join-meta
  {:desc "Concatenates the elements of one or more one-dimensional arrays using a specified delimiter."
   :args [{:desc "The character or string to place between each concatenated value."}
          {:desc "The value or values to be appended using arg1."}
          {:desc "Additional value or array to be appended using arg1."
           :opt true
           :repeatable true}]})

;; TODO
;; (defn fixed-fn [])

(defn left
  [text & [number]]
  (let [number (or number 1)]
    (if (> number (count text))
      text
      (subs text 0 number))))

(def left-meta
  {:desc "Returns a substring from the beginning of a specified string."
   :args [{:desc "The string from which the left portion will be returned."}
          {:desc "The number of characters to return from the left side of arg1."
           :opt true}]})

(defn len
  [text]
  (let [text (cond
               (string? text) text
               (seqable? text) (first text))]
    (count (coercion/excel-str text))))

(def len-meta
  {:desc "Returns the length of a string."
   :args [{:desc "The string whose length will be returned."}]})

(defn lower
  [text]
  (string/lower-case (coercion/excel-str text)))

(def lower-meta
  {:desc "Converts a specified string to lowercase."
   :args [{:desc "The string to convert to lowercase."}]})

(defn mid
  [text start number]
  (let [start (coercion/excel-number start)
        number (coercion/excel-number number)
        text (coercion/excel-str text)
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

(def mid-meta
  {:desc "Returns a segment of a string."
   :args [{:desc "The string to extract a segment from."}
          {:desc "The index from the left of arg1 from which to begin extracting. The first character in arg1 has the index 1."}
          {:desc "The length of the segment to extract."}]})

(defn proper
  [text]
  (string/replace (coercion/excel-str text) #"\w*" string/capitalize))

(def proper-meta
  {:desc "Capitalizes each word in a specified string."
   :args [{:desc "The text which will be returned with the first letter of each word in uppercase and all other letters in lowercase."}]})

(defn regexextract
  [text regular-expression]
  (let [res (re-find (re-pattern regular-expression)
                     text)]
    (cond
      (string? res) res
      (vector? res) (second res)
      :otherwise res)))

(def regexextract-meta
  {:desc "Extracts matching substrings according to a regular expression."
   :args [{:desc "The input text."}
          {:desc "The first part of arg1 that matches this expression will be returned."}]})

(defn regexmatch
  [text regular-expression]
  (boolean (regexextract text regular-expression)))

(def regexmatch-meta
  {:desc "Whether a piece of text matches a regular expression."
   :args [{:desc "The text to be tested against the regular expression."}
          {:desc "The regular expression to test the text against."}]})

(defn regexreplace
  [text regular-expression replacement]
  (string/replace text (re-pattern regular-expression) replacement))

(def regexreplace-meta
  {:desc "Replaces part of a text string with a different text string using regular expressions."
   :args [{:desc "The text, a part of which will be replaced."}
          {:desc "The regular expression. All matching instances in text will be replaced."}
          {:desc "The text which will be inserted into the original text."}]})

(defn replace*
  [text position length new-text]
  (let [position (coercion/excel-number position)
        length (coercion/excel-number length)]
    (str (subs text 0 (dec position))
         (coercion/excel-str new-text)
         (subs text (+ (dec position) length)))))

(def replace*-meta
  {:desc "Replaces part of a text string with a different text string."
   :args [{:desc "The text, a part of which will be replaced."}
          {:desc "The position where the replacement will begin (starting from 1)."}
          {:desc "The number of characters in the text to be replaced."}
          {:desc "The text which will be inserted into the original text."}]})

(defn rept
  [text number]
  (let [number (coercion/excel-number number)]
    (->> (constantly text)
         (repeatedly number)
         (apply str))))

(def rept-meta
  {:desc "Returns specified text repeated a number of times."
   :args [{:desc "The character or string to repeat."}
          {:desc "The number of times arg1 should appear in the value returned."}]})

(defn right
  [text & [number]]
  (let [number (or number 1)]
    (if (<= (count text) number)
      text
      (subs text (- (count text)
                    number)))))

(def right-meta
  {:desc "Returns a substring from the end of a specified string."
   :args [{:desc "The string from which the right portion will be returned."}
          {:desc "The number of characters to return from the right side of arg1."
           :opt true}]})

(defn roman
  [n]
  (let [n (int (coercion/excel-number n))
        alphabet (sort-by val > roman-numerals)]
    (loop [res "" n n]
      (if (zero? n) res
          (let [[rom arab] (some #(when (<= (val %) n) %) alphabet)]
            (recur (str res rom) (- n arab)))))))

(def roman-meta
  {:desc "Formats a number in Roman numerals."
   :args [{:desc "The number to format, between 1 and 3999, inclusive."}]})

(defn search
  [find-text within-text & [position]]
  (let [position (or position 0)]
    (inc
     (string/index-of (string/lower-case within-text)
                      (string/lower-case find-text)
                      position))))

(def search-meta
  {:desc "Returns the position at which a string is first found within text and ignores capitalization of letters. Returns #VALUE! if the string is not found."
   :args [{:desc "The string to look for within arg2."}
          {:desc "The text to search for the first occurrence of arg1."}
          {:desc "The character within arg2 at which to start the search."
           :opt true}]})

(defn split
  [text delimeter & [split-by-each & [remove-empty-text]]]
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

(def split-meta
  {:desc "Divides text around a specified character or string, and puts each fragment into an array."
   :args [{:desc "The text to divide."}
          {:desc "The character or characters to use to split arg1."}
          {:desc "Whether or not to divide arg1 around each character contained in arg2."
           :opt true}
          {:desc "Whether or not to remove empty text messages from the split results. The default behavior is to treat consecutive delimiters as one (if TRUE). If FALSE, null values are added between consecutive delimiters."
           :opt true}]})

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

(defn substitute
  [text old-text new-text & [occurrence]]
  (let [occurrence (cond
                     (nil? occurrence) :all
                     :otherwise (coercion/excel-number occurrence))]
    (substitute-fn* (coercion/excel-str text)
                    (coercion/excel-str old-text)
                    (coercion/excel-str new-text)
                    occurrence)))

(def substitute-meta
  {:desc "Replaces existing text with new text in a string."
   :args [{:desc "The text within which to search and replace."}
          {:desc "The string to search for within text_to_search."}
          {:desc "The string that will replace search_for."}
          {:desc "The instance of arg2 within arg1 to replace with arg3. By default, all occurrences of arg2 are replaced; however, if arg4 is specified, only the indicated instance of arg2 is replaced."
           :opt true}]})

(defn t
  [value]
  (when (string? value)
    value))

(def t-meta
  {:desc "Returns string arguments as text."
   :args [{:desc "The argument to be converted to text."}]})

;; TODO
;; (defn text-fn [])

(defn trim
  [& args]
  (string/trim
   (string/replace (-> args first coercion/excel-str) #"\ +" " ")))

(def trim-meta
  {:desc "Removes leading, trailing, and repeated spaces in text."
   :args [{:desc "The text or reference to a cell containing text to be trimmed."}]})

(defn upper
  [& args]
  (-> args
      first
      coercion/excel-str
      string/upper-case))

(def upper-meta
  {:desc "Converts a specified string to uppercase."
   :args [{:desc "The string to convert to uppercase."}]})

(defn value
  [s]
  (cond
    (and (seqable? s)
         (empty? s))
    0

    (not (boolean? s))
    (coercion/excel-number s)

    :otherwise
    (throw (ex-info (str "Cannot coerce string `" s "` into number") {}))))

(def value-meta
  {:desc "Converts a string in any of the recognizeable date, time or number formats into a number."
   :args [{:desc "The string containing the value to be converted."}]})

(defn textjoin
  [delimeter ignore-empty & items]
  (->> items
       flatten
       (map coercion/excel-str)
       (filter (if ignore-empty
                 not-empty
                 identity))
       (string/join delimeter)))

(def textjoin-meta
  {:desc "Combines the text from multiple strings and/or arrays, with a specifiable delimiter separating the different texts."
   :args [{:desc "A string, possibly empty, or a reference to a valid string. If empty, text will be simply concatenated."}
          {:desc "A boolean; if TRUE, empty strings selected in the text arguments won't be included in the result."}
          {:desc "Any text item. This could be a string, or an array of strings in a range."}
          {:desc "Additional text item(s)."
           :opt true
           :repeatable true}]})

(defn clean
  [text]
  (string/replace text #"[\x00-\x1F]" ""))

(def clean-meta
  {:desc "Returns the text with the non-printable ASCII characters removed."
   :args [{:desc "The text whose non-printable characters are to be removed."}]})

(def-excel-fn
  "ARABIC"
  arabic
  arabic-meta

  "CHAR"
  char*
  char*-meta

  "CODE"
  code
  code-meta

  "CONCATENATE"
  concatenate
  concatenate-meta

  "CLEAN"
  clean
  clean-meta

  "DOLLAR"
  dollar
  dollar-meta

  "EXACT"
  exact
  exact-meta

  "FIND"
  find*
  find*-meta

  "JOIN"
  join
  join-meta

  "LEFT"
  left
  left-meta

  "LEN"
  len
  len-meta

  "LOWER"
  lower
  lower-meta

  "MID"
  mid
  mid-meta

  "PROPER"
  proper
  proper-meta

  "REGEXEXTRACT"
  regexextract
  regexextract-meta

  "REGEXMATCH"
  regexmatch
  regexmatch-meta

  "REGEXREPLACE"
  regexreplace
  regexreplace-meta

  "REPLACE"
  replace*
  replace*-meta

  "REPT"
  rept
  rept-meta

  "RIGHT"
  right
  right-meta

  "ROMAN"
  roman
  roman-meta

  "SEARCH"
  search
  search-meta

  "SPLIT"
  split
  split-meta

  "SUBSTITUTE"
  substitute
  substitute-meta

  "T"
  t
  t-meta

  "TRIM"
  trim
  trim-meta

  "UPPER"
  upper
  upper-meta

  "VALUE"
  value
  value-meta

  "TEXTJOIN"
  textjoin
  textjoin-meta)
