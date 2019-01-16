(ns axel-f.functions.text
  (:require #?(:clj [axel-f.macros :refer [def-excel-fn]]
               :cljs [axel-f.macros :refer-macros [def-excel-fn]])
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

(def arabic
  ^{:desc "Computes the value of a Roman numeral."
    :args [{:desc "The Roman numeral to format, whose value must be between 1 and 3999, inclusive."}]}
  (fn [s]
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
          (* sign acc))))))

;; TODO
;; (defn asc-fn [])

(def char*
  ^{:desc "Convert a number into a character according to the current Unicode table."
    :args [{:desc "The number of the character to look up from the current Unicode table in decimal format."}]}
  (fn [number]
    (if number
      (if-let [number (some-> number coercion/excel-number int)]
        (if (< 0 number 65536)
          #?(:clj (-> number char str)
             :cljs (js/String.fromCharCode number))
          (throw (error/error "#NUM!" (str "Function CHAR parameter 1 value " number " is out of range."))))
        (throw (error/error "#VALUE!" (str "Function CHAR parameter 1 expects number values. But '" number "' is a text."))))
      (throw (error/error "#N/A" "Wrong number of args (0) passed to: CHAR")))))

(def code
  ^{:desc "Returns the numeric Unicode map value of the first character in the string provided."
    :args [{:desc "The string whose first character's Unicode map value will be returned."}]}
  (fn [text]
    (if-let [text (coercion/excel-str text)]
      #?(:clj (some-> text first int)
         :cljs (let [res (.charCodeAt text 0)]
                 (when-not (js/isNaN res) res))))))

(def concatenate
  ^{:desc "Appends strings to one another."
    :args [{:desc "The initial string."}
           {:desc "More strings to append in sequence."
            :opt true
            :repeatable true}]}
  (fn [st1 & stx]
    (apply (find-impl "TEXTJOIN") "" false st1 stx)))

(def dollar
  ^{:desc "Formats a number into the locale-specific currency format."
    :args [{:desc "The value to be formatted."}
           {:desc "The number of decimal places to display."
            :opt true}]}
  (fn [number & [number-of-places]]
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
        (throw (error/error "#VALUE!" "Function DOLLAR parameter 1 expects number values."))))))

(def exact
  ^{:desc "Tests whether two strings are identical."
    :args [{:desc "The first string to compare"}
           {:desc "The second string to compare"}]}
  (fn [str1 str2]
    (= (coercion/excel-str str1)
       (coercion/excel-str str2))))

(def find*
  ^{:desc "Returns the position at which a string is first found within text where the capitalization of letters matters. Returns #VALUE! if the string is not found."
    :args [{:desc "The string to look for within arg2."}
           {:desc "The text to search for the first occurrence of arg1."}
           {:desc "The character within arg2 at which to start the search."
            :opt true}]}
  (fn [substr str & [from-index]]
    (let [from-index (or from-index 0)]
      (some-> str
              (string/index-of substr from-index)
              inc))))

(def join
  ^{:desc "Concatenates the elements of one or more one-dimensional arrays using a specified delimiter."
    :args [{:desc "The character or string to place between each concatenated value."}
           {:desc "The value or values to be appended using arg1."}
           {:desc "Additional value or array to be appended using arg1."
            :opt true
            :repeatable true}]}
  (fn [delimeter arg & args]
    (if (and delimeter arg)
      (apply (find-impl "TEXTJOIN") delimeter false arg args)
      (throw (error/error "#N/A" (str "Wrong number of args (" (count (filter identity [delimeter arg]))
                                      ") passed to: JOIN"))))))

;; TODO
;; (defn fixed-fn [])

(def left
  ^{:desc "Returns a substring from the beginning of a specified string."
    :args [{:desc "The string from which the left portion will be returned."}
           {:desc "The number of characters to return from the left side of arg1."
            :opt true}]}
  (fn [text & [number]]
    (let [number (or number 1)]
      (if (> number (count text))
        text
        (subs text 0 number)))))

(def len
  ^{:desc "Returns the length of a string."
    :args [{:desc "The string whose length will be returned."}]}
  (fn [text]
    (let [text (cond
                 (string? text) text
                 (seqable? text) (first text))]
      (count (coercion/excel-str text)))))

(def lower
  ^{:desc "Converts a specified string to lowercase."
    :args [{:desc "The string to convert to lowercase."}]}
  (fn [text]
    (string/lower-case (coercion/excel-str text))))

(def mid
  ^{:desc "Returns a segment of a string."
    :args [{:desc "The string to extract a segment from."}
           {:desc "The index from the left of arg1 from which to begin extracting. The first character in arg1 has the index 1."}
           {:desc "The length of the segment to extract."}]}
  (fn [text start number]
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
                          (error/format-not-a-number-error "MID" 2 start))))))

(def proper
  ^{:desc "Capitalizes each word in a specified string."
    :args [{:desc "The text which will be returned with the first letter of each word in uppercase and all other letters in lowercase."}]}
  (fn [text]
    (string/replace (coercion/excel-str text) #"\w*" string/capitalize)))

(def regexextract
  ^{:desc "Extracts matching substrings according to a regular expression."
    :args [{:desc "The input text."}
           {:desc "The first part of arg1 that matches this expression will be returned."}]}
  (fn [text regular-expression]
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
          :otherwise res)))))

(def regexmatch
  ^{:desc "Whether a piece of text matches a regular expression."
    :args [{:desc "The text to be tested against the regular expression."}
           {:desc "The regular expression to test the text against."}]}
  (fn [text regular-expression]
    (cond
      (not (string? text))
      (throw (error/error "#VALUE!"
                          (error/format-not-a-string-error "REGEXMATCH" 1 text)))

      (not (string? regular-expression))
      (throw (error/error "#VALUE!"
                          (error/format-not-a-string-error "REGEXMATCH" 2 regular-expression)))

      :otherwise
      (boolean ((find-impl "REGEXEXTRACT") text regular-expression)))))

(def regexreplace
  ^{:desc "Replaces part of a text string with a different text string using regular expressions."
    :args [{:desc "The text, a part of which will be replaced."}
           {:desc "The regular expression. All matching instances in text will be replaced."}
           {:desc "The text which will be inserted into the original text."}]}
  (fn [text regular-expression replacement]
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
      (string/replace text (re-pattern regular-expression) replacement))))

(def replace*
  ^{:desc "Replaces part of a text string with a different text string."
    :args [{:desc "The text, a part of which will be replaced."}
           {:desc "The position where the replacement will begin (starting from 1)."}
           {:desc "The number of characters in the text to be replaced."}
           {:desc "The text which will be inserted into the original text."}]}
  (fn [text position length new-text]
    (if-let [position (coercion/excel-number position)]
      (if-let [length (coercion/excel-number length)]
        (str (subs text 0 (dec position))
             (coercion/excel-str new-text)
             (subs text (+ (dec position) length)))
        (throw (error/error "#VALUE!"
                            (error/format-not-a-number-error "REPLACE" 3 length))))
      (throw (error/error "#VALUE!"
                          (error/format-not-a-number-error "REPLACE" 2 position))))))

(def rept
  ^{:desc "Returns specified text repeated a number of times."
    :args [{:desc "The character or string to repeat."}
           {:desc "The number of times arg1 should appear in the value returned."}]}
  (fn [text number]
    (if-let [number (coercion/excel-number number)]
      (->> (constantly text)
           (repeatedly number)
           (apply str))
      (throw (error/error "#VALUE!"
                          (error/format-not-a-number-error "REPT" 2 number))))))

(def right
  ^{:desc "Returns a substring from the end of a specified string."
    :args [{:desc "The string from which the right portion will be returned."}
           {:desc "The number of characters to return from the right side of arg1."
            :opt true}]}
  (fn [text & [number]]
    (let [number (or number 1)]
      (if (<= (count text) number)
        text
        (subs text (- (count text)
                      number))))))

(def roman
  ^{:desc "Formats a number in Roman numerals."
    :args [{:desc "The number to format, between 1 and 3999, inclusive."}]}
  (fn [n]
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
                          (error/format-not-a-number-error "ROMAN" 1 n))))))

(def search
  ^{:desc "Returns the position at which a string is first found within text and ignores capitalization of letters. Returns #VALUE! if the string is not found."
    :args [{:desc "The string to look for within arg2."}
           {:desc "The text to search for the first occurrence of arg1."}
           {:desc "The character within arg2 at which to start the search."
            :opt true}]}
  (fn [find-text within-text & [position]]
    (let [position (or position 0)]
      (inc
       (string/index-of (string/lower-case within-text)
                        (string/lower-case find-text)
                        position)))))

(def split
  ^{:desc "Divides text around a specified character or string, and puts each fragment into an array."
    :args [{:desc "The text to divide."}
           {:desc "The character or characters to use to split arg1."}
           {:desc "Whether or not to divide arg1 around each character contained in arg2."
            :opt true}
           {:desc "Whether or not to remove empty text messages from the split results. The default behavior is to treat consecutive delimiters as one (if TRUE). If FALSE, null values are added between consecutive delimiters."
            :opt true}]}
  (fn [text delimeter & [split-by-each & [remove-empty-text]]]
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
                   -1))))

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

(def substitute
  ^{:desc "Replaces existing text with new text in a string."
    :args [{:desc "The text within which to search and replace."}
           {:desc "The string to search for within text_to_search."}
           {:desc "The string that will replace search_for."}
           {:desc "The instance of arg2 within arg1 to replace with arg3. By default, all occurrences of arg2 are replaced; however, if arg4 is specified, only the indicated instance of arg2 is replaced."
            :opt true}]}
  (fn [text old-text new-text & [occurrence]]
    (if-let [occurrence (cond
                          (nil? occurrence) :all
                          :otherwise (coercion/excel-number occurrence))]
      (substitute-fn* (coercion/excel-str text)
                      (coercion/excel-str old-text)
                      (coercion/excel-str new-text)
                      occurrence)
      (throw (error/error "#VALUE!"
                          (error/format-not-a-number-error "SUBSTITUTE" 4 occurrence))))))

(def t
  ^{:desc "Returns string arguments as text."
    :args [{:desc "The argument to be converted to text."}]}
  (fn [value]
    (when (string? value)
      value)))

;; TODO
;; (defn text-fn [])

(def trim
  ^{:desc "Removes leading, trailing, and repeated spaces in text."
    :args [{:desc "The text or reference to a cell containing text to be trimmed."}]}
  (fn [& args]
    (string/trim
     (string/replace (-> args first coercion/excel-str) #"\ +" " "))))

(def upper
  ^{:desc "Converts a specified string to uppercase."
    :args [{:desc "The string to convert to uppercase."}]}
  (fn [& args]
    (-> args
        first
        coercion/excel-str
        string/upper-case)))

(def value
  ^{:desc "Converts a string in any of the recognizeable date, time or number formats into a number."
    :args [{:desc "The string containing the value to be converted."}]}
  (fn [s]
    (or
     (when (and (seqable? s)
                (empty? s))
       0)
     (when-not (boolean? s)
       (coercion/excel-number s))
     (throw (error/error "#VALUE!" (str "VALUE parameter '" (coercion/excel-str s) "' cannot be parsed to number."))))))

(def textjoin
  ^{:desc "Combines the text from multiple strings and/or arrays, with a specifiable delimiter separating the different texts."
    :args [{:desc "A string, possibly empty, or a reference to a valid string. If empty, text will be simply concatenated."}
           {:desc "A boolean; if TRUE, empty strings selected in the text arguments won't be included in the result."}
           {:desc "Any text item. This could be a string, or an array of strings in a range."}
           {:desc "Additional text item(s)."
            :opt true
            :repeatable true}]}
  (fn [delimeter ignore-empty & items]
    (->> items
         flatten
         (map coercion/excel-str)
         (filter (if ignore-empty
                   not-empty
                   identity))
         (string/join delimeter))))

(def-excel-fn
  "ARABIC" arabic
  "CHAR" char*
  "CODE" code
  "CONCATENATE" concatenate
  "DOLLAR" dollar
  "EXACT" exact
  "FIND" find*
  "JOIN" join
  "LEFT" left
  "LEN" len
  "LOWER" lower
  "MID" mid
  "PROPER" proper
  "REGEXEXTRACT" regexextract
  "REGEXMATCH" regexmatch
  "REGEXREPLACE" regexreplace
  "REPLACE" replace*
  "REPT" rept
  "RIGHT" right
  "ROMAN" roman
  "SEARCH" search
  "SPLIT" split
  "SUBSTITUTE" substitute
  "T" t
  "TRIM" trim
  "UPPER" upper
  "VALUE" value
  "TEXTJOIN" textjoin
  )
