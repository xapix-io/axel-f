(ns axel-f.functions
  (:require [clojure.string :as string]
            [axel-f.error :as error]))

(defn- sum-fn [& items]
  (reduce #?(:clj +' :cljs +) (flatten items)))

(defn- min-fn [& items]
  (reduce min (flatten items)))

(defn- max-fn [& items]
  (reduce max (flatten items)))

(defn- excel-str [item]
  (case item
    true "TRUE"
    false "FALSE"
    (str item)))

(defn- concatenate-fn [& items]
  (->> items
      flatten
      (map excel-str)
      (apply str)))

(defn- average-fn [& items]
  (let [items (filter number? (mapcat identity items))
        len (count items)]
    (when-not (zero? len)
      (/ (apply sum-fn items)
         len))))

(defn- round-fn
  ([d] (round-fn d 0))
  ([d precision]
   (let [factor (Math/pow 10 precision)
         res (/ (Math/round (* d factor)) factor)]
     (if (> precision 0)
       res
       (int res)))))

(defn- and-fn [& args]
  (every? identity args))

(defn- or-fn [& args]
  (boolean (some identity args)))

(defn- clean-fn [text]
  (string/replace text #"[\x00-\x1F]" ""))

(defn- code-fn [text]
  #?(:clj (-> text first int)
     :cljs (.charCodeAt text 0)))

(defn- exact-fn [str1 str2]
  (= str1 str2))

(defn- find-fn
  ([substr str] (find-fn substr str 0))
  ([substr str from-index]
   (some-> str
           (string/index-of substr from-index)
           (inc))))

(defn- left-fn
  ([text] (left-fn text 1))
  ([text number]
   (if (> number (count text))
     text
     (subs text 0 number))))

(defn- len-fn [text]
  (let [text (cond
               (string? text) text
               (seqable? text) (first text))]
    (count (excel-str text))))

(defn- lower-fn [text]
  (string/lower-case (excel-str text)))

(defn- mid-fn [text start number]
  (let [text-end (count text)
        params-start (dec start)
        params-end (+ (dec start) number)
        start (if (> params-start text-end)
                text-end
                params-start)
        end (if (> params-end text-end)
              text-end
              params-end)]
    (subs text start end)))

(defn- proper-fn [text]
  (string/replace (excel-str text) #"\w*" string/capitalize))

(defn- replace-fn
  [text position length new-text]
  (str (subs text 0 (dec position))
       new-text
       (subs text (+ (dec position) length))))

;; TODO: REGEX fns

(defn- rept-fn
  [text number]
  (->> (constantly text)
       (repeatedly number)
       (apply str)))

(defn- right-fn
  ([text] (right-fn text 1))
  ([text number]
   (if (<= (count text) number)
     text
     (subs text (- (count text)
                   number)))))

(defn- search-fn
  ([find-text within-text] (search-fn find-text within-text 0))
  ([find-text within-text position]
   (inc
    (string/index-of (string/lower-case within-text)
                     (string/lower-case find-text)
                     position))))

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

(defn- split-fn [text delimeter & [split-by-each & [remove-empty-text]]]
  (keep
   (if (or (nil? remove-empty-text)
           remove-empty-text)
     not-empty
     identity)
   (string/split (excel-str text)
                 (re-pattern (if split-by-each
                               (string/join "|" (map regex-escape
                                                     (vec (excel-str delimeter))))
                               (regex-escape (excel-str delimeter))))
                 -1)))

(defn- substitute-fn* [text old-text new-text occurrence]
  (if (empty? old-text)
    text
    (if (nil? occurrence)
      (string/replace text (re-pattern old-text) new-text)
      (loop [i 1
             index (string/index-of text old-text)]
        (if index
          (if (= i occurrence)
            (str (subs text 0 index)
                 new-text
                 (subs text (+ index (count old-text))))
            (recur (inc i) (string/index-of text old-text (inc index))))
          text)))))

(defn substitute-fn [& args]
  (cond

    (and (nth args 3 nil)
         (not (integer? (nth args 3))))
    (throw (error/error "#VALUE!" (str "Function SUBSTITUTE parameter 4 expects number values. But '" (nth args 3) "' is a text and cannot be coerced to a number.")))

    :otherwise
    (let [[text old-text new-text & [occurrence]] args]
      (substitute-fn*
       (excel-str text)
       (excel-str old-text)
       (excel-str new-text)
       occurrence))))

(defn- trim-fn [& args]
  (string/trim
   (string/replace (-> args first excel-str) #"\ +" " ")))

(defn- upper-fn [& args]
  (-> args
      first
      excel-str
      string/upper-case))

(defn- count-fn [& args]
  (->> args
      (mapcat (fn [item]
                (cond
                  (sequential? item) item
                  :otherwise [item])))
      (filter number?)
      count))

(def functions-map
  {"SUM"         {:impl #'sum-fn
                  :desc "Returns the sum of a series of numbers and/or references."
                  :args [{:desc "The first number or range to add together."}
                         {:desc "Additional numbers or ranges to add to arg1."
                          :opt true
                          :repeatable true}]}

   "MIN"         {:impl #'min-fn
                  :desc "Returns the minimum value in a numeric dataset."
                  :args [{:desc "The first value or range to consider when calculating the minimum value."}
                         {:desc "Additional values or ranges to consider when calculating the minimum value."
                          :opt true
                          :repeatable true}]}

   "MAX"         {:impl #'max-fn
                  :desc "Returns the maximum value in a numeric dataset."
                  :args [{:desc "The first value or range to consider when calculating the maximum value."}
                         {:desc "Additional values or ranges to consider when calculating the maximum value."
                          :opt true
                          :repeatable true}]}

   "CONCATENATE" {:impl #'concatenate-fn
                  :desc "Appends strings to one another."
                  :args [{:desc "The initial string."}
                         {:desc "More strings to append in sequence."
                          :opt true
                          :repeatable true}]}

   "AVERAGE"     {:impl #'average-fn
                  :desc "Returns the numerical average value in a dataset, ignoring text."
                  :args [{:desc "The first value or range to consider when calculating the average value."}
                         {:desc "Additional values or ranges to consider when calculating the average value."
                          :opt true
                          :repeatable true}]}

   "ROUND"       {:impl #'round-fn
                  :desc "Rounds a number to a certain number of decimal places according to standard rules."
                  :args [{:desc "The value to round to places number of places."}
                         {:desc "The number of decimal places to which to round."
                          :opt true}]}

   "AND"         {:impl #'and-fn
                  :desc "Returns true if all of the provided arguments are logically true, and false if any of the provided arguments are logically false."
                  :args [{:desc "Returns true if all of the provided arguments are logically true, and false if any of the provided arguments are logically false."}
                         {:desc "An expression or reference to some logical value, i.e. TRUE or FALSE, or can be coerced to a logical value."
                          :opt true
                          :repeatable true}]}

   "OR"          {:impl #'or-fn
                  :desc "Returns true if any of the provided arguments are logically true, and false if all of the provided arguments are logically false."
                  :args [{:desc "An expression or reference to some logical value, i.e. TRUE or FALSE, or can be coerced to a logical value."}
                         {:desc "More expressions that evaluate to logical values."
                          :opt true
                          :repeatable true}]}

   "CLEAN"       {:impl #'clean-fn
                  :desc "Returns the text with the non-printable ASCII characters removed."
                  :args [{:desc "The text whose non-printable characters are to be removed."}]}

   "CODE"        {:impl #'code-fn
                  :desc "Returns the numeric Unicode map value of the first character in the string provided."
                  :args [{:desc "The string whose first character's Unicode map value will be returned."}]}

   "EXACT"       {:impl #'exact-fn
                  :desc "Tests whether two strings are identical."
                  :args [{:desc "The first string to compare"}
                         {:desc "The second string to compare"}]}

   "FIND"        {:impl #'find-fn
                  :desc "Returns the position at which a string is first found within text where the capitalization of letters matters. Returns #VALUE! if the string is not found."
                  :args [{:desc "The string to look for within arg2."}
                         {:desc "The text to search for the first occurrence of arg1."}
                         {:desc "The character within arg2 at which to start the search."
                          :opt true}]}

   "LEFT"        {:impl #'left-fn
                  :desc "Returns a substring from the beginning of a specified string."
                  :args [{:desc "The string from which the left portion will be returned."}
                         {:desc "The number of characters to return from the left side of arg1."
                          :opt true}]}

   "LEN"         {:impl #'len-fn
                  :desc "Returns the length of a string."
                  :args [{:desc "The string whose length will be returned."}]}

   "LOWER"       {:impl #'lower-fn
                  :desc "Converts a specified string to lowercase."
                  :args [{:desc "The string to convert to lowercase."}]}

   "MID"         {:impl #'mid-fn
                  :desc "Returns a segment of a string."
                  :args [{:desc "The string to extract a segment from."}
                         {:desc "The index from the left of arg1 from which to begin extracting. The first character in arg1 has the index 1."}
                         {:desc "The length of the segment to extract."}]}

   "PROPER"      {:impl #'proper-fn
                  :desc "Capitalizes each word in a specified string."
                  :args [{:desc "The text which will be returned with the first letter of each word in uppercase and all other letters in lowercase."}]}

   "REPLACE"     {:impl #'replace-fn
                  :desc "Replaces part of a text string with a different text string."
                  :args [{:desc "The text, a part of which will be replaced."}
                         {:desc "The position where the replacement will begin (starting from 1)."}
                         {:desc "The number of characters in the text to be replaced."}
                         {:desc "The text which will be inserted into the original text."}]}

   "REPT"        {:impl #'rept-fn
                  :desc "Returns specified text repeated a number of times."
                  :args [{:desc "The character or string to repeat."}
                         {:desc "The number of times arg1 should appear in the value returned."}]}

   "RIGHT"       {:impl #'right-fn
                  :desc "Returns a substring from the end of a specified string."
                  :args [{:desc "The string from which the right portion will be returned."}
                         {:desc "The number of characters to return from the right side of arg1."
                          :opt true}]}

   "SEARCH"      {:impl #'search-fn
                  :desc "Returns the position at which a string is first found within text and ignores capitalization of letters. Returns #VALUE! if the string is not found."
                  :args [{:desc "The string to look for within arg2."}
                         {:desc "The text to search for the first occurrence of arg1."}
                         {:desc "The character within arg2 at which to start the search."
                          :opt true}]}

   "SPLIT"       {:impl #'split-fn
                  :desc "Divides text around a specified character or string, and puts each fragment into an array."
                  :args [{:desc "The text to divide."}
                         {:desc "The character or characters to use to split arg1."}
                         {:desc "Whether or not to divide arg1 around each character contained in arg2."
                          :opt true}
                         {:desc "Whether or not to remove empty text messages from the split results. The default behavior is to treat consecutive delimiters as one (if TRUE). If FALSE, null values are added between consecutive delimiters."
                          :opt true}]}

   "SUBSTITUTE"  {:impl #'substitute-fn
                  :desc "Replaces existing text with new text in a string."
                  :args [{:desc "The text within which to search and replace."}
                         {:desc "The string to search for within text_to_search."}
                         {:desc "The string that will replace search_for."}
                         {:desc "The instance of arg2 within arg1 to replace with arg3. By default, all occurrences of arg2 are replaced; however, if arg4 is specified, only the indicated instance of arg2 is replaced."
                          :opt true}]}

   "TRIM"        {:impl #'trim-fn
                  :desc "Removes leading, trailing, and repeated spaces in text."
                  :args [{:desc "The text or reference to a cell containing text to be trimmed."}]}

   "UPPER"       {:impl #'upper-fn
                  :desc "Converts a specified string to uppercase."
                  :args [{:desc "The string to convert to uppercase."}]}

   "COUNT"       {:impl #'count-fn
                  :desc "Returns a count of the number of numeric values in a dataset."
                  :args [{:desc "The first value or range to consider when counting."}
                         {:desc "Additional values or ranges to consider when counting."
                          :repeatable true
                          :opt true}]}

   "IF"          {:impl :special-form
                  :desc "Returns one value if a logical expression is TRUE and another if it is FALSE."
                  :args [{:desc "An expression or reference to a context value containing an expression that represents some logical value, i.e. TRUE or FALSE."}
                         {:desc "The value the function returns if arg1 is TRUE."}
                         {:desc "The value the function returns if arg1 is FALSE."
                          :opt true}]}

   "OBJREF"      {:impl :special-form
                  :desc "Returns a value from execution context."
                  :args [{:desc "Key-reference, number index or star (*)."
                          :repeatable true}]}
   })

(defn- infinity-args? [args]
  (-> args
      last
      :repeatable))

(defn- min-arity [args]
  (->> args
      (filter #(not (:opt %)))
      count))

(defn- max-arity [args]
  (when-not (infinity-args? args)
    (count args)))

(defn- format-wrong-arity-error [fnname min-arity max-arity total]
  (str "Wrong number of arguments to "
       fnname
       ". Expected "
       (if (and min-arity max-arity
                (not= min-arity max-arity))
         (str "between " min-arity " and " max-arity " arguments,")
         (if (and min-arity (nil? max-arity))
           (str "at least " min-arity " argument" (when-not (= 1 min-arity) "s") ",")
           (str "exact " min-arity " argument" (when-not (= 1 min-arity) "s") ",")))
       " but got "
       total
       " arguments."))

(defn check-arity [fnname args]
  (let [fn-args (get-in functions-map [fnname :args])
        infinity-args? (infinity-args? fn-args)
        min-arity (min-arity fn-args)
        max-arity (max-arity fn-args)]
    (or (and (= min-arity 0) infinity-args?)
        (<= min-arity (count args) (or max-arity #?(:clj Double/POSITIVE_INFINITY
                                                    :cljs js/Infinity)))
        (throw (error/error "#N/A"
                            (format-wrong-arity-error fnname min-arity max-arity (count args)))))))
