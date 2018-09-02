(ns axel-f.functions
  (:require [clojure.string :as string]
            [axel-f.error :as error]
            [axel-f.functions.text :as text]
            [axel-f.functions.math :as math]
            [axel-f.functions.stat :as stat]
            [axel-f.functions.logic :as logic]))

(defn- clean-fn [text]
  (string/replace text #"[\x00-\x1F]" ""))

(def functions-map
  {"SUM"         {:impl #'math/sum-fn
                  :desc "Returns the sum of a series of numbers and/or references."
                  :args [{:desc "The first number or range to add together."}
                         {:desc "Additional numbers or ranges to add to arg1."
                          :opt true
                          :repeatable true}]}

   "MIN"         {:impl #'stat/min-fn
                  :desc "Returns the minimum value in a numeric dataset."
                  :args [{:desc "The first value or range to consider when calculating the minimum value."}
                         {:desc "Additional values or ranges to consider when calculating the minimum value."
                          :opt true
                          :repeatable true}]}

   "MAX"         {:impl #'stat/max-fn
                  :desc "Returns the maximum value in a numeric dataset."
                  :args [{:desc "The first value or range to consider when calculating the maximum value."}
                         {:desc "Additional values or ranges to consider when calculating the maximum value."
                          :opt true
                          :repeatable true}]}

   "AVERAGE"     {:impl #'stat/average-fn
                  :desc "Returns the numerical average value in a dataset, ignoring text."
                  :args [{:desc "The first value or range to consider when calculating the average value."}
                         {:desc "Additional values or ranges to consider when calculating the average value."
                          :opt true
                          :repeatable true}]}

   "ROUND"       {:impl #'math/round-fn
                  :desc "Rounds a number to a certain number of decimal places according to standard rules."
                  :args [{:desc "The value to round to places number of places."}
                         {:desc "The number of decimal places to which to round."
                          :opt true}]}

   "AND"         {:impl #'logic/and-fn
                  :desc "Returns true if all of the provided arguments are logically true, and false if any of the provided arguments are logically false."
                  :args [{:desc "Returns true if all of the provided arguments are logically true, and false if any of the provided arguments are logically false."}
                         {:desc "An expression or reference to some logical value, i.e. TRUE or FALSE, or can be coerced to a logical value."
                          :opt true
                          :repeatable true}]}

   "OR"          {:impl #'logic/or-fn
                  :desc "Returns true if any of the provided arguments are logically true, and false if all of the provided arguments are logically false."
                  :args [{:desc "An expression or reference to some logical value, i.e. TRUE or FALSE, or can be coerced to a logical value."}
                         {:desc "More expressions that evaluate to logical values."
                          :opt true
                          :repeatable true}]}

   "NOT"         {:impl #'logic/not-fn
                  :desc "Returns the opposite of a logical value - `NOT(TRUE)` returns `FALSE`; `NOT(FALSE)` returns `TRUE`."
                  :args [{:desc "An expression or reference holding an expression that represents some logical value, i.e. TRUE or FALSE."}]}

   "CLEAN"       {:impl #'clean-fn
                  :desc "Returns the text with the non-printable ASCII characters removed."
                  :args [{:desc "The text whose non-printable characters are to be removed."}]}

   "ARABIC"      {:impl #'text/arabic-fn
                  :desc "Computes the value of a Roman numeral."
                  :args [{:desc "The Roman numeral to format, whose value must be between 1 and 3999, inclusive."}]}

   "CHAR"        {:impl #'text/char-fn
                  :desc "Convert a number into a character according to the current Unicode table."
                  :args [{:desc "The number of the character to look up from the current Unicode table in decimal format."}]}

   "CODE"        {:impl #'text/code-fn
                  :desc "Returns the numeric Unicode map value of the first character in the string provided."
                  :args [{:desc "The string whose first character's Unicode map value will be returned."}]}

   "CONCATENATE" {:impl (partial text/textjoin-fn "" false)
                  :desc "Appends strings to one another."
                  :args [{:desc "The initial string."}
                         {:desc "More strings to append in sequence."
                          :opt true
                          :repeatable true}]}

   "DOLLAR"      {:impl #'text/dollar-fn
                  :desc "Formats a number into the locale-specific currency format."
                  :args [{:desc "The value to be formatted."}
                         {:desc "The number of decimal places to display."
                          :opt true}]}

   "EXACT"       {:impl #'text/exact-fn
                  :desc "Tests whether two strings are identical."
                  :args [{:desc "The first string to compare"}
                         {:desc "The second string to compare"}]}

   "FIND"        {:impl #'text/find-fn
                  :desc "Returns the position at which a string is first found within text where the capitalization of letters matters. Returns #VALUE! if the string is not found."
                  :args [{:desc "The string to look for within arg2."}
                         {:desc "The text to search for the first occurrence of arg1."}
                         {:desc "The character within arg2 at which to start the search."
                          :opt true}]}

   "JOIN"        {:impl (fn [delimeter & args]
                          (apply text/textjoin-fn delimeter false args))
                  :desc "Concatenates the elements of one or more one-dimensional arrays using a specified delimiter."
                  :args [{:desc "The character or string to place between each concatenated value."}
                         {:desc "The value or values to be appended using arg1."}
                         {:desc "Additional value or array to be appended using arg1."
                          :opt true
                          :repeatable true}]}

   "LEFT"        {:impl #'text/left-fn
                  :desc "Returns a substring from the beginning of a specified string."
                  :args [{:desc "The string from which the left portion will be returned."}
                         {:desc "The number of characters to return from the left side of arg1."
                          :opt true}]}

   "LEN"         {:impl #'text/len-fn
                  :desc "Returns the length of a string."
                  :args [{:desc "The string whose length will be returned."}]}

   "LOWER"       {:impl #'text/lower-fn
                  :desc "Converts a specified string to lowercase."
                  :args [{:desc "The string to convert to lowercase."}]}

   "MID"         {:impl #'text/mid-fn
                  :desc "Returns a segment of a string."
                  :args [{:desc "The string to extract a segment from."}
                         {:desc "The index from the left of arg1 from which to begin extracting. The first character in arg1 has the index 1."}
                         {:desc "The length of the segment to extract."}]}

   "PROPER"      {:impl #'text/proper-fn
                  :desc "Capitalizes each word in a specified string."
                  :args [{:desc "The text which will be returned with the first letter of each word in uppercase and all other letters in lowercase."}]}

   "REGEXEXTRACT" {:impl #'text/regexextract-fn
                   :desc "Extracts matching substrings according to a regular expression."
                   :args [{:desc "The input text."}
                          {:desc "The first part of arg1 that matches this expression will be returned."}]}

   "REGEXMATCH"  {:impl #'text/regexmatch-fn
                  :desc "Whether a piece of text matches a regular expression."
                  :args [{:desc "The text to be tested against the regular expression."}
                         {:desc "The regular expression to test the text against."}]}

   "REGEXREPLACE" {:impl #'text/regexreplace-fn
                   :desc "Replaces part of a text string with a different text string using regular expressions."
                   :args [{:desc "The text, a part of which will be replaced."}
                          {:desc "The regular expression. All matching instances in text will be replaced."}
                          {:desc "The text which will be inserted into the original text."}]}

   "REPLACE"     {:impl #'text/replace-fn
                  :desc "Replaces part of a text string with a different text string."
                  :args [{:desc "The text, a part of which will be replaced."}
                         {:desc "The position where the replacement will begin (starting from 1)."}
                         {:desc "The number of characters in the text to be replaced."}
                         {:desc "The text which will be inserted into the original text."}]}

   "REPT"        {:impl #'text/rept-fn
                  :desc "Returns specified text repeated a number of times."
                  :args [{:desc "The character or string to repeat."}
                         {:desc "The number of times arg1 should appear in the value returned."}]}

   "RIGHT"       {:impl #'text/right-fn
                  :desc "Returns a substring from the end of a specified string."
                  :args [{:desc "The string from which the right portion will be returned."}
                         {:desc "The number of characters to return from the right side of arg1."
                          :opt true}]}

   "ROMAN"       {:impl #'text/roman-fn
                  :desc "Formats a number in Roman numerals."
                  :args [{:desc "The number to format, between 1 and 3999, inclusive."}]}

   "SEARCH"      {:impl #'text/search-fn
                  :desc "Returns the position at which a string is first found within text and ignores capitalization of letters. Returns #VALUE! if the string is not found."
                  :args [{:desc "The string to look for within arg2."}
                         {:desc "The text to search for the first occurrence of arg1."}
                         {:desc "The character within arg2 at which to start the search."
                          :opt true}]}

   "SPLIT"       {:impl #'text/split-fn
                  :desc "Divides text around a specified character or string, and puts each fragment into an array."
                  :args [{:desc "The text to divide."}
                         {:desc "The character or characters to use to split arg1."}
                         {:desc "Whether or not to divide arg1 around each character contained in arg2."
                          :opt true}
                         {:desc "Whether or not to remove empty text messages from the split results. The default behavior is to treat consecutive delimiters as one (if TRUE). If FALSE, null values are added between consecutive delimiters."
                          :opt true}]}

   "SUBSTITUTE"  {:impl #'text/substitute-fn
                  :desc "Replaces existing text with new text in a string."
                  :args [{:desc "The text within which to search and replace."}
                         {:desc "The string to search for within text_to_search."}
                         {:desc "The string that will replace search_for."}
                         {:desc "The instance of arg2 within arg1 to replace with arg3. By default, all occurrences of arg2 are replaced; however, if arg4 is specified, only the indicated instance of arg2 is replaced."
                          :opt true}]}

   "T"           {:impl #'text/t-fn
                  :desc "Returns string arguments as text."
                  :args [{:desc "The argument to be converted to text."}]}

   "TRIM"        {:impl #'text/trim-fn
                  :desc "Removes leading, trailing, and repeated spaces in text."
                  :args [{:desc "The text or reference to a cell containing text to be trimmed."}]}

   "UPPER"       {:impl #'text/upper-fn
                  :desc "Converts a specified string to uppercase."
                  :args [{:desc "The string to convert to uppercase."}]}

   "VALUE"       {:impl #'text/value-fn
                  :desc "Converts a string in any of the date, time or number formats that axel-f understands into a number."
                  :args [{:desc "The string containing the value to be converted."}]}

   "TEXTJOIN"    {:impl #'text/textjoin-fn
                  :desc "Combines the text from multiple strings and/or arrays, with a specifiable delimiter separating the different texts."
                  :args [{:desc "A string, possibly empty, or a reference to a valid string. If empty, text will be simply concatenated."}
                         {:desc "A boolean; if TRUE, empty strings selected in the text arguments won't be included in the result."}
                         {:desc "Any text item. This could be a string, or an array of strings in a range."}
                         {:desc "Additional text item(s)."
                          :opt true
                          :repeatable true}]}

   "COUNT"       {:impl #'stat/count-fn
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
                          :repeatable true}]}})

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
