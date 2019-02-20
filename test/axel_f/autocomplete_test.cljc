(ns axel-f.autocomplete-test
  (:require [axel-f.core :as af]
            [axel-f.autocomplete :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest suggestions-test

  (t/testing "incomplete reference"

    (t/is (= {:suggestions
              [{:type :REF, :value "bar", :desc "Field in the context"}],
              :context nil}
             (sut/suggestions "foo.bar" {"foo" {"bar" 1}})))

    (t/is (= {:suggestions
              [{:type :REF, :value "bar", :desc "Field in the context"}
               {:type :REF, :value "baz", :desc "Field in the context"}],
              :context nil}
             (sut/suggestions "foo." {"foo" {"bar" 1
                                             "baz" 2}})))

    (t/is (= {:suggestions
              [{:type :REF, :value "foo", :desc "Field in the context"}],
              :context nil}
             (sut/suggestions "fo" {"foo" {"bar" 1}})))

    (t/is (= {:suggestions
              [{:type :FN,
                :value "EXACT",
                :desc "Tests whether two strings are identical.",
                :args
                [{:desc "The first string to compare"}
                 {:desc "The second string to compare"}]}
               {:type :FN,
                :value "T",
                :desc "Returns string arguments as text.",
                :args [{:desc "The argument to be converted to text."}]}
               {:type :FN,
                :value "VALUE",
                :desc
                "Converts a string in any of the recognizeable date, time or number formats into a number.",
                :args [{:desc "The string containing the value to be converted."}]}
               {:type :FN,
                :value "JSON.DECODE",
                :args [{:desc "JSON-encoded string to be decoded"}],
                :desc
                "Returns an object corresponding to the given JSON-encoded string."}
               {:type :FN,
                :value "ARABIC",
                :desc "Computes the value of a Roman numeral.",
                :args
                [{:desc
                  "The Roman numeral to format, whose value must be between 1 and 3999, inclusive."}]}
               {:type :FN,
                :value "MID",
                :desc "Returns a segment of a string.",
                :args
                [{:desc "The string to extract a segment from."}
                 {:desc
                  "The index from the left of arg1 from which to begin extracting. The first character in arg1 has the index 1."}
                 {:desc "The length of the segment to extract."}]}
               {:type :FN,
                :value "SUM",
                :desc "Returns the sum of a series of numbers and/or references.",
                :args
                [{:desc "The first number or range to add together."}
                 {:desc "Additional numbers or ranges to add to arg1.",
                  :opt true,
                  :repeatable true}]}
               {:type :FN,
                :value "CLEAN",
                :desc
                "Returns the text with the non-printable ASCII characters removed.",
                :args
                [{:desc
                  "The text whose non-printable characters are to be removed."}]}
               {:type :FN,
                :value "ROMAN",
                :desc "Formats a number in Roman numerals.",
                :args
                [{:desc "The number to format, between 1 and 3999, inclusive."}]}
               {:type :FN,
                :value "REGEXMATCH",
                :desc "Whether a piece of text matches a regular expression.",
                :args
                [{:desc "The text to be tested against the regular expression."}
                 {:desc "The regular expression to test the text against."}]}
               {:type :FN,
                :value "COUNT",
                :desc
                "Returns a count of the number of numeric values in a dataset.",
                :args
                [{:desc "The first value or range to consider when counting."}
                 {:desc "Additional values or ranges to consider when counting.",
                  :repeatable true,
                  :opt true}]}
               {:type :FN,
                :value "BASE64.ENCODE",
                :args [{:desc "String to encode"}],
                :desc "Creates a base-64 encoded ASCII string from a String"}
               {:type :FN,
                :value "TEXTJOIN",
                :desc
                "Combines the text from multiple strings and/or arrays, with a specifiable delimiter separating the different texts.",
                :args
                [{:desc
                  "A string, possibly empty, or a reference to a valid string. If empty, text will be simply concatenated."}
                 {:desc
                  "A boolean; if TRUE, empty strings selected in the text arguments won't be included in the result."}
                 {:desc
                  "Any text item. This could be a string, or an array of strings in a range."}
                 {:desc "Additional text item(s).", :opt true, :repeatable true}]}
               {:type :FN,
                :value "LEFT",
                :desc
                "Returns a substring from the beginning of a specified string.",
                :args
                [{:desc "The string from which the left portion will be returned."}
                 {:desc
                  "The number of characters to return from the left side of arg1.",
                  :opt true}]}
               {:type :FN,
                :value "TRIM",
                :desc "Removes leading, trailing, and repeated spaces in text.",
                :args
                [{:desc
                  "The text or reference to a cell containing text to be trimmed."}]}
               {:type :FN,
                :value "REPT",
                :desc "Returns specified text repeated a number of times.",
                :args
                [{:desc "The character or string to repeat."}
                 {:desc
                  "The number of times arg1 should appear in the value returned."}]}
               {:type :FN,
                :value "FIND",
                :desc
                "Returns the position at which a string is first found within text where the capitalization of letters matters. Returns #VALUE! if the string is not found.",
                :args
                [{:desc "The string to look for within arg2."}
                 {:desc "The text to search for the first occurrence of arg1."}
                 {:desc "The character within arg2 at which to start the search.",
                  :opt true}]}
               {:type :FN,
                :value "SPLIT",
                :desc
                "Divides text around a specified character or string, and puts each fragment into an array.",
                :args
                [{:desc "The text to divide."}
                 {:desc "The character or characters to use to split arg1."}
                 {:desc
                  "Whether or not to divide arg1 around each character contained in arg2.",
                  :opt true}
                 {:desc
                  "Whether or not to remove empty text messages from the split results. The default behavior is to treat consecutive delimiters as one (if TRUE). If FALSE, null values are added between consecutive delimiters.",
                  :opt true}]}
               {:type :FN,
                :value "SUBSTITUTE",
                :desc "Replaces existing text with new text in a string.",
                :args
                [{:desc "The text within which to search and replace."}
                 {:desc "The string to search for within text_to_search."}
                 {:desc "The string that will replace search_for."}
                 {:desc
                  "The instance of arg2 within arg1 to replace with arg3. By default, all occurrences of arg2 are replaced; however, if arg4 is specified, only the indicated instance of arg2 is replaced.",
                  :opt true}]}
               {:type :FN,
                :value "BASE64ENCODE",
                :args [{:desc "String to encode"}],
                :desc "Creates a base-64 encoded ASCII string from a String"}
               {:type :FN,
                :value "MIN",
                :desc "Returns the minimum value in a numeric dataset.",
                :args
                [{:desc
                  "The first value or range to consider when calculating the minimum value."}
                 {:desc
                  "Additional values or ranges to consider when calculating the minimum value.",
                  :opt true,
                  :repeatable true}]}
               {:type :FN,
                :value "UPPER",
                :desc "Converts a specified string to uppercase.",
                :args [{:desc "The string to convert to uppercase."}]}
               {:type :FN,
                :value "JSONENCODE",
                :args [{:desc "Object to be encoded"}],
                :desc "Returns a JSON-encoding String for the given object."}
               {:type :FN,
                :value "NOT",
                :desc
                "Returns the opposite of a logical value - `NOT(TRUE)` returns `FALSE`; `NOT(FALSE)` returns `TRUE`.",
                :args
                [{:desc
                  "An expression or reference holding an expression that represents some logical value, i.e. TRUE or FALSE."}]}
               {:type :FN,
                :value "MAX",
                :desc "Returns the maximum value in a numeric dataset.",
                :args
                [{:desc
                  "The first value or range to consider when calculating the maximum value."}
                 {:desc
                  "Additional values or ranges to consider when calculating the maximum value.",
                  :opt true,
                  :repeatable true}]}
               {:type :FN,
                :value "REGEXREPLACE",
                :desc
                "Replaces part of a text string with a different text string using regular expressions.",
                :args
                [{:desc "The text, a part of which will be replaced."}
                 {:desc
                  "The regular expression. All matching instances in text will be replaced."}
                 {:desc "The text which will be inserted into the original text."}]}
               {:type :FN,
                :value "AND",
                :desc
                "Returns true if all of the provided arguments are logically true, and false if any of the provided arguments are logically false.",
                :args
                [{:desc
                  "Returns true if all of the provided arguments are logically true, and false if any of the provided arguments are logically false."}
                 {:desc
                  "An expression or reference to some logical value, i.e. TRUE or FALSE, or can be coerced to a logical value.",
                  :opt true,
                  :repeatable true}]}
               {:type :FN,
                :value "JSON.ENCODE",
                :args [{:desc "Object to be encoded"}],
                :desc "Returns a JSON-encoding String for the given object."}
               {:type :FN,
                :value "SEARCH",
                :desc
                "Returns the position at which a string is first found within text and ignores capitalization of letters. Returns #VALUE! if the string is not found.",
                :args
                [{:desc "The string to look for within arg2."}
                 {:desc "The text to search for the first occurrence of arg1."}
                 {:desc "The character within arg2 at which to start the search.",
                  :opt true}]}
               {:type :FN,
                :value "JSONDECODE",
                :args [{:desc "JSON-encoded string to be decoded"}],
                :desc
                "Returns an object corresponding to the given JSON-encoded string."}
               {:type :FN,
                :value "REGEXEXTRACT",
                :desc
                "Extracts matching substrings according to a regular expression.",
                :args
                [{:desc "The input text."}
                 {:desc
                  "The first part of arg1 that matches this expression will be returned."}]}
               {:type :FN,
                :value "CODE",
                :desc
                "Returns the numeric Unicode map value of the first character in the string provided.",
                :args
                [{:desc
                  "The string whose first character's Unicode map value will be returned."}]}
               {:type :FN,
                :value "RIGHT",
                :desc "Returns a substring from the end of a specified string.",
                :args
                [{:desc "The string from which the right portion will be returned."}
                 {:desc
                  "The number of characters to return from the right side of arg1.",
                  :opt true}]}
               {:type :FN,
                :value "GEO.DISTANCE",
                :args
                [{:desc
                  "List of points. Each point must be a tuple of latitude and longitude"}],
                :desc
                "Calculate the distance for the path described as a list of geo points. Each point must a tuple of two or three float numbers."}
               {:type :FN,
                :value "AVERAGE",
                :desc
                "Returns the numerical average value in a dataset, ignoring text.",
                :args
                [{:desc
                  "The first value or range to consider when calculating the average value."}
                 {:desc
                  "Additional values or ranges to consider when calculating the average value.",
                  :opt true,
                  :repeatable true}]}
               {:type :FN,
                :value "BASE64DECODE",
                :args [{:desc "String to decode"}],
                :desc
                "Decodes a string of data which has been encoded using base-64 encoding"}
               {:type :FN,
                :value "PROPER",
                :desc "Capitalizes each word in a specified string.",
                :args
                [{:desc
                  "The text which will be returned with the first letter of each word in uppercase and all other letters in lowercase."}]}
               {:type :FN,
                :value "DOLLAR",
                :desc "Formats a number into the locale-specific currency format.",
                :args
                [{:desc "The value to be formatted."}
                 {:desc "The number of decimal places to display.", :opt true}]}
               {:type :FN,
                :value "REPLACE",
                :desc
                "Replaces part of a text string with a different text string.",
                :args
                [{:desc "The text, a part of which will be replaced."}
                 {:desc
                  "The position where the replacement will begin (starting from 1)."}
                 {:desc "The number of characters in the text to be replaced."}
                 {:desc "The text which will be inserted into the original text."}]}
               {:type :FN,
                :value "ROUND",
                :desc
                "Rounds a number to a certain number of decimal places according to standard rules.",
                :args
                [{:desc "The value to round to places number of places."}
                 {:desc "The number of decimal places to which to round.",
                  :opt true}]}
               {:type :FN,
                :value "OR",
                :desc
                "Returns true if any of the provided arguments are logically true, and false if all of the provided arguments are logically false.",
                :args
                [{:desc
                  "An expression or reference to some logical value, i.e. TRUE or FALSE, or can be coerced to a logical value."}
                 {:desc "More expressions that evaluate to logical values.",
                  :opt true,
                  :repeatable true}]}
               {:type :FN,
                :value "LOWER",
                :desc "Converts a specified string to lowercase.",
                :args [{:desc "The string to convert to lowercase."}]}
               {:type :FN,
                :value "CHAR",
                :desc
                "Convert a number into a character according to the current Unicode table.",
                :args
                [{:desc
                  "The number of the character to look up from the current Unicode table in decimal format."}]}
               {:type :FN,
                :value "JOIN",
                :desc
                "Concatenates the elements of one or more one-dimensional arrays using a specified delimiter.",
                :args
                [{:desc
                  "The character or string to place between each concatenated value."}
                 {:desc "The value or values to be appended using arg1."}
                 {:desc "Additional value or array to be appended using arg1.",
                  :opt true,
                  :repeatable true}]}
               {:type :FN,
                :value "LEN",
                :desc "Returns the length of a string.",
                :args [{:desc "The string whose length will be returned."}]}
               {:type :FN,
                :value "CONCATENATE",
                :desc "Appends strings to one another.",
                :args
                [{:desc "The initial string."}
                 {:desc "More strings to append in sequence.",
                  :opt true,
                  :repeatable true}]}
               {:type :FN,
                :value "BASE64.DECODE",
                :args [{:desc "String to decode"}],
                :desc
                "Decodes a string of data which has been encoded using base-64 encoding"}
               {:type :REF, :value "foo", :desc "Field in the context"}
               {:type :REF, :value "bar", :desc "Field in the context"}],
              :context nil}
             (sut/suggestions "" {"foo" 1
                                  "bar" 2})))

    (t/testing "keywords in references"

      (t/is (= {:suggestions
                [{:type :REF, :value ":/baz", :desc "Field in the context"}
                 {:type :REF, :value ":/bav", :desc "Field in the context"}],
                :context nil}
               (sut/suggestions ":foo/bar.:/ba" {:foo/bar {:baz 1
                                                           :bav 2}}))))

    (t/testing "array in suggestions"

      (t/is (= {:suggestions
                [{:type :REF, :value "bar", :desc "Field in the context"}
                 {:type :REF, :value "baz", :desc "Field in the context"}],
                :context nil}
               (sut/suggestions "foo.[*].ba" {"foo" [{"bar" 1} {"baz" 2}]}))))))

(t/deftest function-call-test

  (t/testing "incomplete arguments list"

    (t/is (= {:suggestions [],
              :context
              {:current-arg 2,
               :type :FNCALL,
               :value "SUM",
               :desc "Returns the sum of a series of numbers and/or references.",
               :args
               [{:desc "The first number or range to add together."}
                {:desc "Additional numbers or ranges to add to arg1.",
                 :opt true,
                 :repeatable true}]}}
             (sut/suggestions "SUM(1, 2, foo." {})))))

(t/deftest core-test

  (t/is (= {:suggestions [],
            :context
            {:current-arg 2,
             :type :FNCALL,
             :value "SUM",
             :desc "Returns the sum of a series of numbers and/or references.",
             :args
             [{:desc "The first number or range to add together."}
              {:desc "Additional numbers or ranges to add to arg1.",
               :opt true,
               :repeatable true}]}}
           (af/suggestions "SUM(1, 2, foo." {})))

  (t/is (= {:suggestions [],
            :context
            {:current-arg 2,
             :type :FNCALL,
             :value "SUM",
             :desc "Returns the sum of a series of numbers and/or references.",
             :args
             [{:desc "The first number or range to add together."}
              {:desc "Additional numbers or ranges to add to arg1.",
               :opt true,
               :repeatable true}]}}
           (af/suggestions "SUM(1, 2, foo."))))
