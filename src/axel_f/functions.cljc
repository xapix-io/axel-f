(ns axel-f.functions
  (:require [clojure.string :as string]))


(defn sum-fn [& items]
  (reduce #?(:clj +' :cljs +) items))

(defn min-fn [& items]
  (reduce min-fn items))

(defn max-fn [& items]
  (reduce max items))

(defn concatenate-fn [& items]
  (reduce str items))

(defn average-fn [& items]
  (let [len (count items)]
    (when-not (zero? len)
      (/ (apply sum-fn items)
         len))))

(defn round-fn
  ([d] (round-fn d 0))
  ([d precision]
   (let [precision (or precision 0)
         factor (Math/pow 10 precision)
         res (/ (Math/round (* d factor)) factor)]
     (if (> precision 0)
       res
       (int res)))))

(defn and-fn [& args]
  (every? identity args))

(defn or-fn [& args]
  (some identity args))

;; ASC
;; BAHTTEXT
;; CHAR

(defn clean-fn
  "removes all nonprintable characters from text.

  Use CLEAN on text imported from other applications that contains
  characters that may not print with your operating system. For
  example, you can use CLEAN to remove some low-level computer code
  that is frequently at the beginning and end of data files and cannot
  be printed."
  [text]
  (string/replace text #"[0-x1F]" ""))



(defn code-fn
  "Returns a numeric code for the first character in a text string."
  [])

;; CONCATENATE
;; DBCS
;; DOLLAR

(defn exact-fn
  "Compares two text strings and returns TRUE if they are exactly the same, FALSE otherwise. EXACT is case-sensitive but ignores formatting differences."
  [str1 str2]
  (= str1 str2))


(defn find-fn
  "FIND locate one text string within a second text string, and return the number of the starting position of the first text string from the first character of the second text string."
  [])

(defn fixed-fn
  "Rounds a number to the specified number of decimals, formats the number in decimal format using a period and commas, and returns the result as text."
  [])
;; HTML

(defn left-fn
  "LEFT returns the first character or characters in a text string, based on the number of characters you specify."
  [])

(defn len-fn
  "LEN returns the number of characters in a text string."
  [])

(defn lower-fn
  "Converts all uppercase letters in a text string to lowercase."
  [])

(defn mid-fn
  "MID returns a specific number of characters from a text string, starting at the position you specify, based on the number of characters you specify."
  [])

(defn numbervalue-fn
  "Converts text to a number"
  [])

;; PRONETIC

(defn proper-fn
  "Capitalizes the first letter in a text string and any other letters in text that follow any character other than a letter. Converts all other letters to lowercase letters."
  [])

;; REGEXEXTRACT
;; REGEXMATCH
;; REGEXREPLACE

(defn replace-fn
  "REPLACE replaces part of a text string, based on the number of characters you specify, with a different text string."
  [])

(defn rept-fn
  "Repeats text a given number of times. Use REPT to fill a cell with a number of instances of a text string."
  [])

(defn right-fn
  "RIGHT returns the last character or characters in a text string, based on the number of characters you specify."
  [])

(defn search-fn [])

(defn split-fn [])

(defn substitute-fn
  "Substitutes new_text for old_text in a text string. Use SUBSTITUTE when you want to replace specific text in a text string; use REPLACE when you want to replace any text that occurs in a specific location in a text string."
  [])
;; T

;; TEXT

(defn trim-fn
  "Removes all spaces from text except for single spaces between words. Use TRIM on text that you have received from another application that may have irregular spacing."
  [])

(defn upper-fn
  "Converts text to uppercase."
  [])

;; VALUE

(def functions-map
  {"SUM"         sum-fn
   "COUNT"       len-fn
   "LEN"         len-fn
   "MIN"         min-fn
   "MAX"         max-fn
   "CONCATENATE" concatenate-fn

   "AVERAGE"     average-fn
   "ROUND"       round-fn

   "AND"         and-fn
   "OR"          or-fn

   "EXACT"       exact-fn
   })
