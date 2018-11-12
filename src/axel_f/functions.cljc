(ns axel-f.functions
  (:require [clojure.string :as string]
            [axel-f.macros #?(:clj :refer :cljs :refer-macros) [def-excel-fn]]
            axel-f.functions.math
            axel-f.functions.text
            axel-f.functions.stat
            axel-f.functions.logic))

(def-excel-fn clean
  "Returns the text with the non-printable ASCII characters removed."
  {:args [{:desc "The text whose non-printable characters are to be removed."}]}
  [text]
  (string/replace text #"[\x00-\x1F]" ""))
