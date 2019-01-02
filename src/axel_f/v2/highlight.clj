(ns axel-f.v2.highlight
  (:require [instaparse.core :as insta]
            [axel-f.v2.parser :refer [parser]]))

(defn tokenize [formula]
  (insta/parse parser formula :unhide :all :total true))
