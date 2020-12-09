(ns utils)


(defn at-index [s i] (subs s i (inc i)))


(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))
