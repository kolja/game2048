(ns game2048.core
  (:gen-class))

;; TODO
;; inject a new number "2" into the board
;; detect lost -> no more space
;; detect win -> 2048
;; golf it (inline functions and so on)


;; game-loop
;; read character input
;; render function

(def v [[2 2 2 0]
        [0 2 0 2]
        [2 2 2 2]
        [2 4 4 2]])

(defn insert-2 [board]
  (let [flat (into [] (flatten board))
        indexes (seq (keep-indexed
                       #(when (zero? %2) %1)
                       flat))]
    (cond
      (some #{16} flat) "you win"
      (empty? indexes) "game over"
      :else (partition 4 4
                       (assoc flat (rand-nth indexes) 2)))))

(defn rotate [board]
  (map
    (fn [n]
        (map #(nth % n)
             board))
    [3 2 1 0]))

(defn remove-zeros [row]
 (remove zero? row))

(defn fill [row]
 (take 4 (concat row [0 0 0 0])))

;(defn recur-adder [acc [x & xs]]
;  (if x
;    (let [l (last acc)]
;      (if (= x l)
;        (recur-adder (conj (pop acc) (+ l x) 0) xs)
;        (recur-adder (conj acc x) xs)))
;    (remove-zeros acc)))

;(def sum-pairs-recursive (partial recur-adder []))

(defn reduce-adder [acc x]
  (let [l (last acc)]
    (if (= x l)
      (conj (pop acc) (+ l x) 0)
      (conj acc x))))

(defn sum-pairs [v]
  (remove-zeros (reduce reduce-adder [] v)))

(defn render [v]
  (doseq [line (for [row v]
                 (apply str (map #(format "%5d " %) row)))]
    (println line "\n")))

(defn slide [board]
 (map (comp
        fill
        sum-pairs
        remove-zeros
        ) board))

;(defn game-loop [board]
;  (render board)
;  (let [input ({"h" 4 "k" 3 "l" 2 "j" 1} (read-line))
;        command (apply comp
;                       (assoc (into [] (repeat 5 rotate))
;                              input slide))]
;    (-> board command game-loop)))

(defn game-loop [board]
  (render board)
  (case (read-line)
    "h" (-> board slide rotate rotate rotate rotate insert-2 game-loop)
    "k" (-> board rotate slide rotate rotate rotate insert-2 game-loop)
    "l" (-> board rotate rotate slide rotate rotate insert-2 game-loop)
    "j" (-> board rotate rotate rotate slide rotate insert-2 game-loop)))

