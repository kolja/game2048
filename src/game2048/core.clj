(ns game2048.core
  (:gen-class))

(defn insert-2 [board]
  (let [flat (into [] (flatten board))
        zero-indices (seq (keep-indexed
                       #(when (zero? %2) %1)
                       flat))]
    (cond
      (some #{2048} flat) (pr "you win")
      (empty? zero-indices) (pr "game over")
      :else (partition 4 (assoc flat (rand-nth zero-indices) 2)))))

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

(defn sum-up [acc x]
  (let [l (last acc)]
    (if (= x l)
      (conj (pop acc) (+ l x) 0)
      (conj acc x))))

(defn sum-pairs [v]
  (remove-zeros (reduce sum-up [] v)))

(defn render [v]
  (doseq [line (for [row v]
                 (apply str (map #(format "%5d " %) row)))]
    (println line "\n")) v)

(defn slide [board]
 (map (comp
        fill
        sum-pairs
        remove-zeros
        ) board))


(defn -main []
  (loop [board (insert-2 (partition 4 (repeat 16 0)))]
    (when (render board)
      (let [input ({"h" 4 "k" 3 "l" 2 "j" 1} (read-line))
            command (apply comp
                           (assoc (into [] (repeat 5 rotate))
                                  input slide))] ;; (comp rotate rotate slide rotate rotate)
        (-> board command insert-2 recur)))))

