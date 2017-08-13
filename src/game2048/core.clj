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

(defn remove-zeros [row]
 (remove zero? row))

(defn render [v]
  (doseq [line (for [row v]
                 (apply str (map #(format "%5d " %) row)))]
    (println line "\n")) v)

(defn slide [board]
 (map (comp
        #(take 4 (concat % [0 0 0 0]))
        (fn [v] (remove-zeros (reduce #(let [l (last %1)]
                                 (if (= %2 l)
                                   (conj (pop %1) (+ l %2) 0)
                                   (conj %1 %2)))
                              [] v)))
        remove-zeros
        ) board))

(defn -main []
  (loop [board (insert-2 (partition 4 (repeat 16 0)))]
    (when (render board)
      (let [input ({"h" 4 "k" 3 "l" 2 "j" 1} (read-line))
            command (apply comp
                           (assoc (into [] (repeat 5 (fn [v] (map (fn [n] (map #(nth % n) v)) [3 2 1 0]))))
                                  input slide))]
        (-> board command insert-2 recur)))))

