(ns game2048.core
  (:gen-class))

(defn -main []
(let[t partition w repeat a assoc y apply
     i(fn[b]
        (let[f(into[](flatten b))
             z(seq(keep-indexed #(when(zero? %2)%1)f))]
          (cond (some #{2048}f)(pr"you win")
                (empty? z)(pr"game over")
                :e(t 4(a f(rand-nth z)2)))))
     r #(remove zero? %)]
  (loop[b(i(t 4(w 16 0)))]
    (when((fn[v](doseq[l(for[a v](y str(map #(format"%5d "%)a)))]
                  (println l "\n"))v)b)
      (-> b((y comp
               (a(into[](w 5(fn[v](map(fn[n](map #(nth % n)v))[3 2 1 0]))))
                           ({"h"4 "k"3 "l"2 "j"1}(read-line))
                           (partial map(comp #(take 4(concat %(w 4 0)))
                                             (fn[v](r(reduce #(let[l(last %1)]
                                                                (if(= %2 l)
                                                                  (conj(pop %1)(+ l %2)0)
                                                                  (conj %1 %2)))
                                                             []v)))r)))))i recur)))))

