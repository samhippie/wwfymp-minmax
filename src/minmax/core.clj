(ns minmax.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn  new-game
  []
  {:pieces {0 {:color nil :edges []}}})

(defn max-id
  [game]
  (apply max (filter number? (keys (:pieces game)))))

(defn add-edge
  [game to from]
  (->
   game
   (update-in [:pieces to :edges] conj from)
   (update-in [:pieces from :edges] conj to)))

(defn add-piece
  [game color neighbors]
  (let [max-id (max-id game)
        new-id (inc max-id)
        game (assoc-in game [:pieces new-id] {:color color :edges []})]
    (reduce #(add-edge %1 %2 new-id) game neighbors)))

(defn remove-edge
  [game to from]
  (->
   game
   (update-in [:pieces to :edges] (fn [edges] (remove #(= from %) edges)))
   (update-in [:pieces from :edges] (fn [edges] (remove #(= to %) edges)))))

(defn grounded?
  ([game id] (grounded? game id [id] #{}))
  ([game id open closed]
   (cond
     (some (partial = 0) open) true
     (empty? open) false
     true (let [new-open (flatten (map #(get-in game [:pieces % :edges]) open))
                new-open (remove #(contains? closed %) new-open)
                new-closed (into closed open)]
            (recur game id new-open new-closed)))))

(defn remove-piece
  [game id]
  (let [edges (get-in game [:pieces id :edges])
        game (reduce #(remove-edge %1 id %2) game edges)
        game (update-in game [:pieces] dissoc id)
        floaters (remove (partial grounded? game) (keys (:pieces game)))]
    (reduce remove-piece game floaters)))

(defn get-moves
  [game player]
  (->>
   game
   (:pieces)
   (filter #(= (:color (second %)) player))
   (map first)))

(defn other-player
  [player]
  (case player
    :red :blue
    :blue :red))

;is there a nicer way to do this?
(defn pick-result
  [player results]
  (let [comp-fn (case player
                  :red <
                  :blue >)]
    (first (sort-by (fn [[a-player a-score]]
                      (if (= a-player player) -1 1))
                    (sort (fn [[a-player a-score] [b-player b-score]]
                            (comp-fn a-score b-score))
                          results)))))

(defn inc-score
  [player score]
  (case player
    :red (dec score)
    :blue (inc score)))

(defn dbg
  [value]
  (println value)
  value)

(defn unchunk [s]
  (when (seq s)
    (lazy-seq
     (cons (first s)
           (unchunk (next s))))))

(defn minimax
  [games player]
  (let [moves (map-indexed (fn [idx game] [idx (get-moves game player)]) games)
        moves (apply concat (map (fn [[idx moves]] (map #(vector idx %) moves)) moves))]
    (if (empty? moves)
       ;we have no moves, we lose
      (other-player player)
       ;we still have moves to make, so make all moves and return best value
      (or (first (unchunk
                  (filter
                   #(= player %)
                   (map (fn [[idx move]]
                          (->
                           games
                           (update idx #(remove-piece % move))
                           (minimax (other-player player))))
                        moves))))
          (other-player player)))))

(defn test-games
  [games]
  {:blue (minimax games :blue)
   :red (minimax games :red)})

(def half-game
  (->
   (new-game)
   (add-piece :blue [0])
   (add-piece :red [1])))

(defn single-game
  [color]
  (->
   (new-game)
   (add-piece color [0])))

(def single-red (single-game :red))
(def single-blue (single-game :blue))

(minimax [single-red] :red)
(minimax [single-red] :blue)

;demonstrates that the half game is a 1/2 turn advantage to blue
;as 1/2 + 1/2 - 1 = 0
(test-games [single-red half-game half-game])

(def game-7a
  (->
   (new-game)
   (add-piece :blue [0])
   (add-piece :blue [1])
   (add-piece :red [2])))

(defn chain-game
  [colors]
  (let [colors (map-indexed vector colors)]
    (reduce (fn [game [idx color]] (add-piece game color [idx]))
            (new-game)
            colors)))

(test-games
 [(chain-game [:blue :red :red]) ; 1/4 blue
  (chain-game [:blue :red :red]) ; 1/4 blue
  (chain-game [:red :blue])]) ; 1/2 red

(test-games
 [(chain-game [:blue :red :blue]) ; ? blue
  (chain-game [:red :blue :blue]) ; 1/4 red, still blue advantage
  (chain-game [:red :blue])]) ; 1/2 red, makes it a zero game
  ;so ? is 3/4 blue

(defn stack-games
  [games num-games num-blue num-red]
  (apply vector
         (concat (apply concat (repeat num-games games))
                 (repeat num-red single-red)
                 (repeat num-blue single-blue))))

(defn test-results
  [{bw :blue
    rw :red}]
  (cond
    (and (= bw :blue) (= rw :red)) :star
    (and (= bw :red) (= rw :blue)) :0
    true rw))

(defn eval-search
  ([games]
   (case (test-results (test-games games))
     :star :star
     :0 :0
    ;we'll go ahead and give ourselves an opposing game to evaluate
     :red (eval-search {} games :blue 1 1)
     :blue (eval-search {} games :red 1 1)))
  ; opp-color is the opposing color we're increasing
  ; num-games is how many copies of the given game we've made
  ; num-color is the copies of the opposing color we've increased
  ([result-cache games opp-color num-games num-color]
   (println "start" opp-color num-games num-color result-cache)
   (if (> (+ num-color num-games) 7)
     "oops"
     (let [cache-key (/ num-color num-games)
           score (or (get result-cache cache-key)
                     (let [num-blue (if (= opp-color :blue) num-color 0)
                           num-red (if (= opp-color :red) num-color 0)
                           stack (stack-games games num-games num-blue num-red)
                           game-results (test-games stack)
                           score (test-results game-results)]
                       score))
           new-cache (assoc result-cache cache-key score)]
       (println "score" score)
       (println "-----------------")
       (condp = score
         :star :star
         :0 (* (/ num-color num-games) (if (= opp-color :red) 1 -1))
         opp-color (recur new-cache games opp-color (inc num-games) num-color)
         (recur new-cache games opp-color num-games (inc num-color)))))))

(eval-search [single-blue])

(eval-search [(chain-game [:blue :red :blue])])
