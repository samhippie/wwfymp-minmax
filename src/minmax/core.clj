(ns minmax.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn  new-game
  [starting-player]
  {:player starting-player
   :pieces {0 {:color nil :edges []}}})

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

(defn remove-piece
  [game id]
  (let [edges (get-in game [:pieces id :edges])
        game (reduce #(remove-edge %1 id %2) game edges)
        game (update-in game [:pieces] dissoc id)
        floaters (remove (partial grounded? game) (keys (:pieces game)))]
    (reduce remove-piece game floaters)))

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

(defn minimax
  ([game] (minimax game nil nil))
  ([game score winner]
   (println score winner game)
   (let [player (:player game)
         moves (get-moves game player)]
     (cond
       ;we have no moves, and we haven't already won
       ;continue and let the other play see how many moves they can make
       (or (= winner (other-player player)) (and (nil? winner) (empty? moves)))
       (minimax (update game :player other-player)
                (or score 0)
                (or winner (other-player player)))

       ;we have already won and can make no more moves
       (empty? moves)
       [winner score]

       ;we still have moves to make, so make all moves and return best value
       true
       (pick-result player
                    (map #(->
                           game
                           (remove-piece %)
                           (update :player other-player)
                           (minimax (and winner (inc-score winner score)) winner))
                         moves))))))

(defn test-game
  [game]
  {:blue (minimax (assoc game :player :blue))
   :red (minimax (assoc game :player :red))})

(def my-game
  (->
   (new-game :blue)
   (add-piece :blue [0])
   (add-piece :red [1])))

(defn single-game
  [color]
  (->
   (new-game :blue)
   (add-piece color [0])))