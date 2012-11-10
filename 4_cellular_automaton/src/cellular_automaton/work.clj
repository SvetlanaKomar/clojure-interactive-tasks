(ns cellular-automaton.work
  (:use quil.core))



;;; Your task is to implement cellular automaton.
;;; The most famous example of cellular automaton is Conway's Game of Life.
;;; Unlike previous tasks now you have to implement visualization and bots. So you need to implement everything :)
;;; I suggest to use quil library for animation (it was used in all previous tasks): https://github.com/quil/quil
;;; But of course you can use whatever you want.
;;; Keep in mind that is should be simple to run your simulator with different automata (Game of Life is only 1 example).


;;; Implement and run Brian's Brain automaton in your simulator: http://en.wikipedia.org/wiki/Brian%27s_Brain


;;; Implement Wireworld automaton: http://en.wikipedia.org/wiki/Wireworld


;;; Add Wireworld implementation to Rosetta Code (it's not present here yet): http://rosettacode.org/wiki/Wireworld


;;; Implement Von Neumann cellular automaton: http://en.wikipedia.org/wiki/Von_Neumann_cellular_automata


;;; Implement Langton's ant: http://en.wikipedia.org/wiki/Langton%27s_ant


;;; Add ability to change cells' states by mouse click, to restart and pause simulation.


(def w 800)

(def h 600)

(def cell-size 10)

(def board-width (/ w cell-size))

(def board-height (/ h cell-size))

(def new(atom {}))

(def do-draw(atom true))

(def draw-net(atom true))

(def init-cells {[0 2] :alive [1 0] :alive [1 2] :alive [2 1] :alive [2 2 ]:alive})

(def stats {:alive nil, nil :alive})

(defn rules [c-state states]
  (let [alive (count (filter #( = :alive %) states))]
    (cond (= alive 2) c-state
          (= alive 3) :alive
          :else nil)))

(defn getcol [state] 
  (cond (= state :alive) [128 30 80]
        :else [0 0 0]))

(def c1(atom {:in-c init-cells, :update-fn rules, :color-fn getcol, :states-m stats}))

(defn to-real-coords [cell]
  (map #(* cell-size %) cell))

(defn norm-w [x]
  (mod (+ x board-width) board-width))

(defn norm-h [x]
  (mod (+ x board-height) board-height))

(defn draw-cell [cell [r g b]]
  (fill r g b )
  (let [[real-x real-y] (to-real-coords cell)]
    (rect real-x real-y cell-size cell-size)))

(defn  get-state [cell]
  ((@c1 :in-c) cell))

(defn n-states [cell]
  (let [x (first cell)
        y (last cell)]
   ( vector (get-state [(norm-w (- x 1)) (norm-h (- y 1))])
      (get-state [(norm-w (- x 1))  y ])
      (get-state [(norm-w (- x 1)) (norm-h (+ y 1))])
      (get-state [ x  (norm-h (- y 1))])
      (get-state [ x  (norm-h (+ y 1))])
      (get-state [(norm-w (+ x 1)) (norm-h (- y 1))])
      (get-state [(norm-w (+ x 1))  y ])
      (get-state [(norm-w (+ x 1)) (norm-h(+ y 1))]))))

(defn update-cells []
  (reset! new {})

    (doseq [x (range 0 board-width)]
      (doseq [y (range 0 board-height)]
        (let [state (get-state [x y])
              neib-s (n-states [x y])
              new-s ((@c1 :update-fn) state neib-s)]
          (if (not (= new-s nil)) 
             (swap! new assoc [x y] new-s)
             ))))
    (swap! c1 #(assoc-in % [:in-c] @new))
  )

(defn draw-coord-net[]
  (doseq [x (range 0 board-width)]
    (line (* x cell-size) 0 (* x cell-size) h))
  (doseq [y (range 0 board-height)]
    (line 0 (* y cell-size) w (* y cell-size) ))

  )

(defn draw-cells []
 (let [cells (@c1 :in-c)
       def-col (@c1 :color-fn)]
   (background 200)
   (when @draw-net
    (draw-coord-net)
     )
   (doseq [[c s]  cells]
     (draw-cell c (def-col s))))


  )

(defn proc-key []
  (cond (= (raw-key) \space) (swap! do-draw not)
        (= (raw-key) \c) (swap! c1 #(assoc-in % [:in-c] init-cells))
        (= (raw-key) \n) (swap! draw-net not))
  (draw-cells)
  )

(defn mouse-clicked []
  (reset! new (@c1 :in-c))
  (let [x (quot (mouse-x) cell-size)
        y (quot (mouse-y) cell-size)
        state (get-state [x y])
        new-st ((@c1 :states-m) state)]
    (if (= new-st nil) (swap! new #(dissoc % [x y]))
      (swap! new #(assoc % [x y] new-st)))
  (swap! c1 #(assoc-in % [:in-c] @new)) )
  (draw-cells)
  )

(defn setup []
  (smooth)                          ;;Turn on anti-aliasing
  (set-state!
 ;  :to-draw (atom true)
   :cells (atom init-cells)
  )
  (frame-rate 5)                    ;;Set framerate to n FPS
  (background 200))                 ;;Set the background colour to

(defn draw []
 (when @do-draw 
   (update-cells)
   (draw-cells )
   )
				)         

(defn run [] (sketch                  ;;Define a new sketch named example
  :title "Cellular automat"  		;;Set the title of the sketch
  :setup setup                      ;;Specify the setup fn
  :draw draw                        ;;Specify the draw fn
  :key-pressed proc-key
  :mouse-clicked mouse-clicked
  :size [w h]))                 	    ;size