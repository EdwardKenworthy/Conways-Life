(ns conways-life.core
  (:use seesaw.core
        seesaw.graphics
        seesaw.color))

(native!)

;
; Conway's Life
;
(defn neighbours
  "Returns the co-ords of the neighbours of the given co-ords."
	[[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
  	[(+ dx x) (+ dy y)]))

; (neighbours [10 21])

(defn step
	"The core of the Life simulation. Yields the next state of the world"
  [cells]
  (set (for [[loc n] (frequencies (mapcat neighbours cells))
  						:when (or (= n 3) (and (= n 2) (cells loc)))]
              loc)))

; (step #{[1 2] [1 1] [2 2]})

(def starting-live-cells #{})
(def current-live-cells (atom starting-live-cells))

;
; GUI
;

(defn paint [c g]
  "Paints the currently live cells to the window."
  (let [w (.getWidth c)
        h (.getHeight c)
        cell-width 10
        cell-height 10
        black (color 255 255 255 255)]
    (doseq [{:as cell} @current-live-cells]
      (let [cell-x (* cell-width (cell 0))
            cell-y (* cell-height (cell 1))]
        (when (and (> cell-x 0) (> cell-x 0)
                   (< cell-x w) (< cell-y h))
          (draw g
  	        (ellipse  (mod cell-x w)
    	  	            (mod cell-y h) cell-width cell-height)
              (style :background black)))))))

(defn cells-changed
  "called when current-live-cells changes"
  [fr _ _ _ _]
  (repaint! (select fr [:#canvas])))

;
; Functions to add special shapes
;

; be nice to get rid of mvr
(defn- mvr
  "adjusts x and y by dx and dy respectively"
  [[dx dy] [x y]]
  [(+ dx x) (+ dy y)])

(defn move-shape [shape offset]
    (map (partial mvr offset) shape))

;; Glider

(def shape-glider [[2 0][2 1][2 2][1 2][0 1]])

(defn add-glider
  [offset cells]
  (into cells (move-shape shape-glider offset)))

;; Blinker

(def shape-blinker [[5 5][5 6][5 7]])

(defn add-blinker
  [offset cells]
  (into cells (move-shape shape-blinker offset)))

;; LWSS

(def shape-lwss
  [[0 1][0 2][0 3]
  [1 0][1 3]
  [2 3]
  [3 3]
  [4 0][4 2]])

(defn add-lwss
  "Adds a light weight star ship. Currently immediately flies off to the left!"
  [offset cells]
  (into cells (move-shape shape-lwss offset)))

;; Gosper gun

(def shape-gosper-gun
  [[0 4][0 5]
  [1 4][1 5]
  [10 4][10 5][10 6]
  [11 3][11 7]
  [12 2][12 8]
  [13 2][13 8]
  [14 5]
  [15 3][15 7]
  [16 4][16 5][16 6]
  [17 5]
  [20 2][20 3][20 4]
  [21 2][21 3][21 4]
  [22 1][22 5]
  [24 0][24 1][24 5][24 6]
  [34 2][34 3]
  [35 2][35 3]])

(defn add-gosper-gun
  [offset cells]
  (into cells (move-shape shape-gosper-gun offset)))

(defn gosper-gun [_]
  (swap! current-live-cells (partial add-gosper-gun [0 0])))

(defn lwss [_]
  (swap! current-live-cells (partial add-lwss [0 0])))

(defn blinker [_]
  (swap! current-live-cells (partial add-blinker [0 0])))

(defn glider [_]
  (swap! current-live-cells (partial add-glider [0 0])))

(defn reset [_]
  (swap! current-live-cells (fn [_] #{})))

(defn a-exit  [_] (System/exit 0))

(def menus
  (let [a-reset (action :handler reset :name "Reset" :tip "Reset the universe.")
        a-new-ggun (action :handler gosper-gun :name "Gosper Gun" :tip "Add a Gosper Gun")

        a-new-blinker (action :handler blinker :name "Blinker" :tip "Add a Blinker")
        a-new-glider (action :handler glider :name "Glider" :tip "Add a Glider")

        a-new-lwss (action :handler lwss :name "Lightweight Spaceship" :tip "Add a lightweight space ship")
        a-exit (action :handler a-exit :name "Exit" :tip "Exit the editor.")]
       (menubar
        :items [(menu :text "File" :items [a-exit])
                (menu :text "Life" :items [a-new-glider a-new-blinker a-new-ggun a-new-lwss a-reset])])))

(def main-panel
  ; Create the canvas with initial nil paint function, i.e. just canvas
  ; will be filled with it's background color and that's it.
  (border-panel :hgap 5 :vgap 5 :border 5
    :center (canvas :id :canvas :background "#BBBBDD" :paint paint)))

(defn life []
  (let
    [f (frame :title "Conway's Life"
              :width 500 :height 300
              :content main-panel
              :on-close :exit
              :menubar menus)
     evolve-delay 100] ; delay in milliseconds
    (add-watch current-live-cells :log (partial cells-changed f))
    (defn evolve []
      (Thread/sleep evolve-delay)
      (swap! current-live-cells step)
      (recur))
    (future (evolve))

  (show! f)))

(defn -main [& args]
  (life))

(life)
