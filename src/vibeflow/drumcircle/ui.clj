(ns vibeflow.drumcircle.ui
  (:require [quil.core :as q]))

(def colors {:green  [0x3A 0x60 0x29]
             :yellow [0xF6 0xCE 0x1F]
             :red    [0xA6 0x3A 0x2B] #_[0xE1 0x24 0x0B]
             :ochre  [0xD1 0x8D 0x2C]
             :brown  [0x84 0x4F 0x21]
             :blue   [0x6E 0xAB 0xB7]
             :light1 [0xEF 0xEF 0xE6]
             :light2 [0xDE 0xD8 0xC2]
             :light3 [0xA1 0xA5 0x86]
             })

(def background [0xE3 0xD2 0x85])

(def canvas-size 900)

(def pattern (atom {:pattern #{[0 0]
                               [0 4]
                               [0 8]
                               [0 12]
                               [1 2]
                               [2 3]
                               [3 9]}
                    :colors [:red :green :blue :yellow :ochre :brown]
                    :tick 3}))

(def TAU (* Math/PI 2))

(defn stroke-color [k]
  (let [[r g b] (get colors k)]
    (q/stroke r g b)))

(defn setup []
  (q/no-fill)
  (q/stroke-cap :square)
  (q/no-loop))

(let [rsize 300
      rx 250
      ry 250
      rings 3])

(defn arc-segment [{:keys [rx ry
                           ring
                           segment
                           arc-height
                           arc-width
                           arc-margin
                           arc-offset
                           stroke
                           ]}]
  (let [ring-size (- (* ring arc-height) stroke)]
    (q/arc rx ry
           ring-size ring-size
           (+ arc-offset (* segment arc-width) arc-margin)
           (+ arc-offset (* (inc segment) arc-width) (- arc-margin)))))

(defn draw [{:keys [pattern colors tick]}]
  (apply q/background background)
  (let [rsize (* canvas-size 2/3)
        rx (/ canvas-size 2)
        ry (/ canvas-size 2)
        rings 4
        margin 3
        segments 16
        arc-height (/ rsize rings)
        arc-width (/ TAU segments)
        arc-offset (- 0 (/ TAU 4) (/ TAU segments 2))
        stroke (- (/ arc-height 2) margin)
        circumference (* Math/PI rsize)
        arc-margin (* TAU (/ margin circumference))]
    (q/stroke-weight stroke)
    (doseq [ring (range rings)
            segment (range segments)]
      (if (get pattern [ring segment])
        (stroke-color (get colors ring))
        (stroke-color (cond
                        (= 0 (mod segment 4)) :light3
                        (even? segment) :light2
                        :else :light1)))
      (arc-segment {:rx rx
                    :ry ry
                    :ring (- rings ring)
                    :segment segment
                    :arc-height arc-height
                    :arc-width arc-width
                    :arc-margin arc-margin
                    :arc-offset arc-offset
                    :stroke stroke}))
    (stroke-color :blue)
    (arc-segment {:rx rx
                  :ry ry
                  :ring (inc rings)
                  :segment tick
                  :arc-height arc-height
                  :arc-width arc-width
                  :arc-margin arc-margin
                  :arc-offset arc-offset
                  :stroke stroke})))

(defn start-ui []
  (let [watch-key (keyword (str *ns*) (str (gensym "draw")))
        applet (q/sketch :title "Drumcircle"
                         :settings #(q/smooth 4)
                         :setup (fn []
                                  (setup))
                         :draw (fn []
                                 (draw @pattern))
                         :size [canvas-size canvas-size]
                         :on-close #(remove-watch pattern watch-key))]
    (add-watch pattern watch-key (fn [_ _ _ _]
                                   (quil.applet/with-applet applet
                                     (q/redraw))))
    applet))

(comment
  (start-ui)
  (swap! pattern update :tick (fn [t] (mod (inc t) 16)))
  (swap! pattern update :pattern conj [0 1]))
