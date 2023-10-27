(ns time-master
  (:require
   [clojure.string :as str]
   [vibeflow.midi.jack :as jack])
  (:import
   (org.jaudiolibs.jnajack JackPosition)))

(set! *warn-on-reflection* true)

(defn calculate-timings [frame-rate frame {:keys [bpm
                                                  beats-per-bar
                                                  beat-type
                                                  ticks-per-beat]
                                           :as timing}]
  (let [frames-per-beat (/ frame-rate (/ bpm 60))
        frames-per-tick (/ frames-per-beat ticks-per-beat)
        ticks-per-bar (* ticks-per-beat beats-per-bar)
        ticks-total (/ frame frames-per-tick)
        tick (mod (/ frame frames-per-tick) ticks-per-beat)
        beat (mod (/ frame frames-per-beat) beats-per-bar)
        bar (/ frame frames-per-beat beats-per-bar)
        bar-start-tick (- ticks-total (mod ticks-total ticks-per-bar))]
    ;; coerce to the types that JackPosition uses
    {:bar (int bar)
     :beat (int beat)
     :tick (int tick)
     :bar-start-tick (double bar-start-tick)
     :beats-per-bar (float beats-per-bar)
     :beat-type (float beat-type)
     :ticks-per-beat (double ticks-per-beat)
     :beats-per-minute (double bpm)}))

(defn populate-jack-pos [^JackPosition pos {:keys [^int bar
                                                   ^int beat
                                                   ^int tick
                                                   ^double bar-start-tick
                                                   ^float beats-per-bar
                                                   ^float beat-type
                                                   ^double ticks-per-beat
                                                   ^double beats-per-minute]}]
  (doto pos
    (.setBar bar)
    (.setBeat beat)
    (.setTick tick)
    (.setBarStartTick bar-start-tick)
    (.setBeatsPerBar beats-per-bar)
    (.setBeatType beat-type)
    (.setTicksPerBeat ticks-per-beat)
    (.setBeatsPerMinute beats-per-minute)))

(def timing (atom {:bpm 120
                   :beats-per-bar 4
                   :beat-type 4
                   :ticks-per-beat 1920}))

#_(swap! timing assoc :ticks-per-beat 16)

(def client (jack/make-time-master (jack/client "time-master")))

(jack/register
 client
 :update-position
 ::time-master
 (fn [client state nframes ^JackPosition pos new-pos?]
   (populate-jack-pos pos (calculate-timings (.getFrameRate pos) (.getFrame pos) @timing))))

(comment
  (jack/start-transport! client)
  (jack/stop-transport! client)
  (let [pos (JackPosition.)]
    (.transportQuery (:client client) pos)
    (bean pos))

  (.transportLocate (:client client) 0)
  )
