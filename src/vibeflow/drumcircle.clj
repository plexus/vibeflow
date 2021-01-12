(ns vibeflow.drumcircle
  (:import (org.jaudiolibs.jnajack Jack
                                   JackClient
                                   JackException
                                   JackMidi
                                   JackMidi$Event
                                   JackOptions
                                   JackPosition
                                   JackPortFlags
                                   JackPortType
                                   JackProcessCallback
                                   JackShutdownCallback
                                   JackBufferSizeCallback
                                   JackPortConnectCallback
                                   JackPortRegistrationCallback
                                   JackSampleRateCallback
                                   JackShutdownCallback
                                   JackSyncCallback
                                   JackClientRegistrationCallback
                                   JackTimebaseCallback
                                   JackTransportState
                                   JackGraphOrderCallback
                                   JackStatus)
           (java.util EnumSet))
  (:require [vibeflow.midi.jack :as jack]
            [vibeflow.midi.core :as midi]))



(def state (atom {:pattern #{[0 :kick]
                             [1/4 :snare]
                             [2/4 :kick]
                             [5/8 :snare]
                             [3/4 :kick]}
                  :instruments {:kick [0 40 127]
                                :snare [0 53 127]
                                :hi-hat [0 71 127]}
                  :bar 0
                  :beat 0
                  :tick 0
                  :ticks-per-beat 1920
                  :frame 0
                  :frame-rate 44100
                  :playing? false
                  :beats-per-minute 120
                  :beats-per-bar 4 ;; Together these
                  :beat-type 4     ;; form the time signature (4/4)
                  }))

(defn note-in-range [^double n ^double start ^double end]
  (or (<= start n end)
      (and (< 1 end)
           (or (<= start n)
               (<= n (- end 1))))))

(defn note-offset [^double n ^double start]
  (if (< n start)
    (- (+ n 1) start)
    (- n start)))

(defn write-cycle-beats [client port cycle-frames
                         {:keys [bar beat tick ticks-per-beat
                                 frame frame-rate
                                 beats-per-minute beats-per-bar beat-type
                                 playing?
                                 pattern instruments]}]
  (let [resolution 2
        frames-per-tick (/ frame-rate ticks-per-beat)
        ticks-per-cycle (/ (* ticks-per-beat beats-per-minute cycle-frames)
                           60
                           frame-rate)
        ;; these are "normalized" as a fraction of beat-type beats e.g.
        ;; cycle-start 0.75 with a 4/4 signature means we are on the 3rd beat
        cycle-start (+ (/ (+ (- beat 1) (/ tick ticks-per-beat)) beat-type))
        cycle-end (+ (/ (+ (- beat 1) (/ (+ tick ticks-per-cycle) ticks-per-beat)) beat-type))
        note-type (Math/round (* beat-type resolution))
        frames-per-bar (* (/ beats-per-minute 60) frame-rate)]

    (doseq [[fraction inst] pattern
            :when (note-in-range fraction cycle-start cycle-end)]
      (let [[chan note velocity] (get instruments inst)
            offset (* frames-per-bar (note-offset fraction cycle-start))]
        (jack/write-midi-event port
                               (long offset)
                               (midi/message chan :note-on note velocity))))))

(defn assoc-pos [state ^JackPosition pos]
  (assoc state
         :bar (.getBar pos)
         :beat (.getBeat pos)
         :tick (.getTick pos)
         :ticks-per-beat (.getTicksPerBeat pos)
         :frame (.getFrame pos)
         :frame-rate (.getFrameRate pos)
         :beats-per-minute (.getBeatsPerMinute pos)
         :beats-per-bar (.getBeatsPerBar pos)
         :beat-type (.getBeatType pos)))

(defn start-sequencer []
  (let [client (jack/client "vibeflow")
        out (jack/midi-out-port client :drumcircle)
        pos (JackPosition.)]
    (jack/register client
                   :process
                   ::my-process
                   (fn [client cycle-frames]
                     (let [tstate (.transportQuery client pos)
                           playing? (= tstate JackTransportState/JackTransportRolling)
                           new-state (swap! state (fn [state]
                                                    (assoc (assoc-pos state pos) :playing? playing?)))]
                       (JackMidi/clearBuffer out)

                       (when playing?
                         (write-cycle-beats client out cycle-frames new-state)))
                     true))))

(comment
  (start-sequencer)
  (swap! state assoc
         :pattern
         #{[0 :kick]
           [1/4 :snare]
           [5/8 :kick]
           [3/4 :snare]

           [0 :hi-hat]
           [1/8 :hi-hat]
           [1/4 :hi-hat]
           [3/8 :hi-hat]
           [1/2 :hi-hat]
           [5/8 :hi-hat]
           [3/4 :hi-hat]
           [7/8 :hi-hat]}))
