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

(defn note-in-range [^double n ^double start ^double end]
  (or (<= start n end)
      (and (< 1 end)
           (or (<= start n)
               (<= n (- end 1))))))

(defn note-offset [^double n ^double start]
  (if (< n start)
    (- (+ n 1) start)
    (- n start)))

(defn write-cycle-beats [client port cycle-frames beats ^JackPosition pos]
  (let [beat (.getBeat pos)
        tick (.getTick pos)
        bpm  (.getBeatsPerMinute pos)   ;; 4 /
        bpb  (.getBeatsPerBar pos)   ;; 4 /
        beat-type (.getBeatType pos) ;;     4
        resolution 2 ;; if beat-type == 4 then resolution 2 => 8th notes
        ticks-per-beat  (.getTicksPerBeat pos)
        frames-per-second (.getFrameRate pos)
        frames-per-tick (/ frames-per-second ticks-per-beat)
        ticks-per-cycle (/ (* ticks-per-beat bpm cycle-frames)
                           60
                           frames-per-second)
        ;; these are "normalized" as a fraction of beat-type beats e.g.
        ;; cycle-start 0.75 with a 4/4 signature means we are on the 3rd beat
        cycle-start (+ (/ (+ (- beat 1) (/ tick ticks-per-beat)) beat-type))
        cycle-end (+ (/ (+ (- beat 1) (/ (+ tick ticks-per-cycle) ticks-per-beat)) beat-type))
        note-type (Math/round (* beat-type resolution))
        frames-per-bar (* (/ bpm 60) frames-per-second)]
    (doseq [i (range note-type)]
      (when-let [note (beats (/ i note-type))]
        (when (note-in-range note cycle-start cycle-end)
          (let [offset (* frames-per-bar (note-offset note cycle-start))]
            (jack/write-midi-event port (long offset) (byte-array [-112 64 127]))
            (jack/write-midi-event port (long offset) (byte-array [-128 64 0]))))))))

(midi/message 0 :note-on 64 0)
(comment
  (let [ccc (jack/client "vibeflow")
        out (jack/midi-out-port ccc :drumcircle)
        pos (JackPosition.)]
    (jack/register ccc
                   :process
                   ::my-process
                   (fn [client cycle-frames]
                     (let [tstate (.transportQuery client pos)
                           beats #{0 1/4 2/4 3/4 5/8}]
                       (JackMidi/clearBuffer out)
                       (when (= tstate JackTransportState/JackTransportRolling)
                         (write-cycle-beats ccc out cycle-frames beats pos)))
                     true)))

  (bean pos)
  (str tstate)

  )
