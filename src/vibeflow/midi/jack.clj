(ns vibeflow.midi.jack
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
           (java.util EnumSet)))

(set! *warn-on-reflection* true)

;; The jack docs say to reuse a single instance... Might want to make this
;; thread-local or lock on it.
(def ^JackMidi$Event global-midi-event (JackMidi$Event.))

(defonce clients (atom {}))

(defprotocol Registry
  (register [this type key val])
  (unregister [this type key])
  (lookup [this type key]))

(defrecord JackClientWrapper [^JackClient client registry]
  Registry
  (register [_ t k val]
    (swap! registry assoc-in [t k] val))
  (unregister [_ t k]
    (swap! registry update t dissoc k))
  (lookup [_ t k]
    (get-in @registry [t k]))

  clojure.lang.IDeref
  (deref [_]
    @registry))

(defmacro registry-callback [client registry cb-type set-cb-type & methods]
  `(~(symbol (str "." set-cb-type))
    ~client
    (reify ~(symbol (str "Jack" cb-type "Callback"))
      ~@(for [[method k args] (partition 3 methods)]
          `(~method ~(into '[_] args)
            (reduce (fn [ok?# [k# cb#]]
                      (and ok?# (try
                                  (cb# ~@args)
                                  (catch Exception e#
                                    (println "Error in" '~method "callback" k#)
                                    (println e#)
                                    ok?#))))
                    true
                    (~k @~registry)))))))

(defmethod print-method JackClientWrapper [x ^java.io.Writer writer]
  (.append writer "#<JackClientWrapper>"))

;; (macroexpand-1 '(registry-callback client registry ClientRegistration
;;                                    clientRegistered :client-registered [client name]
;;                                    clientUnregistered :client-unregistered [client name]))


;; (require 'clojure.reflect)

;; (clojure.reflect/reflect
;;  JackBufferSizeCallback)

(defn make-client  [name]
  (let [status (EnumSet/noneOf JackStatus)
        client (.openClient (Jack/getInstance)
                            name
                            (EnumSet/of JackOptions/JackNoStartServer)
                            status)
        registry (atom {})]
    (when (seq status)
      (println "make-client:" (map str status)))
    (registry-callback client registry Process setProcessCallback
                       process :process [client frames])
    (registry-callback client registry BufferSize setBuffersizeCallback
                       buffersizeChanged :buffer-size-changed [client buffersize])
    (registry-callback client registry ClientRegistration setClientRegistrationCallback
                       clientRegistered :client-registered [client name]
                       clientUnregistered :client-unregistered [client name])
    (registry-callback client registry PortConnect setPortConnectCallback
                       portsConnected :ports-connected [client port-name-1 port-name-2]
                       portsDisconnected :ports-disconnected [client port-name-1 port-name-2])
    (registry-callback client registry PortRegistration setPortRegistrationCallback
                       portRegistered :port-registered [client port-name]
                       portUnregistered :port-unregistered [client port-name])
    (registry-callback client registry SampleRate setSampleRateCallback
                       sampleRateChanged :sample-rate-changed [client rate])
    (registry-callback client registry Shutdown onShutdown
                       clientShutdown :client-shutdown [client])

    ;; Immediately throws, not clear why
    ;; (registry-callback client registry Sync setSyncCallback
    ;;                    syncPosition :sync-position [client position state])

    ;; Not doing this because it would mean we become jack time master Also the
    ;; .setTimebaseCallback takes an extra arg (boolean conditional), so the
    ;; registry-callback macro doesn't work, see manually expanded version below
    ;;
    ;; (registry-callback client registry Timebase setTimebaseCallback
    ;;                    updatePosition :update-position [client state frame pos new-pos])
    ;;
    ;; (.setTimebaseCallback
    ;;  client
    ;;  (clojure.core/reify
    ;;    JackTimebaseCallback
    ;;    (updatePosition [_ client state frame pos new-pos]
    ;;      (reduce
    ;;       (fn [_ [k cb]]
    ;;         (try
    ;;           (cb client state frame pos new-pos)
    ;;           (catch java.lang.Exception e
    ;;             (println "Error in" 'updatePosition "callback" k)
    ;;             (println e))))
    ;;       nil
    ;;       (:update-position @registry))))
    ;;  true ;; conditional, only become master if there is no master yet
    ;;  )

    (registry-callback client registry GraphOrder setGraphOrderCallback
                       graphOrderChanged :graph-order-changed [client])

    (.activate client)
    (let [c (->JackClientWrapper client registry)]
      (swap! clients assoc name c)
      c)))

(defn client [name]
  (or (get @clients name) (make-client name)))

(defn midi-port [^JackClientWrapper client name type]
  (assert (keyword? name))
  (if-let [port (lookup client type name)]
    port
    (let [port (.registerPort ^JackClient (:client client)
                              (subs (str name) 1)
                              JackPortType/MIDI
                              (case type
                                :midi/input JackPortFlags/JackPortIsInput
                                :midi/output JackPortFlags/JackPortIsOutput))]
      (register client type name port)
      port)))

(defn midi-in-port [client name]
  (midi-port client name :midi/input))

(defn midi-out-port [client name]
  (midi-port client name :midi/output))

(defn read-midi-event [port idx]
  #_(locking global-midi-event)
  (JackMidi/eventGet global-midi-event port idx)
  (let [msg (byte-array (.size global-midi-event))]
    (.read global-midi-event msg)
    [msg (.time global-midi-event)]))

(defn read-midi-events [port]
  (doall
   (for [idx (range (JackMidi/getEventCount port))]
     (read-midi-event port idx))))

(defn write-midi-event [port time msg]
  (JackMidi/eventWrite port time msg (count msg)))

(defn filter-pipe [in out pred]
  (try
    (JackMidi/clearBuffer out)
    (dotimes [idx (JackMidi/getEventCount in)]
      (let [[msg time] (read-midi-event in idx)]
        (when (pred msg)
          (write-midi-event out time msg))))
    true
    (catch JackException e
      (println "JackException:" e)
      true)))
