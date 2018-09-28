(ns cljs.convert
  (:require ["midi-file-parser" :as midi-file-parser]
            ["prompt-confirm" :as Confirm]
            ["argparse" :as args]
            ["path" :as path]
            ["fs" :as fs]))


(def argument-parser
  (new (.-ArgumentParser args)
       #js {:version     "1.0.0-alpha"
            :addHelp     "true"
            :description "midi2score"}))

(defn add-argument [argument config]
  (.addArgument argument-parser
                argument config))

(add-argument "file"
              #js {:help "A midifile (.mid/.midi)"
                   :type "string"})

(add-argument #js ["-o" "--out"]
              #js {:help "Output file"})

(add-argument #js ["-t" "--tempo"]
              #js {:help    "Tempo in bpm"
                   :default 60})

(add-argument #js ["-f" "--force"]
              #js {:help    "Overwrite a score file if it exists"
                   :default false})

(add-argument "--edn"
              #js {:help "Exports .edn dataformat (clj/cljs) instead of midi"})


(defn new-empty-string [length]
  (apply str (take length (cycle " "))))

(defn edn2sco [edn]
  (let [max-p2 (apply max (map (comp count str :p2) edn))
        max-p3 (apply max (map (comp count str :dur) edn))
        max-p4 (apply max (map (comp count str :vel) edn))]
    (reduce (fn [out-str data]
              (let [cur-p2-len (- max-p2 ((comp count str :p2) data))
                    cur-p3-len (- max-p3 ((comp count str :dur) data))
                    cur-p4-len (- max-p4 ((comp count str :vel) data))]
                (str out-str "\n"
                     "p" (inc (:channel data)) " "
                     (:p2 data) (new-empty-string cur-p2-len) " "
                     (:dur data) (new-empty-string cur-p3-len) " "
                     (:vel data) (new-empty-string cur-p4-len) " "
                     (:midinn data))))
            "" edn)))

(defn parse-track [events tempo tick-resolution]
  (loop [[event & events] events
         last-time        0
         cur-notes-on     {}
         edn-out          []]
    (if (empty? event)
      edn-out
      (let [event-type (:subtype event)
            cur-time   (+ last-time (* tempo (/ (or (:deltaTime event) 0)
                                                tick-resolution)))]
        (cond
          (= "noteOn" event-type)
          (recur events
                 cur-time
                 (assoc cur-notes-on (:noteNumber event)
                        {:p2 cur-time :vel (:velocity event)})
                 edn-out)
          (= "noteOff" event-type)
          (recur events
                 cur-time
                 (dissoc cur-notes-on (:noteNumber event))
                 (let [matching-event (get cur-notes-on (:noteNumber event))]
                   (conj edn-out {:p2      (:p2 matching-event)
                                  :midinn  (:noteNumber event)
                                  :vel     (:vel matching-event)
                                  :dur     (- cur-time (:p2 matching-event))
                                  :channel (:channel matching-event)})))
          :else
          (recur events last-time cur-notes-on edn-out))))))

(defn main [& args]
  (let [args            (.parseArgs argument-parser)
        abs-path        (path/normalize
                         (path/resolve
                          (.-file args)))
        base-filename   (.-name (path/parse (.-file args)))
        tempo           (/ 60 (.-tempo args))
        midi-file       (fs/readFileSync abs-path "binary")
        raw-midi-json   (midi-file-parser midi-file)
        raw-edn         (js->clj raw-midi-json :keywordize-keys true)
        tick-resolution (get-in raw-edn [:header :ticksPerBeat])
        raw-tracks      (:tracks raw-edn)
        tempo-track     (first raw-tracks)
        first-track     (first (rest raw-tracks))
        first-track-edn (parse-track first-track tempo tick-resolution)
        first-track-sco (edn2sco first-track-edn)
        spit-output     #(if (.-edn args)
                           (fs/writeFileSync (str base-filename ".edn") first-track-edn)
                           (fs/writeFileSync (str base-filename ".sco") first-track-sco))]
    (if (and (not (.-edn args)) (not (.-force args))
             (fs/existsSync (str base-filename ".sco")))
      (-> (new Confirm (str "Do you want to overwrive " (str base-filename ".sco") "?"))
          (.run)
          (.then (fn [ok?]
                   (when ok? (spit-output))
                   (.exit js/process 0))))
      (do
        (spit-output)
        (.exit js/process 0)))))