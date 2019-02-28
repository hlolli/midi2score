(ns cljs.cli
  (:require
   [cljs.convert :refer [edn2sco parse-midi]]
   ["midi-file-parser" :as midi-file-parser]
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
              #js {:help         "Tempo in bpm"
                   :defaultValue 60})

(add-argument #js ["-f" "--force"]
              #js {:help         "Overwrite a score file if it exists"
                   :defaultValue false})

(add-argument "--edn"
              #js {:help "Exports .edn dataformat (clj/cljs) instead of midi"})


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
        first-track-edn (parse-midi first-track tempo tick-resolution)
        first-track-sco (edn2sco first-track-edn nil)
        spit-output     #(if (.-edn args)
                           (fs/writeFileSync (or (.-out args)
                                                 (str base-filename ".edn")) first-track-edn)
                           (fs/writeFileSync (or (.-out args)
                                                 (str base-filename ".sco")) first-track-sco))]
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
