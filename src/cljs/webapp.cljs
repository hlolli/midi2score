(ns cljs.webapp
  (:require [reagent.core :as r]
            [clojure.string :as string]
            [cljs.convert :refer [parse-midi edn2sco]]
            [cljs.midi-standard :refer [program-change-names]]
            ["react-clipboard.js" :default Clipboard]
            ;; ["react-alert" :refer [withAlert]]
            ["simple-react-alert" :default Alert :refer [openAlert]]
            ["midi-file-parser" :as midi-file-parser]
            ["react-dropzone" :as Dropzone]))

(defn header []
  [:header [:h1 "Midi to Csound Score"]])

(def app-state (r/atom {:drop-hover false :files []
                        :error      []    :score ""}))

(defn drop-overlay []
  [:div {:class-name "drop-overlay"}
   [:div {:class-name "drop-overlay-message"}
    [:h5 "Drop .mid or .midi file here"]]])

(defn score-computer [files]
  (r/create-class
   {:component-did-update
    (fn [this prev next]
      (let [files    (:files @app-state)
            raw-edns (mapv :raw-edn files)]
        (swap! app-state assoc :score "")
        (doseq [file files]
          (let [is-first? (empty? (:score @app-state))]
            (swap! app-state update :score str (str (if is-first?  "" "\n\n")
                                                    ";; File: " (:filename file)))
            (doseq [{:keys [track-channel track-events track-name track-program] :as track} (:tracks file)]
              (let [global-bpm   (get-in file [:metadata :global-bpm])
                    tick-res     (:tick-resolution file)
                    edn-score    (parse-midi track-events (or global-bpm 60) tick-res)
                    csnd-score   (edn2sco edn-score (:instrument-number file))
                    program-name (get program-change-names track-program)
                    comment      (str "\n\n;; channel: " track-channel
                                      (when-not (= 0 (:midi-format file))
                                        "\n;; track name: " track-name
                                        "\n;; program: " track-program
                                        (when program-name (str " (" program-name ")"))) "\n")]
                (swap! app-state update :score str (str comment csnd-score))))))))
    :reagent-render (fn [this] [:span])}))

(defn score-section [score-cursor]
  (r/create-class
   {:reagent-render
    (fn [this files]
      (let [lines (-> @score-cursor
                      (string/trim)
                      (string/split "\n"))]
        (into [:code
               [:div {:class-name "score-line-container score-line-container-dummy"}
                [:span {:class-name "score-line-number score-line-number-dummy"}]
                [:p {:class-name "score-line-code"}]]]
              (map-indexed
               (fn [index line]
                 (let [line-type (case (first line)
                                   ";" :comment
                                   "i" :instr-event
                                   :newline)]
                   [:div {:class-name "score-line-container"}
                    [:span {:class-name "score-line-number"} (str (inc index))]
                    (case line-type
                      :comment     [:em {:class-name "score-line-code score-line-comment"} line]
                      :instr-event [:p {:class-name "score-line-code"}
                                    [:span {:style {:color "aquamarine" :font-weight 900}} "i "]
                                    (subs line 1)]
                      :newline     [:p {:class-name "score-line-code"} line])])) lines))))}))

(defn extract-metadata [{:keys [tracks] :as midi-edn}]
  (let [global-meta         (first tracks)
        global-timesig-meta (first (filter #(= (:subtype %) "timeSignature") global-meta))
        global-bpm          (if global-timesig-meta (:metronome global-timesig-meta) 60)
        tracks              (rest tracks)]
    [{:global-bpm global-bpm
      ;; TODO
      :global-key 0}
     (reduce
      (fn [ret track]
        (conj ret
              (let [track-name-meta      (first (filter #(= (:subtype %) "trackName") track))
                    track-name           (when track-name-meta
                                           (:text track-name-meta)
                                           #_(str "track" (inc (count ret))))
                    track-program-change (first (filter #(= (:subtype %) "programChange") track))
                    track-program        (if track-program-change (:programNumber track-program-change) 0)
                    track-cc             (vec (filter #(= (:subtype %) "controller") track))
                    track-events         (vec (filter #(or (= (:subtype %) "noteOff")
                                                           (= (:subtype %) "noteOn")) track))
                    track-channel        (:channel (first track-events))]
                {:track-name    track-name    :track-program     track-program
                 :track-cc      track-cc      :track-events      track-events
                 :track-channel track-channel :instrument-number track-channel}))) [] tracks)]))

(defn extract-metadata-format-0 [{:keys [tracks] :as midi-edn}]
  [nil
   (reduce
    (fn [ret track]
      (conj ret
            (let [track-name-meta      (first (filter #(= (:subtype %) "trackName") track))
                  track-name           (when track-name-meta
                                         (:text track-name-meta)
                                         #_(str "track" (inc (count ret))))
                  track-program-change (first (filter #(= (:subtype %) "programChange") track))
                  track-program        (if track-program-change (:programNumber track-program-change) 0)
                  track-cc             (vec (filter #(= (:subtype %) "controller") track))
                  track-events         (vec (filter #(or (= (:subtype %) "noteOff")
                                                         (= (:subtype %) "noteOn")) track))
                  track-channel        (:channel (first track-events))]
              {:track-name    track-name    :track-program     track-program
               :track-cc      track-cc      :track-events      track-events
               :track-channel track-channel :instrument-number track-channel}))) [] tracks)])

(defn file-event-handler [files-array]
  (doseq [index (range files-array.length)]
    (let [fileobj  (aget files-array index)
          filename (.-name fileobj)
          filetype (.-type fileobj)
          max-p1   (if (empty? (:files @app-state))
                     0
                     (let [all-p1 (map :p1 (:files @app-state))
                           nums   (filter number? all-p1 )]
                       (if (empty? nums)
                         0 (apply max (map #(int (js/Math.abs %)) nums)))))]
      (if-not (= "audio/midi" filetype)
        (swap! app-state update :error conj
               {:error-message (str "The file " filename
                                    " is not of type .mid or .midi!")})
        (let [file-reader    (new js/FileReader)
              read-finish-cb (fn [event]
                               (let [raw-json (try (midi-file-parser (.-result file-reader))
                                                   (catch js/Error e (str "Error in midi parser: " e)))]
                                 ;; if raw-json isn't a map, it's an error message
                                 (if (string? raw-json)
                                   (swap! app-state update :error conj
                                          {:error-message raw-json})
                                   (let [raw-edn           (js->clj raw-json :keywordize-keys true)
                                         header            (:header raw-edn)
                                         midi-format       (:formatType header)
                                         tick-resolution   (:ticksPerBeat header)
                                         [metadata tracks] (if (= 0 midi-format)
                                                             (extract-metadata-format-0 raw-edn)
                                                             (extract-metadata raw-edn))]
                                     (swap! app-state update :files conj
                                            {:fileobj         fileobj
                                             :raw-edn         raw-edn
                                             :metadata        metadata
                                             :midi-format     midi-format
                                             :tracks          tracks
                                             :filename        filename
                                             :tick-resolution tick-resolution
                                             :include?        true
                                             :p1              (inc max-p1)
                                             :tempo           1.00})))))]
          (set! (.-onload file-reader) read-finish-cb)
          (.readAsBinaryString file-reader fileobj))))))



(defn file-selector []
  (r/create-class
   {:render
    (fn [this this2]
      (let [props (r/props (r/current-component))]
        [:div {:class-name "file-selector-container"}
         [:input {:type      "file" :id "files" :style {:display "none"}
                  :on-change (fn [event]
                               (when (< 0 event.target.files.length)
                                 (file-event-handler event.target.files)))}]
         [:label {:for "files" :class-name "pure-button pure-button-primary"}
          "Open midi file"]
         #_[:button {:class-name "button-success pure-button"} "Play"]
         #_[:button {:class-name "button-error pure-button"} "Stop"]
         [:> Clipboard
          (if (empty? (:score @app-state))
            {:on-click   #(openAlert #js {:message "the score is empty, please import some midi files!"
                                          :type    "warning"})
             :class-name "button-secondary pure-button"}
            {"data-clipboard-text"
             (:score @app-state)
             :class-name "button-secondary pure-button"
             :on-click   (fn [] (openAlert #js {:message "copied to clipboard!" :type "success"}))
             })
          "Copy to Clipboard"]
         [:button {:class-name "pure-button" :on-click #(swap! app-state assoc :files [] :score "")}
          "Clear"]]))})
  )

(defn files-list [files]
  (into [:div]
        (mapv #(vector :h5 {:style {:display "inline-block"}}
                       (str (:filename %))) files))
  #_(into [:ul {:class-name "files-list"}
           [:li [:div {:style {:margin-left "40px"}}
                 [:i {:style {:margin-right "64px"}} "instr"]
                 [:i {:style {:margin-right "36px"}} "tempo"]
                 [:i "midifile"]]]]
          (mapv
           (fn [file]
             [:li
              [:div {:class-name "files-list-column pure-form"}
               [:input {:type    "checkbox" :class-name "pure-checkbox"
                        :checked (:include? file)}]
               [:input {:type        "text"
                        :class-name  "p1val"
                        :placeholder "p1"
                        :value       (:tempo file)}]
               [:input {:type        "number"
                        :class-name  "tempoval"
                        :step        "0.01"
                        :min         "0.01"
                        :placeholder "tempo"
                        :value       (:tempo file)}]
               [:h5 {:style {:display "inline-block"}}
                (str (:filename file))]]])
           files)))

(defn main []
  (let [score-cursor (r/cursor app-state [:score])]
    [:> Dropzone
     {:disable-click true
      :className     "main-container"
      :style         {:position "relative"}
      :accept        ""
      :on-drop       #(do (swap! app-state assoc :drop-hover false)
                          (file-event-handler %))
      :on-drag-enter #(swap! app-state assoc :drop-hover true)
      :on-drag-leave #(swap! app-state assoc :drop-hover false)}
     [:> Alert]
     (when (:drop-hover @app-state)
       [drop-overlay])
     [header]
     [:> (file-selector)]
     #_[files-list  [{:filename "fileX" :p1 1 :include? true :tempo 1.00}]]
     (when-not (empty? (:files @app-state))
       [files-list (:files @app-state)])
     ;; [:h1 (str (:score @app-state))]
     [score-computer (:files @app-state)]
     [score-section score-cursor]]))

(defn ^:export run []
(r/render [main]
          (js/document.getElementById "app")))

(run)
