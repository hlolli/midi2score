(ns cljs.webapp
  (:require [reagent.core :as r]
            ["react-dom" :as react-dom]
            [clojure.string :as string]
            [cljs.convert :refer [parse-midi edn2sco]]
            [cljs.midi-standard :refer [program-change-names]]
            ;; [hx.react :as hx :refer [defnc]]
            ;; [hx.hooks :refer [<-state]]
            ["react-clipboard.js" :default Clipboard]
            ["react-dropdown" :default Dropdown]
            ;; ["react-alert" :refer [withAlert]]
            ["simple-react-alert" :default Alert :refer [openAlert]]
            ["midi-file-parser" :as midi-file-parser]
            ["react-dropzone" :as Dropzone]))

(defn header []
  [:header [:h1 "MIDI to Csound Score"]])

(def app-state (r/atom {:drop-hover false :files []
                        :error      []    :score ""}))

(defn drop-overlay []
  [:div {:class-name "drop-overlay"}
   [:div {:class-name "drop-overlay-message"}
    [:h5 "Drop .mid or .midi file here"]]])

(defn apply-midi-conversion [edn-score p4-op p5-op]
  (loop [edn edn-score
         op [p4-op p5-op]
         index 0]
    (if (empty? op)
      (vec edn)
      (recur (mapv (fn [event]
                     (assoc event (if (zero? index) :p4 :p5)
                            (case (first op)
                              ;; vel
                              0 (:vel event)
                              ;; vel->amp
                              1 (* (/ (js/Math.log (+ 1 (:vel event)))
                                      (js/Math.log 128))
                                   32767)
                              ;; vel->ampfs1
                              2 (/ (js/Math.log (+ 1 (:vel event)))
                                   (js/Math.log 128))
                              ;; vel->db
                              3 (* 10 (js/Math.log (/ (max (:vel event) 0.0001) 127)))
                              ;; midinn
                              4 (:midinn event)
                              ;; midinn->freq
                              5 (* 440 (js/Math.pow 2.0 (/ (- (:midinn event) 69) 12)))))) edn)
             (rest op)
             (inc index))))
  #_edn-score)

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
            (dotimes [index (count (:tracks file))]
              (let [{:keys [track-channel track-events track-name
                            track-program tempo] :as track}
                    (nth (:tracks file) index)
                    p4-operator  (nth (:p4-operator file) index)
                    p5-operator  (nth (:p5-operator file) index)
                    global-bpm (let [gbpm (get-in file [:metadata :global-bpm])]
                                 (if (or (not gbpm) (zero? gbpm)) 60 gbpm))
                    global-bpm   (* global-bpm (:tempo file))
                    tick-res     (:tick-resolution file)
                    edn-score    (parse-midi track-events (or global-bpm 60) tick-res)
                    edn-score    (apply-midi-conversion edn-score p4-operator p5-operator)
                    csnd-score   (edn2sco edn-score (nth (:track-names file) index))
                    program-name (get program-change-names track-program)
                    comment      (if-not track-channel
                                   "\n"
                                   (str "\n\n;; channel: " track-channel
                                        (when-not (= 0 (:midi-format file))
                                          "\n;; track name: " track-name
                                          "\n;; program: " track-program
                                          (when program-name (str " (" program-name ")"))) "\n"))]
                (swap! app-state update :score str (str comment csnd-score)))
              #_(:tracks file))))))
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
        global-timesig-meta (first (filter #(= (:subtype %) "setTempo") global-meta))
        global-bpm          (if global-timesig-meta
                              (/ 60
                                 (/ (or (:microsecondsPerBeat global-timesig-meta)
                                        1000000)
                                    1000000)) 60)
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
          ;; max-p1   (if (empty? (:files @app-state))
          ;;            0
          ;;            (let [all-p1 (map :p1 (:files @app-state))
          ;;                  nums   (filter number? all-p1 )]
          ;;              (if (empty? nums)
          ;;                0 (apply max (map #(int (js/Math.abs %)) nums)))))
          ]
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
                                                             (extract-metadata raw-edn))
                                         tracks (->> tracks
                                                     (remove #(and (empty? (:track-cc %))
                                                                   (empty? (:track-events %)))))
                                         track-names (reduce
                                                      (fn [i track]
                                                        (let [channel-num (if (empty? (:track-cc track))
                                                                            (:channel (first (:track-events track)))
                                                                            (:channel (first (:track-cc track))))]
                                                          (if (or (not channel-num) (zero? channel-num))
                                                            (conj i 1)
                                                            (conj i (inc channel-num))))) [] tracks)]
                                     (swap! app-state update :files conj
                                            {:fileobj         fileobj
                                             :raw-edn         raw-edn
                                             :metadata        metadata
                                             :midi-format     midi-format
                                             :tracks          tracks
                                             :filename        filename
                                             :tick-resolution tick-resolution
                                             :include?        true
                                             :track-names     track-names
                                             :p4-operator     (vec (repeat (count tracks) 0))
                                             :p5-operator     (vec (repeat (count tracks) 4))
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
          "Open MIDI file"]
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
        (map-indexed
         (fn [index file]
           (vector :div
                   [:h5 {:style {:display "inline-block"
                                 :margin 0 :height "10px"}}
                    (str (:filename file))]
                   [:p {:style {:margin 0 :display "inline-block" :margin-left "32px"}} "tempo: "]
                   [:input {:type        "number"
                            :on-change (fn [event]
                                         (swap! app-state update :files assoc index
                                                (assoc (nth (:files @app-state) index) :tempo
                                                       event.target.value)))
                            :class-name  "tempoval"
                            :step        "0.01"
                            :min         "0.01"
                            :placeholder "tempo"
                            :value       (:tempo file)}]
                   (into
                    [:ul {:class-name "files-list"}
                     [:li [:div {:style {:margin-left "40px"}}
                           [:i {:style {:margin-right "34px"}} "channel"]
                           [:i {:style {:margin-right "96px"}} "instr"]
                           [:i {:style {:margin-right "156px"}} "p4"]
                           [:i {:style {:margin-right "136px"}} "p5"]
                           #_[:i {:style {:margin-right "0px"}} "p6 (optinal filler values)"]
                           #_[:i "midifile"]]]
                     #_[:li (str  file #_(:tracks file))]]
                    (let [options #js [#js {:value 0 :label "vel"}
                                       #js {:value 1 :label "vel->amp"}
                                       #js {:value 2 :label "vel->ampfs1"}
                                       #js {:value 3 :label "vel->db"}
                                       #js {:value 4 :label "midinn"}
                                       #js {:value 5 :label "midinn->freq"}]]
                      (map-indexed
                       (fn [index-inner channel]
                         (vector
                          :li
                          [:div
                           {:style {:margin "0 auto"
                                    :margin-left "40px"
                                    :width "600px"
                                    :height "40px"}}
                           [:b {:style {:display "inline-block" :width "70px"
                                        :line-height "40px"}}
                            (str "# " (:channel (if (empty? (:track-cc channel))
                                                  (first (:track-events channel))
                                                  (first (:track-cc channel)))))]
                           [:input {:type        "text"
                                    :class-name  "p1val"
                                    :placeholder (nth (:track-names file) index-inner)
                                    :on-change (fn [event]
                                                 (swap! app-state update :files assoc index
                                                        (assoc file :track-names
                                                               (assoc (:track-names file) index-inner event.target.value))))
                                    :value     (nth (:track-names file) index-inner)}]
                           [:> Dropdown
                            {:class-name "inline-dropdown"
                             :on-change (fn [event]
                                          (swap! app-state update :files assoc index
                                                 (assoc file :p4-operator
                                                        (assoc (:p4-operator file) index-inner (.-value event)))))
                             :value (nth options (nth (:p4-operator file) index-inner))
                             :options options}]
                           [:> Dropdown
                            {:class-name "inline-dropdown inline-dropdown-right"
                             :on-change (fn [event]
                                          (swap! app-state update :files assoc index
                                                 (assoc file :p5-operator
                                                        (assoc (:p5-operator file) index-inner (.-value event)))))
                             :value (nth options (nth (:p5-operator file) index-inner))
                             :options options}]]))
                       (:tracks file)
                       #_(->> (:tracks file)
                              (remove #(and (empty? (:track-cc %))
                                            (empty? (:track-events %))))))))))
         files)))

(defn main []
  (let [score-cursor (r/cursor app-state [:score])]
    [:div {:class-name "main-container"}
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
     [score-section score-cursor]
     [:> Dropzone
      {:disable-click true
       ;; :style         {:position "relative"}
       :accept        ""
       :onDrop       #(do (swap! app-state assoc :drop-hover false)
                          (file-event-handler %))
       :on-drag-enter #(swap! app-state assoc :drop-hover true)
       :on-drag-leave #(swap! app-state assoc :drop-hover false)}
      (fn [^js env]
        (r/as-element [:div (merge {:id "dropzone"}
                                   (js->clj ((.-getRootProps env))))]))]]))

(defn ^:export run []
  #_(react-dom/render (hx/f [Main])
                      (js/document.getElementById "app"))

  (r/render [main]
            (js/document.getElementById "app")))

(run)
