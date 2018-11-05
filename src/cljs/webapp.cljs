(ns cljs.webapp
  (:require [reagent.core :as r]
            [clojure.string :as string]
            ;; [cljs.convert :refer ]
            ["react-dropzone" :as Dropzone]))


(defn header []
  [:header [:h1 "Midi to Csound Score"]])


(def app-state (r/atom {:drop-hover false :files []
                        :error      []}))

(defn drop-overlay []
  [:div {:class-name "drop-overlay"}
   [:div {:class-name "drop-overlay-message"}
    [:h5 "Drop .mid or .midi file here"]]])

(defn score-section [files]
  (let [score (r/atom "")]
    (r/create-class
     {:component-did-update (fn [this prev next] (prn "UPDATE!"))
      :component-did-mount  (fn [this] )
      :reagent-render
      (fn [this]
        (let [lines (string/split @score "\n")]
          (into [:code
                 [:div {:class-name "score-line-container score-line-container-dummy"}
                  [:span {:class-name "score-line-number score-line-number-dummy"}]
                  [:p {:class-name "score-line-code"}]]]
                (map-indexed
                 (fn [index line]
                   [:div {:class-name "score-line-container"}
                    [:span {:class-name "score-line-number"} (str (inc index))]
                    [:p {:class-name "score-line-code"} line]]) lines))))})))

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
        (swap! app-state update :files conj {:fileobj  fileobj
                                             :filename filename
                                             :include? true
                                             :p1       (inc max-p1)
                                             :tempo    1.00})))))

(defn file-selector []
[:div {:class-name "file-selector-container"}
 [:input {:type      "file" :id "files" :style {:display "none"}
          :on-change (fn [event]
                       (when (exists? event.target.files)
                         (when (< 0 event.target.files.length)
                           (file-event-handler event.target.files))))}]
 [:label {:for "files" :class-name "pure-button pure-button-primary"}
  "Open midi file"]
 [:button {:class-name "button-success pure-button"} "Play"]
 [:button {:class-name "button-error pure-button"} "Stop"]
 [:button {:class-name "button-secondary pure-button"} "Copy to Clipboard"]])

(defn files-list [files]
  (into [:ul {:class-name "files-list"}
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
  (let []
    [:> Dropzone
     {:disable-click true
      :className     "main-container"
      :style         {:position "relative"}
      :accept        ""
      :on-drop       #(do (swap! app-state assoc :drop-hover false)
                          (file-event-handler %))
      :on-drag-enter #(swap! app-state assoc :drop-hover true)
      :on-drag-leave #(swap! app-state assoc :drop-hover false)}
     (when (:drop-hover @app-state)
       [drop-overlay])
     [header]
     [file-selector]
     #_[files-list  [{:filename "fileX" :p1 1 :include? true :tempo 1.00}]]
     (when-not (empty? (:files @app-state))
       [files-list (:files @app-state)])
     [score-section (:files @app-state)]]))

(defn ^:export run []
(r/render [main]
          (js/document.getElementById "app")))

(run)
