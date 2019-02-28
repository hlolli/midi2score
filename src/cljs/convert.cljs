(ns cljs.convert)

(defn new-empty-string [length]
  (apply str (take length (cycle " "))))

(defn edn2sco [edn track-name]
  (let [max-p2 (apply max (map (comp count str :p2) edn))
        max-p3 (apply max (map (comp count str :dur) edn))
        max-p4 (apply max (map (comp count str #(or (:p4 %) (:vel %))) edn))]
    (reduce (fn [out-str data]
              (let [cur-p2-len (- max-p2 ((comp count str :p2) data))
                    cur-p3-len (- max-p3 ((comp count str :dur) data))
                    cur-p4-len (- max-p4 ((comp count str #(or (:p4 %) (:vel %))) data))]
                (str out-str "\n"
                     "i" track-name " "
                     (:p2 data) (new-empty-string cur-p2-len) " "
                     (:dur data) (new-empty-string cur-p3-len) " "
                     (or (:p4 data) (:vel data)) (new-empty-string cur-p4-len) " "
                     (or (:p5 data) (:midinn data)))))
            "" edn)))

(defn parse-midi [events tempo tick-resolution]
  (loop [[event & events] events
         last-time        0
         cur-notes-on     {}
         edn-out          []]
    (if (empty? event)
      (sort-by :p2 edn-out)
      (let [event-type (:subtype event)
            cur-time   (+ last-time (/ (or (* (/ 60 tempo) (:deltaTime event)) 0) (or tick-resolution 256)))]
        (cond
          (= "noteOn" event-type)
          (recur events
                 cur-time
                 (assoc cur-notes-on (:noteNumber event)
                        {:p2 (double cur-time) :vel (:velocity event)})
                 edn-out)
          (= "noteOff" event-type)
          (recur events
                 cur-time
                 (dissoc cur-notes-on (:noteNumber event))
                 (let [matching-event (get cur-notes-on (:noteNumber event))]
                   (conj edn-out {:p2      (or (:p2 matching-event)
                                               (if (empty? edn-out)
                                                 0 (:p2 (last edn-out))))
                                  :midinn  (or (:noteNumber event) 0)
                                  :vel     (or (:vel matching-event) 0)
                                  :dur     (max 0 (- cur-time (:p2 matching-event)))
                                  :channel (:channel matching-event)})))
          :else
          (recur events last-time cur-notes-on edn-out))))))
