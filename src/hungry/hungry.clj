(ns hungry.hungry
  (:require [clojure.java.io :as io]
            [clojure.repl :refer [doc]]
            [hungry.boot :refer [connected]]
            [leipzig.melody :refer :all]
            [leipzig.live :as lz]
            [leipzig.scale :as scale]
            [leipzig.melody :as melody]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]
            [leipzig.scale :as scale]
            [overtone.inst.piano :as piano]
            [overtone.inst.synth :as synth]
            [overtone.inst.drum :as drum]
            [hungry.inst :refer [organ bass sing wobble-organ
                                  dub2 reese string bass2 organ2]]
            [overtone.core :refer :all :exclude [tap]]))

(defmacro defperc [name path]
  `(let [buf# (sample ~path)]
     (definst ~name [~(symbol "amp") 1]
       (~'* ~(symbol "amp") (play-buf 2 buf# :action FREE)))))

(def chosen-scale (comp scale/C scale/major))

(defn as-bass [phrase]
  (->> phrase
       (where :pitch chosen-scale)
       (all :part :bass)
       (all :amp 1)))

(defn as-organ [phrase]
  (->> phrase
       (where :pitch chosen-scale) ;; index->midi
       (all :part :organ)
       (all :amp 1)))

(defn as-piano [phrase]
  (->> phrase
       (where :pitch chosen-scale) ;; index->midi
       (all :part :piano)))

(defn as-inst
  "applies the given instrument key to the notes and converts note
  indices to midi"
  [inst phrase]
  (->> phrase
       (where :pitch chosen-scale)
       (all :part inst)))

(defperc perc4 "resources/samples/PERCS/PERC4.wav")
(defperc fat-kick "resources/samples/KICKS/KICK7.wav")
(defperc clap4 "resources/samples/SNARES:CLAPS/CLAP4.wav")
(defperc kick10 "resources/samples/KICKS/KICK10.wav")
(defperc shaker4 "resources/samples/SHAKERS/SHAKER4.wav")
(defperc snare6 "resources/samples/SNARES:CLAPS/SNARE6.wav")
(def kit {:fat-kick {:sound fat-kick :amp 1}
          :close-hat {:sound clap4 :amp 1}
          :kick {:sound kick10 :amp 1}
          :open-hat {:sound shaker4 :amp 1}
          :snare {:sound snare6 :amp 1}
          :dub {:sound (fn [_ _] (dub2 :freq (-> -16 chosen-scale temperament/equal)))}
          :stick {:sound perc4}})

;(lz/play-note {:drum :stick :part :beat :amp 1})

(defmethod lz/play-note :beat [note]
  (when-let [fn (-> (get kit (:drum note)) :sound)]
    (fn :amp 0.6)))

(defmethod lz/play-note :bass [{:keys [pitch duration]}]
  (synth/bass :freq (/ pitch 2) :t (* 2 duration)))

(defmethod lz/play-note :organ [{:keys [pitch duration]}]
  (organ :freq (temperament/equal pitch) :dur duration))

(defmethod lz/play-note :piano [{:keys [pitch duration]}]
  (piano/piano pitch :vel 70))

(defmethod lz/play-note :piano2 [{:keys [pitch duration]}]
  (when pitch
    (piano/piano pitch :vel 60 :hard 0.2 :muffle 0.2 :velcurve 0.3)))

(defmethod lz/play-note :piano [{:keys [pitch duration]}]
  (when pitch
    (piano/piano pitch :vel 70)))

(defmethod lz/play-note :piano3 [{:keys [pitch duration]}]
  (when pitch
    (piano/piano pitch :vel 60 :velcurve 0.1 :decay 0.2)))

(defmethod lz/play-note :reese [{:keys [pitch duration]}]
  (reese :freq (/ (temperament/equal pitch) 4)
         :amp 1
         :dur duration))

(defmethod lz/play-note :rest [_] nil)

(defmethod lz/play-note :dub-inst [{:keys [pitch]}]
  (dub2 :freq (temperament/equal  (- pitch 36))))

(defn tap [drum times length & {:keys [amp] :or {amp 1}}]
  (map #(zipmap [:time :duration :drum :amp]
                [%1 (- length %1) drum amp]) times))

(def chords {:i (chord/root chord/triad -7)
             :ii (chord/root chord/triad -6)
             :v7 (-> chord/seventh (chord/root -3) (chord/inversion 3))
             :v (-> chord/triad (chord/root -3))
             :iv (chord/root chord/triad -4)
             :vi (-> chord/triad (chord/root -2) (chord/inversion 1))})

(defn inst-phrase [inst times notes]
  (as-inst inst (phrase times notes)))

(def hf  2)
(def qtr 1)
(def eth 1/2)
(def sth 1/4)
(def swup (partial * 1.1))
(def swbk (partial * 0.9))

(def swing-beat
  (->>
   (reduce with
           [ ;; (tap :dub (range 8) 8)
            (tap :fat-kick (range 16) 16)
            (tap :snare (range 2 16 4) 16)
            (tap :close-hat (range 0 16 qtr) 16)
            (tap :close-hat (range (swup eth) 16 qtr) 16)])
   (all :part :beat)))

(def track (atom nil))
(reset! track
        (let [base-drum (filter #(< (:time %) 16) swing-beat)
              ticks (all :part :beat (reduce with
                                             [(tap :close-hat (range 0 16 qtr) 16)
                                              (tap :close-hat (range (swup eth) 16 qtr) 16)]))
              chords1 (with
                       (inst-phrase :piano
                                    (repeat 16 qtr)
                                    [3 5 3 5 3 4 5 5  4 6 8 11 12 11 12 11])
                       (inst-phrase :piano
                                    [4 4 hf hf 4]
                                    (map chords [:iv :iv :v :v :vi])))

              chords2 (with ;; TODO better
                       (inst-phrase :piano
                                    (repeat 16 qtr)
                                    [1 1 1 1 5 5 5 5 4 3 4 3 4 4 4 4])
                       (inst-phrase :piano
                                    [hf hf hf hf hf hf 4]
                                    (map chords [:ii :ii :vi :vi :v7 :v7 :v7])))


              ]
          (->> ;; base-drum
           ;; (then (with ticks base-drum chords1))
           (times 3 (with ticks chords1))
           (then chords2)
           (tempo (bpm 110)))))
(comment
  ;; i  ii iii iv v  vi vii
  ;; 0  1  2   3  4  5  6
  ;; -7 -6 -5  -4 -3 -2 -1

  (lz/play @track)
  (lz/jam track)
  (lz/stop)

  (do
    (recording-start "hungry.wav")
    @(lz/play @track)(lz/play @track)
    (recording-stop))

  )
