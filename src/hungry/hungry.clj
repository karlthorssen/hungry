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
            [overtone.inst.sampled-piano :refer [sampled-piano]]
            [hungry.inst :refer [organ bass sing wobble-organ supersaw
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
(defperc snare "resources/samples/SNARES:CLAPS/SNARE3.wav")
(def kit {:fat-kick {:sound fat-kick :amp 0.3}
          :close-hat {:sound clap4 :amp 0.1}
          :kick {:sound kick10 :amp 0.3}
          :open-hat {:sound shaker4 :amp 0.1}
          :snare {:sound snare :amp 0.05}
          :dub {:sound (fn [_ _] (dub2 :freq (-> -16 chosen-scale temperament/equal)))}
          :stick {:sound perc4 :amp 0.1}})

;; (perc4 :amp 3)
;; (snare6 :amp 3)

; (lz/play-note {:drum :close-hat :part :beat})


(defmethod lz/play-note :beat [note]
  (when-let [fn (-> (get kit (:drum note)) :sound)]
    (let [default-amp (-> (get-in kit [(:drum note) :amp] 0.3))]
     (fn :amp (or (:amp note) default-amp)))))

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
  (when pitch
   (reese :freq (temperament/equal pitch)
          :amp 0.12
          :dur duration)))

(defmethod lz/play-note :supersaw [{:keys [pitch duration amp] :as note}]
  (when pitch
    (supersaw :freq (temperament/equal pitch)
              :amp (or amp 0.4)
              :dur duration)))

(defmethod lz/play-note :rest [_] nil)

(defmethod lz/play-note :dub-inst [{:keys [pitch]}]
  (dub2 :freq (temperament/equal  (- pitch 36))))

(defn tap [drum times length ]
  (map #(zipmap [:time :duration :drum]
                [%1 (- length %1) drum]) times))

(def chords {:i (chord/root chord/triad -14)
             :ii (chord/root chord/triad -13)
             :iii (chord/root chord/triad -12)
             :iii-inv (-> chord/triad (chord/root -5) (chord/inversion 1))
             :v7 (-> chord/seventh (chord/root -10) (chord/inversion 3))
             :v (-> chord/triad (chord/root -10))
             :iv (chord/root chord/triad -11)
             :vi (-> chord/triad (chord/root -9))})

(defn inst-phrase [inst times notes]
  (as-inst inst (phrase times notes)))

(def hf  2)
(def qtr 1)
(def eth 1/2)
(def sth 1/4)
(def swup (partial * 1.05))
(def swbk (partial * 0.95))

(def base-drum (->>
                (reduce with
                        [(tap :fat-kick (range 16) 16)
                         (tap :fat-kick [12.5 15.5] 16)
                         (tap :snare (range 2 16 4) 16)
                         (tap :snare [7 7.5 7.75] 16)
                         (tap :close-hat (range 0 16 qtr) 16)
                         (tap :close-hat (range (swup eth) 16 qtr) 16)])
                (all :part :beat)))

(def track (atom nil))
(reset! track
        (let [ticks (all :part :beat (reduce with
                                             [(tap :kick (range 0 16 qtr) 16)]))
              prog1 (phrase
                     [4 4 hf hf 4]
                     (map chords [:iv :iv :v :v :vi]))
              prog1-piano (as-inst :piano2 prog1)
              prog1-saw (->> prog1
                             (as-inst :supersaw)
                             (where :pitch (comp scale/high scale/high)))

              chorus (with
                      prog1-piano
                      (all :amp 0.6
                       (inst-phrase :supersaw
                                    (repeat 16 qtr)
                                    [3 5 3 5 3 4 5 5  4 6 8 11 12 11 12 11])))

              chorus-bass (inst-phrase :reese [4 4 hf hf 4] (map (partial + -14) [3 3 4 4 5]))

              prog2 (phrase [hf hf hf hf hf hf 4]
                                 (map chords [:ii :ii :iv :iv :iii :iii :iii-inv]))
              verse-piano (with
                           (inst-phrase :piano2 ;; TODO better melody here
                                        [1 1 1 1 4  1 1 1 1 4]
                                        [3 3 3 4 5  5 5 4 4 4])
                           prog1-piano)
              verse-bass (inst-phrase :reese
                                      [1     3 4   4  1    3]
                                      [nil -9 nil nil nil -9])

              build-drum (->> (reduce with
                                      [(tap :kick (range 16) 16)
                                       (tap :kick [(- 16 1/4)] 16)
                                       (tap :fat-kick (range 8 16 eth) 16)
                                       (tap :snare (range 2 16 4) 16)
                                       (tap :snare (range 12 16 sth) 16)])
                              (all :part :beat))
              drop-drum (->> (tap :kick [0  7/4 2 4 6] 16)
                             (all :amp 0.7)
                             (all :part :beat)
                             (with base-drum))

              steady-drum (->> [(tap :kick (range 0 16 2) 16)
                                (tap :stick [0] 16)
                                (tap :snare (range 1 16 2)  16)
                                (tap :snare (range 3/4 16 2)  16)
                                (tap :snare [(+ 13 1/4)] 16)
                                (tap :kick [5 (+ 13 1/2)] 16)
                                ]
                               (reduce with)
                               (all :part :beat))

              steady-intro (all :amp 0.5
                            (inst-phrase :supersaw
                                         [qtr qtr qtr qtr]
                                         [3 3 3 4]))
              steady-melody (inst-phrase :piano2
                                         [4   eth eth eth eth eth eth eth eth eth eth eth eth eth eth eth eth 2 2]
                                         [nil 5   5   5   5   5   5   5   5   6   6   6   6   4   4   4   4   5 5])]
          ;; (->>
          ;;  verse-piano
          ;;  (then (times 2 verse-piano))
          ;;  (then (with ticks verse-piano))

          ;;  (then (with base-drum verse-piano))
          ;;  (then (with base-drum verse-piano verse-bass))
          ;;  (then (with ticks      prog1-piano verse-bass))
          ;;  (then (with build-drum prog1-piano chorus-bass))

          ;;  (then (with drop-drum  chorus chorus-bass prog1-saw))
          ;;  (then (with steady-drum steady-intro steady-melody verse-bass prog1-saw))
          ;;  (then (with steady-drum steady-melody chorus-bass prog1-saw))
          ;;  (then (with steady-drum (all :part :piano2 chorus) verse-bass prog1-saw))

          ;;  (then (with steady-drum chorus chorus-bass prog1-saw))
          ;;  (then (with steady-drum steady-intro steady-melody verse-bass prog1-saw))
          ;;  (then (with steady-drum steady-melody chorus-bass prog1-saw))
          ;;  (then (with steady-drum steady-melody chorus-bass))


          ;;  (tempo (bpm 128)))
          (->>
           (with steady-drum (as-inst :piano2 prog2) )
           (tempo (bpm 128)))
          ))

(comment
  ;; i  ii iii iv v  vi vii
  ;; 0  1  2   3  4  5  6
  ;; -7 -6 -5  -4 -3 -2 -1
  (lz/jam track)
  (lz/stop)
  (lz/play @track)
  (lz/stop)

  (time @(lz/play @track))

  (->>
   (phrase
    [hf hf hf hf hf hf 4]
    (map chords [:iii :iii :vi :vi :ii :ii :ii]))
   (as-inst :supersaw)
   (tempo (bpm 110))
   (where :pitch (comp scale/high scale/high))
   lz/play)


  (->> (inst-phrase :supersaw
                    [4 4 hf hf 4]
                    (map chords [:iv :iv :v :v :vi]))
       (tempo (bpm 110))
       (where :pitch (comp scale/high scale/high))
       lz/play)

  (bass2 :amp 0.3)
  (reese    (temperament/equal (chosen-scale -13)))


  (do
    (recording-start "/Users/jonathan/src/music/hungry/hungry.wav")
    @(lz/play @track)
    (recording-stop))

  (piano/piano 16))
