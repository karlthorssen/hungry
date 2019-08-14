(ns hungry.inst
  (:require [clojure.repl :refer [doc]]
            [overtone.core :refer :all]
            [overtone.inst.piano :as piano]
            [overtone.inst.synth :as synth]
            [overtone.inst.drum :as drum]))

(comment
  (require '[overtone.live :refer :all])

  )

(defn detune-saw [freq]
  (saw [freq (* 0.99 freq) (* 1.01 freq)]))

(definst dub2
  [freq   {:default 80 :min 40 :max 220 :step 1}
   amp    {:default 1 :min 0 :max 3 :step 0.001}]
  (let [cutoff-env (perc 0.001 1 freq -20)
        amp-env (perc 0.001 1 1 -8)
        osc-env (perc 0.001 1 freq -8)
        noiz (* 2 (lpf (white-noise) (+ (env-gen:kr cutoff-env) 20)))
        snd  (lpf (sin-osc (+ (env-gen:kr osc-env) freq)) 200)
        mixed (* (+ noiz snd) (env-gen amp-env :action FREE))]
    (* 3 amp mixed)))

(defn raw-splay [in]
  (splay :in-array in :level-comp? false))

(definst reese
  [freq 110
   dur 1
   amp 1]
  (let [amp-env (env-gen (lin :attack 0.01
                              :sustain (- dur 0.03)
                              :level (* 5 amp)
                              :release 0.02
                              :curve -1)
                         :action FREE)
        detune-fn (fn [n] [n (* 0.995 n) (* 1.02 n)])
        saw-fn (fn [f] (-> f lf-saw (delay-c 0.005 (ranged-rand 0.0001 0.01))))
        main (raw-splay (->> freq detune-fn (map saw-fn)))
        harm (raw-splay (map saw-fn
                             (mapcat detune-fn
                                     [freq  (* 3/2 freq) (* 2 freq) (* 5/2 freq) (* 4 freq)])))
        noiz (pan2 (lpf (* amp (pink-noise)) (* 2 freq)))
        snd (* amp-env (+ main (* 0.2 harm) noiz))
        ;; harm (raw-splay (map (comp saw (partial * freq)) [2 4 6]))
        ]
    (-> snd
        (clip2 amp)
        (lpf  (* 3 freq)))
    ))

;; (cs80 :freq 330 :dur 0.4 :att 0.1 :fatt 0.1 :decay 0.1 :rel 0.1)
(definst cs80
  [freq 880
   amp 0.5
   att 0.75
   decay 0.5
   sus 0.8
   rel 1.0
   fatt 0.75
   fdecay 0.5
   fsus 0.8
   frel 1.0
   cutoff 200
   dtune 0.002
   vibrate 4
   vibdepth 0.015
   dur 1
   ratio 1
   cbus 1
   freq-lag 0.1]
  (let [freq (lag freq freq-lag)
        cuttoff (in:kr cbus)
        env     (env-gen (adsr att decay sus rel) (line:kr 1 0 dur)
                         :action FREE)
        vib     (+ 1 (lin-lin:kr (sin-osc:kr vibrate) -1 1 (- vibdepth) vibdepth))
        freq    (* freq vib)
        sig     (mix (* env amp (saw [freq (* freq (+ dtune 1))])))]
    sig))

(definst supersaw [freq 440 dur 0.2 release 0.5 amp 0.6 cutoff 3500 env-amount 0.5]
  (let [snd-fn (fn [freq]
                 (let [tune (ranged-rand 0.99 1.01)]
                   (-> (lf-saw (* freq tune))
                       (delay-c 0.005 (ranged-rand 0.0001 0.01)))))
        hi-saws (splay (repeatedly 5 #(snd-fn freq)))
        lo-saws (splay (repeatedly 5 #(snd-fn (/ freq 2))))
        noise (pink-noise)
        snd (+ (* 0.65 hi-saws) (* 0.85 lo-saws) (* 0.12 noise))
        env (env-gen (adsr 0.001 0.7 0.2 0.1) (line:kr 1 0 (+ dur release)) :action FREE)]
    (-> snd
        (clip2 0.45)
        (rlpf (+ freq (env-gen (adsr 0.001) (line:kr 1 0 dur) :level-scale cutoff)) 0.75)
        (free-verb :room 1.8 :mix 0.45)
        (* env amp)
        pan2)))

(comment do
  (supersaw (midi->hz 60))
  (supersaw (midi->hz 64))
  (supersaw (midi->hz 67))
  (supersaw (midi->hz 72)))

(definst brass [freq 220
                dur 1
                att 0.1
                sus 0.9
                decay 0.5
                rel 0.01
                amp 1]
  (let [
        voices (range 0.99 1.01 0.002)
        freqs (raw-splay
               (for [d voices] (-> freq (* (+ 1 d)) saw
                                   (delay-c 0.005 (ranged-rand 0.0001 0.01)))))
        hi-freqs (raw-splay
                  (for [d voices] (-> (* freq 2) (* (+ 1 d)) saw
                                      (delay-c 0.005 (ranged-rand 0.0001 0.01)))))
        noiz (pink-noise)
        snd (+ (* 0.85 freqs) (* 0.5 hi-freqs) (* 0.1 noiz))]
    (-> snd
        (clip2 0.45)
        (* (env-gen (adsr att decay sus rel 1) (line:kr 1 0 dur) :action FREE))
        (free-verb :room 0.99 :mix 0.7)
        (* 3 amp))))


(definst organ [freq 440 dur 1.0 vol 1.0 pan 0.0 wet 0.5 room 0.5 limit 20000 attack 0.1]
  (->
   (map #(sin-osc (* freq %)) (range 1 5))
   mix
   (* (env-gen (asr attack 1.0 0.5) (line:kr 1.0 0.0 dur)))
   (lpf (mul-add (sin-osc 5) freq (* freq 5)))
   ;; (effects :pan pan :wet wet :room room :volume vol :high limit)
   ))

(definst bass [freq 110 dur 1.0 volume 1.0]
  (-> (square freq)
      (* (env-gen (adsr 0.0001 0.8 0.5 0.1) (line:kr 1 0 dur) :action FREE))
      (rlpf (+ (* 440 (sin-osc 3)) 880) 0.3)
      (+ (square (* 1/2 freq)))
      (lpf 2000)
      (* (env-gen (adsr 0.0001 1.0 0.8 0.01) (line:kr 1 0 dur) :action FREE))
      (* volume)))

(definst bass2 [freq 110 dur 1.0 amp 1.0]
  (-> (square freq)
      (* (env-gen (adsr 0.0001 0.8 0.5 0.1) (line:kr 1 0 dur) :action FREE))
      (rlpf 880 0.3)
      (+ (square (* 1/2 freq)))
      (lpf 2000)
      (* (env-gen (adsr 0.0001 1.0 0.8 0.01) (line:kr 1 0 dur) :action FREE))
      (* amp)))

(definst sing [freq 440 dur 1 volume 1.0]
  (-> (square freq)
      (+ (sin-osc (* 3 freq)))
      (+ (sin-osc (* 4.95 freq)))
      (+ (sin-osc (* 6.95 freq)))
      (* (env-gen (adsr 0.01 0.5 0.1) (line:kr 1 0 dur) :action FREE))
      (pan2 0.4)
      (* 1/4 volume)))

(definst wobble-organ [freq 440 dur 1 volume 1.0]
  (-> (square (+ freq (* 40 (sin-osc 6))))
      (* (env-gen (adsr dur 0.8 0.5 0.2) (line:kr 1 0 dur) :action FREE))
      (lpf 2600)
      (hpf 440)
      (pan2 0.6)
      (* 1/4 volume)))

(definst organ2 [freq 440 dur 1 volume 1.0]
  (-> (square freq)
      (* (env-gen (adsr dur 0.8 0.5 0.2) (line:kr 1 0 dur) :action FREE))
      (lpf 2600)
      (hpf 440)
      (pan2 0.6)
      (* 1/4 volume)))

(definst string [note 60 amp 1.0 dur 0.5 decay 30 coef 0.3 gate 1]
  (let [freq (midicps note)
        noize (* 0.8 (white-noise))
        dly   (/ 1.0 freq)
        plk   (pluck noize gate dly dly decay coef)
        dist  (distort plk)
        filt  (rlpf dist (* 12 freq) 0.6)
        clp   (clip2 filt 0.8)
        reverb (free-verb clp 0.4 0.8 0.2)]
    (* amp (env-gen (perc 0.0001 dur) :action 0) reverb)))

(definst plucky [freq 440 dur 1 amp 1 cutoff 1500 fil-dur 0.1]
  (let [env (env-gen (asr 0 1 1) (line:kr 1.0 0.0 dur) :action FREE)
        level (+ (* 0.85 freq) (env-gen (perc 0 fil-dur) :level-scale cutoff))]
    (-> (pulse freq)
        (lpf level)
        (free-verb :room 1 :damp 0.45)
        (* env amp))))

(comment

  )
