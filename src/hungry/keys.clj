(ns hungry.keys
  (:require [seesaw.core :as seesaw]))


(def alphabet-keycodes (zipmap
                        (range 65 91)
                        "abcdefghijklmnopqrstuvwxyz"))

(def numeric-keycodes {49 \1
                       50 \2
                       51 \3
                       52 \4
                       53 \5
                       54 \6
                       55 \7
                       56 \8
                       57 \9
                       48 \0
                       59 \;})

(defn keycode->char
  "'leaky' keycode->char converter
gives 65 -> 'a'
ie when given a code not a char, it just send the keycode through"
  [keycode]
  (get
   (merge alphabet-keycodes numeric-keycodes)
   keycode keycode))

(defn listen-keys [& [keydown keyup]]
  (let [keydown (or keydown prn)
        keyup (or keyup prn)
        indicator (seesaw/label "this window needs focus to play keys")
        the-label (seesaw/label "listening")
        frame (seesaw/frame :title "Jammer"
                            :content (seesaw/left-right-split the-label indicator :divider-location 5/6))]
    (-> frame seesaw/pack! seesaw/show!)
    (seesaw/listen frame
                   :window-activated (fn [e]
                                       (println "window activated!")
                                       (future
                                         (seesaw/config! the-label :text "listening to keyboard")
                                         (seesaw/config! the-label :background :lightgrey)))
                   :window-deactivated (fn [e]
                                         (println "window deactivated!")
                                         (future
                                           (seesaw/config! the-label :text "no focus, no keys")
                                           (seesaw/config! the-label :background :grey)))
                   :key-pressed (fn [e]
                                  (future (seesaw/config! indicator :background :lightgrey))
                                  (keydown (. e getKeyCode)))
                   :key-released (fn [e]
                                   (future (seesaw/config! indicator :background "#DDD"))
                                   (keyup (. e getKeyCode))))))

(def pitch-map
  (zipmap "qwertyasdfghjkl;" (range -6 10)))
(defn play-inst [player-fn]
  (listen-keys (fn [keycode]
                 (let [_ (prn "down " keycode)
                       pitch (-> keycode keycode->char pitch-map)]
                   (player-fn {:pitch pitch :duration 1})))
               identity))
