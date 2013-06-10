(ns leipzig.example.row-row-row-your-boat
  (:use
    leipzig.melody
    leipzig.scale
    leipzig.canon
    leipzig.live)
  (:require [overtone.live :as overtone]
            [overtone.synth.stringed :as strings])) 

(strings/gen-stringed-synth ektara 1 true)

(defn pick [distort amp {midi :pitch, start :time, length :duration}] 
    (let [synth-id (overtone/at start
                     (ektara midi :distort distort :amp amp :gate 1))]
      (overtone/at (+ start length) (overtone/ctl synth-id :gate 0))))

(defmethod play-note :leader [note]
  (pick 0.7 1.0 note))
(defmethod play-note :follower [note]
  (pick 0.3 1.0 note))
(defmethod play-note :bass [note]
  (pick 0.9 0.2 (update-in note [:pitch] #(- % 12))))

(defmethod play-note :sop1 [note]
  (pick 0.6 0.4 note))

(defmethod play-note :alto1 [note]
  (pick 0.5 0.6 note))

(def melody "A simple melody built from durations and pitches."
               ; Row, row, row your boat,
  (->> (phrase [3/3 3/3 2/3 1/3 3/3]
               [  0   0   0   1   2])
    (then
               ; Gently down the stream,
       (phrase [2/3 1/3 2/3 1/3 6/3]
               [  2   1   2   3   4]))
    (then
               ; Merrily, merrily, merrily, merrily,
       (phrase (repeat 12 1/3) 
               (mapcat (partial repeat 3) [7 4 2 0])))
    (then
               ; Life is but a dream!
       (phrase [2/3 1/3 2/3 1/3 6/3] 
               [  4   3   2   1   0]))
    (where :part (is :leader))))

(def melody-ode 
               
  (->> (phrase [1/4 1/4 1/4 1/4]
               [  2   2   3   4])
       (then
        (phrase [1/4 1/4 1/4 1/4]
                [  4   3   2   1]))
       (then
        (phrase [1/4 1/4 1/4 1/4]
                [  0   0   1   2]))
       (then
        (phrase [3/8 1/8 2/4]
                [  2   1   1]))
    ;; (then
    ;;            ; Gently down the stream,
    ;;    (phrase [2/3 1/3 2/3 1/3 6/3]
    ;;            [  2   1   2   3   4]))
    ;; (then
    ;;            ; Merrily, merrily, merrily, merrily,
    ;;    (phrase (repeat 12 1/3) 
    ;;            (mapcat (partial repeat 3) [7 4 2 0])))
    ;; (then
    ;;            ; Life is but a dream!
    ;;    (phrase [2/3 1/3 2/3 1/3 6/3] 
    ;;            [  4   3   2   1   0]))
    (where :part (is :sop1))))
(def countermelody-ode 
               
  (->> (phrase [1/4 1/4 1/4 1/4]
               [  0   0   1   2])
       (then
        (phrase [1/4 1/4 1/4 2/12 1/12]
                [  2   1   0   0 1]))
       (then
        (phrase [1/4 1/4 1/4 1/4]
                [  2   2   -3   0]))
       (then
        (phrase [3/8 1/8 2/4]
                [  0   -1   -1]))
    ;; (then
    ;;            ; Gently down the stream,
    ;;    (phrase [2/3 1/3 2/3 1/3 6/3]
    ;;            [  2   1   2   3   4]))
    ;; (then
    ;;            ; Merrily, merrily, merrily, merrily,
    ;;    (phrase (repeat 12 1/3) 
    ;;            (mapcat (partial repeat 3) [7 4 2 0])))
    ;; (then
    ;;            ; Life is but a dream!
    ;;    (phrase [2/3 1/3 2/3 1/3 6/3] 
    ;;            [  4   3   2   1   0]))
    (where :part (is :alto1))))

(def countermelody-ode-alto2
               
  (->> (phrase [1/4 1/4 1/4 1/4]
               [  0   0   0   0])
       (then
        (phrase [1/4 1/4 1/4 2/12 1/12]
                [  -3   -3   -3   -2 -1]))
       (then
        (phrase [1/4 1/4 1/4 1/4]
                [  0   -3   -3   -3]))
       (then
        (phrase [3/8 1/8 2/4]
                [  -3   -3   -3]))
    ;; (then
    ;;            ; Gently down the stream,
    ;;    (phrase [2/3 1/3 2/3 1/3 6/3]
    ;;            [  2   1   2   3   4]))
    ;; (then
    ;;            ; Merrily, merrily, merrily, merrily,
    ;;    (phrase (repeat 12 1/3) 
    ;;            (mapcat (partial repeat 3) [7 4 2 0])))
    ;; (then
    ;;            ; Life is but a dream!
    ;;    (phrase [2/3 1/3 2/3 1/3 6/3] 
    ;;            [  4   3   2   1   0]))
    (where :part (is :alto1))))

(def bass "A bass part to accompany the melody."
  (->> (phrase [1  1 2]
               [0 -3 0])
     (where :part (is :bass))
     (times 4)))

(defn row-row
  "Play the tune 'Row, row, row your boat' as a round."
  [speed key]
  (->> melody
    (with bass)
    (times 2)
    (canon (comp (simple 4)
                 (partial where :part (is :follower))))
    (where :time speed)
    (where :duration speed)
    (where :pitch key)
    play))

(defn ode-to-joy

  [speed key]
  (->> melody-ode
    (with countermelody-ode)
    (with countermelody-ode-alto2)
    (times 1)
      (where :time speed)
    (where :duration speed)
    (where :pitch key)
    play))

(comment
  (row-row (bpm 120) (comp C sharp major))
  (row-row (bpm 90) (comp low B flat minor))
  (ode-to-joy (bpm 50) (comp C major))
)
