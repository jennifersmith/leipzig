(ns leipzig.test.temperament
  (:use midje.sweet [leipzig.temperament :as temperament]))

(fact "Concert A is 440 Hz."
  (temperament/equal 69)       => 440.0
  ((temperament/pythagorean 69) 69) => 440
  ((temperament/pythagorean 70) 69) => 440) 

(fact "An equal temperament octave (roughly) doubles the frequency."
  (temperament/equal (+ 69 12)) => (roughly 880) 
  (temperament/equal (- 69 12)) => (roughly 220))

(fact "A equal temperament perfect fifth is flat by about two cents."
  (let [cent (java.lang.Math/pow 2 1/1200)]
    (/ (* 3/2 (temperament/equal 69)) (temperament/equal (+ 69 7)))
      => (roughly (java.lang.Math/pow cent 1.96))
    (/ (temperament/equal (- 69 7)) (* 2/3 (temperament/equal 69)))
      => (roughly (java.lang.Math/pow cent 1.96))))

(fact "An equal temperament major third is sharp by about 14 cents."
  (let [cent (java.lang.Math/pow 2 1/1200)]
    (/ (temperament/equal (+ 69 4)) (* 5/4 (temperament/equal 69)))
      => (roughly (java.lang.Math/pow cent 13.69))
    (/ (* 4/5  (temperament/equal 69)) (temperament/equal (- 69 4)))
      => (roughly (java.lang.Math/pow cent 13.69))))

(fact "An equal temperament semitone is the twelfth root of two."
  (/ (temperament/equal 70) (temperament/equal 69)) =>
    (roughly (java.lang.Math/pow 2 1/12))
  (/ (temperament/equal 69) (temperament/equal 68)) =>
    (roughly (java.lang.Math/pow 2 1/12)))

(fact "Pythagorean temperament has ideal fifths, fourths and octaves."
  (/ ((temperament/pythagorean 69) (+ 69 6))  ((temperament/pythagorean 69) 69)) => 3/2
  (/ ((temperament/pythagorean 69) (- 69 7))  ((temperament/pythagorean 69) 69)) => 2/3
  (/ ((temperament/pythagorean 69) (+ 69 4))  ((temperament/pythagorean 69) 69)) => 4/3 
  (/ ((temperament/pythagorean 69) (- 69 5))  ((temperament/pythagorean 69) 69)) => 3/4 
  (/ ((temperament/pythagorean 69) (+ 69 11)) ((temperament/pythagorean 69) 69)) => 2/1
  (/ ((temperament/pythagorean 69) (- 69 11)) ((temperament/pythagorean 69) 69)) => 1/2)