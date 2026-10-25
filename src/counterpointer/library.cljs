(ns counterpointer.library
  (:require [counterpointer.theory :as t]))

;; Cantus firmi from Fux, Gradus ad Parnassum (1725), one per mode.

(def fux
  [{:name "Fux · D Dorian" :key {:mode :dorian :final {:l 1 :a 0}}
    :cf "D4 F4 E4 D4 G4 F4 A4 G4 F4 E4 D4"}
   {:name "Fux · E Phrygian" :key {:mode :phrygian :final {:l 2 :a 0}}
    :cf "E4 C4 D4 C4 A3 A4 G4 E4 F4 E4"}
   {:name "Fux · F Lydian" :key {:mode :lydian :final {:l 3 :a 0}}
    :cf "F4 G4 A4 F4 D4 E4 F4 C5 A4 F4 G4 F4"}
   {:name "Fux · G Mixolydian" :key {:mode :mixolydian :final {:l 4 :a 0}}
    :cf "G4 C5 B4 G4 C5 E5 D5 G5 E5 C5 D5 B4 A4 G4"}
   {:name "Fux · A Aeolian" :key {:mode :aeolian :final {:l 5 :a 0}}
    :cf "A3 C4 B3 D4 C4 E4 D4 C4 B3 A3"}
   {:name "Fux · C Ionian" :key {:mode :ionian :final {:l 0 :a 0}}
    :cf "C4 E4 F4 G4 E4 A4 G4 E4 F4 E4 D4 C4"}])

(def example
  {:name "Example solution · D Dorian, above" :key {:mode :dorian :final {:l 1 :a 0}}
   :cf "D4 F4 E4 D4 G4 F4 A4 G4 F4 E4 D4"
   :cp "D5 C5 G4 A4 B4 D5 C5 E5 D5 C#5 D5"})

(defn line [key text] (:line (t/parse-line key text 28)))
