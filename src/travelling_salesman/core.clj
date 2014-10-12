(ns travelling-salesman.core
  (:require [clojure.algo.generic.math-functions :refer :all])
  (:gen-class))

(def villes [{:name "Paris" :lat 48.8575 :lon 2.3458}
             {:name "Lyon" :lat 45.7564 :lon 4.8333}
             {:name "Marseille" :lat 43.2815 :lon 5.3751}
             {:name "Toulouse" :lat 43.6013 :lon 1.4375}
             {:name "Clermont-ferrand" :lat 45.7824 :lon 3.0987}
             {:name "Montpellier" :lat 43.6067 :lon 3.8763}
             {:name "Bordeaux" :lat  44.8521 :lon -0.5852}
             {:name "Poitiers" :lat 44.8521 :lon -0.585}
             {:name "Nantes" :lat 47.2238 :lon -1.5557}
             {:name "Rennes" :lat 48.1138 :lon -1.6806}
             {:name "Caen" :lat 49.1832 :lon -0.3710}
             {:name "Rouen" :lat 49.4440 :lon 1.0894}
             {:name "Lille" :lat 50.6310 :lon 3.0561}
             {:name "Amiens" :lat 49.8968 :lon 2.2931}
             {:name "Reims" :lat 49.2542 :lon 4.0452}
             {:name "Nancy" :lat 48.67301 :lon 6.17070}
             {:name "Strasbourg" :lat 48.59759 :lon 7.76836}
             {:name "Dijon" :lat 47.3265 :lon 5.0398}
             {:name "Besançon" :lat 47.2419 :lon 6.0221}
             {:name "Ajaccio" :lat 41.9254 :lon 8.7311}
             {:name "Limoges" :lat 45.8390 :lon 1.2539}
             {:name "Orléans" :lat 47.8886 :lon 1.9140}])

;useful constants
(def pi 3.1411592653596)
(def radius 6378)

(defn degrees->rad [angle]
  "convertit les degrés en radians"
  ((comp
      #(* pi %)
      #(/ % 180.0))
   angle))

(defn dist [ville1 ville2]
  "calcule la distance entre deux villes"
  (let [lat1 (degrees->rad (get ville1 :lat))
        lat2 (degrees->rad (get ville2 :lat))
        lon1 (degrees->rad (get ville1 :lon))
        lon2 (degrees->rad (get ville2 :lon))]
    (* radius (acos
               (reduce + [(reduce * [(cos lat1) (cos lon1) (cos lat2) (cos lon2)])
                          (reduce * [(cos lat1) (sin lon1) (cos lat2) (sin lon2)])
                          (* (sin lat1) (sin lat2))])))))

(defn randomized-cities
  "take a vector of cities and return this vector in a random order
  except for the first and last positions that are unchanged"
  [vecVilles]
  ; let's use the magical shuffle
  (let [x (first vecVilles)
        y (last vecVilles)
        inner (butlast (rest vecVilles))]
    (vec (cons x (conj (shuffle inner) y)))))

(defn name->ville
  "takes a string reprenting the name of city and gets
  the corresponding map in the vector of cities called villes"
  ([nom]
   (name->ville nom villes))
  ([nom villes]
  (let [[head & tail] villes]
    (if (= (get head :name) nom)
      head
      (name->ville nom tail)))))

(defn cost
  "takes a list of cities (ordered) and returns the total length of a trip
  by the travelling salesman when he goes from city to city in the same order
  as in the list"
  [listVilles]
  (let [[x & xs] listVilles]
    (if (empty? xs)
      0
      (let [ depart (name->ville x)
             arrivee (name->ville (first xs))]
        (+ (dist depart arrivee) (cost xs))))))


(defn accept?
  "check if we should accept the new draw"
  [nouveau ancien temperature]
  (let [oldCost (cost ancien)
        newCost (cost nouveau)]
    (if (and (or (> oldCost newCost)
            (< (rand 1) (exp (/ (- oldCost  newCost) temperature))))
            (not (= (reverse nouveau) ancien)))
        true
        false)))

(defn simulated-annealing
  "perform the simulated annealing routine,
  e.g. optimizing the travel between cities"
  [cities init-temp cooling-speed max-iteration]
  (spit "optimization-results.txt" "Here are the results")
  (loop [x 0
         temperature init-temp
         result cities]
   (spit "optimization-results.txt"
         (str "Iteration: " x ", temperature: " temperature ",result: " result ", cost: " (cost result) "\n")
         :append :true)
   (if (or (> x max-iteration) (< temperature 1))
     [result (cost result)]
     (let [nouveau (randomized-cities cities)]
       (if (accept? nouveau result temperature)
          (recur (inc x)
                 (* cooling-speed temperature)
                 nouveau)
          (recur (inc x)
                 (* cooling-speed temperature)
                 result))))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Les villes connues sont: ")
  (println (map #(get % :name) villes))
  (println "Indiquez les villes que vous souhaitez visiter dans cities.txt")
  (println "format de saisie [\"Départ\" \"Ville1\" \"Ville2\" ... \"VilleN\" \"Arrivée\"]")
  (println "exemple: [\"Lille\" \"Paris\" \"Lyon\" \"Marseille\" \"Toulouse\" \"Lille\"]")
  (println "then press any key")
  (def junk (read-line))
  (def cities (slurp "cities.txt"))
  (println (str "Here are the cities you chose: " cities))
  (def results (simulated-annealing (read-string cities) 1000 0.95 10000))
  (println "The optimal trip is: ")
  (println (first results))
  (println "The total distance covered by the trip is: ")
  (println (str (second results) "km")))
