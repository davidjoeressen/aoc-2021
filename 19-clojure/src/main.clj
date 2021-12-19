(ns main
  (:gen-class)
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (mapv #(Integer/parseInt %) (str/split line #",")))

(defn all-relative-edges [verts]
  (->> verts
       (mapcat (fn [x] (map (fn [y] (map - x y)) verts)))
       (filter #(not (every? zero? %)))))

(defn matrix-multiplication [m1 m2]
  (mapv #(apply + (mapv * m2 %)) m1))

(defn rotation-matrix [a b c]
  (let [sina (Math/round (Math/sin a)) sinb (Math/round (Math/sin b)) sinc (Math/round (Math/sin c))
        cosa (Math/round (Math/cos a)) cosb (Math/round (Math/cos b)) cosc (Math/round (Math/cos c))]
    (vector (vector (* cosa cosb) (- (* cosa sinb sinc) (* sina cosc)) (+ (* cosa sinb cosc) (* sina sinc)))
            (vector (* sina cosb) (+ (* sina sinb sinc) (* cosa cosc)) (- (* sina sinb cosc) (* cosa sinc)))
            (vector (- sinb) (* cosb sinc) (* cosb cosc)))))

(def rotation-matrices
  (let [four (map #(* Math/PI %) '(0 1/2 1 3/2))
        two  (map #(* Math/PI %) '(1/2 3/2))]
    (concat (mapcat (fn [a] (map #(rotation-matrix a 0 %) four)) four)
            (mapcat (fn [b] (map #(rotation-matrix 0 b %) four)) two))))

(defn rotations [verts]
  (map (fn [rot] (map #(matrix-multiplication rot %) verts)) rotation-matrices))

(defn read-file [filename]
  (as-> filename $
        (slurp $)
        (str/split $ #"\n\n")
        (map (comp #(map parse-line %) rest str/split-lines) $)))

(defn translations [x y]
  (mapcat (fn [a] (map #(mapv - a %) y)) x))

(defn overlap [x y]
  (let [tr (translations x y)]
    (filter #(<= 12 (count (filter (partial = %) tr))) (distinct tr))))

(defn overlapping-rotation [x y]
  (->> (rotations y)
       (map #(list % (first (overlap x %))))
       (filter second)
       (first)
       (#(if % % (list y nil)))))

(defn translate [verts tr]
  (map #(mapv + tr %) verts))

(defn get-matches [x xs]
  (let [candidates (map #(overlapping-rotation x %) xs)
        hits (filter second candidates)
        misses (filter #(nil? (second %)) candidates)]
    (list hits (map first misses))))

(defn first-matches [done todo]
  (->> done
       (map #(get-matches % todo))
       (filter #(seq (first %)))
       (first)))

(defn match-up [scanners]
  (loop [done (list (first scanners))
         beacons '([0 0 0])
         todo (rest scanners)]
    (if (empty? todo)
      (list done beacons)
      (let [match (first-matches done todo)]
        (recur (concat done (map (partial apply translate) (first match)))
               (concat beacons (map second (first match)))
               (second match))))))

(defn distance [x y]
  (apply + (map (fn [a b] (Math/abs (- a b))) x y)))

(defn biggest-distance [scanners]
  (->> scanners
       (mapcat (fn [x] (map (partial distance x) scanners)))
       (apply max)))

(defn solve [filename]
  (let [result (->> (read-file filename) (match-up))]
    (do (println filename)
        (print "Part1: ")
        (println (->> result (first) (apply concat) (distinct) (count)))
        (print "Part2: ")
        (println (->> result (second) (biggest-distance))))))

(defn -main [& args]
  (if (empty? args)
    (println "must provide filename")
    (dorun (map solve args))))
