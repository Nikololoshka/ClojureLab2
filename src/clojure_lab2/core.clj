(ns clojure-lab2.core
    (:require [clojure.core.async :refer [>! <! <!! chan go go-loop close! put!]]))

(defn vec-to-channel
    "Делает из вектора канал с данными."
    [vec]
    (let [channel (chan 10)]
        (go
            (doseq [x vec]
                (>! channel x))
            (close! channel))
        channel))

(defn collect-channel
    "Асинхронно берет значения из канала и записывает их в вектор,
     который затем передает как строку в другой (выходной) канал."
    [channel number output]
    (go-loop [v []]
        (if-let [x (<! channel)]
            (recur (conj v x))
            (>! output (str "С остатком " number " = " v)))
        ))

(defn split-div-channel
    "Принимает на вход канал из чисел и число,
     и возвращает массив из каналов,в которые будут распределяться
     значения из первого канала в зависимости от остатка
     от деления на переданное число."
    [channel number]
    (let [channels (repeatedly number #(chan 10))]
        (go-loop []
            (if-let [data (<! channel)]
                (do
                    ;; (println "Put: " data)
                    (>! (nth channels (mod data number)) data)
                    (recur))
                (doseq [c channels]
                    (close! c))))
        channels))

(defn -main [& _]
    (let [numbers (vec-to-channel (range 0 200))
          mod-number 7
          channels (split-div-channel numbers mod-number)
          result-channel (chan 5)]

        (doseq [[number channel] (map-indexed vector channels)]
            (collect-channel channel number result-channel))

        ;; сбор результатов из каналов
        (<!! (go-loop [i 0]
                 (if (< i (count channels))
                     (if-let [data (<! result-channel)]
                         (do
                             (println data)
                             (recur (inc i)))
                         (close! result-channel))
                     (close! result-channel))))
        )
    )
