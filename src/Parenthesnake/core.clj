(ns Parenthesnake.core
  (:require [clojure.java.io :as io])
  (:import (javax.swing JPanel JFrame Timer JOptionPane ImageIcon)
           (java.awt.event KeyEvent ActionListener KeyListener)))


(defn show-msg-dialog []
  (let [img-icon (ImageIcon. (io/resource "chicken.gif"))
        optionpane    (JOptionPane/showMessageDialog
                        nil "Go eat something!" "Game Over!"
                        JOptionPane/INFORMATION_MESSAGE
                        img-icon)]
    (System/exit 0)))



(defn balanced?
  ([expr] (balanced? (clojure.string/split expr #"") 0))
  ([[x & xs] count]
   (cond (neg? count) false
         (nil? x) (zero? count)
         (= x "(") (recur xs (inc count))
         (= x ")") (recur xs (dec count))
         :else (recur xs count))))


(defn gen-single-paren [] [(rand-int 600) (rand-int 300) (if (= 1 (rand-int 2)) "(" ")") ])
(defn gen-scattered-parens []  (take 10 (repeatedly gen-single-paren)))

(def paren-str (ref  ""))
(def snake (ref {:body (list [10 10]) :dir [10 0]}))
(def parens (ref (gen-scattered-parens)))
(def dirs {KeyEvent/VK_LEFT [-10 0] KeyEvent/VK_RIGHT [10 0]
           KeyEvent/VK_UP   [0 -10] KeyEvent/VK_DOWN  [0 10]})

(defn move [{body :body dir :dir :as snake} & [grow]]
  (into snake {:body (cons (vec (map #(+ (dir %) ((first body) %)) [0 1]))
                           (if grow body (butlast body)))}))

(defn turn [snake newdir] (if newdir {:body (:body snake) :dir newdir} snake))


(defn collision? [ a]
  (let [[b] (:body @snake)]
    (every? #(<= (- (a %) 10) (b %) (+ 10 (a %))) [0 1])))

(defn activate [c-paren]
  (dosync (ref-set paren-str (str @paren-str c-paren))
          (ref-set parens (gen-scattered-parens))
          (alter snake move true)))


;receives the collided paren str
(defn handle-collision [c-paren]
  (cond
    (= "(" c-paren)  (activate c-paren)
    (balanced? (str @paren-str c-paren)) (activate c-paren)
    :else (show-msg-dialog)))

(let [panel (proxy [JPanel ActionListener KeyListener] []
              (paintComponent [g] (proxy-super paintComponent g)
                ;draw with seq
                (doseq [pr @parens]
                  (.drawString g (pr 2) (pr 0) (pr 1)))
                (doseq [b (:body @snake)]
                  (.fillRect g (b 0) (b 1) 10 10))
                (when-let [collided-paren (first (filter collision? @parens))] (handle-collision (collided-paren 2)))
                (.drawString g @paren-str 20 20))
              (actionPerformed [e] (dosync (alter snake move)) (.repaint this))
              (keyPressed [e] (dosync (alter snake turn (dirs(.getKeyCode e)))))
              (keyReleased [e])
              (keyTyped [e]))]
  (doto panel (.setFocusable true) (.addKeyListener panel))
  (doto (JFrame. "Parenthesnake Game") (.add panel) (.setSize 800 450) (.setVisible true))
  (.start (Timer. 100 panel)))


