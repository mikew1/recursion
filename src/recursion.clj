(ns recursion)

(defn product [coll]
  (if (empty? coll)
      1
      (* (first coll)
         (product (rest coll)))))   ; linear recursion: expr. constructed grows linearly w. size of input:

; (product [1 2 4])
; = (product (cons 1 (cons 2 (cons 4 '())))); outer call
;   (* 1 (product (cons 2 (cons 4 '()))))   ; first recurs
;   (* 1 (* 2 (product (cons 4 '()))        ; second recurs
;   (* 1 (* 2 (* 4 '())))                   ; third recurs
;   (* 1 (* 2 (* 4 1)))                     ; third return (empty? '()) is true, so = 1
;   (* 1 (* 2 4))                           ; second return
;   (* 1 8)                                 ; first return
;   8                                       ; outer return

(defn singleton? [coll]
 (if (empty? coll) 
     false                   ; looks like a base case
     (empty? (rest coll))))  ; but not recursive

(defn my-last [a-seq]
  (if (empty? a-seq)
      nil                    ; specific edge case as per spec
      (if (singleton? a-seq)
        (first a-seq)
        (my-last (rest a-seq)))))

(defn max-element [a-seq]
  (let [ max-el (fn mx [m rem]   ; inner recursive function that holds the carrying max val.
                  (if (empty? rem)
                      m
                      (mx (max m (first rem))
                          (rest rem)))) ]
    (if (empty? a-seq)
        nil                  ; max will fail with null pointer if passed nil, so need check empty & reply nil
        (max-el 0 a-seq))))  ; start with 0 as current max value
                             ; need to understand use of nil

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
      seq-1
      seq-2))

(defn longest-sequence [a-seq]
  (let [ longest (fn lng [l rem]   ; identical form to above; inner recursive fn with carry.
                   (if (empty? rem)
                       l
                       (lng (seq-max l (first rem)) 
                            (rest rem)) )) ]
    (if (empty? a-seq) ; best way to write it: write just signature of inner fn then write this part first.
        nil
        (longest [] a-seq))))


(defn my-filter [pred? a-seq]  ; cf. clj source
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)     ; this time, instead of just returning result of recursive call, we cons to it.
            (my-filter pred? (rest a-seq)))  ; that is, once the calls exit, we cons to them.
      (my-filter pred? (rest a-seq)))))


(defn sequence-contains? [elem a-seq]
  (cond 
    (empty? a-seq)
      false
    (= elem (first a-seq))            ; may not descend all the way, no need recurse further
      true                            ; true goes up the stack
    :else                           
      (sequence-contains? elem (rest a-seq))))             


(defn my-take-while [pred? a-seq]     
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq)                ; for take while, if condition passes, cons it to recurs on rest.
            (my-take-while pred? (rest a-seq)))
      '())))                             ; if condition fails, stop looking, stop recursing, & return.

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq)) ; for drop, if cond passes, just call with rest (obv. no cons)
      a-seq)))                           ; if condition fails, just return rest of seq.

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))   ; could make fns both-empty?, not-nil? to simplify.
      true
    (and (not (nil? (first a-seq)))       ; (first []) and (first [nil]) both give nil,
         (not (nil? (first b-seq)))       ;  passing (= nil nil), which we don't want.
         (= (first a-seq) (first b-seq))) 
      (seq= (rest a-seq) (rest b-seq))    ; 
    :else
      false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()                                   ; cons needs a list to cons to somewhere, & this is it.
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (zero? n) 0                            ; template for recursion on numbers
   (zero? k) 1                            ; if (zero? n) ...    note diff. from tpl for recursion on seqs.
   :else
   (* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ 
            (fib (- n 1))                 ; tree recursion, refers to shape of the computation
            (fib (- n 2)))))              ; natural for hierarchical ds's, here, numbers.
                                          ; tree because each non-terminal comp splits into two; 2 calls to self.

                                          ; use recursion to implement all these fns from clojure.core
(defn my-repeat [n what]
  (if (<= n 0)
    '()
    (cons what (my-repeat (dec n) what))))

(defn my-range [up-to]
  (if (zero? up-to) 
    '()
    (cons (- up-to 1) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (= 0 (count a-seq))
    '( ())
    (cons a-seq (tails (rest a-seq)))))  ; add (reverse (into '() a-seq)) if want vec to list

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq)))) ; to recurs, need to spot where can use first & rest. here, use reverse.

(defn rotations [a-seq]
  (let [ rot (fn r [s c] (if (zero? c)  ; very unreadable, text implies maybe can be done w/o helper fn.
                           '()          ; one test missed here may be artifact of expectation to not use helper..?
                           (cons s (r (concat (rest s) (list (first s))) (dec c)))))]
    (rot a-seq (count a-seq))))        


(defn my-frequencies-helper [freqs a-seq]  ; recursive def with helper (called by main fn blw); already done abv.
  (if (empty? a-seq)
    freqs
    (if (contains? freqs (first a-seq))    ; works, but need big change in skill in incing a map el, though.
      ; you can't assoc then return a mutated freqs in next expression, must assoc in the one you return: 
      (my-frequencies-helper (assoc freqs (first a-seq) (inc (get freqs (first a-seq)))) (rest a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq)                               1) (rest a-seq)))))

(defn my-frequencies [a-seq]               
  (my-frequencies-helper {} a-seq))        ; extra param that we assoc into; need to inc existing el., though.

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [ el    (get (first a-map) 0)
           times (get (first a-map) 1) ]
      (concat (repeat times el) 
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [ h (int (/ (count a-seq) 2)) ]    
    [(my-take h a-seq) (my-drop h a-seq)]))


(defn seq-merge [a-seq b-seq] 
  (cond
   (and (empty? a-seq) (empty? b-seq))
     '()
   (empty? a-seq)
     (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
   (empty? b-seq)
     (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   (< (first a-seq) (first b-seq))
     (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else
     (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))



(defn merge-sort [a-seq]  ; seq-merge is the key thing in merge sort,
  (if (< (count a-seq) 2) ; merging [1] and [2] gives [1 2], not [2 1]
    a-seq                 ; the sort happens there, at the lowest level.
    (apply seq-merge (map merge-sort (halve a-seq)))))


; Meant to be tricky; skipped for now
(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

