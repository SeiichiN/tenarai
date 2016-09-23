;; Scheme手習い
;; p16
;; atomであるかどうかを判定
(define lat?
          (lambda (l)
            (cond
              ((null? l) #t)
              ((atom? (car l))(lat? (cdr l)))
              (else #f))))

;; p23
(define member?
          (lambda (a lat)
            (cond
              ((null? lat) #f)
              (else (or (equal? (car lat) a)
                        (member? a (cdr lat)))))))

;; p35
;(define rember
;  (lambda (a lat)
;    (cond
;     ((null? lat)(quote ()))
;     (else (cond
;            ((eq? (car lat) a)(cdr lat))
;            (else (cons (car lat)
;                        (rember a (cdr lat)))))))))

;; p46
(define firsts
  (lambda (l)
    (cond
     ((null? l) (quote()) )
     (else (cons (car (car l))
                 (firsts (cdr l)))))))

;; p52
;(define insertR
;  (lambda (new old lat)
;    (cond
;     ((null? lat)(quote ()))
;     (else
;      (cond
;       ((eq? (car lat) old)
;        (cons old (cons new (cdr lat))))
;       (else (cons (car lat)
;                   (insertR new old (cdr lat)))))))))

;; 52
;(define insertL
;          (lambda (new old lat)
;            (cond
;              ((null? lat)(quote()))
;              (else (cond
;                      ((eq? (car lat) old)
;                       (cons new lat))
;                      (else (cons (car lat)
;                                  (insertL new old (cdr lat)))))))))

;; 53
;; リストlatの中のoldをnewで置き換える
(define subst
          (lambda (new old lat)
            (cond
              ((null? lat)(quote()))
              (else (cond
                      ((eq? (car lat) old)
                       (cons new (cdr lat)))
                      (else (cons (car lat)
                                  (subst new old (cdr lat)))))))))

;; 53
(define subst2
          (lambda (new o1 o2 lat)
            (cond
              ((null? lat)(quote()))
              (else (cond
                      ((or (eq? (car lat) o1)(eq? (car lat) o2))
                       (cons new (cdr lat)))
                      (else (cons (car lat)
                                  (subst2 new old (cdr lat)))))))))

;; p54
(define multirember
  (lambda (a lat)
    (cond
     ((null? lat)(quote ()))
     (else (cond
            ((equal? (car lat) a)(multirember a (cdr lat)))
            (else (cons (car lat) (multirember a (cdr lat)))))))))

;; p57
(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat)(quote ()))
     (else
      (cond
       ((eq? (car lat) old)
        (cons old (cons new (multiinsertR new old (cdr lat)))))
       (else (cons (car lat)
                   (multiinsertR new old (cdr lat)))))))))

;; 58
(define multiinsertL
          (lambda (new old lat)
            (cond
              ((null? lat)(quote()))
              (else (cond
                      ((eq? (car lat) old)
                       (cons new (cons old (multiinsertL new old (cdr lat)))))
                      (else (cons (car lat)
                                  (multiinsertL new old (cdr lat)))))))))

;; 59
(define multisubst
          (lambda (new old lat)
            (cond
              ((null? lat)(quote()))
              (else (cond
                      ((eq? (car lat) old)
                       (cons new (multisubst new old (cdr lat))))
                      (else (cons (car lat)
                                  (multisubst new old (cdr lat)))))))))

;; p61
(define add1
          (lambda (n)
            (+ n 1)))

(define sub1
          (lambda (n)
            (- n 1)))

;;p62
(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (o+ n (sub1 m)))))))

;; p63
(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (o- n (sub1 m)))))))

;; p66
(define addtup
          (lambda (tup)
            (cond
              ((null? tup) 0)
              (else (o+ (car tup)(addtup (cdr tup)))))))
;; 67
(define x
          (lambda (n m)
            (cond
              ((zero? m) 0)
              (else (o+ n (x n (sub1 m)))))))


;; p73
(define tup+
          (lambda (tup1 tup2)
            (cond
              ((null? tup1) tup2)
              ((null? tup2) tup1)
              (else
               (cons (o+ (car tup1)(car tup2))
                     (tup+
                      (cdr tup1)(cdr tup2)))))))


;; p74
(define >
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (> (sub1 n)(sub1 m))))))

;; p75
(define <
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (< (sub1 n)(sub1 m))))))

;; p76
(define =
          (lambda (n m)
            (cond
              ((> n m) #f)
              ((< n m) #f)
              (else #t))))

;; p76
(define o^
          (lambda (n m)
            (cond
              ((zero? m) 1)
              (else (x n (o^ n (sub1 m)))))))

;; p77
(define waru
          (lambda (n m)
            (cond
              ((< n m) 0)
              (else (add1 (waru (o- n m) m))))))

;; p78
(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (length (cdr lat)))))))

;;
(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n))(car lat))
     (else
      (pick (sub1 n)(cdr lat))))))

;; p78
(define rempick
          (lambda (n lat)
            (cond
              ((zero? (sub1 n)) (cdr lat))
              (else
               (cons (car lat)(rempick (sub1 n)(cdr lat)))))))

;; p79
(define no-nums
  (lambda (lat)
    (cond
     ((null? lat)(quote ()))
     ((number? (car lat))(no-nums (cdr lat)))
     (else
      (cons (car lat)(no-nums (cdr lat)))))))

;; p80
(define all-nums
  (lambda (lat)
    (cond
     ((null? lat)(quote ()))
     ((number? (car lat))
       (cons (car lat)(all-nums (cdr lat))))
     (else
      (all-nums (cdr lat))))))

;; p80
(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1)(number? a2))
      (= a1 a2))
     ((or (number? a1)(number? a2)) #f)
     (else
      (eq? a1 a2)))))

;; p80
(define occur
          (lambda (a lat)
            (cond
              ((null? lat) 0)
              (else
               (cond
                 ((eqan? a (car lat))
                  (add1 (occur a (cdr lat))))
                 (else
                  (occur a (cdr lat))))))))

;; p81
(define one?
  (lambda (n)
    (= n 1)))


;; p81
(define rempick
          (lambda (n lat)
            (cond
              ((one? n) (cdr lat))
              (else
               (cons (car lat)(rempick (sub1 n)(cdr lat)))))))

;; p83
;; リストｌからａをとりのぞいたリストをつくる
(define rember*
  (lambda (a l)
    (cond
     ((null? l)(quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) a)(rember* a (cdr l)))
       (else
        (cons (car l)(rember* a (cdr l))))))
     (else
      (cons (rember* a (car l))(rember* a (cdr l)))))))

(define atom?
  (lambda (x)
    (and (not (pair? x))(not (null? x)))))

;; p84
;; oldの右にアトムnewを入れる関数
(define insertR*
  (lambda (new old l)
    (cond
     ((null? l)(quote ()))
     ((atom? (car l))
      (cond
       ((eq? old (car l))
        (cons old
              (cons new
                    (insertR* new old (cdr l)))))
       (else
        (cons (car l)
              (insertR* new old (cdr l))))))
     (else
      (cons (insertR* new old (car l))
            (insertR* new old (cdr l)))))))

;; p86
;; 引数リストlの中の引数aの個数を求める。
(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? a (car l))
        (add1 (occur* a (cdr l))))
       (else
        (occur* a (cdr l)))))
     (else
      (o+ (occur* a (car l))
          (occur* a (cdr l)))))))

;; p87
;; リストlの中のoldをnewに置き換える
(define subst*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? old (car l))
        (cons new (subst* new old (cdr l))))
       (else
        (cons (car l) (subst* new old (cdr l))))))
     (else
      (cons (subst* new old (car l))
            (subst* new old (cdr l)))))))

;; p89
;; リストlの中のoldの左にnewを入れる。
(define insertL*
  (lambda (new old l)
    (cond
     ((null? l)(quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons new (cons (car l)(insertL* new old (cdr l)))))
       (else
        (cons (car l)(insertL* new old (cdr l))))))
     (else
      (cons (insertL* new old (car l))
            (insertL* new old (cdr l)))))))

;; p89
;; リストの中にaが含まれていたら#tを返す
(define member*
  (lambda (a l)
    (cond
     ((null? l) nil)
     ((atom? (car l))
      (or
       (eq? (car l) a) 
       (member* a (cdr l))))
     (else
      (or (member* a (car l))
          (member* a (cdr l)))))))

(define nil '())

;; p90
;; 空でないリストの一番左にあるアトムを見つける。
(define leftmost
          (lambda (l)
            (cond
              ((atom? (car l))(car l))
              (else (leftmost (car l))))))

;; p94
;; リストl1とl2がまったく等しければ#tを返す
;(define eqlist?
;  (lambda (l1 l2)
;    (cond
;     ((and (null? l1)(null? l2)) #t)
;     ((or (null? l1)(null? l2)) #f)
;     ((and (atom? (car l1))(atom? (car l2)))
;      (and (eqan? (car l1)(car l2))
;           (eqlist? (cdr l1)(cdr l2))))
;     ((or (atom? (car l1))(atom? (car l2))) #f)
;     (else
;      (and (eqlist? (car l1)(car l2))
;           (eqlist? (cdr l1)(cdr l2)))))))

;; p96
;; s1とs2が同じなら#tを返す
(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1)(atom? s2))
      (eqan? s1 s2))
     ((or(atom? s1)(atom? s2)) #f)
     (else (eqlist? s1 s2)))))

;; p94
;; リストl1とl2がまったく等しければ#tを返す
(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1)(null? l2)) #t)
     ((or (null? l1)(null? l2)) #f)
     (else
      (and (equal? (car l1)(car l2))
           (eqlist? (cdr l1)(cdr l2)))))))

;; p97
;; リストlの中のS式sを取り除く
;; p35のremberをアトム対象からS式対象に変更
(define rember
  (lambda (s l)
    (cond
     ((null? l)(quote ()))
     ((equal? (car l) s)(cdr l))
     (else
      (cons (car l)
            (rember s (cdr l)))))))

;; p103
;; aexpが算術式であることがわかっているとき
;; aexpが数値であることを確認する
(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp)(numbered? aexp))
     (else
      (and (numbered? (car aexp))
           (numbered? (car (cdr (cdr aexp)))))))))

;; p105
;; プラス、かける、累乗についての算術式を表す
(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car (cdr nexp))(quote o+))
      (o+ (value (car nexp))(value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp))(quote x))
      (x (value (car nexp))(value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp))(quote o^))
      (o^ (value (car nexp))(value (car (cdr (cdr nexp))))))
     (else
      (display "Noe Susiki.\n")))))

;; p107 - p108
;; 算術式
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2st-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))


;; p109
;; ゼロかどうか調べる
(define sero?
  (lambda (n)
    (null? n)))

;; p113
;; 集合かどうかを調べる
(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat)(cdr lat)) #f)
     (else (set? (cdr lat))))))


;; p114
;; 集合をつくる
(define makeset
  (lambda (lat)
    (cond
     ((null? lat)(quote ()))
     ((member? (car lat)(cdr lat))
      (makeset (cdr lat)))
     (else
      (cons (car lat)(makeset (cdr lat)))))))

;; p114
;; multiremberを使ってmakesetを書く
(define mkset
  (lambda (lat)
    (cond
     ((null? lat)(quote ()))
     (else
      (cons (car lat)(mkset (multirember (car lat)(cdr lat))))))))

;; p115
;; 引数1の各アトムが引数２に含まれているか。
(define subset?
          (lambda (set1 set2)
            (cond
              ((null? set1) #t)
              (else
               (and
                (member? (car set1) set2)
                (subset? (cdr set1) set2))))))

;; p116
;; ２つの集合は等しいか？
(define eqset?
          (lambda (set1 set2)
            (and
             (subset? set1 set2)(subset? set2 set1))))

;; p117
;; set1の中の要素がひとつでもset2の中に含まれていれば#t
(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else
      (or(member? (car set1) set2)
         (interset? (cdr set1) set2))))))

;; p118
;; set1の中の要素とset2の要素との共通項を抜き出す
(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) nil)
     ((member? (car set1) set2)
      (cons (car set1)(intersect (cdr set1) set2)))
     (else
      (intersect (cdr set1) set2)))))

;; p118
;; set1の中の要素とset2の要素との和集合をつくる
(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else
      (cons (car set1)(union (cdr set1) set2))))))

;; p118  
(define xxx
  (lambda (set1 set2)
    (cond
     ((null? set1)(quote ()))
     ((member? (car set1) set2)
      (xxx (cdr set1) set2))
     (else
      (cons (car set1)(xxx (cdr set1) set2))))))

;; p119
;; 各リストの中で、共通なアトムを抜き出す
(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set))(car l-set))
     (else
      (intersect (car l-set)(intersectall (cdr l-set)))))))

;; p119
;; ペアであるかどうか
(define a-pair?
  (lambda (x)
    (cond
     ((null? x) #f)
     ((atom? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

;; p120
;; first second third build
(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

;; ペアをつくる
(define build
  (lambda (a1 a2)
    (cons a1 (cons a2 (quote ())))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

;; p122
(define fun?
  (lambda (rel)
    (set? (first rel))))

;; p122
;; 各リストを逆順にする
(define revrel
  (lambda (rel)
    (cond
     ((null? rel)(quote ()))
     (else
      (cons (revpair (car rel))
            (revrel (cdr rel)))))))

;; p123
;; ２つの各要素を交換する
(define revpair
  (lambda (pair)
    (build (second pair)(first pair))))

;; p123
;; 第２要素が集合かどうか
(define fullfun?
  (lambda (fun)
    (set? (second fun))))

;; p124
(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

;;p131
;; test? に=かeq?かequal?を使う
;; 関数に名前をつけることができる
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l)(quote ()))
       ((test? (car l) a)(cdr l))
       (else
        (cons (car l)((rember-f test?) a (cdr l))))))))

;; p129
;; カリー化
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

           
;; p132
(define insertL-f
  (lambda (test?)
          (lambda (new old lat)
            (cond
              ((null? lat)(quote()))
              (else (cond
                      ((test? (car lat) old)
                       (cons new (cons old (cdr lat))))
                      (else (cons (car lat)
                                  ((insertL-f test?) new old (cdr lat))))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
       ((null? lat)(quote ()))
       (else
        (cond
         ((test? (car lat) old)
          (cons old (cons new (cdr lat))))
         (else (cons (car lat)
                     ((insertR-f test?) new old (cdr lat))))))))))

(define seqL
  (lambda (new old lat)
    (cons new (cons old lat))))

(define seqR
  (lambda (new old lat)
    (cons old (cons new lat))))


(define insert-g
  (lambda (seq)
    (lambda (new old lat)
      (cond
       ((null? lat)(quote ()))
       ((eq? (car lat) old)
        (seq new old (cdr lat)))
       (else
        (cons (car lat)((insert-g seq) new old (cdr lat))))))))

(define seqL
  (lambda (new old lat)
    (cons new (cons old lat))))

(define seqR
  (lambda (new old lat)
    (cons old (cons new lat))))

        
(define insertL
  (insert-g seqL))

(define insertR
  (insert-g seqR))

(define insertL
  (insert-g
   (lambda (new old lat)
     (cons new (cons old lat)))))

(define insertR
  (insert-g
   (lambda (new old lat)
     (cons old (cons new lat)))))


(define seqS
  (lambda (new old lat)
    (cons new lat)))

(define subst
  (insert-g seqS))

;; p135
;; リストlatの中のoldをnewで置き換える
(define subst
  (insert-g
   (lambda (new old lat)
     (cons new lat))))


;; p135
;;
(define yyy
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(define seqrem
  (lambda (new old l)
    l))

;; p105
;; プラス、かける、累乗についての算術式を表す
(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((atom-to-function (operator nexp))
      (value (1st-sub-exp nexp))
      (value (2nd-sub-exp nexp))))))

;     ((eq? (car (cdr nexp))(quote o+))
;      (o+ (value (car nexp))(value (car (cdr (cdr nexp))))))
;     ((eq? (car (cdr nexp))(quote x))
;      (x (value (car nexp))(value (car (cdr (cdr nexp))))))
;     ((eq? (car (cdr nexp))(quote o^))
;      (o^ (value (car nexp))(value (car (cdr (cdr nexp))))))
;     (else
;      (display "Noe Susiki.\n")))))

(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x (quote o+)) o+)
     ((eq? x (quote x)) x)
     (else o^))))

    
