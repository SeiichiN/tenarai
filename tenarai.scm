;; Scheme手習い
;; p16
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
              ((null? lat) nil)
              (else (or (eq? (car lat) a)
                        (member? a (cdr lat)))))))

