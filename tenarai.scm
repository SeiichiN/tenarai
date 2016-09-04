;;; Scheme手習い
;;; p16
(defun lat? (l)
           (cond
             ((null l) t)
             ((atom (car l))(lat? (cdr l)))
             (t nil)))

;;; p23
(defun member? (a lat)
           (cond
             ((null lat) nil)
             (t (or (eq (car lat) a)
                    (member? a (cdr lat))))))

