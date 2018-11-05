
(define NULL 'null)

;====================================
;=            Cerința 1             =
;= Definirea elementelor de control =
;=          20 de puncte            =
;====================================

;= Funcțiile de acces
(define init-database
  (λ ()
    null
    ))

(define create-table
  (λ (name-table columns-name)
    (let tables ((columns columns-name) (acc (list name-table)))
      (if(null? columns)
         acc
         (tables (cdr columns) (append acc (list(list(car columns))))) 
         )
      )
    )
  )
  
(define get-name
  (λ (table)
    (car table)))

(define get-columns
  (λ (table)
    (let name-col ((table (cdr table)) (acc null))
      (if (null? table)
          acc
          (name-col (cdr table) (append acc (list(caar table))))
          )
    )
  )
)

(define get-tables
  (λ (db)
    db
    ))

(define get-table
  (λ (db table-name)
    (car(filter (λ (tb) (if (equal? (get-name tb) table-name) #t #f)) db))
    )
  )

(define add-table
  (λ (db table)
    (append db (list table))
    )
  )

(define remove-table
  (λ (db table-name)
    (filter (λ (tb) (if (equal? (get-name tb) table-name) #f #t)) db)
    )
  )

;= Pentru testare, va trebui să definiți o bază de date (având identificatorul db) cu următoarele tabele

;============================================================================================
;=                         Tabela Studenți                                                   =
;= +----------------+-----------+---------+-------+-------+                                  =
;= | Număr matricol |   Nume    | Prenume | Grupă | Medie |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;= |            123 | Ionescu   | Gigel   | 321CA |  9.82 |                                  =
;= |            124 | Popescu   | Maria   | 321CB |  9.91 |                                  =
;= |            125 | Popa      | Ionel   | 321CC |  9.99 |                                  =
;= |            126 | Georgescu | Ioana   | 321CD |  9.87 |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;=                                                                                           =
;=                                         Tabela Cursuri                                    =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | Anul | Semestru |            Disciplină             | Număr credite | Număr teme |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | I    | I        | Programarea calculatoarelor       |             5 |          2 |      =
;= | II   | II       | Paradigme de programare           |             6 |          3 |      =
;= | III  | I        | Algoritmi paraleli și distribuiți |             5 |          3 |      =
;= | IV   | I        | Inteligență artificială           |             6 |          3 |      =
;= | I    | II       | Structuri de date                 |             5 |          3 |      =
;= | III  | II       | Baze de date                      |             5 |          0 |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;============================================================================================

(define db
  (list
  (list "Studenți"
	(list "Număr matricol" 123 124 125 126 )
	(list "Nume" "Ionescu" "Popescu" "Popa" "Georgescu")   
	(list "Prenume" "Gigel" "Maria" "Ionel"  "Ioana" ) 
	(list "Grupă" "321CA" "321CB" "321CC""321CD" ) 
	(list "Medie" 9.82 9.91 9.99 9.87)
     )
  (list "Cursuri"
	(list "Anul" "I" "II" "III" "IV" "I" "III" )
	(list "Semestru"  "I" "II" "I" "I" "II" "II" )
	(list "Disciplină" "Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date" "Baze de date" )
	(list "Număr credite" 5 6 5 6 5 5 )  
	(list "Număr teme" 2 3 3 3 3 0)
     )
  )
)

;====================================
;=            Cerința 2             =
;=         Operația insert          =
;=            10 puncte             =
;====================================

(define insert
  (λ (db table-name record)
    (let-values ( ( (lst1 lst2) (splitf-at db (λ(tb) (if (equal? table-name (car tb)) #f #t))) ))
      (append lst1 (list(inser db table-name record)) (cdr lst2))
      )))
    

(define inser
  (λ (db table-name record)
      (let ihelp ((tb (cdr(get-table db table-name))) (acc (list table-name)) (record record) (col (map car record)) (rec record) )
        (if (null? tb)
          acc
          (if (not(member (caar tb) col))
              (ihelp (cdr tb) (append acc (list (append (car tb) (list NULL)))) record col rec)
              (if (null? record)
                  (ihelp tb acc rec col rec)
                  (if (equal? (caar tb) (caar record))
                      (ihelp (cdr tb) (append acc (list (append (car tb) (list(cdr(car record)))))) (cdr record) col rec)
                      (ihelp  tb acc (cdr record) col rec)
                      )
                  )
              )
          )
        )
    )
  )


;====================================
;=            Cerința 3 a)          =
;=     Operația simple-select       =
;=             10 puncte            =
;====================================

(define simple-select
  (λ (db table-name columns)
    (define cols (get-columns (get-table db table-name)))
    (let shelp ((tb (cdr(get-table db table-name))) (acc null)(columns columns)(col cols))
      (if (null? columns)
          (if (equal? (check-null acc) #t)
              null
              (map cdr acc))
          (if (member (car columns) cols)
              (shelp tb (append acc (filter (λ(L) (if(equal? (car columns) (car L)) #t #f)) tb)) (cdr columns) col)
              (shelp tb acc (cdr columns) col)
              )   
          )
      )
    )
  )

(define check-null
  (λ(tb)
    (if (null? tb)
        #t
        (if (not(null? (cdr(car tb))))
            (and #f (check-null (cdr tb)))
            (and #t (check-null (cdr tb)))
            )
        )
    )
  )


;====================================
;=            Cerința 3 b)          =
;=           Operația select        =
;=            30 de puncte          =
;====================================

(define select
  (λ (db table-name columns conditions)
    (define table (fullfill-cond (cdr(get-table db table-name)) conditions))
    (let slt-help ((tb table) (columns columns) (acc null))
      (if (null? columns)
          acc
          (if (pair? (car columns))
              (cond
                [(equal? (caar columns) (car(list 'min)))
                 (slt-help tb (cdr columns) (append acc (list (apply min (sel-column tb (list(cdr(car columns))))))))]
                [(equal? (caar columns) (car(list 'max)))
                 (slt-help tb (cdr columns) (append acc (list (apply max (sel-column tb (list(cdar columns)))))))]
                [(equal? (caar columns) (car(list 'sum)))
                 (slt-help tb (cdr columns) (append acc (list (apply + (sel-column tb (list(cdar columns)))))))]
                [(equal? (caar columns) (car(list 'avg)))
                 (slt-help tb (cdr columns) (append acc (list (/ (apply + (sel-column tb (list(cdar columns))))
                                                                 (length (sel-column tb (list(cdar columns))))))))]
                [(equal? (caar columns) (car(list 'count)))
                 (slt-help tb (cdr columns) (append acc (list (length (counter (sel-column tb (list(cdar columns))))))))]
                [(equal? (caar columns) (car(list 'sort-asc)))
                 (slt-help tb (cdr columns) (append acc (list (sort (sel-column tb (list(cdar columns))) <))))]
                [(equal? (caar columns) (car(list 'sort-desc)))
                 (slt-help tb (cdr columns) (append acc (list (sort (sel-column tb (list(cdar columns))) >))))]
                )
              (slt-help tb (cdr columns) (append acc (list (sel-column tb (list(car columns))))))
              )
          )
      )
    ))
                
                         

                 


(define counter
  (λ(L)
    (foldr (λ (x L) (if (member x L) L (append (list x) L))) null L)
    ))

(define sel-column
  (λ(tb lst)
    (cdr(car(filter (λ(L)(if (equal? (car L) (car lst)) #t #f)) tb)))
    ))

(define find-column
  (λ(tb conditie)
    (car(filter (λ(L) (if  (equal? (car L) (car(cdr conditie))) #t #f)) tb))
    ))

(define help-cond
  (λ(tb conditie)
    (let helpme ((trans-tb (cdr(apply map list tb)))
          (conditie conditie)
          (acc (list(car(apply map list tb))))
          (col (cdr(find-column tb conditie))))
      (if (null? trans-tb)
          (apply map list acc)
          (if (equal? NULL (car col))
              (helpme (cdr trans-tb) conditie (append acc (list(car trans-tb)))(cdr col))
              (if ((car conditie)  (car col) (car(cdr(cdr conditie))))
                  (helpme (cdr trans-tb) conditie (append acc (list(car trans-tb)))(cdr col))
                  (helpme (cdr trans-tb) conditie acc (cdr col))                                         
              ))))))


(define fullfill-cond
  (λ(tb conditions)
      (if (null? conditions)
          tb
          (fullfill-cond (help-cond tb (car conditions)) (cdr conditions)) 
          )
    ) 
  )

;====================================
;=             Cerința 4            =
;=           Operația update        =
;=            20 de puncte          =
;====================================

;pentru liste egale
(define check-lists
  (λ(L1 L2)
    (if (or (null? L1) (null? L2))
        #t
        (if(equal? (car L1)(car L2))
           (and #t (check-lists (cdr L1)(cdr L2)))
           (and #f (check-lists (cdr L1)(cdr L2)))
            )
        )
    )
  )
            


;nr de coloane de dinainte de coloana dorita
(define find-elem
  (λ(tb val)
    (if (member val (car(apply map list tb)))
        (let find-el ((tb tb)(val val)(acc 0)(ok 0))
          (if (= ok 1)
              acc
              (if (equal? (caar tb) val)
                  (find-el (cdr tb) val  acc 1)
                  (find-el (cdr tb) val (+ 1 acc) 0)
                  )
              )
          )
        null
        )
    )
  )

(define up-table
  (λ(tbb value conditions)
    (define table (apply map list (fullfill-cond (help-cond tbb (car conditions)) (cdr conditions))))
    (define contor (find-elem tbb (car value)))
    (if(null? contor) tbb
       (let up-tb ((tb (cdr(apply map list tbb)))(newtb (cdr table))(acc (list(car table)))(val (cdr value)) (dim (length (apply map list tbb))))
         (if( = (length acc) dim)
            (apply map list acc)
            (if (null? newtb)
                (up-tb (cdr tb) newtb (append acc (list (car tb))) val dim )
                (if (equal? (check-lists (car tb) (car newtb)) #t)
                    (up-tb (cdr tb)(cdr newtb)(append acc (list (list-set (car tb) contor val))) val dim )
                    (up-tb (cdr tb) newtb (append acc (list (car tb))) val dim )
                    )
                )
            )
         )
       )
    )
  )

(define updt
  (λ(tb values conditions)
    (if (null? values)
        tb
        (updt (up-table tb (car values) conditions) (cdr values) conditions)
        )
    )
  ) 

(define update
    (λ (db table-name values conditions)
      (let-values (( (lst1 lst2) (splitf-at db (λ(tb) (if (equal? table-name (car tb)) #f #t))) ))
        (append lst1 (list (append (list table-name) (updt (cdr(get-table db table-name)) values conditions))) (cdr lst2))
        )))

;====================================
;=             Cerința 5            =
;=           Operația remove        =
;=              10 puncte           =
;====================================

;tb fara nume tine si tu minte
(define del-help
  (λ(tb conditions)
    (define table (apply map list (fullfill-cond (help-cond tb (car conditions)) (cdr conditions))))
    (if (null? conditions)
        (list(car(apply map list tb)));lista de lista
        (let del ((tb (cdr(apply map list tb))) (newtb (cdr table)) (acc (list(car table))) (dim (- (length (apply map list tb)) (length table) )) )
          (if( = (length acc) (+ 1 dim))
             (apply map list acc)
             (if (null? newtb)
                 (del (cdr tb) newtb (append acc (list (car tb))) dim )
                 (if (equal? (check-lists (car tb) (car newtb)) #t)
                     (del (cdr tb)(cdr newtb) acc dim)
                     (del (cdr tb) newtb (append acc (list (car tb))) dim )
                     )
                 )
             )
          )
        )
    )
  )

(define delete
  (λ (db table-name conditions)
    (let-values (( (lst1 lst2) (splitf-at db (λ(tb) (if (equal? table-name (car tb)) #f #t))) ))
        (append lst1 (list (append (list table-name) (del-help (cdr(get-table db table-name)) conditions))) (cdr lst2))
        )
    )
  )
                 
 

;====================================
;=               Bonus              =
;=            Natural Join          =
;=            20 de puncte          =
;====================================

(define intersect-columns
  (λ(tb1 tb2)
    (reverse(set-intersect (car(apply map list tb1)) (car(apply map list tb2))))
    )
  )

(define elim-elem
  (λ(L index)
    (let-values (( (L1 L2) (split-at L index ) ))
        (append L1  (cdr L2))
        )))

(define col-sel
  (λ(tb index elem)
    (filter (λ(L)(if (equal? (list-ref L index) elem) #t #f)) tb)
    ))

(define col-diff
  (λ(tb index elem)
    (filter (λ(L)(if (equal? (list-ref L index) elem) #f #t)) tb)
    ))


(define comp
  (λ(l1 tb2 i1 i2 elem)
    (let cp ((l1 l1) (tb2 tb2) (i1 i1) (i2 i2) (elem elem) (acc null))
      (if (null? tb2)
          acc
          (cp l1 (cdr tb2) i1 i2 elem (append acc (list (append (list elem) (elim-elem (car tb2) i2) (elim-elem l1 i1)))))
          )
      )
    )
  )
     
          
;transpusele tabelelor
(define intersect-table
  (λ(tb1 tb2 nume-col)
    (define ind1 (index-of (car tb1) nume-col))
    (define ind2 (index-of (car tb2) nume-col))
    (let join ((tb1 (cdr tb1))
               (tb2 (cdr tb2))
               (ind1 ind1)
               (ind2 ind2)
               (acc (list(append (list nume-col) (elim-elem (car tb2) ind2) (elim-elem (car tb1) ind1)))))
      (if (or (null? tb1) (null? tb2))
          (apply map list acc)
          (if (null? (col-sel tb2 ind2 (list-ref (car tb1) ind1)))
              (join (col-diff tb1 ind1 (list-ref (car tb1) ind1)) tb2 ind1 ind2 acc)
              (join (col-diff tb1 ind1 (list-ref (car tb1) ind1)) (col-diff tb2 ind2 (list-ref (car tb1) ind1)) ind1 ind2 (append acc (comp (car tb1) (col-sel tb2 ind2 (list-ref (car tb1) ind1)) ind1 ind2 (list-ref (car tb1) ind1))))

              )
          )
      )
    )
  )

(define natural-join
  (λ(db tables columns conditions)
    
    (define table1 (apply map list (fullfill-cond (cdr(get-table db (car tables))) conditions)))
    (define table2 (apply map list (fullfill-cond (cdr(get-table db (car(cdr tables)))) conditions)))
    (define col (intersect-columns (cdr(get-table db (car tables))) (cdr(get-table db (car(cdr tables)))) ))
    (define final-tb (intersect-table table2 table1 (car col)))
    (define cols (car(apply map list final-tb)))
    (define final-tbt (reorder (apply map list table1) final-tb))
    
    (let nhelp ((tb final-tbt) (acc null)(columns columns)(col cols))
      (if (null? columns)
          (map cdr acc)
          (if (member (car columns) col)
              (nhelp tb (append acc (filter (λ(L) (if(equal? (car columns) (car L)) #t #f)) tb)) (cdr columns) col)
              (nhelp tb acc (cdr columns) col)
              )   
          )
      )
  )
 )

(define reorder
  (λ(tb1 tb2)
    (let order ((tb1 (cdr(apply map list (extract tb1 (caar tb2)))))
                (tb2 (cdr(apply map list tb2)))
                (ftb2 (cdr(apply map list tb2)))
                (acc (list (car (apply map list tb2)))))
      
      (if(= (+ 1(length ftb2)) (length acc))
         (apply map list acc)
         (if (null? tb2)
             (order tb1 ftb2 ftb2 acc)
             (if (equal? (check-lists (car tb1) (cdr(car tb2))) #t)
                 (order (cdr tb1) (cdr tb2) ftb2 (append acc (list (car tb2))))
                 (order tb1 (cdr tb2) ftb2 acc)
                 )
             )
         )
      )
    )
  )

(define extract
  (λ(tb nm-col)
    (filter (λ(L) (if(equal? (car L) nm-col) #f #t)) tb)
    ))
       