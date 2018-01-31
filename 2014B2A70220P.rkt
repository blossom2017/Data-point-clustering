#lang racket
(provide (all-defined-out))
(define inputfile (vector-ref (current-command-line-arguments) 0))


(define datalist (file->string inputfile )) 

(define get-index (lambda (e lst)(if (null? lst) -1 (if (eq? (car lst) e) 0 (if (= (get-index e (cdr lst)) -1) -1 (+ 1 (get-index e (cdr lst))))))))


(define (firs dl datalistn) (regexp-split #px"\n" dl))


(define ourlist (firs datalist '()))

(define (createlist old new) (if (null? old) new  (createlist (cdr old) (cons (string-split (car old )) new)  )  ))    
(define dlist (createlist ourlist '()))
(define dlistrev (cdr dlist) )

(define (reverse ls) (if(null? ls) '() (append (reverse (cdr ls )) (list (car ls))) ))

(define datalistfinal (reverse dlistrev))

(define pointslist (cdr datalistfinal))
(define info (car datalistfinal))


(define (f1 item cnt)(list cnt item))
(define (makecl l count)(if (null? l)  '()  (cons (f1 (car l) count)(makecl (cdr l) (+ count 1))) ))
(define step1 (makecl pointslist 1))

(define (sumsq ptx pty sum ) (if (null? ptx) sum   (sumsq (cdr ptx) (cdr pty) (+ sum (expt (- (car ptx) (car pty) ) 2))) ))
(define (f3 pta ptb) (if(equal? (car pta ) (car ptb )) +inf.0 (sqrt (sumsq (append* (cdr pta)) (append* (cdr ptb)) 0 ))) )
(define (f21 pt1 all lnew) (if (null? all ) lnew (f21 pt1 (cdr all ) (cons (f3 pt1 (car all) ) lnew)   )))



(define (n1 lll) (if(null? lll ) '()  (cons  (string->number ( car lll)) ( n1 (cdr lll )) )  ))
(define (numeric ll llnew) (if(null? ll) llnew (numeric (cdr ll) (cons (n1 (car ll) ) llnew )  )))
(define ptlistnum (reverse(numeric pointslist '())))


(define ptlistnumx (makecl ptlistnum 1  ))


(define (createnew lc unchangedl newl) (if(null? lc) newl (createnew (cdr lc) unchangedl (cons (makecl (reverse (f21 (car lc) unchangedl '() ) ) 1 )newl )) ))


(define step2i (reverse (createnew ptlistnumx ptlistnumx '())))


(define precision '6)  
(define (mysetprecision n p)   (if (= n +inf.0) +inf.0       (string->number (~r n #:precision p))   ) )   (define (precision_util lst)   (if (null? lst) '()       (cons (list (car(car lst)) (mysetprecision (car(cdr(car lst))) precision))  (precision_util (cdr lst)))) )  
(define (modify_precision lst)   (if (null? lst) '()   (cons (precision_util (car lst)) (modify_precision (cdr lst)))) )


(define step2(modify_precision step2i))

(define (sort-by-point lst )(sort lst (lambda (x y) (< (car x) (car y) ))) )
(define (sort-by-distance lst) (sort lst (lambda (x y) (< (car(cdr x)) (car (cdr y)) ))) )



(define newlist(sort-by-distance (car step2)))

(define infonumericinrev(reverse (n1 info )) )
(define minpoints (car infonumericinrev))
(define eps (car (cdr infonumericinrev)))
(define k (car(cdr (cdr infonumericinrev))) )


(define (createsorted sim sor ) (if(null? sim) sor  (createsorted ( cdr sim ) (cons (sort-by-distance(sort-by-point (car sim) ) )sor ) ) ))
(define sortmatrix (reverse (createsorted step2 '())))


(define (pickk liss kitem cnt) (if(equal? cnt k)  kitem ( pickk (cdr liss ) (cons (car (car liss))kitem ) (+ cnt 1) ) ))
(define (ksortlist all n) (if(null? all) (reverse n)   (ksortlist (cdr all) (cons (pickk (car all ) '() 0 )n )     )     ))

(define kslist (ksortlist sortmatrix '()))


(define (sortind l nn) (if (null? l) (reverse nn) (sortind (cdr l) (cons (sort (car l) <)nn ) ) ))
(define step3 (sortind kslist '()))

(define counts3 (makecl step3 1))

(define  (makepairs  ele l ) (cartesian-product (cons ele '()) l))
(define (allp l n) (if (null? l) (reverse n) (allp (cdr l) (cons  (makepairs (car(car l) ) (car(cdr ( car l)) ) ) n) )))

(define alll (allp counts3 '()))

(define (find op ls ) (cond ((null? ls) #f) ((equal? op (car ls)) #t)
       (else (find op (cdr ls))) ))

(define (filter all unchanged new) (if (null? all ) new  (if(equal? #t  (find (reverse (car all)) unchanged )  ) (filter (cdr all) unchanged (append new (cons (car all) '()) )) (filter (cdr all) unchanged new ) )      ))


(define unchanged (append* alll))

(define (createfiltered j un new) (if(null? j) (reverse new ) (createfiltered (cdr j) un (cons (filter (car j) un '() ) new) )) )

(define tryfilter(createfiltered alll unchanged '()))


(define accessvec (list->vector step3)  )

(define (weight op av ) (cons (car( cdr op)) (cons(length (set-intersect (vector-ref av(- (car op) 1)) (vector-ref av (- (cadr op) 1 ) ) )) '() ) ))
(define (createng l av new) (if(null? l) new (createng (cdr l ) av (cons  (weight (car l) av  ) new ) ) ) ) 


(define (createg fil av n) (if (null? fil) (reverse n) (createg (cdr fil) av (cons ( createng (car fil) av  '()) n) ) ) )

(define step4temp (createg tryfilter accessvec '()))

(define (sort-ng1 lst) (sort lst (lambda (x y) (< (car x)  (car  y) ))) )
(define (sort-ng2 lst ) (sort lst (lambda (x y) (> (car (cdr x) ) (car (cdr y) )) )))
(define (createsorted1 sim sor ) (if(null? sim) (reverse sor)  (createsorted1 ( cdr sim ) (cons (sort-ng1 (car sim))sor ) ) ))
(define (createsorted2 sim sor ) (if(null? sim) (reverse sor)  (createsorted2 ( cdr sim ) (cons (sort-ng2 (car sim))sor ) ) ))


(define step41 (createsorted1 step4temp '()) )
(define step4 (createsorted2 step41 '()))


(define (weightcheck lis sum) (if (null? lis) sum (if(>= ( car (cdr (car lis)))  eps) (weightcheck (cdr lis) (+ sum 1))  (weightcheck (cdr lis ) sum) ) ))
(define (pointdensity lis n) (if(null? lis) (reverse n) (pointdensity (cdr lis) (cons (weightcheck (car lis) 0)n ))))
(define step5 (pointdensity step4 '()))


(define (corepoint lis  newl num )(if (null? lis)  newl  (if(>= (car lis ) minpoints) (corepoint (cdr lis) (append newl (cons num '())) (+ num 1) )(corepoint (cdr lis) newl  (+ num 1) ) ) ) )
(define (corepointcompl lis  newl num )(if (null? lis)  newl  (if(< (car lis ) minpoints) (corepointcompl (cdr lis) (append newl (cons num '())) (+ num 1) )(corepointcompl (cdr lis) newl  (+ num 1) ) ) ) )


(define step6 (corepoint step5 '() 1))


(define selectnoise (corepointcompl step5 '() 1))

(define edgelist(append* tryfilter))

 (define (giveweight op av) (length (set-intersect (vector-ref av(- (car op) 1)) (vector-ref av (- (cadr op) 1 ) ) )))
(define (crwtfledge list av new ) (if(null? list) (reverse new) (if(>= (giveweight (car list ) av ) eps ) (crwtfledge (cdr list) av (cons (car list)new)) (crwtfledge (cdr list) av  new)  ) ))

(define filedge (crwtfledge edgelist accessvec '()))



(define (children ele lis newlist) (if (null? lis) newlist (if (equal? (caar lis)  ele ) (children ele (cdr lis) (cons (car(cdr (car lis)))newlist ) ) (children ele (cdr lis) newlist) )))
(define (dfs  visited notv) (if (null? notv) visited (dfs (cons (car notv) visited)  (remove-duplicates (set-union (set-subtract (children (car notv) filedge2 '() ) visited) (cdr notv)) ) ) ) )

(define universecore(cartesian-product step6 step6))
(define (corefil lis fl) (if (null? lis) fl (if (equal? (find (car lis) universecore) #t) (corefil (cdr lis) (cons (car lis)fl ))  (corefil (cdr lis) fl) )))

(define filedge2 (corefil filedge '()) )
(define (createclus lis  new) (if (null? lis) (reverse new) (createclus (sort (set-subtract lis (set-intersect (dfs '() (cons (car lis) '()))step6 )) < ) (cons (set-intersect(dfs '() (cons (car lis) '())) step6) new))))
(define tempx (createclus step6 '()))

(define (clussort lis new ) (if (null? lis) (reverse new) (clussort (cdr lis) (cons (sort (car lis) <)new))))

(define tempx1 (clussort tempx '()))
(define copytempx1(clussort tempx '()))
(define step7 (makecl tempx1 1))

(define fornoise(append* filedge ))
(define universe(set-union step6 selectnoise))


(define noise1(set-subtract selectnoise fornoise))
(define step8 (sort noise1 <))

(define borderpt(set-subtract selectnoise noise1))
(define step9 (sort borderpt <))

(define (checksim lis first) (if (null? lis) (reverse first) (checksim (cdr lis) (cons (car (car lis))first ))))
(define clist (checksim tempx1 '()))

(define idlist(makecl clist 0))
(define (getid ele idlist) (if(equal? ele (car (cdr (car idlist ))) ) (car (car idlist)) (getid ele (cdr idlist))  ))


(define (getmaxindex op av lis maxval minindex) (if (null? lis) minindex (if(> (length (set-intersect (vector-ref av(- op 1)) (vector-ref av (-  (car lis)  1 )) ) ) maxval ) (getmaxindex op av (cdr lis ) (length (set-intersect (vector-ref av(- op 1)) (vector-ref av (-  (car lis)  1 )) ) )(car lis ) ) 

  (getmaxindex op av (cdr lis) maxval minindex)  )) ) 
(define (createord bdplist av cptlist newl) (if(null? bdplist) (reverse newl) (createord (cdr bdplist) av cptlist (cons   (getmaxindex (car bdplist) av cptlist 0 k) newl )) ))

(define ty1  (createord step9 accessvec  step6  '()))

(define (findindex ele total) (if (equal?(find ele (car total) )#t) (car (car total)) (findindex ele (cdr total)) ))
(define (crstartc lis total newl) (if (null? lis) (reverse newl) (crstartc (cdr lis) total (cons (findindex (car lis) total)newl ) ) ))


(define ty(crstartc ty1 tempx1 '()))



(define (finalapp b c lis unchanged newl)  (cond ((null? unchanged) (reverse newl))  ((equal? c (car (car unchanged))  )  ( finalapp b c (cdr lis) (cdr unchanged) (cons  (cons b (car lis) )newl) )) (else (finalapp b c (cdr lis) (cdr unchanged) (cons (car lis) newl) )  )))



(define (finalapp2 blist corelist lis) (if (null? blist) lis (finalapp2 (cdr blist) (cdr corelist) (finalapp (car blist)(car corelist) lis tempx1 '()  ) ) ))
(define tz (finalapp2 step9  ty tempx1  ))


(define tp(clussort tz '()))
(define step10 (makecl tp 1)) 

