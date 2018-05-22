(import :std/pregexp :std/srfi/1 :gerbil/gambit/os)
(export find-password read-pgpass-file find-pgpass-file)

(def (find-pgpass-file)
  "=> filename-as-string OR #f"
  (let ((path (or (getenv "PGPASSFILE" #f)
		  (string-append (getenv "HOME") "/.pgpass"))))
    (if (file-exists? path)
      path
      #f)))

(defstruct pgpass
  (hostname port database username password))

(def (read-pgpass-file (filename (find-pgpass-file)))

  (def (read-lines port)
    (let ((line (read-line port)))
      (if (eof-object? line)
        '()
        ;; comments
        (if (char=? (string-ref line 0) #\#)
  	(read-lines port)
  	(cons line (read-lines port))))))

  (def (read-pgpass-line-from-string string)
    (let ((list (pregexp-split "(?<!\\\\):" string)))
      (apply (lambda args
	       (with-catch 
                (lambda (_) 
                  ;;; if there's an error, read the rest anyway
		  "hostname:port:database:username:password")
		  (lambda () (apply make-pgpass args))))
	     (map (lambda (s)
		    ;; If an entry needs to contain : or \, escape
		    ;; this character with \.
		    (pregexp-replace*
		     "\\\\\\\\"
		     (pregexp-replace* "\\\\:" s ":")
		     "\\\\"))
		  list))))

 (if (file-exists? filename)
   (map read-pgpass-line-from-string
	(call-with-input-file filename read-lines))
   #f))

(def (find-pgpass
	hostname: (hostname #f) port: (port #f)
	user: (user #f) db: (db #f)
	pgpass-list: (pgpass-list (read-pgpass-file (find-pgpass-file))))
    (def (find-things thing name list)
      ;; (displayln "finding thing "  name)
      (if (not name)
	list
	(append (filter (lambda (i) (equal? (thing i) name)) list)
		(filter (lambda (i) (equal? "*" (thing i))) list))))
    (let* ((thing
	    (find-things
	     pgpass-database db
	     (find-things
	      pgpass-username user
	      (find-things
	       pgpass-port port
	       (find-things
		pgpass-hostname hostname
		pgpass-list))))))
      (if (and thing (not (null? thing)))
	thing
	#f)))

  (def (make-cache-table)
    (make-hash-table test: equal?))
  (def cache (make-cache-table))
	
  (def (find-password hostname: (hostname #f) port: (port #f)
		      user: (user #f) db: (db #f)
		      pgpass: (pgpass (find-pgpass-file)))
    (if (not pgpass)
      #f
      (let ((mtime (time->seconds
		    (file-info-last-modification-time
		    (file-info pgpass))))
	    (hash-list (list hostname port user db)))
	;;   (displayln "Looking for " hash-list)
	(def (find-pass reload research)
	  ;;	(displayln "reload " reload  "research " research)
	  (if reload
	    (let ((allpass (read-pgpass-file pgpass)))
	      ;; (displayln "Read " allpass)

	      (hash-put! cache pgpass (cons mtime allpass))
	      (find-pass #f #t))
	    (if research
	      (let ((pgpass-cons-list (hash-get cache pgpass)))
	       ;; (displayln "Research " pgpass-cons-list)
		(if (not pgpass-cons-list)
		  (find-pass #t #t)
		  (with ([last-modification-time . pgpass-list] pgpass-cons-list)
		    (if (not (= last-modification-time mtime))
		      (find-pass #t #t)
		      (let ((result (find-pgpass hostname: hostname
						 port: port
						 user: user
						 db: db
						 pgpass-list: pgpass-list)))
		      ;;  (displayln "Foound :" result " for " hash-list)
			(let ((ret (if (pair? result)
				     (first result)
				     #f)))
			  (begin0 (pgpass-password ret)
			    (hash-put! cache hash-list (cons mtime ret)))))))))
	    
	      (let ((pgpass-cons-result (hash-get cache hash-list)))
;;		  (displayln "Looking for " hash-list " found " pgpass-cons-result)
		(if (pair? pgpass-cons-result)
		  (with ([last-modification-time . result] pgpass-cons-result)
		    (if (not (= last-modification-time mtime))
		      (find-pass #t #t)
		      (if result
			(pgpass-password result)
			result)))
		  (find-pass #f #t))))))		    	    
	(find-pass #f #f))))
