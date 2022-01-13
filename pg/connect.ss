(import ../db/dbi ../db/postgresql ./pgpass)
(export connect) 

(def (connect host: (host "127.0.0.1")
	          port: (port 5432)
	          user: user
	          passwd: (passwd #f)
	          db: (db #f)
	          pgpass-file: (pgpass-file #t))
  
  (def actual-passwd
    (or passwd
	    (find-password hostname: host port: port
		               user: user db: db
		               pgpass: (if (eq? #t pgpass-file)
				                 (find-pgpass-file)
				                 pgpass-file))))
  (postgresql-connect host: host
		              port: port
		              user: user
		              passwd: actual-passwd
		              db: db))
