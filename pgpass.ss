(import :std/pregexp)
(export #t)

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
       (if (char=? (string-ref line 0) #\#)
	 (read-lines port)
	 (cons line (read-lines port))))))

 (def (read-pgpass-line-from-string string)
   (let ((list (pregexp-split "(?<!\\\\):" string)))
     (apply make-pgpass (map (lambda (s) (pregexp-replace* "\\\\" s "")) list))))

 (if (file-exists? filename)
   (map read-pgpass-line-from-string
	(call-with-input-file filename read-lines))
   #f))
