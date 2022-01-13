#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("db/postgresql-driver" "db/postgresql" "db/dbi"
    "db/postgresql-test" "db/postgresql-upstream-test"
    "pg/pgpass" "pg/connect"))
