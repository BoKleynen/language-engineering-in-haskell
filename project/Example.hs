{-# LANGUAGE BlockArguments #-}

module Example where

import Postgres (postgresMigration)
import Migration
import SQLite (sqliteMigration)

main :: IO ()
main = sqliteMigration "db.sqlite3" do
    dropTable "movies"
