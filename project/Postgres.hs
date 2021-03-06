{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Postgres where

import Database.PostgreSQL.Simple

import Migration
import Control.Monad.State
import Data.String
import Data.List ( intercalate )
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask))
import Control.Exception (bracket)


postgresMigration :: String -> PostgresMigrator a -> IO a
postgresMigration connString (PostgresMigrator migration)
  = bracket (connectPostgreSQL (fromString connString)) close (runReaderT migration)

newtype PostgresMigrator a = PostgresMigrator (ReaderT Connection IO a)
  deriving (Functor, Applicative, Monad)

instance MonadMigration PostgresMigrator where
  createTable name CreateTableM {..} = PostgresMigrator do
      conn <- ask
      let td = execState execCreateTableM (TableDefinition [] [])
      liftIO $ execute_ conn (q td)
      return td
    where
      q TableDefinition {..} = fromString $ "CREATE TABLE " <> name <> " ("
        <> intercalate ", " (map columnDefinitionToQuery columns)
        <> intercalate ", " (map tableConstraintToQuery tableconstraints)
        <> ");"

  alterTable name AlterTableM {..} = PostgresMigrator do
      conn <- ask
      let at = execState execAlterTableM (AlterTable [] [] [] [])
      liftIO $ execute_ conn (q at)
      return at
    where
      q AlterTable {..} = fromString $ "ALTER TABLE " <> name <> " ("
        <> intercalate ", " (
          map columnAddToQuery columnAdds
          ++ map columnDropToQuery columnDrops
          ++ map tcAddToQuery tcAdds
          ++ map tcDropToQuery tcDrops)
        <> ");"
      columnAddToQuery cd = "ADD COLUMN " ++ columnDefinitionToQuery cd
      columnDropToQuery col = "DROP COLUMN " ++ col
      tcAddToQuery tc = "ADD " ++ tableConstraintToQuery tc
      tcDropToQuery con = "DROP CONSTRAINT " ++ con

  dropTable name = PostgresMigrator do
      conn <- ask
      liftIO $ execute_ conn q
      return ()
    where
      q = fromString $ "DROP TABLE " ++ name


columnDefinitionToQuery :: ColumnDefinition -> String
columnDefinitionToQuery ColumnDefinition {..} = columnName ++ columnTypeToQuery typ ++ unwords (map columnConstraintToQuery constraints)

columnTypeToQuery :: ColumnType -> String
columnTypeToQuery String = "TEXT"
columnTypeToQuery Int = "INTEGER"
columnTypeToQuery BigInt = "BIGINT"
columnTypeToQuery Bool = "BOOL"
columnTypeToQuery (Raw str) = str

columnConstraintToQuery :: Constraint -> String
columnConstraintToQuery PK = "PRIMARY KEY"
columnConstraintToQuery NotNull = "NOT NULL"
columnConstraintToQuery Unique = "UNIQUE"
columnConstraintToQuery (Check str) = "CHECK " ++ str
columnConstraintToQuery (Default str) = "DEFAULT " ++ str
columnConstraintToQuery (References str) = "REFERENCES " ++ str

tableConstraintToQuery :: TableConstraint -> String
tableConstraintToQuery (TableConstraint (UniqueTC cols) name)
  = case name of
      Nothing -> ""
      (Just s) -> s
  <> " ("
  <> intercalate ", " cols
  <> " )"
tableConstraintToQuery (TableConstraint (CheckTC expr) name)
  = case name of
      Nothing -> ""
      (Just s) -> s
  <> expr
