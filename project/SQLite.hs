{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE StandaloneDeriving  #-}

module SQLite where

import Database.SQLite.Simple
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask))
import Control.Exception (bracket)
import Data.String (IsString(fromString))
import Text.Regex.PCRE
import Migration
    ( TableConstraintKind(CheckTC, UniqueTC),
      TableConstraint(..),
      Constraint(..),
      ColumnType(..),
      ColumnDefinition(..),
      TableDefinition(TableDefinition, tableconstraints, columns),
      CreateTableM(CreateTableM, execCreateTableM),
      AlterTable(AlterTable, tcDrops, tcAdds, columnDrops, columnAdds),
      AlterTableM(AlterTableM, execAlterTableM),
      MonadMigration(..) )
import Control.Monad.State
import Data.List (intercalate)
import Data.Bits ((.|.))
import GHC.Generics (Generic)


newtype SQLiteMigrator a = SQLiteMigrator (ReaderT Connection IO a)
  deriving newtype (Functor, Applicative, Monad)

sqliteMigration :: String -> SQLiteMigrator a -> IO a
sqliteMigration path (SQLiteMigrator migration)
  = bracket (open (fromString path)) close (runReaderT migration)

createTable_ :: Connection -> String -> TableDefinition -> IO TableDefinition
createTable_ conn name td = do
    liftIO (execute_ conn (q td))
    return td
  where
      q TableDefinition {..} = fromString $ "CREATE TABLE " <> name <> " ("
        <> intercalate ", " (map columnDefinitionToQuery columns)
        <> intercalate ", " (map tableConstraintToQuery tableconstraints)
        <> ");"

instance MonadMigration SQLiteMigrator where
  createTable name CreateTableM {..} = SQLiteMigrator do
      conn <- ask
      let td = execState execCreateTableM (TableDefinition [] [])
      liftIO $ createTable_ conn name td

  dropTable name = SQLiteMigrator do
      conn <- ask
      liftIO (execute_ conn q)
      return ()
    where
      q = fromString $ "DROP TABLE " ++ name

  alterTable table AlterTableM {..} = SQLiteMigrator do
      conn <- ask
      let at@AlterTable {..} = execState execAlterTableM (AlterTable [] [] [] [])

      if null tcAdds && null tcDrops
        then
          liftIO $ alterTableSimple conn columnAdds columnDrops
        else
          liftIO $ alterTableWithCopy conn at
      return at
    where
      alterTableSimple :: Connection -> [ColumnDefinition] -> [String] -> IO ()
      alterTableSimple conn columnAdds columnDrops = execute_ conn $ fromString $ "ALTER TABLE " <> table <> " ("
        <> intercalate ", " (
          map columnAddToQuery columnAdds
          ++ map columnDropToQuery columnDrops)
        <> ");"

      alterTableWithCopy :: Connection -> AlterTable -> IO ()
      alterTableWithCopy conn AlterTable {..} = do
        constraints <- checkConstraints conn table
        let constraints' = filter (\ConstraintDef { tableName } -> tableName `elem` tcDrops) constraints
        -- let newConstraints = map tableConstraintToConstraintDef tcAdds AlterTable
        return ()

      tableConstraintToConstraintDef :: TableConstraint -> ConstraintDef
      tableConstraintToConstraintDef _ = error ""

      columnAddToQuery cd = "ADD COLUMN " ++ columnDefinitionToQuery cd
      columnDropToQuery col = "DROP COLUMN " ++ col

columnDefinitionToQuery :: ColumnDefinition -> String
columnDefinitionToQuery ColumnDefinition {..} = columnName ++ columnTypeToQuery typ ++ unwords (map columnConstraintToQuery constraints)

columnTypeToQuery :: ColumnType -> String
columnTypeToQuery String = "TEXT"
columnTypeToQuery Int = "INTEGER"
columnTypeToQuery BigInt = "INTEGER"
columnTypeToQuery Bool = "BOOLEAN"
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

checkConstraints :: Connection -> String -> IO [ConstraintDef]
checkConstraints conn table = do
  [Only tableSQL]  <- query_ conn $ fromString $ "SELECT sql \
                      \FROM sqlite_master \
                      \WHERE name = '" ++ table ++ "' AND type = 'table'" :: IO [Only String]
  let regex = makeRegexOpts (defaultCompOpt .|. compCaseless) defaultExecOpt "CONSTRAINT\\s+(?<name>\\w+)\\s+CHECK\\s+\\((?<expression>(:?[^()]|\\(\\g<expression>\\))+)\\)"
  return $ map (\[_, expr, name] -> ConstraintDef table expr name) (match regex tableSQL)

data ConstraintDef = ConstraintDef
  { tableName :: String
  , expr :: String
  , name :: String
  }

tableStructure :: Connection -> String -> IO [ColumnDefinition]
tableStructure conn table = do
    cols <- query_ conn $ fromString $ "PRAGMA table_info('" <> table <> "');" :: IO [TableInfo]
    moveTable conn table "" (map tableInfoToColumnDef cols)
    return []
  where
    tableInfoToColumnDef :: TableInfo -> ColumnDefinition
    tableInfoToColumnDef (TableInfo _ name typ notNull dfltVal pk) = ColumnDefinition name Int []


data TableInfo = TableInfo Int String String Bool String Bool
  deriving Generic

deriving instance FromRow TableInfo

moveTable :: Connection -> String -> String -> [ColumnDefinition] -> IO ()
moveTable conn table alteredTable cols = do
    createTable_ conn alteredTable TableDefinition {columns=cols, tableconstraints=[]}
    copyData
    execute_ conn $ fromString $ "DROP TABLE " <> table
  where
    copyData :: IO ()
    copyData =
      let columnNames = intercalate ", " $ map (\ColumnDefinition {columnName} -> columnName) cols
      in execute_ conn $ fromString $ "INSERT INTO " <> alteredTable
      <> " (" <> columnNames <> ") "
      <> "SELECT " <> columnNames
      <> " FROM " <> table
