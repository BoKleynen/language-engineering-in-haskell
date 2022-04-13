{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Migration where

import Control.Monad.State (State, MonadState (state), execState)


example :: MigrationM TableDefinition
example = do
  dropTable "users"

  createTable "movies" do
    integer "id" PK NotNull
    string "title" NotNull
    string "director" NotNull
    unique ["director", "title"]


newtype MigrationM a = MigrationM { unMigrationM :: State [TableDefinition] a }
  deriving (Functor, Applicative, Monad)

createTable :: String -> CreateTableM a -> MigrationM TableDefinition
createTable name table = MigrationM $ state addTable
  where
    addTable tds =
      let td = execState (execCreateTableM table) (tableWithName name)
      in (td, td : tds)

dropTable :: String -> MigrationM ()
dropTable = error ""

alterTable :: String -> String -> MigrationM ()
alterTable = error ""

newtype AlterTableM a = AlterTableM { execAlterTableM :: State AlterTable a }

data AlterTable = AlterTable
  { tableDefinition :: TableDefinition
  , columnAdds :: [ColumnDefinition]
  , columnDrops :: [String]
  }

newtype CreateTableM a = CreateTableM { execCreateTableM :: State TableDefinition a }
  deriving (Functor, Applicative, Monad)

data TableDefinition = TableDefinition
  { tableName :: String
  , columns :: [ColumnDefinition]
  , tableconstraints :: [TableConstraint]
  }

tableWithName :: String -> TableDefinition
tableWithName name = TableDefinition name [] []

data ColumnDefinition = ColumnDefinition
  { columnName :: String
  , typ :: ColumnType
  , constraints :: [Constraint]
  }

data ColumnType
  = String
  | Int
  | BigInt
  | Bool
  | Raw String

data Constraint
  = PK
  | NotNull
  | Unique
  | Check String
  | Default String

createColumn :: CreateColumnType r => ColumnType -> String -> r
createColumn typ name = createColumn_ typ name []

class CreateColumnType r where
  createColumn_ :: ColumnType -> String -> [Constraint] -> r

instance a ~ ColumnDefinition => CreateColumnType (CreateTableM a) where
  createColumn_ :: ColumnType -> String -> [Constraint]  -> CreateTableM ColumnDefinition
  createColumn_ typ name constraints = CreateTableM $ state addColumn
    where
        addColumn t@TableDefinition{..} =
          let col = ColumnDefinition name typ constraints
          in (col, t { columns = col : columns })

instance CreateColumnType r => CreateColumnType (Constraint -> r) where
  createColumn_ :: ColumnType -> String -> [Constraint] ->  Constraint -> r
  createColumn_ typ name cs c = createColumn_ typ name (c : cs)

string :: CreateColumnType r => String -> r
string = createColumn String

integer :: CreateColumnType r => String -> r
integer = createColumn Int

bigInt :: CreateColumnType r => String -> r
bigInt = createColumn BigInt

bool :: CreateColumnType r => String -> r
bool = createColumn Bool

raw :: CreateColumnType r => String -> String -> r
raw name typ = createColumn (Raw typ) name


data TableConstraint = TableConstraint TableConstraintKind (Maybe String)

data TableConstraintKind
  = UniqueTC [String]
  | CheckTC String

unique :: [String] -> CreateTableM TableConstraint
unique columns = constraint $ TableConstraint (UniqueTC columns) Nothing

check :: String -> CreateTableM TableConstraint
check expr = constraint $ TableConstraint (CheckTC expr) Nothing

constraint :: TableConstraint -> CreateTableM TableConstraint
constraint tc = CreateTableM $ state addConstraint
  where
    addConstraint t@TableDefinition{..} = (tc, t { tableconstraints = tc : tableconstraints })
