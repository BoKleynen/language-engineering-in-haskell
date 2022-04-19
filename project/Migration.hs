{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Migration where

import Control.Monad.State (State, MonadState (state))


class Monad m => MonadMigration m where
  createTable :: String -> CreateTableM a -> m TableDefinition

  dropTable :: String -> m ()

  alterTable :: String -> AlterTableM a -> m AlterTable

newtype MigrationM a = MigrationM { unMigrationM :: State [TableDefinition] a }
  deriving (Functor, Applicative, Monad)

newtype AlterTableM a = AlterTableM { execAlterTableM :: State AlterTable a }

data AlterTable = AlterTable
  { columnAdds :: [ColumnDefinition]
  , columnDrops :: [String]
  , tcAdds :: [TableConstraint]
  , tcDrops :: [String]
  }

newtype CreateTableM a = CreateTableM { execCreateTableM :: State TableDefinition a }
  deriving (Functor, Applicative, Monad)

data TableDefinition = TableDefinition
  { columns :: [ColumnDefinition]
  , tableconstraints :: [TableConstraint]
  }

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
  | References String

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

instance a ~ ColumnDefinition => CreateColumnType (AlterTableM a) where
  createColumn_ :: ColumnType -> String -> [Constraint] -> AlterTableM ColumnDefinition
  createColumn_ typ name constraints = AlterTableM $ state addColumn
    where
      addColumn at@AlterTable{..} =
          let col = ColumnDefinition name typ constraints
          in (col, at { columnAdds = col : columnAdds })

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
