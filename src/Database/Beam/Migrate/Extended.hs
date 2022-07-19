{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Migrate.Extended
  ( module Database.Beam.Migrate,
    dropNotNullColumn,
  )
where

import Control.Monad.Writer.Strict
import Database.Beam.Backend (tableName)
import Database.Beam.Backend.SQL.AST (TableName (TableName))
import Database.Beam.Migrate
import Database.Beam.Postgres (Postgres)
import Universum

dropNotNullColumn ::
  ColumnMigration a ->
  TableMigration Postgres (ColumnMigration (Maybe a))
dropNotNullColumn column = TableMigration $ do
  (TableName curSchema curNm, _) <- get
  tell [alterTableSyntax (tableName curSchema curNm) (alterColumnSyntax (columnMigrationFieldName column) setNullSyntax)]
  pure column {columnMigrationFieldChecks = filter notNullConstraint (columnMigrationFieldChecks column)}
  where
    qName = QualifiedName (Just "1") "1"
    notNullConstraint (FieldCheck f) = show (f qName "1") == "(TableColumnHasConstraint Postgres: Column QualifiedName (Just \"1\") \"1\".\"1\" has constraint NOT NULL)"