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
  pure column {columnMigrationFieldChecks = filter (not . notNullConstraint) (columnMigrationFieldChecks column)}
  where
    qName = QualifiedName (Just "") ""
    notNullConstraint (FieldCheck f) = show (f qName "") == "(TableColumnHasConstraint Postgres: Column QualifiedName (Just \"\") \"\".\"\" has constraint NOT NULL)"