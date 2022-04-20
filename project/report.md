# Motivation

Nowadays lots of applications depend on a relational database to persist certain data. There exist numerous database engines, for example postgres,
sqlite, mysql etc. All of them implement a part of the SQL standard together with some of their own extensions. You might wonder why do we need a DSL
when there's already a standard? The problem lies in the fact that none of them implement the entire SQL specification and most of them implement
different parts. It is true that they do all implement the same very basic features like a `SELECT` and `DELETE` statement. The variation in terms of
Data Definition Language (DDL) statements makes it very hard to write DDL statements that work accross different engines, especially the `ALTER TABLE` command is very limited in sqlite.

It is not uncommon for applications to rely on a client/server database engine like postgres for production workloads and use sqlite for test
environments or other environments where it's impossible to install postgres. Whilst most business queries can probably be reused, this is an entirely
different story for DDL statements once `ALTER TABLE` is involved since sqlite if very limitted in terms of what aspects of a table can be changed
without having to recreate the table and copy all data.

Copying the data will still be necessary in these cases, but perhaps it's possible to eleviate some of the burden of having to maintain a different set of migrations for each supported database engine. This project focusses on providing a proof of concept of how such a system could be build in haskell for postgres and sqlite.

# How to use?

Migrations are created using a monadic DSL that should be instantly familiar to anyone that has some basic knowledge of SQL.

This describes a migration to create a new table:
```haskell
createTable "movies" do
    bigInt "id" PK
    integer "duration" NotNull
    string "title" NotNull
    string "director" NotNull
    string "code" NotNull Unique

    unique ["director", "title"]
```

Once a table has been created, it can be changed using `alterTable`:
```haskell

```

When a table is no longer needed it can be dropped using:
```haskell
dropTable "movies"
```

In order to actually run a migration it should be provided to a database engine specific function that can execute it, e.g.: `sqliteMigration`
```haskell
main :: IO ()
main = sqliteMigration "db.sqlite3" do
    dropTable "movies"
```

# How it works



# Problems

These are some of the problems I personally encountered whilst working on this project in no particular order.

- Most of the available documentation and blog posts only mention the `=~` and `=~~` operators which doesn't allow for case insensitive matching, which
which due to the cases insensitive nature of SQL was needed to extract certain info from sqlite_master table. Another issue I encountered while trying
to figure out how the regex libraries in haskell work is their highly polymorphic nature, whilst probably great for advanced users, it does make it
harder to figure out what arguments have to be provided and what can be expected as output.

- Most sqlite libraries I encountered aren't actively maintained.

- Certain information about the schema in sqlite is quite hard to retrieve, which makes it quite tricky to emulate certain features that postgres has
out of the box. The reason for this is the fact that sqlite stores the schema information as plain text in a special table called sqlite_master, this
this is probably the same reason that the alter table support is so poor in sqlite.
