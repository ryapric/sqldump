context("SQL dumps (sql) import")

iris_in_db <- iris
colnames(iris_in_db) <- tolower(gsub("\\.", "_", colnames(iris_in_db)))
iris_in_db$species <- as.character(iris_in_db$species)

db_tables <- list(iris = iris_in_db)

test_that("SQLite dump is imported as expected", {
  sqlite_dump <- system.file("example_data/example-sqlite-dump.sql",
                             package = packageName(), mustWork = TRUE)
  expect_equal(read_sqldump(sqlite_dump), db_tables)
})

# test_that("MySQL dump is imported as expected", {
#   mysql_dump <- system.file("examples/example-mysql-dump.sql",
#                                package = packageName(), mustWork = TRUE)
#   expect_equal(read_sqldump(mysql_dump), db_tables)
# })

# test_that("PostgreSQL dump is imported as expected", {
#   postgres_dump <- system.file("examples/example-postgresql-dump.sql",
#                                package = packageName(), mustWork = TRUE)
#   expect_equal(read_sqldump(postgres_dump), db_tables)
# })
