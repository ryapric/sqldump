# Get table names from SQL dump file
#
# @param query SQL query script, as a `character` vector, returned from reading
#   a SQL dump file via `readLines()`.
get_sql_tablenames <- function(query) {
  tables <- grep("CREATE TABLE", query, ignore.case = TRUE, value = TRUE)
  tables <- gsub("CREATE TABLE (.+?) .*", "\\1", tables, ignore.case = TRUE)
  tables <- gsub(".*\\.(.*)", "\\1", tables)
  tables
}

# Build SQL query from SQL dump file
#
# @param file SQL dump script.
build_sql_query <- function(file) {
  # Vector adjustment, just in case the dump has multiple lines per statement
  query <- paste(readLines(file), collapse = "\n")
  tablenames <- get_sql_tablenames(query)

  dump_type <- "sqlite"
  if (grepl("-- PostgreSQL database dump", query)) {
    dump_type <- "postgres"
  }
  query <- unlist(strsplit(query, split = ";\n"))
  query <- trimws(unlist(strsplit(query, split = "--")))
  query <- paste0(query, ";")
  query <- gsub(";;", ";", query)

  if (dump_type == "postgres") {
    # PostgreSQL dump uses pasted values (stdin) as its raw data input stream, so big parse loop here
    for (i in length(query[grepl("COPY .* FROM stdin", query)])) {
      col_names <- query[i]
      col_names <- gsub("COPY .* (\\(.*\\)).*", "\\1", col_names)

      stdin_data <- query[which(grepl("COPY .* FROM stdin", query)) + 1]
      # stdin_data <- gsub("\\.;", "", stdin_data, fixed = TRUE)
      # stdin_data <- unlist(strsplit(stdin_data, "\n"))

      postgres_query <- paste("COPY", tablenames[i], col_names, "FROM stdin;")

      # postgres_query <- paste(
      #   "INSERT INTO", tablenames[i],
      #   col_names,
      #   "VALUES",
      #
      # )
    }
  }

  # For SQLite compatibilty with other RDBMS, only keep SQL standards
  delete_stmts <- c(
    "sqlite_sequence"
    # etc, based on other RDBMS
  )
  keep_stmts <- c(
    "CREATE TABLE",
    "INSERT INTO"
    # etc, based on other RDBMS
  )

  query <- query[!grepl(paste(delete_stmts, collapse = "|"), query, ignore.case = TRUE)]
  query <- query[grepl(paste(keep_stmts, collapse = "|"), query, ignore.case = TRUE)]
  if (dump_type == "postgres") {
    query <- c(query, postgres_query)
  }

  query
}



#' Read SQL Dump File
#'
#' @param file File path to a SQL dump. Should have extension `.sql`.
#'
#' @examples
#'  read_sqldump(system.file("example_data/example-sqlite-dump.sql", package = "sqldump", mustWork = TRUE))
#'  read_sqldump(system.file("example_data/example-mysql-dump.sql", package = "sqldump", mustWork = TRUE))
#'  read_sqldump(system.file("example_data/example-postgresql-dump.sql", package = "sqldump", mustWork = TRUE))
#'
#' @export
read_sqldump <- function(file) {
  query <- build_sql_query(file)

  con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
  for (statement in query) {
    RSQLite::dbExecute(con, statement)
  }

  db_tables <- list()
  for (db_table in RSQLite::dbListTables(con)) {
    db_tables[[db_table]] <- RSQLite::dbReadTable(con, db_table)
  }

  RSQLite::dbDisconnect(con)
  db_tables
}