#' Write SQL dump file from data frame
#'
#' Provided a `data.frame`-like object, writes the equivalent SQL dump file to
#' `outfile`.
#'
#' @param table_list Named list of `data.frame`s to convert to SQL text. Names will
#'   be the names of the tables created from the dump file.
#' @param outfile Dump file destination
#'
#' @examples
#'  write_sqldump(list(iris = iris), file.path(tempdir(), "dump.sql"))
#'
#' @export
write_sqldump <- function(table_list, outfile) {

    stopifnot(class(table_list) == "list")
    stopifnot(any(grepl("data\\.frame", unlist(lapply(table_list, class)))))

    if (file.exists(outfile)) file.remove(outfile)

    for (tblname in names(table_list)) {

        # For less-verbose calls; re-assigned at end of loop
        df_i <- table_list[[tblname]]

        # Schema builder
        colnames(df_i) <- gsub("[[:punct:]]", "_", colnames(df_i))
        schema <- lapply(df_i, class)
        schema <- lapply(schema, function(x) gsub("character|factor", "TEXT", x))
        schema <- lapply(schema, function(x) gsub("numeric|double|integer", "NUMERIC", x))

        textlines <- data.frame(sql = "", stringsAsFactors = FALSE)
        textlines <- rbind(textlines, paste("CREATE TABLE", tblname, "("))
        for (field in names(schema)) {
            textlines <- rbind(textlines, sprintf("    %s %s, ", field, schema[[field]]))
        }
        textlines <- rbind(textlines, ");")
        last_field <- which(textlines$sql == ");") - 1
        textlines$sql[last_field] <- gsub(", $", "", textlines$sql[last_field])

        # INSERT VALUES
        # Convert factors to characters, and wrap values in single quotes for SQL
        for (i in 1:ncol(df_i)) {
            if (class(df_i[, i]) == "factor") {
                df_i[, i] <- as.character(df_i[, i])
            }
            if (class(df_i[, i]) == "character") {
                df_i[, i] <- paste0("'",df_i[, i],"'")
            }
        }

        textlines <- rbind(textlines, paste("INSERT INTO", tblname, "VALUES"))
        for (datum in 1:nrow(df_i)) {
            textline_i <- paste0("    (", paste(df_i[datum, ], collapse = ", "), "),")
            # Escape single quotes
            textline_i <- gsub("'", "''", textline_i)
            textlines <- rbind(textlines, textline_i)
        }
        textlines <- rbind(textlines, ";")
        last_field <- max(which(textlines$sql == ";")) - 1
        textlines$sql[last_field] <- gsub(",$", "", textlines$sql[last_field])

        # Write out
        cat(textlines$sql[2:length(textlines$sql)], file = outfile, sep = "\n", append = TRUE)

        # Re-assign back to actual table list
        table_list[[tblname]] <- df_i
    }
}
