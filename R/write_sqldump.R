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
#'  write_sqldump(iris, file.path(tempdir(), "dump.sql"))
#'
#' @export
write_sqldump <- function(table_list, outfile) {

    stopifnot(class(table_list) == "list")
    stopifnot(all(grepl("data\\.frame", unlist(lapply(table_list, class)))))

    if (file.exists(outfile)) file.remove(outfile)

    for (tblname in names(table_list)) {

        # Schema builder
        colnames(table_list[[tblname]]) <- gsub("[[:punct:]]", "_", colnames(table_list[[tblname]]))
        schema <- lapply(table_list[[tblname]], class)
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
        for (i in 1:ncol(table_list[[tblname]])) {
            if (class(table_list[[tblname]][, i]) == "factor") {
                table_list[[tblname]][, i] <- as.character(table_list[[tblname]][, i])
            }
            if (class(table_list[[tblname]][, i]) == "character") {
                table_list[[tblname]][, i] <- paste0("'",table_list[[tblname]][, i],"'")
            }
        }

        textlines <- rbind(textlines, paste("INSERT INTO", tblname, "VALUES ("))
        for (datum in 1:nrow(table_list[[tblname]])) {
            textlines <- rbind(textlines,
                               paste0("    ", paste(table_list[[tblname]][datum, ], collapse = ", "), ","))
        }
        textlines <- rbind(textlines, ");")
        last_field <- which(textlines$sql == ");") - 1
        textlines$sql[last_field] <- gsub(",$", "", textlines$sql[last_field])

        # Write out
        cat(textlines$sql[2:length(textlines$sql)], file = outfile, sep = "\n", append = TRUE)

    }
}
