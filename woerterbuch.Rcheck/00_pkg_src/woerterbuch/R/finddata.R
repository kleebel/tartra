

#' Apply Term Search Across Multiple Documents (Parallel)
#'
#' Diese Funktion durchsucht mehrere Dokumente parallel nach Begriffen gemäß den Bedingungen.
#'
#' @import stringr
#' @import foreach
#' @import doParallel
#' @param corpus Ein Korpus von Dokumenten (Liste von Zeichenketten).
#' @return Eine Liste der relevanten Dokumente mit den gefundenen Begriffen.
apply_term_search <- function(corpus) {
  # Erstelle einen Parallel-Cluster
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)

  # Parallele Verarbeitung der Dokumente
  relevant_documents <- foreach(
    doc_id = names(corpus), .combine = "c", .packages = c("stringr", "foreach", "doParallel")
  ) %dopar% {
    doc <- corpus[[doc_id]]
    found_words <- find_terms(doc, oder_und_oder)

    # Wenn relevante Begriffe gefunden wurden, speichere sie
    if (length(found_words) > 0) {
      return(list(doc_id = doc_id, document = doc, found_words = found_words))
    } else {
      return(NULL)
    }
  }

  # Stoppe den Parallel-Cluster
  stopCluster(cl)

  # Entferne NULL-Werte (Dokumente ohne gefundene Begriffe)
  relevant_documents <- relevant_documents[!sapply(relevant_documents, is.null)]

  return(relevant_documents)
}


#' helper function for apply_term_search()
#' @rdname apply_term_search
#' @import stringr
#' @import dplyr
#' @param doc Ein Dokument (Zeichenkette), das durchsucht werden soll.
#' @param conditions Eine Liste von Bedingungen mit AND-OR-Kombinationen.
#' @return Eine Liste der gefundenen Begriffe, die den Bedingungen entsprechen.
find_terms <- function(doc, conditions) {
  found_words <- list()

  # Iteriere durch alle Bedingungen
  for (condition in conditions) {
    and_match <- TRUE
    matched_words <- list()

    # Überprüfe jede OR-Gruppe innerhalb der AND-Bedingung
    for (or_group_name in names(condition$AND)) {
      or_terms <- condition$AND[[or_group_name]]
      or_matches <- sapply(or_terms, function(word) stringr::str_detect(doc, word))

      if (any(or_matches)) {
        matched_words[[or_group_name]] <- or_terms[or_matches]
      } else {
        and_match <- FALSE
        break
      }
    }

    # Wenn alle OR-Gruppen erfüllt sind, füge die gefundenen Begriffe hinzu
    if (and_match) {
      found_words <- c(found_words, matched_words)
    }
  }

  return(found_words)
}
