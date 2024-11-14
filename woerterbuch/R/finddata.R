

#' Apply Complex Term Search Across Multiple Documents with Dynamic Conditions
#'
#' Diese Funktion durchsucht mehrere Dokumente nach Begriffen basierend auf den dynamisch generierten Bedingungen.
#' Der Benutzer kann angeben, welche OR-Gruppen innerhalb einer AND-Bedingung er suchen möchte.
#'
#' @import foreach
#' @import doParallel
#' @param corpus Ein Korpus von Dokumenten (Liste von Zeichenketten).
#' @param conditions Eine Liste von Bedingungen, die durch generate_conditions erzeugt wurde.
#' @return Eine Liste der relevanten Dokumente mit den gefundenen Begriffen.
#' @export
apply_term_search <- function(corpus, conditions) {
  found_documents <- list()

  for (doc_id in names(corpus)) {
    doc <- corpus[[doc_id]]

    matched_words <- list()
    for (and_group in conditions) {
      # Jede AND-Gruppe wird durch OR-Gruppen innerhalb der Gruppe überprüft
      or_matches <- sapply(and_group$OR, function(word) stringr::str_detect(doc, word))

      # Wenn ein Treffer für eine OR-Gruppe gefunden wird, speichern wir den gefundenen Begriff
      if (any(or_matches)) {
        matched_words <- c(matched_words, and_group$OR[or_matches])
      }
    }

    if (length(matched_words) > 0) {
      found_documents[[doc_id]] <- list(
        doc_id = doc_id,
        document = doc,
        found_words = matched_words
      )
    }
  }

  return(found_documents)
}



#' helper function for apply_term_search()
#' @rdname apply_term_search
#' @import stringr
#' @import dplyr
#' @param doc Ein Dokument (Zeichenkette), das durchsucht werden soll.
#' @param conditions Eine Liste von Bedingungen mit AND-OR-Kombinationen.
#' @return Eine Liste der gefundenen Wörter, die die Bedingungen erfüllen.
#' @export
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
