
#' Search multiple OR-Groups Across Multiple Documents with Context (Parallel)
#'
#' Diese Funktion durchsucht mehrere Dokumente parallel nach Begriffen und berücksichtigt dabei den Kontext (eine Seite davor und danach).
#'
#' @import foreach
#' @import doParallel
#' @param dataset Ein DataFrame mit den Seiteninformationen.
#' @param conditions Eine Liste mit Suchbedingungen.
#' @return Eine Liste der relevanten Dokumente mit den gefundenen Begriffen und Kontext.
#' @export
moreor_apply_contextual_search <- function(dataset, conditions) {
  # Erstelle einen Parallel-Cluster
  cl <- makeCluster(detectCores())
  clusterExport(cl, varlist = c("moreor_term_search"))
  registerDoParallel(cl)

  # Liste für relevante Ergebnisse
  relevant_pages <- foreach(
    page_id = seq_len(nrow(dataset)), .combine = "rbind", .packages = c("stringr", "dplyr")
  ) %dopar% {
    # Extrahiere aktuelle Seite und realfilename
    current_page <- dataset[page_id, ]
    current_filename <- current_page$realfilename

    # Bestimme vorherige und nächste Seite nur, wenn sie denselben realfilename haben
    prev_page <- if (page_id > 1 && dataset[page_id - 1, "realfilename"] == current_filename) dataset[page_id - 1, ] else NULL
    next_page <- if (page_id < nrow(dataset) && dataset[page_id + 1, "realfilename"] == current_filename) dataset[page_id + 1, ] else NULL

    # Kombiniere Text der Kontextseiten nur bei gleichem realfilename
    context_text <- paste(
      if (!is.null(prev_page)) prev_page$text else "",
      current_page$text,
      if (!is.null(next_page)) next_page$text else "",
      sep = " "
    )

    # Suche nach Bedingungen im kombinierten Text
    found_words <- moreor_term_search(context_text, conditions)

    # Wenn relevante Begriffe gefunden wurden, speichere sie
    if (length(found_words) > 0) {
      return(data.frame(
        page = current_page$page,
        realfilename = current_filename,
        found_words = I(list(found_words))
      ))
    } else {
      return(NULL)
    }
  }

  # Stoppe den Parallel-Cluster
  stopCluster(cl)

  # Entferne NULL-Werte
  relevant_pages <- relevant_pages[!sapply(relevant_pages, is.null), ]

  return(relevant_pages)
}


#' Search multiple OR-Groups Across Multiple Documents (Parallel)
#'
#' Diese Funktion durchsucht mehrere Dokumente parallel nach Begriffen gemaess den Bedingungen.
#'
#' @import foreach
#' @import doParallel
#' @param corpus Ein Korpus von Dokumenten (Liste von Zeichenketten).
#' @param conditions Eine Liste von Bedingungen.
#' @return Eine Liste der relevanten Dokumente mit den gefundenen Begriffen.
#' @export
moreor_apply_search <- function(corpus, conditions) {
  # Erstelle einen Parallel-Cluster
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)

  # Parallele Verarbeitung der Dokumente
  relevant_documents <- foreach(
    doc_id = names(corpus), .combine = "c", .packages = c("stringr", "foreach", "doParallel")
  ) %dopar% {
    doc <- corpus[[doc_id]]
    found_words <- moreor_term_search(doc, conditions)

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


#' helper function for moreor_apply_search()
#' @rdname moreor_apply_search
#' @import stringr
#' @import dplyr
#' @param doc Ein Dokument (Zeichenkette), das durchsucht werden soll.
#' @param conditions Eine Liste von Bedingungen mit AND-OR-Kombinationen.
#' @return Eine Liste der gefundenen Woerter, die die Bedingungen erfuellen.
#' @export
moreor_term_search <- function(doc, conditions) {
  found_words <- list()

  # Iteriere durch alle Bedingungen
  for (condition in conditions) {
    and_match <- TRUE
    matched_words <- list()

    # Ueberpruefe jede OR-Gruppe innerhalb der AND-Bedingung
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

    # Wenn alle OR-Gruppen erfuellt sind, fuege die gefundenen Begriffe hinzu
    if (and_match) {
      found_words <- c(found_words, matched_words)
    }
  }

  return(found_words)
}



#' Process Findings of multiple OR-Groups
#'
#' Diese Funktion verarbeitet die Ergebnisse der Suche mit mehreren OR-Gruppen und formatiert sie in einen
#' Datensatz, der Mehrfach-Treffer pro Dokument beruecksichtigt und in ein breites Format bringt.
#'
#' @import dplyr
#' @import rrapply
#' @import tidyr
#' @param relevant_documents Eine Liste von relevanten Dokumenten (wie von apply_term_search zurueckgegeben).
#' @return Ein DataFrame mit den formatierten Ergebnissen.
#' @export
moreor_process_findings <- function(relevant_documents) {

  # 1. Melt die relevanten Dokumente, um eine flache Struktur zu erzeugen
  findings <- rrapply(relevant_documents, how = "melt")

  # 2. Identifiziere Gruppen für mehrfach Treffer pro Dokument (z.B. mehrere relevante Begriffe pro Dokument)
  findings$group <- cumsum(findings$L1 == "doc_id")

  # 3. Auswahl der relevanten Spalten und Hinzufügen von Suffixen zur Unterscheidung der Suchgruppen
  df <- findings %>%
    select(L1, L2, value, group) %>%
    mutate(L1 = ifelse(is.na(L2), L1, paste0(L1, "_", L2))) %>%
    select(-L2)

  # 4. Umwandlung in breites Format und Entpacken von Listen
  result <- df %>%
    pivot_wider(names_from = "L1", values_from = "value") %>%
    mutate(across(everything(), ~ if (is.list(.x)) { map(.x, ~ paste0(.x, collapse = ", ")) } else { .x })) %>%
    unnest(cols = c(starts_with("found_words_"), document, doc_id)) %>%
    select(-group)

  # Rueckgabe des formatierten DataFrames
  return(result)
}


