
#' Generate Search Conditions
#'
#' Diese Funktion erzeugt die Suchbedingungen basierend auf den Benutzereingaben.
#' Der Benutzer kann angeben, ob er die gesamte AND-Bedingung oder nur eine OR-Gruppe innerhalb einer AND-Bedingung verwenden möchte.
#'
#' @param and_conditions Ein Vektor mit AND-Bedingungen, jede AND-Bedingung enthält OR-Gruppen.
#' @param or_group (Optional) Eine spezifische OR-Gruppe, die gesucht werden soll.
#' @return Eine Liste von Bedingungen, die in apply_term_search verwendet werden können.
#' @export
generate_conditions <- function(and_conditions, or_group = NULL) {
  conditions <- list()

  # Wenn search_or_group angegeben ist, suchen wir nur in der entsprechenden OR-Gruppe
  if (!is.null(or_group)) {
    # Suche nur in der spezifischen OR-Gruppe innerhalb der AND-Bedingung
    for (and_name in names(and_conditions)) {
      or_groups <- and_conditions[[and_name]]

      if (or_group %in% names(or_groups)) {
        conditions[[and_name]] <- list(
          OR = or_groups[[or_group]]
        )
      }
    }
  } else {
    # Wenn keine spezifische OR-Gruppe angegeben ist, erstellen wir die gesamten AND-Bedingungen
    for (and_name in names(and_conditions)) {
      conditions[[and_name]] <- list(
        OR = unlist(and_conditions[[and_name]], use.names = FALSE)
      )
    }
  }

  return(conditions)
}
