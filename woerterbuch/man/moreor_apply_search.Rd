% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/more_or.R
\name{moreor_apply_search}
\alias{moreor_apply_search}
\alias{moreor_term_search}
\title{Search multiple OR-Groups Across Multiple Documents (Parallel)}
\usage{
moreor_apply_search(corpus, conditions)

moreor_term_search(doc, conditions)
}
\arguments{
\item{corpus}{Ein Korpus von Dokumenten (Liste von Zeichenketten).}

\item{conditions}{Eine Liste von Bedingungen mit AND-OR-Kombinationen.}

\item{doc}{Ein Dokument (Zeichenkette), das durchsucht werden soll.}
}
\value{
Eine Liste der relevanten Dokumente mit den gefundenen Begriffen.

Eine Liste der gefundenen Woerter, die die Bedingungen erfuellen.
}
\description{
Diese Funktion durchsucht mehrere Dokumente parallel nach Begriffen gemaess den Bedingungen.
}
