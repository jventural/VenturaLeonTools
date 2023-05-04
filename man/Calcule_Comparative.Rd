\name{Calcule_Comparative}
\alias{Calcule_Comparative}
\title{Tools of Professor Jose Ventura Leon}

\description{
This package provides a function to examine the difference of two groups by reporting statistics and the d cohen robustness.
}

\usage{
Calcule_Comparative(data, cols, group_var)
}

\arguments{
\item{data}{It is the database}
\item{cols}{The variables under study are}
\item{group_var}{It is the comparison variable}

}


\examples{
result <- calculate_stats(
  data = Data,
  cols = c("Soledad", "Columna2", "Columna3", "Columna4", "Columna5"),
  group_var = "Grupo_Edad"
)
result
}
