\name{calcular_correlaciones}
\alias{calcular_correlaciones}
\title{Tools of Professor Jose Ventura Leon}

\description{
This package provides a function to obtain a pearson, spearman or winsorized correlation matrix.
}

\usage{
calcular_correlaciones(data, columnas, method = c("spearman", "pearson"), winsorize = FALSE)
}

\arguments{
\item{data}{It is the database}
\item{columnas}{Are the names of the columns you want to be correlated.}
\item{method}{The choice of Spearman or Pearson correlations}
\item{winsorize}{Default FALSE, when true plus Pearson's method, the Winsorized Pearson's coefficient is obtained.}
}

\examples{
result <- calcular_correlaciones(data, c("columna1", "columna2", "columna3"), method = "spearman")
}
