\name{calcular_correlaciones}
\alias{calcular_correlaciones}
\title{Tools of Professor Jose Ventura Leon}

\description{
This package provides a function to obtain a pearson, spearman or winsorized correlation matrix.
}

\usage{
calcular_correlaciones(data, columna_inicial, columna_final, method = c("spearman", "pearson"), winsorize = FALSE)
}

\arguments{
\item{data}{It is the database}
\item{columna_inicial}{It is the first column with the initial variable of the correlation.}
\item{columna_final}{It is the last column with the final variable of the correlation.}
\item{method}{The choice of Spearman or Pearson correlations}
\item{winsorize}{Default FALSE, when true plus Pearson's method, the Winsorized Pearson's coefficient is obtained.}
}

\examples{
resultado <- calcular_correlacion(df_new_renombrado, "Ansiedad de separación", "Búsqueda de atención", method = "pearson", winsorize = T)
print(resultado)

}
