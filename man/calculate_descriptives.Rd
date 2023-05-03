\name{Calcule_descriptives}
\alias{VenturaleonTools}
\title{Tools of Professor Jose Ventura Leon}

\description{
Este paquete ofrece una función para calcular los estadisticos descriptivos de los factores en estudio.
}

\usage{
Calcule_descriptives(data, start_col, end_col)
}

\arguments{
\item{data}{is the database}
\item{start_col}{is the first column}
\item{end_col}{is the last column}

}


\examples{
Descriptivos <- calculate_descriptives(data, "Depression", "Anxiety")
Descriptivos
}
