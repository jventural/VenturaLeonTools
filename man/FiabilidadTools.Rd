\name{FiabilidadTools}
\alias{VenturaleonTools}
\title{Herramientos del profesor Jose Ventura Leon}

\description{
This package offers a function to calculate the reliability by means of the omega coefficient of the factors under study.
}

\usage{
Fiabilidad(vars, data)
}

\arguments{
\item{vars}{Object obtained by extracted function}
\item{data}{is the database}

}


\examples{
vars <- extracted$Insensibilidad
resultado_final <- Fiabilidad(vars = vars, data = Data_Rodriguez_renamed)
print(resultado_final)
}
