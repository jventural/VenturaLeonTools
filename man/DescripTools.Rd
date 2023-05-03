\name{DescripTools}
\title{Tools of Professor Jose Ventura Leon}

\description{
This package offers a function to describe the sociodemographic variables of the study.
}

\usage{
DescripTools(x, columnas)
}

\arguments{
\item{x}{a dataframe}
\item{columnas}{is the name of the sociodemographic variables}

}


\examples{
columnas <- c("Sexo", "Lugar", "Estado_civil")
resultados_lista <- calcular_porcentajes(sociodemo, columnas)
resultados_lista$Sexo
}
