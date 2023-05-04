\name{rename_items}
\alias{rename_items}
\title{Tools of Professor Jose Ventura Leon}

\description{
This package provides a function to rename all items that are with their phrasing to a designated abbreviation.
}

\usage{
rename_items(df_new,prefix1 = "CDE",prefix2 = "A",inici = "1. Me siento desprotegido(a) cuando estoy solo(a).",final = "10. A veces pienso que no sirvo para nada",n_items1 = 23,n_items2 = 10)
}

\arguments{
\item{df_new}{Es la base de datos}
\item{prefix1}{Abreviatura para los items del test 1}
\item{prefix2}{Abreviatura para los items del test 2}
\item{inici}{El fraseo del item donde inicia el renombrado}
\item{final}{El fraseo del item donde termina el renombrado}
\item{n_items1}{Cantidad de los items del test 1}
\item{n_items2}{Cantidad de los items del test 2}
}


\examples{
df_new_renombrado <-rename_items(df_new,
                                 prefix1 = "CDE",
                                 prefix2 = "A",
                                 inici = "1. Me siento desprotegido(a) cuando estoy solo(a).",
                                 final = "10. A veces pienso que no sirvo para nada",
                                 n_items1 = 23,
                                 n_items2 = 10)
}
