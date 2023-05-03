\name{generate_modelos}
\alias{generate_modelos}
\title{Tools of Professor Jose Ventura Leon}

\description{
This package provides a function to generate exploratory factorial models with lavaan
}

\usage{
generate_modelos(n_factors, n_items, name_items, exclude_items = NULL)
}

\arguments{
\item{n_factors}{Are the number of factors}
\item{n_items}{Are the number of items}
\item{name_items}{Are the name of the items}
\item{exclude_items}{These are the items to be excluded. If none of them are excluded, leave NULL.}

}


\examples{
modelos <- generate_modelos(n_factors = 5, n_items = 27, name_items = "CCOV", exclude_items = c("CCOV1", "CCOV5"))
print(modelos)
}
