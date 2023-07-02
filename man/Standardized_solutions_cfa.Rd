\name{Standardized_solutions_cfa}
\alias{Standardized_solutions_cfa}
\title{Tools of Professor Jose Ventura Leon}

\description{
This package provides a function to obtain the factor loadings of the model.
}

\usage{
Standardized_solutions_cfa(specification, name_items, apply_threshold = TRUE)
}

\arguments{
\item{specification}{It is the model obtained with the specification_models function. Specifying the factor through square brackets}
\item{name_items}{It is the name of the items}
\item{apply_threshold}{If TRUE eliminates values less than 0.30 in the other factors. Default FALSE}

}


\examples{
result_df <- Standardized_solutions_cfa(Specifications.fit, name_items = "CCOV", num_factors = 5, apply_threshold = FALSE)
}
