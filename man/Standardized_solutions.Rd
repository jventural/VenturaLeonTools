\name{Standardized_solutions}
\alias{Standardized_solutions}
\title{Tools of Professor Jose Ventura Leon}

\description{
This package provides a function to obtain the factor loadings of the model.
}

\usage{
Standardized_solutions(specification, name_items, apply_threshold = TRUE)
}

\arguments{
\item{specification}{It is the model obtained with the specification_models function. Specifying the factor through square brackets}
\item{name_items}{It is the name of the items}
\item{apply_threshold}{If TRUE eliminates values less than 0.30 in the other factors. Default FALSE}

}


\examples{
result_df <- process_standardized_solution(Specifications[[5]], name_items = "CCOV", num_factors = 5, apply_threshold = FALSE)
}
