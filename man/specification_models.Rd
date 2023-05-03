\name{specification_models}
\alias{specification_models}
\title{Tools of Professor Jose Ventura Leon}

\description{
This package provides a function for specifying exploratory factorial models with lavaan.
}

\usage{
specification_models(modelos, data, estimator)
}

\arguments{
\item{modelos}{It is the model obtained with the generate_models function.}
\item{data}{It is the database}
\item{estimator}{is the estimator}

}


\examples{
Specifications <- specification_models(modelos, data = Data, estimator = "WLSMV")
}
