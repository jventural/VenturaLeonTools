\name{factor_summary}
\alias{VenturaleonTools}
\title{Herramientos del profesor Jose Ventura Leon}

\description{
This package offers a function to extract the information of the loadings, communalities and uniqueness of the items.
}

\usage{
factor_summary(factors_data, num_items, num_factors)
}

\arguments{
\item{factors_data}{It is the object of the psych library}
\item{num_items}{number of items}
\item{num_factors}{number of factors}

}


\examples{
summary_data <- factor_summary(factors_data, num_rows = 24, num_factors = 5)
print(summary_data)
}
