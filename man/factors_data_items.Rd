\name{factors_data_items}
\alias{VenturaleonTools}
\title{Tools of Professor Jose Ventura Leon}

\description{
This package offers a function that incorporates the phrase of the items in the factor structure.
}

\usage{
factors_data_items(summary_data, CCOV_items, num_factors)
}

\arguments{
\item{summary_data}{is the object obtained from the function factor_summary}
\item{CCOV_items}{is the data of the items with phrasing}
\item{num_factors}{It is the number of factors}

}


\examples{
Anxiety_items <- read_excel("Anxiety_items.xlsx")
factor_Patron <- factors_data_items(summary_data, CCOV_items, num_factors = 5)
factor_Patron
}
