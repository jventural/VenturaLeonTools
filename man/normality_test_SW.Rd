\name{normality_test_SW}
\alias{normality_test_SW}
\title{Tools of Professor Jose Ventura Leon}

\description{
This function allows the calculation of the Shapiro-Wilk normality statistic.
}

\usage{
normality_test_SW(data, variables)
}

\arguments{
\item{data}{It is the database}
\item{variables}{Variables of interest}

}


\examples{
normality_test_SW(df_new_renombrado, c(`Ansiedad de separación`:Autoestima))
}
