\name{calcula_omega_all}
\alias{calcula_omega_all}
\title{Professor Jose Ventura Leon's tools}

\description{
This package offers a function to calculate the reliability of all test factors.
}

\usage{
calcula_omega_all(extracted, data)
}

\arguments{
\item{extracted}{Object obtained by extracted function}
\item{data}{is the database}

}


\examples{
extracted <- extract_items(text)
resultado_final <- calcula_omega_all(extracted, data)
print(resultado_final)
}
