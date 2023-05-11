\name{Plot_Likert}
\alias{Plot_Likert}
\title{Tools of Professor Jose Ventura Leon}

\description{
This function generates a bar chart to summarize the items of a Likert-type scale.
}

\usage{
Plot_Likert(Data, name_items, rango)
}

\arguments{
\item{Data}{It is the database}
\item{name_items}{It is the name of the items}
\item{ranges}{range of the number of test items}
\item{exclude}{The number to be placed is the one to be removed}


}


\examples{
Plot_Likert(Data, "CCOV", 1:25, exclude = c(1,2))
}
