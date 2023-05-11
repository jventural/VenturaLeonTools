\name{extract_f_items}
\alias{extract_f_items}
\title{Tools of Professor Jose Ventura Leon}

\description{
This is a function that allows to obtain the organization of the items of a factorial structure.
}

\usage{
extract_f_items(Patron, prefixes)
}

\arguments{
\item{Patron}{It is the pattern matrix or factorial structure of the items.}
\item{prefixes}{They are the name of the factors}

}


\examples{
prefixes <- c("f1", "f2", "f3", "f4", "f5")
extract_f_items(Patron, prefixes)
}
