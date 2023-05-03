\name{generate_code}
\alias{VenturaleonTools}
\title{Herramientos del profesor Jose Ventura Leon}

\description{
This package offers a function to generate the code to calculate the sum of the variables under study.
}

\usage{
generate_code(text, "Data_name")
}

\arguments{
\item{text}{It is a text with the variables and their respective items.}
\item{Data_name}{is the name of the database}

}


\examples{
text <- "Validation: E8, E16
Comprehensibility: E5, E10, E33, E45
Guilt and embarrassment: E4, E14, E26, E31
Simplicity vs. Complexity: E18, E35, E38, E47
Relationship with high values: E21, E25, E42
Controllability: E7, E27, E44
Insensitivity: E15, E32
Rationality and anti-emotionality: E17, E30, E46
Duration of strong feelings: E13, E29
Consensus with others: E3, E19, E39, E41
Acceptance or inhibition: E2, E9, E12, E20, E28, E40
Rumination vs. instrumental style: E1, E36, E37, E48
Expression: E6, E23
Blaming others: E11, E34"

code <- generate_code(text, "Data_Ansiedad")
cat(code)
}
