\name{export.weka}
\alias{export.weka}
\title{
Save data as WEKA ARFF files
}
\description{
Convert a data.frame into a WEKA ARFF file
}
\usage{
export.weka(data, filename, relation)
}
\arguments{
  \item{data}{
data.frame including the data
}
  \item{filename}{
string including the destination file name
}
  \item{relation}{
name of the WEKA relation (An identifier for the dataset)
}
}
\examples{
df <- data.frame(team1 = c("UDI", "CHI", "ROM", "MIL"),
                 score = c(15, 12, 9, 4),
                 goals = c(22, 25, 15, 5))
export.weka(df, "serieA.arff", "serieA")
}