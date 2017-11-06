\name{read.weka}
\alias{read.weka}
\title{
Read WEKA ARFF files
}
\description{
Import WEKA ARFF files and convert coerce data to data.frame
}
\usage{
read.weka(filename, asList = FALSE)
}
\arguments{
  \item{filename}{name of the ARFF file}
  \item{asList}{logical. If TRUE, a list including the dataframe and the WEKA relation is returned}
  \item{tmpFileName}{string. Name of the temporary file saved while processing the ARFF file}
}
\examples{
df <- read.weka("https://raw.githubusercontent.com/dami82/wekaLib/master/data/weather.arff")
head(df)
}
