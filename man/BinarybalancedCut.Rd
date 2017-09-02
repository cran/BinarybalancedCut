\name{Binary_threshold}
\alias{Binary_threshold}
\title{This Supports the datascientist to determine the optimal threshold for binary classifier problem by visuallizing the sensitivity, specificity and accurarcy of the given model}
\usage{
Binary_threshold(probability,class)
}
\arguments{
  \item{probability}{Probability Obtained from the model}

  \item{class}{Actual Class of the datasets}
}
\description{
Prints 'Chart of sensitivity & specificity'.
}
\examples{
set.seed(100);disease <- sample(c("yes","no"), 1000, replace=TRUE);
Probabilities<-sample(seq(0,1,by=0.01),1000,replace=TRUE);
Binary_threshold(Probabilities,disease)}
