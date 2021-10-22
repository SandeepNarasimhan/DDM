% File src/library/base/man/load.Rd
\name{project_DDM_multiple}
\alias{project_DDM_multiple}
\title{Project population using Different Distribution Method (DDM)}
\description{
Project population using Different Distribution Method, The function project_DDM takes input data frame of 16 Age group (00-75+) and two consicutive census population to project future Age Specific Population.

This function is does exactly as the project_DDM function but this can do it for multiple sheets at once
}
\usage{
project_DDM_multiple(file, year, number_of_sheets)

}
\arguments{
  \item{file}{A "CSV" file which contains the age group and census population information,
  Column name of Age Group can be anything but the census population column names must be numeric (Year)}
    \item{year}{year of the latest census population in the CSV file}
    \item{number_of_sheets}{Number of sheets to be imported from the CSV file to do projection}
}


\author{Sandeep N, Rohith Mohan}


\references{
  Ramnath Takiar (2009) \emph{Cancer incidence rates and the problem of denominators - A new approach in Indian cancer registries}.
}
