\name{check_gquality}
\alias{}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to retrieve georeferenced records from GBIF, and then send them successfully to the Geospatial Quality API.
%%  ~~function to do ... ~~
}
\description{
The function achieves to do the following: \cr
1. Download records for a particular taxonomic class from GBIF. \cr
2. Then send these records to the Geospatial Quality API for checking the quality of records. \cr
3. Once the above steps are completed, the funtion returns a list consisting of two data frames. The first contains the records from GBIF and the second contains the flags for each corresponding record in the former. \cr
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
check_gquality(taxonName, countryCode, dataLimit)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{taxonName}{ The name of the Taxonomic Classification level. Example: Mammalia, Theria, Primates.}
  \item{countryCode}{The 2-letter country code (as per ISO-3166-1) of the country for which you require the occurence data.}
  \item{dataLimit}{The number of records you want to obtain from the GBIF portal.}
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns a list of two data frames 
%%  ~Describe the value returned 
%%  If it is a LIST, use
  \item{data}{A dataframe that contains all the records that are obtained from GBIF using the function occ_data()}
  \item{flags}{A dataframe containing flags that help assess the geospatial quality of corresponding records in the above dataframe}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
for Country Codes refer to this \href{https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2}{page on Wikipedia} \cr
for Taxons and Taxonomic Classifications refer to this \href{https://en.wikipedia.org/wiki/Taxonomic_rank}{page on Wikipedia}
}
\author{
Samriddhi Sinha \cr
Undergraduate Student \cr
IIT Kharagpur 
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## With default parameters taxonName ="Mammals", country_code = "AU", dataLimit = 5000
#check_gquality()
## For getting 500 occurence records for Reptiles in India 
#check_gquality(taxonName ="Reptilia", country_code = "IN", dataLimit = 500)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
