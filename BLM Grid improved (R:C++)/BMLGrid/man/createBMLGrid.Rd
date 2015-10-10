\name{createBMLGrid}
\alias{createBMLGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Creates a matrix for the BML Model.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Creates a matrix for the BLM Model with size r times c. This matrix has entries 0, 1, 2 that corresponds to none, red, blue cars. Returns an object with class "BMLGrid". You can specify either the density of cars (between 0 and 1) or the number of cars with ncars.
}
\usage{
createBMLGrid(density, r = 100, c = 100, ncars)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{density}{
%%     ~~Describe \code{density} here~~
optional, double, density of cars (should be between 0 and 1)
}
  \item{r}{
%%     ~~Describe \code{r} here~~
integer, number of rows of the matrix
}
  \item{c}{
%%     ~~Describe \code{c} here~~
integer, number of columns of the matrix
}
  \item{ncars}{
%%     ~~Describe \code{ncars} here~~
optional, integer vector of length two, first component is the number of red cars, second component is the number of blue cars
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
You should specify either density or ncars. Default values r=c=100.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns an object of classes "BMLGrid", "matrix". This matrix has entries 0, 1, 2 that corresponds to none, red, blue cars.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Vladimir Pchelin
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##example1
g=createBMLGrid(0.2,10,10)
g=runBMLGrid(g,100)
par(mfrow=c(1,2))
plot(g,"density=0.2")
g=createBMLGrid(0.5)
g=runBMLGrid(g,100)
plot(g,"density=0.5")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
