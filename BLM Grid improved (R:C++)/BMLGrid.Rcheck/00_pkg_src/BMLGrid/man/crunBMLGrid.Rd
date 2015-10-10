\name{crunBMLGrid}
\alias{crunBMLGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Moves both types of cars in a matrix.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Moves both types of cars in a matrix with 0, 1, 2 values as entries. These 1's and 2's move according to the BML Model. Does that numSteps times. Returns an object with class "BMLGrid".
}
\usage{
crunBMLGrid(g, numSteps = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
%%     ~~Describe \code{g} here~~
a matrix with 0, 1, 2 entries, which correspond to none, red, blue cars
}
  \item{numSteps}{
%%     ~~Describe \code{numSteps} here~~
integer, number of moves you want cars to make
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Matrix with classes "BMLGrid", "matrix".
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
##Example1
g=createBMLGrid(0.2,10,10)
g=crunBMLGrid(g,100)
par(mfrow=c(1,2))
plot(g,"density=0.2")
g=createBMLGrid(0.5)
g=crunBMLGrid(g,100)
plot(g,"density=0.5")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
