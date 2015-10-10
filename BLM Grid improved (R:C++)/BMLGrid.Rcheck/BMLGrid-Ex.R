pkgname <- "BMLGrid"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('BMLGrid')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("createBMLGrid")
### * createBMLGrid

flush(stderr()); flush(stdout())

### Name: createBMLGrid
### Title: Creates a matrix for the BML Model.
### Aliases: createBMLGrid

### ** Examples

##example1
g=createBMLGrid(0.2,10,10)
g=runBMLGrid(g,100)
par(mfrow=c(1,2))
plot(g,"density=0.2")
g=createBMLGrid(0.5)
g=runBMLGrid(g,100)
plot(g,"density=0.5")



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("crunBMLGrid")
### * crunBMLGrid

flush(stderr()); flush(stdout())

### Name: crunBMLGrid
### Title: Moves both types of cars in a matrix.
### Aliases: crunBMLGrid

### ** Examples

##Example1
g=createBMLGrid(0.2,10,10)
g=crunBMLGrid(g,100)
par(mfrow=c(1,2))
plot(g,"density=0.2")
g=createBMLGrid(0.5)
g=crunBMLGrid(g,100)
plot(g,"density=0.5")



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("runBMLGrid")
### * runBMLGrid

flush(stderr()); flush(stdout())

### Name: runBMLGrid
### Title: Moves both types of cars in a matrix.
### Aliases: runBMLGrid

### ** Examples

##Example1
g=createBMLGrid(0.2,10,10)
g=runBMLGrid(g,100)
par(mfrow=c(1,2))
plot(g,"density=0.2")
g=createBMLGrid(0.5)
g=runBMLGrid(g,100)
plot(g,"density=0.5")



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
