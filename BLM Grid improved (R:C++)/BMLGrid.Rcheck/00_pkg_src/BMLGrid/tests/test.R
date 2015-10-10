library(BMLGrid)

grid = matrix(0, 3, 3)
grid[1, 1] = 1
grid[2, 2] = 2

grid = runBMLGrid(grid, 2)


if ( !(grid[1,3]==1 & grid[1,2]==2) )
  stop("Error: 5-step test failed!")

# the code is taken from a Duncan's comment