library(BMLGrid)

test <- function(){
    g1=createBMLGrid(0.5)
    g2=createBMLGrid(1)
    x=all(runBMLGrid(g1,20)==crunBMLGrid(g1,20)) & all(runBMLGrid(g2,20)==crunBMLGrid(g2,20)) 
    x
}

context("Whether our move functions match")

test_that("test() should give TRUE", {
    expect_equal(test(), TRUE)

})
