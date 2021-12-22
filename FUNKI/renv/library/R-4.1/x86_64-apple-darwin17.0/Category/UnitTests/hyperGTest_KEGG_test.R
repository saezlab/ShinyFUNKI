library("Category")
library("org.Sc.sgd.db")

test_KEGG1 <- function() {
    set.seed(434)    
    allYeast <- ls(org.Sc.sgdCHR)
    selGenes <- sample(allYeast, 80)
    kp <- new("KEGGHyperGParams",
              geneIds=selGenes,
              annotation="org.Sc.sgd")
    ans <- hyperGTest(kp)
    checkEquals("KEGG", testName(ans))
}
