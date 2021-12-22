library("Category")
library("org.Sc.sgd.db")

test_PFAM1 <- function() {
    set.seed(434)    
    allYeast <- ls(org.Sc.sgdCHR)
    selGenes <- sample(allYeast, 80)
    pp <- new("PFAMHyperGParams",
              geneIds=selGenes,
              annotation="org.Sc.sgd")
    ans <- hyperGTest(pp)
    checkEquals("PFAM", testName(ans))
}
