library("hgu95av2.db")
library("org.Sc.sgd.db")
library("GOstats")
##library("test_basic_regression_hgu95av2")

makeSimpleGOHyperGParams <- function(chip=c("hgu95av2", "org.Sc.sgd")) {
    set.seed(344)
    chip <- match.arg(chip)
    if (chip == "hgu95av2") {
        probeIds <- ls(hgu95av2ENTREZID)
    } else if (chip == "org.Sc.sgd") {
        probeIds <- ls(org.Sc.sgdCHR)
    }
    randProbeIds <- sample(probeIds, 500)
    if (chip == "org.Sc.sgd") {
        entrezUniverse <- randProbeIds
    } else {
        entrezUniverse <- mget(randProbeIds, hgu95av2ENTREZID,
                               ifnotfound=NA)
    }
    entrezUniverse <- entrezUniverse[!is.na(entrezUniverse)]
    selectedEntrezIds <- sample(entrezUniverse, 30)
    params <- new("GOHyperGParams",
                  geneIds=selectedEntrezIds, 
                  universeGeneIds=entrezUniverse,
                  annotation=chip, 
                  ontology="BP",
                  pvalueCutoff=0.05,
                  conditional=FALSE,
                  testDirection="over")
    params
}
    

test_basic_regression_hgu95av2 <- function() {
    p <- makeSimpleGOHyperGParams(chip="hgu95av2")
    res <- hyperGTest(p)

    checkEquals("hgu95av2", annotation(res))
    checkEquals(c("GO", "BP"), testName(res))
}


test_basic_regression_YEAST <- function() {
    p <- makeSimpleGOHyperGParams(chip="org.Sc.sgd")
    res <- hyperGTest(p)

    checkEquals("org.Sc.sgd", annotation(res))
    checkEquals(c("GO", "BP"), testName(res))
}
