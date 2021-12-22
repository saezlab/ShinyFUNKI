library("hgu95av2.db")
## Set to FALSE to see warning/error messages when running tests
## that verify that errors/warnings are raised.
quiet <- TRUE

.setUp <- function() {
    options(warn=2)
}

.tearDown <- function() {
    options(warn=0)
}

testValid <- function() {
    univ <- 1:100
    sel <- sample(univ, 10)
    p1 <- new("KEGGHyperGParams",
                       geneIds=sel,
                       universeGeneIds=univ,
                       annotation="hgu95av2",
                       pvalueCutoff=0.05,
                       testDirection="over")

    p2 <- new("GOHyperGParams",
                       geneIds=sel,
                       universeGeneIds=univ,
                       annotation="hgu95av2",
                       pvalueCutoff=0.05,
                       testDirection="over")
    
    ## verify that our fixups work as expected
    options(warn=0)
    p3 <- new("GOHyperGParams",
              geneIds=c(sel, sel, 9000L),
              universeGeneIds=c(univ, univ),
              annotation="hgu95av2",
              pvalueCutoff=0.05,
              testDirection="over")
    checkEquals(sel, geneIds(p3))
    checkEquals(univ, universeGeneIds(p3))
}

testDupGeneIds <- function() {
    univ <- 1:100
    sel <- as.integer(c(1, 1, 1, 2, 3, 4, 5))
    checkException(new("KEGGHyperGParams",
                       geneIds=sel,
                       universeGeneIds=univ,
                       annotation="hgu95av2",
                       pvalueCutoff=0.05,
                       testDirection="over"),
                   silent=quiet)

    checkException(new("GOHyperGParams",
                       geneIds=sel,
                       universeGeneIds=univ,
                       annotation="hgu95av2",
                       pvalueCutoff=0.05,
                       testDirection="over"),
                   silent=quiet)

}

testDupUniverseGeneIds <- function() {
    univ <- rep(1:100, 2)
    sel <- 1:10
    checkException(new("KEGGHyperGParams",
                       geneIds=sel,
                       universeGeneIds=univ,
                       annotation="hgu95av2",
                       pvalueCutoff=0.05,
                       testDirection="over"),
                   silent=quiet)

    checkException(new("GOHyperGParams",
                       geneIds=sel,
                       universeGeneIds=univ,
                       annotation="hgu95av2",
                       pvalueCutoff=0.05,
                       testDirection="over"),
                   silent=quiet)
}

testGeneIdsMatchUniverseGeneIds <- function() {
    univ <- 1:100
    sel <- as.character(1:10)
    checkException(new("KEGGHyperGParams",
                       geneIds=sel,
                       universeGeneIds=univ,
                       annotation="hgu95av2",
                       pvalueCutoff=0.05,
                       testDirection="over"),
                   silent=quiet)

    checkException(new("GOHyperGParams",
                       geneIds=sel,
                       universeGeneIds=univ,
                       annotation="hgu95av2",
                       pvalueCutoff=0.05,
                       testDirection="over"),
                   silent=quiet)
}

testBadPvalueCutoff <- function() {
    univ <- as.character(1:100)
    sel <- as.character(1:10)
    checkException(new("KEGGHyperGParams",
                       geneIds=sel,
                       universeGeneIds=univ,
                       annotation="hgu95av2",
                       pvalueCutoff=5,
                       testDirection="over"),
                   silent=quiet)

    checkException(new("GOHyperGParams",
                       geneIds=sel,
                       universeGeneIds=univ,
                       annotation="hgu95av2",
                       pvalueCutoff=-.05,
                       testDirection="over"),
                   silent=quiet)
}
