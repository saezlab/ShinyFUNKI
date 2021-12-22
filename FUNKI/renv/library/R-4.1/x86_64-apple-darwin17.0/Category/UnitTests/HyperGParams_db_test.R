## Set to FALSE to see warning/error messages when running tests
## that verify that errors/warnings are raised.
quiet <- TRUE

.setUp <- function() {
    options(warn=2)
}

.tearDown <- function() {
    options(warn=0)
}

testValidDb <- function() {
    annodb <- AnnotationDbi::loadDb(system.file("UnitTests","test.sqlite",
                                                package = "Category", mustWork = TRUE))
    univ <- keys(annodb)
    sel <- sample(univ, 10)
    p1 <- new("KEGGHyperGParams",
                       geneIds=sel,
                       universeGeneIds=univ,
                       annotation=annodb,
                       pvalueCutoff=0.05,
                       testDirection="over")

    p2 <- new("GOHyperGParams",
                       geneIds=sel,
                       universeGeneIds=univ,
                       annotation=annodb,
                       pvalueCutoff=0.05,
                       testDirection="over")
    
    ## verify that our fixups work as expected
    options(warn=0)
    p3 <- new("GOHyperGParams",
              geneIds=c(sel, sel, 9000L),
              universeGeneIds=c(univ, univ),
              annotation=annodb,
              pvalueCutoff=0.05,
              testDirection="over")
    checkEquals(sel, geneIds(p3))
    checkEquals(univ, universeGeneIds(p3))
    dbFileDisconnect(dbconn(annodb))
}

testDupGeneIdsDb <- function() {
    annodb <- AnnotationDbi::loadDb(system.file("UnitTests","test.sqlite",
                                                package = "Category", mustWork = TRUE))
    univ <- keys(annodb)
    sel <- rep(sample(univ, 4), c(4,3,2,1))
    checkException(new("KEGGHyperGParams",
                       geneIds=sel,
                       universeGeneIds=univ,
                       annotation=annodb,
                       pvalueCutoff=0.05,
                       testDirection="over"),
                   silent=quiet)

    checkException(new("GOHyperGParams",
                       geneIds=sel,
                       universeGeneIds=univ,
                       annotation=annodb,
                       pvalueCutoff=0.05,
                       testDirection="over"),
                   silent=quiet)
    dbFileDisconnect(dbconn(annodb))
}

testDupUniverseGeneIdsDb <- function() {
    annodb <- AnnotationDbi::loadDb(system.file("UnitTests","test.sqlite",
                                                package = "Category", mustWork = TRUE))
    univ <- rep(keys(annodb), 2)
    sel <- unique(sample(univ, 5))
    checkException(new("KEGGHyperGParams",
                       geneIds=sel,
                       universeGeneIds=univ,
                       annotation=annodb,
                       pvalueCutoff=0.05,
                       testDirection="over"),
                   silent=quiet)

    checkException(new("GOHyperGParams",
                       geneIds=sel,
                       universeGeneIds=univ,
                       annotation=annodb,
                       pvalueCutoff=0.05,
                       testDirection="over"),
                   silent=quiet)
    dbFileDisconnect(dbconn(annodb))
}

testGeneIdsMatchUniverseGeneIdsDb <- function() {
    annodb <- AnnotationDbi::loadDb(system.file("UnitTests","test.sqlite",
                                                package = "Category", mustWork = TRUE))
    univ <- keys(annodb)
    sel <- as.character(1:10)
    checkException(new("KEGGHyperGParams",
                       geneIds=sel,
                       universeGeneIds=univ,
                       annotation=annodb,
                       pvalueCutoff=0.05,
                       testDirection="over"),
                   silent=quiet)

    checkException(new("GOHyperGParams",
                       geneIds=sel,
                       universeGeneIds=univ,
                       annotation=annodb,
                       pvalueCutoff=0.05,
                       testDirection="over"),
                   silent=quiet)
    dbFileDisconnect(dbconn(annodb))
}

testBadPvalueCutoffDb <- function() {
    annodb <- AnnotationDbi::loadDb(system.file("UnitTests","test.sqlite",
                                                package = "Category", mustWork = TRUE))
    univ <- keys(annodb)
    sel <- sample(univ, 5)
    checkException(new("KEGGHyperGParams",
                       geneIds=sel,
                       universeGeneIds=univ,
                       annotation=annodb,
                       pvalueCutoff=5,
                       testDirection="over"),
                   silent=quiet)

    checkException(new("GOHyperGParams",
                       geneIds=sel,
                       universeGeneIds=univ,
                       annotation=annodb,
                       pvalueCutoff=-.05,
                       testDirection="over"),
                   silent=quiet)
    dbFileDisconnect(dbconn(annodb))
}


