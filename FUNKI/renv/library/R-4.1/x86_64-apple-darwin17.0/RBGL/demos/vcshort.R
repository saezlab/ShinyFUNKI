##Copyright R. Gentleman, 2003, all rights reserved


library(Biobase)
library(hgu95av2)

#load(paste(homedir, "Genetics/Sabina/Data143/ALL.rda", sep="/"))
load("ALL.rda")

Bs <- grep("^B", as.character(ALL$BT))

ALLsB <- ALL[,Bs]

ALLs <- ALLsB$mol == "BCR/ABL" | ALLsB$mol == "NEG"

##here is our subset of interest

BCRNEGsub <- ALLsB[,ALLs]


##first do some simple filtering....

library(genefilter)

##old filters
##  f1 <- pOverA(.1, 100)
##  f2 <- gapFilter(10, 150, .1)
f1 <- pOverA(.25, 300)
f2 <- gapFilter(200, 250, .1)

ff <- filterfun(f1, f2)

wh <- genefilter(BCRNEGsub, ff)

sum(wh)  ##got 5421 genes

##
BNsub <- BCRNEGsub[wh,]

##now we need to drop all probe sets that have duplicate
##LLIDs - since we don't know what to do with them; for
##now we just take the first, but users should do something more
##sensible

gN <- geneNames(BNsub)

library(annotate)

##FIXME:
##didn't handle the NA's here properly!
##also need to drop the AFFX* genes!
gLL <- getLL(gN, "hgu95av2")
wh2 <- !duplicated(gLL)
BNsub <- BNsub[wh2,]

##it might also make sense to drop those genes for which we have
##no GO annotation, but lets wait till a bit later on


##now lets split this into two and make some graphs
##should have about 4461 genes left

Bsub <- BNsub[, BNsub$mol=="BCR/ABL"]
Nsub <- BNsub[, BNsub$mol=="NEG"]

gN <- geneNames(Bsub)

##now we need to see if we can compute the correlations -
##might be too big

cB <- cor(t(exprs(Bsub)))
cB[-.6 < cB & cB < 0.6] <- 0

##drop the selfloops
diag(cB) <- 0

##restart
if( !exists("cB") ) {
    setwd(homedir)
    load("cB.rda")
}

##this one is about 5% of the size...
library(SparseM)
v1<-as.matrix.csr(cB, nr=dim(cB)[1], nc=dim(cB)[2])

##check how many non-zero entries
length(v1@ra)

save(v1, file="v1.rda")

rm(cB)

library(graph)

##take a sparse matrix - csr and put it into a graph
sparseM2Graph <- function(sM, nodeNames) {
    nN <- dim(sM)[1]
    dd <- diff(sM@ia)
    e1 <- rep(1:nN, dd)
    eL <- split(sM@ja, e1)
    eW <- split(sM@ra, e1)

    edL <- vector("list", length=nN)
    names(edL) <- 1:nN
    for(i in as.character(1:nN) ){
        edL[[i]] <- list(edges=eL[[i]], weights=eW[[i]])
    }
    names(edL) <- nodeNames
    new("graphNEL", nodes=nodeNames, edgeL=edL)
}

##translate a graph to a SparseMatrix:
##ra - the values; these will be 1's for now
##ja - the column indices
##ia the row offsets (
graph2SparseM <- function(g, useweights=FALSE) {
    nr = nc = numNodes(g)
    e1 = g@edgeL
    e2 = lapply(e1, function(x) x$edges)
    eL = listLen(e2)
    if( useweights )
        ra = unlist(lapply(e1, function(x) x$weights))
    else
        ra = rep(1, sum(eL))
    ja = as.integer(unlist(e2))
    ia = as.integer(cumsum(c(1, eL)))
    new("matrix.csr", ra=ra, ja=ja, ia=ia, dimension=c(nr, nc))
}

#if( exists("debug") && debug == TRUE) {
#    debug(graph2SparseM)

#    ss1 <- graph2SparseM(gR) 
# gX = sparseM2Graph(ss1)
    
#}
##seems we need to set the names to be affy probes or we will have
##trouble later on

Bsub.g1 <- sparseM2Graph(v1, gN)

save(Bsub.g1, file="Bsub.g1.rda")

if( !exists("Bsub.g1") ) {
    setwd(homedir)
    library(graph)
    load("Bsub.g1.rda")
    gN <- nodes(Bsub.g1)
    library(hgu95av2)
}

##some tests
EM1 <- edgeMatrix(Bsub.g1)
EW1 <- eWV(Bsub.g1, EM1)

##lets see if/what is connected to what
##and it seems to take forever...
library(RBGL)


  cc1 <- connectedComp(Bsub.g1)

# vcshort.R -- mods to RG's ShortestPath.R that assume
# that you have ALL.rda in the working dir

##now lets find some transcription factors:
#
#
#

library(GO)

##we select:
##GO:0003700  ##MF: "transcription factor activity"

whTFs <- get("GO:0003700", hgu95av2GO2ALLPROBES)

have <- match(gN, whTFs,0)

ourTFs <- gN[have]

##this seems to be way too slow,
##is it vectorized?
sG1 <- subGraph(nodes(Bsub.g1)[1:200], Bsub.g1)
cc1 <- connectedComp(sG1)
sG2 <- subGraph(cc1[[2]], sG1)

#vvX <- dijkstra.sp(sG2, ourTFs[225])

# now it seems that negative distances are a problem for boost
# dijkstra implementations.  to get positive distances, use 1-r

tmp <- lapply( Bsub.g1@edgeL, function(x) 1-x$weights )
for (i in 1:length(Bsub.g1@edgeL)) Bsub.g1@edgeL[[i]]$weights <- tmp[[i]]


# sadly, dijkstra.sp wants a numerical second arg!
# since the first node has no neighbors, use second...
vv <- dijkstra.sp(Bsub.g1, 2)
print(summary(vv$distances))
#
print(sp.between(Bsub.g1, ourTFs[1], ourTFs[2]))
