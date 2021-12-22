### R code from vignette source 'ChromBand.Rnw'

###################################################
### code chunk number 1: setup1
###################################################
library("Category")
library("ALL")
library("hgu95av2.db")
library("annotate")
library("genefilter")
##library("SNPchip")
library("karyoploteR")
library("geneplotter")
library("limma")
library("lattice")
library("graph")


###################################################
### code chunk number 2: chr12ideogram
###################################################
build <- "hg19" ## or hg18
kp <- plotKaryotype(genome=build, chromosomes="chr12", main="Human chromosome 12")
## cyt2 <- getCytoband(build=build)
## cyt2$gieStain <- "foo"
## c12p12_idx <- intersect(grep("^q21", cyt2$name),
##                         which(cyt2$chrom == "12"))
## cyt2[c12p12_idx, "gieStain"] <- rep(c("gpos50", "gpos75"),
##                                     length=length(c12p12_idx))
##
##
## plotCytoband2(chromosome="12", build=build, cytoband=cyt2, outer=FALSE,  ## From SNPchip
##     cex.axis=0.6, main="Human chromosome 12")
## plotCytoband2(chromosome="12", build=build, cytoband=cyt2, outer=FALSE,
##               cex.axis=0.6,
##               main="Human chromosome 12")


###################################################
### code chunk number 3: bcrAblOrNegSubset
###################################################
data(ALL, package="ALL")

subsetType <- "BCR/ABL"
Bcell <- grep("^B", as.character(ALL$BT))
bcrAblOrNegIdx <- which(as.character(ALL$mol.biol) %in% c("NEG", subsetType))

bcrAblOrNeg <- ALL[, intersect(Bcell, bcrAblOrNegIdx)]
bcrAblOrNeg$mol.biol <- factor(bcrAblOrNeg$mol.biol)


###################################################
### code chunk number 4: annMaps
###################################################
annType <- c("db", "env")
entrezMap <- getAnnMap("ENTREZID", annotation(bcrAblOrNeg),
                       type=annType, load=TRUE)
symbolMap <- getAnnMap("SYMBOL", annotation(bcrAblOrNeg),
                       type=annType, load=TRUE)
bandMap <- getAnnMap("MAP", annotation(bcrAblOrNeg),
                     type=annType, load=TRUE)


###################################################
### code chunk number 5: nsFiltering
###################################################
filterAns <- nsFilter(bcrAblOrNeg,
                      require.entrez = TRUE, 
                      remove.dupEntrez = TRUE, 
                      var.func = IQR, var.cutoff = 0.5)
nsFiltered <- filterAns$eset


###################################################
### code chunk number 6: ChromBand.Rnw:208-213
###################################################
hasSYM <- sapply(mget(featureNames(nsFiltered), symbolMap, ifnotfound=NA),
                 function(x) length(x) > 0 && !is.na(x[1]))
hasMAP <- sapply(mget(featureNames(nsFiltered), bandMap, ifnotfound=NA),
                 function(x) length(x) > 0 && !is.na(x[1]))
nsFiltered <- nsFiltered[hasSYM & hasMAP, ]


###################################################
### code chunk number 7: defineGeneUniverse
###################################################
affyUniverse <- featureNames(nsFiltered)
entrezUniverse <- unlist(mget(affyUniverse, entrezMap))
names(affyUniverse) <- entrezUniverse
if (any(duplicated(entrezUniverse)))
    stop("error in gene universe: can't have duplicate Entrez Gene Ids")


###################################################
### code chunk number 8: parametric1
###################################################
design <- model.matrix(~ 0 + nsFiltered$mol.biol)
colnames(design) <- c("BCR/ABL", "NEG")
contr <- c(1, -1) ## NOTE: we thus have BCR/ABL w.r.t NEG
fm1 <- lmFit(nsFiltered, design)
fm2 <- contrasts.fit(fm1, contr)
fm3 <- eBayes(fm2)
ttestLimma <- topTable(fm3, number = nrow(fm3), adjust.method = "none")
ttestLimma <- ttestLimma[featureNames(nsFiltered), ]

tstats <- ttestLimma$t
names(tstats) <- entrezUniverse[rownames(ttestLimma)]
##


###################################################
### code chunk number 9: selectedSubset
###################################################
ttestCutoff <- 0.01
smPV  <- ttestLimma$P.Value < ttestCutoff
pvalFiltered <- nsFiltered[smPV, ]
selectedEntrezIds <- unlist(mget(featureNames(pvalFiltered), entrezMap))
##


###################################################
### code chunk number 10: ChromBand.Rnw:418-434
###################################################

chrSortOrder <- function(df) {
    chrs <- sub("([^pq]+).*$", "\\1", rownames(df))
    xyIdx <- chrs %in% c("X", "Y")
    xydf <- NULL
    if (any(xyIdx)) {
        chrs <- chrs[!xyIdx]
        xydf <- df[xyIdx, ]
        df <- df[!xyIdx, ]
    }
    ord <- order(as.integer(chrs), rownames(df))
    df <- df[ord, ]
    if (!is.null(xydf))
      df <- rbind(df, xydf)
    df
}


###################################################
### code chunk number 11: ChromBand.Rnw:439-454
###################################################

gseaTstatStripplot <- function(bands, g, ..., include.all = FALSE)
{
    chroms <- c(1:22, "X", "Y")
    chromArms <- c(paste(chroms, "p", sep=""), paste(chroms, "q", sep=""))
    egid <- lapply(nodeData(g, bands), "[[", "geneIds")
    if (include.all) {
        egid$All <- 
            unique(unlist(lapply(nodeData(g)[chromArms], "[[", "geneIds")))
    }
    tdf <- do.call(make.groups, lapply(egid, function(x) tstats[x]))
    stripplot(which ~ data, tdf, jitter = TRUE, ...)
}




###################################################
### code chunk number 12: ChromBand.Rnw:459-506
###################################################

esetBWPlot <- function(tmpSet, ..., layout=c(1, nrow(emat)))
{
    emat <- exprs(tmpSet)
    pd <- pData(tmpSet)
    probes <- rownames(emat)
    syms <- 
        sapply(mget(probes, hgu95av2SYMBOL, ifnotfound=NA),
               function(x) if (all(is.na(x))) "NA" else as.character(x)[1])
    selectedAffy <- 
        probes %in% affyUniverse[selectedEntrezIds]
    symsSelected <- syms[selectedAffy]
    symsWithStatus <- 
        paste(syms, 
              ifelse(selectedAffy, "*", ""), 
              sep = "")
    pdat <- 
        cbind(exprs=as.vector(emat),
              genes=factor(probes, levels = probes, labels = syms),
              pd[rep(seq_len(nrow(pd)), each=nrow(emat)), ])
    pdat <- transform(pdat, genes = reorder(genes, exprs))
    panels.to.shade <- levels(pdat$genes) %in% symsSelected
    bwplot(mol.biol ~ exprs | genes, data=pdat, 
           layout = layout,
           auto.key=TRUE,
           scales=list(x=list(log=2L)),
           xlab="Log2 Expression",
           panels.to.shade = panels.to.shade,
           panel = function(..., panels.to.shade) {
               if (panels.to.shade[packet.number()]) 
                   panel.fill(col = "lightgrey")
               panel.bwplot(...)
           },
           strip=FALSE,
           strip.left=TRUE, ...)
}

g1 <- makeChrBandGraph(annotation(nsFiltered), univ=entrezUniverse)
ct <- ChrBandTreeFromGraph(g1)

subsetByBand <- function(eset, ct, band) {
    egIDs <- unlist(nodeData(ct@toChildGraph, n=band, 
                             attr="geneIds"), use.names=FALSE)
    wantedProbes <- affyUniverse[as.character(egIDs)]
    eset[intersect(wantedProbes, featureNames(eset)), ]
}



###################################################
### code chunk number 13: basicParams
###################################################
params <- new("ChrMapHyperGParams",
              conditional=FALSE,
              testDirection="over",
              universeGeneIds=entrezUniverse,
              geneIds=selectedEntrezIds,
              annotation="hgu95av2",
              pvalueCutoff=0.05)

paramsCond <- params
paramsCond@conditional <- TRUE


###################################################
### code chunk number 14: basicTest
###################################################
hgans <- hyperGTest(params)
hgansCond <- hyperGTest(paramsCond)


###################################################
### code chunk number 15: result1
###################################################
sumUn <- summary(hgans, categorySize=1)
chrSortOrder(sumUn)

sumCond <- summary(hgansCond, categorySize=1)
chrSortOrder(sumCond)


###################################################
### code chunk number 16: ChromBand.Rnw:597-612
###################################################
gseaTstatStripplot(c("12q21.1", "12q21", "12q2", "12q"),
                   include.all = TRUE, 
                   g = g1,
                   xlab = "Per-gene t-statistics", 
                   panel = function(...) {
                       require(grid, quietly = TRUE)
                       grid.rect(y = unit(2, "native"), 
                                 height = unit(1, "native"),
                                 gp = 
                                 gpar(fill = "lightgrey", 
                                      col = "transparent"))
                       panel.grid(v = -1, h = 0)
                       panel.stripplot(...)
                       panel.average(..., fun = mean, lwd = 3)
                   })


###################################################
### code chunk number 17: gseaStripplot
###################################################
plot(trellis.last.object())


###################################################
### code chunk number 18: LMtestSetup
###################################################
params <- new("ChrMapLinearMParams",
              conditional = FALSE,
              testDirection = "up",
              universeGeneIds = entrezUniverse,
              geneStats = tstats,
              annotation = "hgu95av2",
              pvalueCutoff = 0.01, 
              minSize = 4L)
params@graph <- makeChrBandGraph(params@annotation, params@universeGeneIds)
params@gsc <- makeChrBandGSC(params@graph)
paramsCond <- params
paramsCond@conditional <- TRUE


###################################################
### code chunk number 19: LMtest
###################################################

lmans <- linearMTest(params)
lmansCond <- linearMTest(paramsCond)

chrSortOrder(summary(lmans))
chrSortOrder(summary(lmansCond))

##


###################################################
### code chunk number 20: ChromBand.Rnw:716-719
###################################################
tmpSet <- subsetByBand(nsFiltered, ct, "1p36.2")
esetBWPlot(tmpSet, ylab="1p36.2", layout = c(2, 8),
           par.strip.text = list(cex = 0.8))


###################################################
### code chunk number 21: dotplot1p362
###################################################
plot(trellis.last.object())


