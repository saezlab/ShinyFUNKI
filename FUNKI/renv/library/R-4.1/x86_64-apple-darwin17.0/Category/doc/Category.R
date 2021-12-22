### R code from vignette source 'Category.Rnw'

###################################################
### code chunk number 1: Setup
###################################################
library("Biobase")
library("annotate")
library("hgu95av2.db")
library("KEGGREST")
library("genefilter")
library("Category")
library("ALL")


###################################################
### code chunk number 2: Example
###################################################
## subset of interest: 37+42 samples
data(ALL)
esetA <- ALL[, intersect(grep("^B", as.character(ALL$BT)),
          which(as.character(ALL$mol) %in% c("NEG", "BCR/ABL")))]

esetA@annotation = "hgu95av2"

esetA$mol.biol = factor(esetA$mol.biol)


esetASub = nsFilter(esetA, var.cutoff=0.5)$eset

##set up some colors

BCRcols = ifelse(esetASub$mol == "BCR/ABL","goldenrod", "skyblue")
library("RColorBrewer")
cols = brewer.pal(10, "RdBu")



###################################################
### code chunk number 3: parametric1
###################################################

ttests = rowttests(esetASub, "mol.biol")

##find the probes that we are going to use
fL = findLargest(featureNames(esetASub), abs(ttests$statistic), "hgu95av2")
fL2 = probes2Path(fL, "hgu95av2")
length(fL2)
inBoth = fL %in% names(fL2)
fL = fL[inBoth]



###################################################
### code chunk number 4: reorder
###################################################
eS = esetASub[match(names(fL2), featureNames(esetASub)),]

tobs = rowttests(eS, "mol.biol")


###################################################
### code chunk number 5: Amat
###################################################
Amat = t(PWAmat("hgu95av2"))
AmER = Amat[,names(fL)]


###################################################
### code chunk number 6: rs
###################################################
rs = rowSums(AmER)
AmER2 = AmER[rs>5,]
rs2   = rs[rs>5]
nCats = length(rs2)


###################################################
### code chunk number 7: computeCatStats
###################################################
##compute observed stats
 tA = AmER2 %*% tobs$statistic
 tA = tA/sqrt(rs2)
 names(tA) = row.names(AmER2)



###################################################
### code chunk number 8: qqplot
###################################################
 qqnorm(tA)


###################################################
### code chunk number 9: findCat
###################################################

 byTT = names(tA)[ tA< -7]



###################################################
### code chunk number 10: KEGGmnplot
###################################################
    KEGGmnplot(byTT, eS, group=eS$mol, data="hgu95av2",
               main=paste(getPathNames(byTT)[[1]], paste("Overall:",
               round(tA[byTT], 3)), sep="\n"))



###################################################
### code chunk number 11: setupHMcols
###################################################
hmcol <- rev(colorRampPalette(brewer.pal(10, "RdBu"))(256))
spcol <- ifelse(eS$mol.biol=="BCR/ABL", "goldenrod", "skyblue")


###################################################
### code chunk number 12: Ribosomeheatmap
###################################################
    tmp1 = KEGG2heatmap(byTT, eS, data="hgu95av2",
               main=paste(getPathNames(byTT)[[1]], paste("Overall:",
               round(tA[byTT], 3)), sep="\n"), col=hmcol,
                 ColSideColors=spcol)



###################################################
### code chunk number 13: HMbysex
###################################################

  spcol2 <- ifelse(eS$sex=="M", "lightgreen", "slategrey")
    tmp2 = KEGG2heatmap(byTT, eS, data="hgu95av2",
               main=paste(getPathNames(byTT)[[1]], paste("Overall:",
               round(tA[byTT], 3)), sep="\n"), col=hmcol,
                 ColSideColors=spcol2)



###################################################
### code chunk number 14: setupperms
###################################################
NPERM = 500
set.seed(444)


###################################################
### code chunk number 15: ttperms
###################################################
v1 = ttperm(exprs(eS), eS$mol.biol, B=NPERM)

permDm <- matrix(0.0, nrow=length(v1$perms[[1]]$statistic),
                 ncol=length(v1$perms))
for (j in 1:ncol(permDm)) {
    permDm[ , j] <- v1$perms[[j]]$statistic
}

permD = AmER2 %*% permDm
##no need to do this second step - if we don't do if for tobs
permD2 = sweep(permD, 1, sqrt(rs2), "/")


pvals = matrix(NA, nr=nCats, ncol=2)
dimnames(pvals) = list(row.names(AmER2), c("Lower", "Upper"))

for(i in 1:nCats) {
    pvals[i,1] = sum(permD2[i,] < tA[i])/NPERM
    pvals[i,2] = sum(permD2[i,] > tA[i])/NPERM
}

ord1 = order(pvals[,1])
lowC = (row.names(pvals)[ord1])[pvals[ord1,1]< 0.05]

highC = row.names(pvals)[pvals[,2] < 0.05]


 getPathNames(lowC)


###################################################
### code chunk number 16: setupHC
###################################################
lnhC = length(highC)


###################################################
### code chunk number 17: getHighNames
###################################################
 getPathNames(highC)[1:5]



###################################################
### code chunk number 18: mnplot1
###################################################
    KEGGmnplot(highC[lnhC], eS, group=eS$mol, data="hgu95av2",
               main=paste(getPathNames(highC[lnhC])[[1]], paste("Overall:",
               round(tA[highC[lnhC]], 3)), sep="\n"), pch=16, col="blue")




###################################################
### code chunk number 19: mnplot2
###################################################
    KEGGmnplot(highC[lnhC-1], eS, group=eS$mol, data="hgu95av2",
               main=paste(getPathNames(highC[lnhC-1])[[1]], paste("Overall:",
               round(tA[highC[lnhC-1]], 3)), sep="\n"))




###################################################
### code chunk number 20: mnplot3
###################################################
    KEGGmnplot(highC[lnhC-2], eS, group=eS$mol, data="hgu95av2",
               main=paste(getPathNames(highC[lnhC-2])[[1]], paste("Overall:",
               round(tA[highC[lnhC-2]], 3)), sep="\n"))




###################################################
### code chunk number 21: HMperm
###################################################
    tmp2 = KEGG2heatmap(highC[lnhC-2], eS, data="hgu95av2",
               main=paste(getPathNames(highC[lnhC-2])[[1]], paste("Overall:",
               round(tA[highC[lnhC-2]], 3)), sep="\n"), col=hmcol,
                 ColSideColors=spcol)



###################################################
### code chunk number 22: appByCat
###################################################

med = applyByCategory(tobs$statistic, AmER2, FUN=median)
wt  = applyByCategory(tobs$statistic, AmER2, 
  FUN = function(x) with(wilcox.test(x), c(statistic, p=p.value)))

head(t(wt[,order(wt[2,])]))
     


