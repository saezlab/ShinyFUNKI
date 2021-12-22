library(CompStatViz)

cache <- function(name, expr) {
  cachefile <- paste("tmp-", name, ".RData", sep="")
  if(file.exists(cachefile)) {
    load(cachefile)
  } else {
    assign(name, expr)
    save(list=name, file=cachefile)
  }
  get(name)
}


CLL.exprs <- cache("CLL.exprs", gcrma(CLLbatch))
sampleNames(CLL.exprs) <- gsub("\\.CEL", "", sampleNames(CLL.exprs))
data(disease)
pD <- new("AnnotatedDataFrame")
pData(pD) <- disease
varLabels(pD) <- list(SampleID = "Sample ID", Disease = "Stable/Progressive")
phenoData(CLL.exprs) <- pD
f1 <- pOverA(.25, log2(100))
f2 <- function(x)(IQR(x)>0.5)
ff <- filterfun(f1,f2)
selected <- genefilter(CLL.exprs, ff)
sum(selected)
goodCell <- !is.na(CLL.exprs$Disease)

CLL.sub <- CLL.exprs[selected,goodCell]


rwtt <- rowttests(CLL.sub, CLL.sub$Disease)

library("Category")

fL <- findLargest(geneNames(CLL.sub), abs(rwtt$statistic), "hgu95av2")

fL2 <- probes2MAP(fL, "hgu95av2")
length(fL2)
inBoth <- fL %in% names(fL2)
fL <- fL[inBoth]


eS <- CLL.sub[match(fL, geneNames(CLL.sub)),]

tobs = rowttests(eS, "Disease")


z1 <- MAPAmat("hgu95av2")

z2 <- t(z1)

whichCol <- match(names(fL), dimnames(z2)[[2]], 0)

z2ER <- z2[, whichCol]

##now we have found which elements map to the genes we have
rs <- rowSums(z2ER)
##drop categories with less than 3
z2ER2 <- z2ER[rs>3,]
rs2 <- rs[rs>3]
nCats <- length(rs2)


m2 <- match(dimnames(z2ER2)[[2]], names(fL))

tA <- z2ER2 %*% (tobs$statistic[m2])
dim(tA) <- NULL
tA <- tA/sqrt(rs2)
names(tA) <- row.names(z2ER2)

qqnorm(tA)
