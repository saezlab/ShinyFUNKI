### R code from vignette source 'howtogenefinder.Rnw'

###################################################
### code chunk number 1: howtogenefinder.Rnw:45-52
###################################################
 library("Biobase")
 library("genefilter")
 data(sample.ExpressionSet)
 igenes<- c(300,333,355,419) ##the interesting genes
 closeg <- genefinder(sample.ExpressionSet, igenes, 10, 
       method="euc", scale="none")
 names(closeg)


###################################################
### code chunk number 2: howtogenefinder.Rnw:61-64
###################################################
closeg$"31539_r_at"
Nms1 <- featureNames(sample.ExpressionSet)[closeg$"31539_r_at"$indices]
Nms1


###################################################
### code chunk number 3: howtogenefinder.Rnw:106-107
###################################################
toLatex(sessionInfo())


