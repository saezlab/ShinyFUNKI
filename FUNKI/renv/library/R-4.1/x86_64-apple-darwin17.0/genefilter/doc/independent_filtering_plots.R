## ----knitr, echo=FALSE, results="hide"----------------------------------------
library("knitr")
opts_chunk$set(tidy=FALSE,dev="png",fig.show="hide",
               fig.width=4,fig.height=4.5,dpi=240,
               message=FALSE,error=FALSE,warning=FALSE)

## ----style, eval=TRUE, echo=FALSE, results="asis"--------------------------
BiocStyle:::latex()

## ----setup, echo=FALSE--------------------------------------------------------
options( width = 80 )

## ----libraries----------------------------------------------------------------
library("genefilter")
library("ALL")
data("ALL")

## ----sample_data, cache=TRUE--------------------------------------------------
bcell <- grep("^B", as.character(ALL$BT))
moltyp <- which(as.character(ALL$mol.biol) %in% 
                c("NEG", "BCR/ABL"))
ALL_bcrneg <- ALL[, intersect(bcell, moltyp)]
ALL_bcrneg$mol.biol <- factor(ALL_bcrneg$mol.biol)
n1 <- n2 <- 3
set.seed(1969)
use <- unlist(tapply(1:ncol(ALL_bcrneg), 
                     ALL_bcrneg$mol.biol, sample, n1))
subsample <- ALL_bcrneg[,use]

## ----stats, cache=TRUE--------------------------------------------------------
S <- rowSds( exprs( subsample ) )
temp <- rowttests( subsample, subsample$mol.biol )
d <- temp$dm
p <- temp$p.value
t <- temp$statistic

## ----filter_volcano, include=FALSE--------------------------------------------
S_cutoff <- quantile(S, .50)
filter_volcano(d, p, S, n1, n2, alpha=.01, S_cutoff)

## ----kappa, include=FALSE-----------------------------------------------------
t <- seq(0, 5, length=100)
plot(t, kappa_t(t, n1, n2) * S_cutoff, 
     xlab="|T|", ylab="Fold change bound", type="l")

## ----table--------------------------------------------------------------------
table(ALL_bcrneg$mol.biol)

## ----filtered_p---------------------------------------------------------------
S2 <- rowVars(exprs(ALL_bcrneg))
p2 <- rowttests(ALL_bcrneg, "mol.biol")$p.value
theta <- seq(0, .5, .1)
p_bh <- filtered_p(S2, p2, theta, method="BH")

## ----p_bh---------------------------------------------------------------------
head(p_bh)

## ----rejection_plot-----------------------------------------------------------
rejection_plot(p_bh, at="sample",
               xlim=c(0,.3), ylim=c(0,1000),
               main="Benjamini & Hochberg adjustment")

## ----filtered_R---------------------------------------------------------------
theta <- seq(0, .80, .01)
R_BH <- filtered_R(alpha=.10, S2, p2, theta, method="BH")

## ----R_BH---------------------------------------------------------------------
head(R_BH)

## ----filtered_R_plot----------------------------------------------------------
plot(theta, R_BH, type="l",
     xlab=expression(theta), ylab="Rejections",
     main="BH cutoff = 0.1")

## ----sessionInfo, results='asis', echo=FALSE----------------------------------
sessionInfo() |> toLatex()

