## ----style-knitr, eval=TRUE, echo=FALSE, results="asis"--------------------
BiocStyle::latex()

## ----loadPkg---------------------------------------------------------------
suppressPackageStartupMessages({
    library(UniProt.ws)
})
up <- UniProt.ws(taxId=9606)

## ----help,eval=FALSE-------------------------------------------------------
#  help("UniProt.ws")

## ----show------------------------------------------------------------------
up

## ----availSpecies----------------------------------------------------------
availableUniprotSpecies(pattern="musculus")

## ----setTaxID--------------------------------------------------------------
mouseUp <- UniProt.ws(10090)
mouseUp

## ----columns---------------------------------------------------------------
head(keytypes(up))

## ----keytypes--------------------------------------------------------------
head(columns(up))

## ----keys,eval=FALSE-------------------------------------------------------
#  egs = keys(up, "ENTREZ_GENE")

## ----select----------------------------------------------------------------
keys <- c("1","2")
columns <- c("PDB","HGNC","SEQUENCE")
kt <- "ENTREZ_GENE"
res <- select(up, keys, columns, kt)
res

## ----<sessionInfo----------------------------------------------------------
sessionInfo()

