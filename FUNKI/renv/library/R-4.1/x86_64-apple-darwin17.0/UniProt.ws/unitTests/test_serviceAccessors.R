##  require(RUnit)
##  require(UniProt.ws)

.check_rect_result <- function(res){
  all(checkTrue(dim(res)[1] >1),
      checkTrue(dim(res)[2] ==2))
}

## problem?
test_mapUniprot <- function(){
    mapUniprot <- UniProt.ws:::mapUniprot
    res <- mapUniprot(
        from='ACC',to='P_REFSEQ_AC',
        query=c('P13368','P20806','Q9UM73','P97793','Q17192')
    )
    .check_rect_result(res)
    checkTrue(res[1,1]=='P13368')
    checkTrue(res[1,2]=='NP_511114.2')

    ## what if I have entrezGene IDs and I want UniProts?
    res <- mapUniprot(
        from='P_ENTREZGENEID', to='ACC', query=c('1','2','3','9','10')
    )
    .check_rect_result(res)
    checkTrue(res[1,1]=='1')
    checkTrue(res[1,2]=='P04217')

    ## I can then map UniProt accessions to Unigene IDs
    res <- mapUniprot(
        from='ACC',to='GENENAME',
        query=c('P04217','P01023','F5H5R8','P18440','Q400J6')
    )
    .check_rect_result(res)

    ## I can catch cases where the server returns results with double tabs
    tt <- 'yourlist:M20181018A7434721E10EE6586998A056CCD0537EBF8181F\tisomap:M20181018A7434721E10EE6586998A056CCD0537EBF8181F\tEntry\tEntry name\tStatus\tProtein names\tGene names\tOrganism\tLength\n1\t\tP04217\tA1BG_HUMAN\treviewed\tAlpha-1B-glycoprotein (Alpha-1-B glycoprotein)\tA1BG\tHomo sapiens (Human)\t495\n1\t\tV9HWD8\tV9HWD8_HUMAN\tunreviewed\tEpididymis secretory sperm binding protein Li 163pA\tHEL-S-163pA\tHomo sapiens (Human)\t495\n2\t\tP01023\tA2MG_HUMAN\treviewed\tAlpha-2-macroglobulin (Alpha-2-M) (C3 and PZP-like alpha-2-macroglobulin domain-containing protein 5)\tA2M CPAMD5 FWP007\tHomo sapiens (Human)\t1474\n9\t\tF5H5R8\tF5H5R8_HUMAN\tunreviewed\tArylamine N-acetyltransferase 1\tNAT1\tHomo sapiens (Human)\t352\n9\t\tP18440\tARY1_HUMAN\treviewed\tArylamine N-acetyltransferase 1 (EC 2.3.1.5) (Arylamide acetylase 1) (Monomorphic arylamine N-acetyltransferase) (MNAT) (N-acetyltransferase type 1) (NAT-1)\tNAT1 AAC1\tHomo sapiens (Human)\t290\n9\t\tQ400J6\tQ400J6_HUMAN\tunreviewed\tArylamine N-acetyltransferase 1 (EC 2.3.1.5) (Arylamine N-acetyltrasnferase 1) (Fragment)\tNAT1\tHomo sapiens (Human)\t92\n10\t\tA4Z6T7\tA4Z6T7_HUMAN\tunreviewed\tArylamine N-acetyltransferase 2\tNAT2\tHomo sapiens (Human)\t290\n10\t\tP11245\tARY2_HUMAN\treviewed\tArylamine N-acetyltransferase 2 (EC 2.3.1.5) (Arylamide acetylase 2) (N-acetyltransferase type 2) (NAT-2) (Polymorphic arylamine N-acetyltransferase) (PNAT)\tNAT2AAC2\tHomo sapiens (Human)\t290\n'
    res <- UniProt.ws:::.cleanup(tt, from = 'P_ENREZGENEID', to = 'ACC')
    
    .check_rect_result(res)
    checkTrue(res[1,1]=='1')
    checkTrue(res[1,2]=='P04217')
}


test_getUniprotGoodies <- function(){
  query = c('P04217','P30443')
  cols = 'sequence'
  res <- UniProt.ws:::getUniprotGoodies(query, cols)
  checkTrue(is(res, "data.frame"))
  checkTrue(dim(res)[1] == 2)
  checkTrue(dim(res)[2] == 2)

  ## can also be used to extract interpro IDs
  query = c('P13368','P20806','Q9UM73','P97793','Q17192')
  cols = 'database(interpro)'
  res <- UniProt.ws:::getUniprotGoodies(query, cols)
  checkTrue(is(res, "data.frame"))
  checkTrue(dim(res)[1] == 5)
  checkTrue(dim(res)[2] == 2)

  ## OR extract a number of other things...
  cols = c('3d','go-id','taxon')
  res <- UniProt.ws:::getUniprotGoodies(query, cols)
  checkTrue(is(res, "data.frame"))
  checkTrue(dim(res)[1] == 5)
  checkTrue(dim(res)[2] == 4)
   
}


## Faster testing:
## BiocGenerics:::testPackage(pattern="^test_serviceAccessors.*\\.R$")

