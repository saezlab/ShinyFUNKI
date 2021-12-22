demCC <- function(nnodes=500, inM=1:2, p=.3)
 {
 library(graph)
 library(RBGL)
 library(Biobase)
gt <- system.time(x <- randomGraph( as.character(1:nnodes), inM, p ))
ct <- system.time(cx <- connectedComp(x))
nn <- length(nodes(x))
ne <- sum(listLen(edges(x))/2)
c("nnodes"=nn, "nedges"=ne, ncc=length(cx), graphTime=gt[3], ccTime=ct[3])
}
set.seed(1234)
print(demCC())
print(demCC(1000))
print(demCC(2000))


