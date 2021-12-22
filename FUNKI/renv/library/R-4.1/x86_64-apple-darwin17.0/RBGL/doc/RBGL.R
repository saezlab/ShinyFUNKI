### R code from vignette source 'RBGL.Rnw'

###################################################
### code chunk number 1: RBGL.Rnw:44-47
###################################################
library(RBGL)
library(Rgraphviz)
library(XML)


###################################################
### code chunk number 2: bfDemo
###################################################
con <- file(system.file("XML/bfsex.gxl", package="RBGL"))
bf <- fromGXL(con)
close(con)


###################################################
### code chunk number 3: figbf
###################################################
  plot(bf, main="a) Breath-First Search Example")


###################################################
### code chunk number 4: dfDemo
###################################################
con <- file(system.file("XML/dfsex.gxl", package="RBGL"))
df <- fromGXL(con)
close(con)


###################################################
### code chunk number 5: figdf
###################################################
  plot(df, main="b) Depth-First Search Example")


###################################################
### code chunk number 6: dijkstraDemo
###################################################
con <- file(system.file("XML/dijkex.gxl", package="RBGL"))
dijk <- fromGXL(con)
close(con)


###################################################
### code chunk number 7: figdijk
###################################################
 plot(dijk, main="c) Dijkstra's Example")


###################################################
### code chunk number 8: connDemo
###################################################
con <- file(system.file("XML/conn.gxl", package="RBGL"))
coex <- fromGXL(con)
close(con)


###################################################
### code chunk number 9: figcoex
###################################################
 plot(coex, main="d) Coex Example")


###################################################
### code chunk number 10: conn2Demo
###################################################
con <- file(system.file("XML/conn2.gxl", package="RBGL"))
coex2 <- fromGXL(con)
close(con)


###################################################
### code chunk number 11: figcoex2
###################################################
  plot(coex2, main="e) Coex2 Example")


###################################################
### code chunk number 12: conn2iDemo
###################################################
con <- file(system.file("XML/conn2iso.gxl", package="RBGL"))
coex2i <- fromGXL(con)
close(con)


###################################################
### code chunk number 13: figcoex2i
###################################################
  plot(coex2i, main="f) Coex2 Isomorphism Example")


###################################################
### code chunk number 14: kmstDemo
###################################################
con <- file(system.file("XML/kmstEx.gxl", package="RBGL"))
km <- fromGXL(con)
close(con)


###################################################
### code chunk number 15: figkmst
###################################################
  plot(km, main="g) Kruskal MST Example")


###################################################
### code chunk number 16: bicoDemo
###################################################
con <- file(system.file("XML/biconn.gxl", package="RBGL"))
bicoex <- fromGXL(con)
close(con)


###################################################
### code chunk number 17: figbico
###################################################
  plot(bicoex, main="h) Biconnected Component Example")


###################################################
### code chunk number 18: ospfDemo
###################################################
con <- file(system.file("XML/ospf.gxl", package="RBGL"))
ospf <- fromGXL(con)
close(con)


###################################################
### code chunk number 19: figospf
###################################################
  plot(ospf, main="i) Ospf Example")


###################################################
### code chunk number 20: zzDemo
###################################################
con <- file(system.file("dot/joh.gxl", package="RBGL"))
joh <- fromGXL(con)
close(con)


###################################################
### code chunk number 21: figjoh
###################################################
  plot(joh, main="j) joh Example")


###################################################
### code chunk number 22: hcsDemo
###################################################
con <- file(system.file("XML/hcs.gxl", package="RBGL"))
hcs <- fromGXL(con)
close(con)


###################################################
### code chunk number 23: fighcs
###################################################
  plot(hcs, main="k) HCS Example")


###################################################
### code chunk number 24: kclexDemo
###################################################
con <- file(system.file("XML/snacliqueex.gxl", package="RBGL"))
kclex <- fromGXL(con)
close(con)


###################################################
### code chunk number 25: figkclex
###################################################
  plot(kclex, main="l) kCliques Example")


###################################################
### code chunk number 26: kcoexDemo
###################################################
con <- file(system.file("XML/snacoreex.gxl", package="RBGL"))
kcoex <- fromGXL(con)
close(con)


###################################################
### code chunk number 27: figkcoex
###################################################
  plot(kcoex, main="m) kCores Example")


###################################################
### code chunk number 28: showFileDep
###################################################
data(FileDep)
FileDep


###################################################
### code chunk number 29: figfd
###################################################
z <- plot(FileDep)


###################################################
### code chunk number 30: DFSdemo
###################################################
print(dfs.res <- dfs(df, "y"))


###################################################
### code chunk number 31: figdfs1
###################################################
plot(df, main="a) DFS Example")


###################################################
### code chunk number 32: figdfs2
###################################################
dfsNattrs <- makeNodeAttrs(df)
dfsNattrs$label[dfs.res$discovered] <- 1:numNodes(df)
plot(df, nodeAttrs=dfsNattrs, main="b) DFS Example with search order")


###################################################
### code chunk number 33: BFSdemo
###################################################
print(bfs.res <- bfs(bf,"s"))


###################################################
### code chunk number 34: figbfs1
###################################################
plot(bf, main="a) BFS Example")


###################################################
### code chunk number 35: figbfs2
###################################################
bfsNattrs <- makeNodeAttrs(bf)
bfsNattrs$label[bfs.res] <- 1:numNodes(bf)
plot(bf, nodeAttrs=bfsNattrs, main="b) BFS Example with search order")


###################################################
### code chunk number 36: dijkdemo1
###################################################
nodes(dijk)
edgeWeights(dijk)
dijkstra.sp(dijk)


###################################################
### code chunk number 37: dijkdemo2
###################################################
nodes(ospf)[6]
dijkstra.sp(ospf,nodes(ospf)[6])
sp.between(ospf, "RT6", "RT1")


###################################################
### code chunk number 38: figospf
###################################################
z <- plot(ospf)


###################################################
### code chunk number 39: bellmanfordDemo
###################################################
dd <- coex2
nodes(dd)
bellman.ford.sp(dd)
bellman.ford.sp(dd,nodes(dd)[2])


###################################################
### code chunk number 40: DAGDemo
###################################################
dd <- coex2
dag.sp(dd)
dag.sp(dd,nodes(dd)[2])


###################################################
### code chunk number 41: johnsonDemo
###################################################
zz <- joh
edgeWeights(zz)
johnson.all.pairs.sp(zz)


###################################################
### code chunk number 42: figjoh
###################################################
z <- plot(zz)


###################################################
### code chunk number 43: floydwarshallDemo
###################################################
floyd.warshall.all.pairs.sp(coex)


###################################################
### code chunk number 44: KMSTdemo
###################################################
mstree.kruskal(km)


###################################################
### code chunk number 45: primDemo
###################################################
mstree.prim(coex2)


###################################################
### code chunk number 46: conndemo
###################################################
km1 <- km
km1 <- graph::addNode(c("F","G","H"), km1)
km1 <- addEdge("G", "H", km1, 1)
km1 <- addEdge("H", "G", km1, 1)
connectedComp(ugraph(km1))


###################################################
### code chunk number 47: figkm1
###################################################
plot(km1, main="Modified Kruskal MST example")


###################################################
### code chunk number 48: sconndemo
###################################################
km2 <- km
km2 <- graph::addNode(c("F","G","H"), km2)
km2 <- addEdge("G", "H", km2, 1)
km2 <- addEdge("H", "G", km2, 1)
strongComp(km2)


###################################################
### code chunk number 49: biConnCompdemo
###################################################
biConnComp(bicoex)
articulationPoints(bicoex)


###################################################
### code chunk number 50: figbicoex
###################################################
z <- plot(bicoex)


###################################################
### code chunk number 51: incrCompdemo
###################################################
jcoex <- join(coex, hcs)
x <- init.incremental.components(jcoex)
incremental.components(jcoex)
same.component(jcoex, "A", "F")
same.component(jcoex, "A", "A1")
jcoex <- addEdge("A", "A1", jcoex)
x <- init.incremental.components(jcoex)
incremental.components(jcoex)
same.component(jcoex, "A", "A1")


###################################################
### code chunk number 52: figjcoex
###################################################
z <- plot(jcoex)


###################################################
### code chunk number 53: MaxFlowdemo
###################################################
edgeWeights(dijk)
edmonds.karp.max.flow(dijk, "B", "D")
push.relabel.max.flow(dijk, "C", "B")


###################################################
### code chunk number 54: SparseMatrixOrderingdemo
###################################################
dijk1 <- ugraph(dijk)
cuthill.mckee.ordering(dijk1)
minDegreeOrdering(dijk1)
sloan.ordering(dijk1)


###################################################
### code chunk number 55: edgeConndemo
###################################################
edgeConnectivity(coex)


###################################################
### code chunk number 56: tsortDemo1
###################################################
tsort(FileDep)


###################################################
### code chunk number 57: tsortDemo2
###################################################
FD2 <- FileDep
# now introduce a cycle
FD2 <- addEdge(from="bar_o", to="dax_h", FD2)
tsort(FD2)


###################################################
### code chunk number 58: Isomorphismdemo
###################################################
isomorphism(dijk, coex2)
isomorphism(coex2i, coex2)


###################################################
### code chunk number 59: figcoex2i
###################################################
z <- plot(coex2i)


###################################################
### code chunk number 60: VertexColoringdemo
###################################################
sequential.vertex.coloring(coex)


###################################################
### code chunk number 61: wavefrontdemo
###################################################
ss <- 1
ith.wavefront(dijk, ss)
maxWavefront(dijk)
aver.wavefront(dijk)
rms.wavefront(dijk)


###################################################
### code chunk number 62: Centralitydemo
###################################################
brandes.betweenness.centrality(coex)
betweenness.centrality.clustering(coex, 0.1, TRUE)


###################################################
### code chunk number 63: mincutdemo
###################################################
minCut(coex)


###################################################
### code chunk number 64: highlyConnSGdemo
###################################################
highlyConnSG(coex)
highlyConnSG(hcs)


###################################################
### code chunk number 65: MaxCliquedemo
###################################################
maxClique(coex)
maxClique(hcs)


###################################################
### code chunk number 66: IsTriangulateddemo
###################################################
is.triangulated(coex)
is.triangulated(hcs)


###################################################
### code chunk number 67: Separatesdemo
###################################################
separates("B", "A", "E", km)
separates("B", "A", "C", km)


###################################################
### code chunk number 68: kCoresdemo1
###################################################
kCores(kcoex)
kcoex2 <- coex2
kCores(kcoex2)
kCores(kcoex2, "in")
kCores(kcoex2, "out")
g1 <- addEdge("C", "B", kcoex2)
kCores(g1, "in")
g2 <- addEdge("C", "A", kcoex2)
kCores(g2, "out")


###################################################
### code chunk number 69: figkcores
###################################################
z <- plot(kcoex)


###################################################
### code chunk number 70: kCliquesdemo
###################################################
kCliques(kclex)


###################################################
### code chunk number 71: figkcliques
###################################################
z <- plot(kclex)


