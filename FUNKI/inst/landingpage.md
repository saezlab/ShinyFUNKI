## Welcome to the FUNKI application
FUNKI is a multi-omic functional integration and analysis platform. It provides a standardize pipeline to process and perform functional analysis on transcriptomic, proteomic, phosphoproteomic and metabolomic datasets. The analysis can be performed both on a single type of omic data and on multi-omic dataset by integrating them in supervised and unsupervised manners.

### Implemented Approaches
FUNKI has currently implemented the following approaches

#### DoRothEA
<a href="https://saezlab.github.io/DoRothEA/" target="_blank">DoRothEA</a> is a gene regulatory network linking Transcription Factors (TFs) with their downstream targets. The unity of TF and its targets is referred as regulon. Such gene regulatory networks allow the computation of TF-activities from gene expression data by enrichment analysis using the regulons as underlying gene-sets. The network has been build integrating TF-target interactions from four different strategies: (i) manually curated interaction repositories, (ii) interactions derived from ChIP-seq binding data, (iii) in silico predictions of TF binding on gene promoters, and (iv) reverse-engineered regulons from large gene expression datasets. Based on the number of supporting evidences each interaction is accompanied with a confidence level ranging from high-confident (A) to low-confident (E).

#### PROGENy
<a href="https://saezlab.github.io/progeny/" target="_blank">PROGENy</a> is a linear model to infer pathway activity from gene expression data. Pathway activities are calculated based on consensus gene signatures obtained from perturbation experiments, that is, the footprint of pathway on gene expression.

#### CARNIVAL
<a href="https://saezlab.github.io/CARNIVAL/" target="_blank">CARNIVAL</a> (CAusal Reasoning for Network identification using Integer VALue programming) is a method for the identification of upstream reguatory signalling pathways from downstream gene expression data. The aim of the CARNIVAL pipeline is to identify a subset of interactions from a prior knowledge network that represent potential regulated pathways linking known or potential targets of perturbation towards active transcription factors derived from expression data.

#### KinAct
<a href="http://saezlab.github.io/kinact/" target="_blank">KinAct</a> is a network linking kinases to phosphorylation sites. In a simmilar way as DoRothEA works, this resource allows the estimation of kinase activities from phosphoproteomics data using prior knowledge about kinase-substrate interactions.
