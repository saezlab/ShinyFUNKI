## Welcome to the FUNKI application
FUNKI is a multi-omic functional integration and analysis platform. It provides a standardize pipeline to process and perform functional analysis on transcriptomic, proteomic, phosphoproteomic and metabolomic datasets. The analysis can be performed both on a single type of omic data and on multi-omic dataset by integrating them in supervised and unsupervised manners.

## Installation and use

FUNKI has beed developed to run locally.
Thus, there are different options to use this server to fit all possible users: 

1. Run FUNKI directly from GitHub

In an R session, run the line below to download and launch FUNKI:

```shiny::runGitHub(repo = "ShinyFUNKI", username = "saezlab", subdir = "FUNKI")``` 

Note: Make sure all required packages are installed beforfehand.
The ```renv.lock``` lockfile (```renv``` packaged) has recorded the state of this project’s private library.
It can be used to restore the state of that library as required by calling ```renv::restore()```.

2. Download the repository and run FUNKI

In an R session, run the line below to download and launch FUNKI:

```shiny::runApp()```

As in case 1, all required packages must be installed beforehand.
The ```renv.lock``` lockfile can be used to resore the library by calling ```renv::restore()```.

3. Create a docker for FUNKI

The docker file is provided to create a docker imagine. 

To build the docker image:

  * Download ```Dockerfile```  and ```FUNKI/renv.lock``` in the directory the imagine is going to be created.
  * In the console (where the ```Dockerfile``` is located), run: ```docker build -t funki .``` 

This process takes some time (~ 2400s).

To create a container just run:

  * docker ```docker run --rm -p 3838:3838 funki```

And there it is, running on ```localhost:3838```

Note: To use CARNIVAL with *cplex* or *cbc*, the software should be installed within the imagine.

## Implemented Approaches

#### DoRothEA
[DoRothEA](https://saezlab.github.io/DoRothEA/) is a gene regulatory network linking Transcription Factors (TFs) with their downstream targets. The unity of TF and its targets is referred as regulon. Such gene regulatory networks allow the computation of TF-activities from gene expression data by enrichment analysis using the regulons as underlying gene-sets. The network has been build integrating TF-target interactions from four different strategies: (i) manually curated interaction repositories, (ii) interactions derived from ChIP-seq binding data, (iii) in silico predictions of TF binding on gene promoters, and (iv) reverse-engineered regulons from large gene expression datasets. Based on the number of supporting evidences each interaction is accompanied with a confidence level ranging from high-confident (A) to low-confident (E).

#### PROGENy
[PROGENy](https://saezlab.github.io/progeny/) is a linear model to infer pathway activity from gene expression data. Pathway activities are calculated based on consensus gene signatures obtained from perturbation experiments, that is, the footprint of pathway on gene expression.

#### CARNIVAL
[CARNIVAL](https://saezlab.github.io/CARNIVAL/) (CAusal Reasoning for Network identification using Integer VALue programming) is a method for the identification of upstream reguatory signalling pathways from downstream gene expression data. The aim of the CARNIVAL pipeline is to identify a subset of interactions from a prior knowledge network that represent potential regulated pathways linking known or potential targets of perturbation towards active transcription factors derived from expression data.

#### KinAct
[KinAct](http://saezlab.github.io/kinact/) is a network linking kinases to phosphorylation sites. In a simmilar way as DoRothEA works, this resource allows the estimation of kinase activities from phosphoproteomics data using prior knowledge about kinase-substrate interactions.
