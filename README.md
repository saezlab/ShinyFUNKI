## Welcome to the FUNKI application
FUNKI is a multi-omic functional integration and analysis platform. It provides a standardised pipeline to process and perform functional analysis on transcriptomic, proteomic, phosphoproteomic and metabolomic datasets. The analysis can be performed both on a single type of omic data and on multi-omic dataset by integrating them in supervised and unsupervised manners.

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

In an R session, run the line below to launch FUNKI once this repo has been downloaded/cloned:

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
[DoRothEA](https://saezlab.github.io/DoRothEA/) (Discriminant Regulon Expression Analysis) is a resource that links transcription factors (TFs) with their downstream targets ( [Garcia-Alonso et al., 2018, 2019](http://europepmc.org/abstract/MED/31340985)  ). The unity of a TF and its targets is called regulon. The regulons are built from four different strategies: (i) manually curated interaction repositories, (ii) interactions derived from ChIP-seq binding data, (iii) in silico predictions of TF binding on gene promoters, and (iv) reverse-engineered regulons from gene expression datasets.
The TFs activities are computed from gene expression by performing an enrichment analysis ( [Alvarez et al., 2016](https://www.nature.com/articles/ng.3593) ), where the regulons are the underlying gene-sets. The users can select the confidence level (A to E based on the type of the supporting evidence of given interactions) for each regulon, as well as their minimum size and the method to perform the enrichment analysis. The organism selection, human or mice, is selected when the data are uploaded ( [Holland, Szalai, et al., 2020](http://europepmc.org/abstract/MED/31525460) ). This method can also be used with single-cell data ( [Holland, Tanevski, et al., 2020](https://europepmc.org/article/MED/32051003) ). The computation yields a matrix with the normalised enrichment scores for each TF across all samples. This result is then visualised in the form of a heatmap, barplots and a network showing a TF with all its targets.

#### PROGENy
[PROGENy](https://saezlab.github.io/progeny/) (Pathway RespOnsive GENes) is a footprint method developed to infer pathway activity from gene expression data ([Schubert et al., 2018](http://europepmc.org/abstract/MED/29295995)). The scores are calculated using a linear models with weights based on consensus gene signatures obtained from publicly available perturbation experiments. 
This method can be used for either bulk or single-cell data [Holland, Tanevski, et al., 2020](https://europepmc.org/article/MED/32051003) from human or mouse ( [Holland, Szalai, et al., 2020](http://europepmc.org/abstract/MED/31525460) ). They can also select the number of top genes from the signatures according to their individual significance. This last parameter is particularly important for the single-cell data as it counteracts the typical low gene coverage of this data type. PROGENy returns a matrix of pathway activity scores across all samples. This result is then visualized as a heatmap, barplot and density-scatter plot.

#### KinAct
[KinAct](http://saezlab.github.io/kinact/) is a resource linking kinases to phosphorylation sites ( [Wirbel et al., 2018](https://www.biorxiv.org/content/10.1101/066019v1) ). It is fully integrated into Omnipath due to the addition of kinase-substrate interaction resources ( [Türei et al., 2021, 2016](http://europepmc.org/abstract/MED/27898060) ). Kinase activity estimation is performed using the same algorithm as DoRothEA to estimate activity scores ( [Alvarez et al., 2016](https://www.nature.com/articles/ng.3593) ). Instead of TF-target interactions, KinAct uses collections of kinase-substrate interactions and phosphoproteomic data instead of transcriptomic data.
The users can select the minimum size of each regulon, as well as the method that VIPER will use to perform the analysis. The result is a matrix of normalised enrichment scores for each phosphosite across all samples. This result is then visualised in the form of a heatmap, barplots and a network showing a kinase with the targeted phosphosites.

#### CARNIVAL
[CARNIVAL](https://saezlab.github.io/CARNIVAL/) (CAusal Reasoning for Network identification using Integer VALue programming) reconstructs signalling networks from downstream TF activities by finding the upstream regulators ( [Liu et al., 2019](https://www.nature.com/articles/s41540-019-0118-z) ). Given a directed prior-knowledge network (PKN) of protein-protein interactions, which can also be signed, CARNIVAL identifies a subnetwork that explains the activities of transcription factors through potential perturbed intermediate genes.
The PKN can be provided by the user, or imported from Omnipath directly within FUNKI ( [Türei et al., 2016, 2016](http://europepmc.org/abstract/MED/27898060) ). Similarly, the user can directly upload the activity of transcription factors, but those can also be calculated using DoRothEA from the expression data. When large initial networks are used, mapping key nodes with values is advised. Thus, the user can upload them or get advantage of the PROGENy scores for this task. As the previous methods, it can run on mouse or human samples. When using mouse data, the network must always be provided. CARNIVAL produces a set of networks that can be directly visualised. A pathway enrichment analysis can be run over the results. Then, these data can be visualised on bar and volcano plots.

#### COSMOS
COSMOS is a tool to integrate multiomic data with a prior knowledge network spanning signaling, gene regulation and metabolism ( [Dugourd et al. 2021](http://europepmc.org/abstract/MED/33502086) ). It uses the ILP formulation of CARNIVAL to connect two sets of upstream and downstream molecular features (e.i. kinase activities, TF activities, deregulated metabolites, enzyme fluxes, genetic or drug perturbations, etc…) with a signed directed transomic network. This resulting network is essentially a set of coherent mechanistic hypotheses that can explain how the measured deregulation may explain each other. Subsets of this network centered on user-defined nodes can be viewed in the shiny app. The network can also be downloaded as a pair of sif and attribute csv files. These files can be imported in tools such as cytoscape to visualise the full network.

## Help for FUNKI application

### Upload Data

DoRothEA, PROGENy, CARNIVAL and COSMOS can be either applied on mouse or human data. 
Independently of the omics technology, they all require a gene expression object with HGNC/MGI symbols in rows and samples in columns.
KinAct can only be applied to human data, and it requires a phosphoproteomic object with HGNC symbols and the phosphorilated site in rows, and samples in columns.

DoRothEA, KinAct and PROGENy can compute the respective activities for multiple contrast/samples in a single run. 
However, CARNIVAL and COSMOS are running on only one sample. 
For this analysis, the **time to find an optimal solution is set to 1h**.

Details of each of the required parameters can be found by clicking in the **?** symbol.
Click on each of the logos to see the parameters required for each computation.

---

#### DoRothEA

##### Control Widgets

* **Number of Transcription Factors to display:** Show the top *n* activated and inhibited TFs (Default: 25).
* **Number of targets to display:** Show the top *n* targets of a selected TF (Default: 5) 
* **Select Sample/Contrast:** Select contrast of interest.
* **Select Transcription Factor:** Select TF of interest. (Default: TF with the highest activity).

##### Figures
* **Bar TF:** The plot shows the activity of the selected TF for all given contrasts.
* **Bar Sample/Contrast:** The plot shows the top *n* regulated TFs for a given sample/contrast. Sample/contrast and number of shown TFs can be adjusted with corresponding widgets.
* **Network:** The plot shows the TF-target interactions of the selected TF and the selected sample/contrast in a network. Blue nodes indicate that the target is over-expressed and red nodes indicate that the target is down-regulated. The color of the edges represent the effect of the TF on its target (either activation or repression). The number of shown nodes can be changed by the corresponding widget.
* **Heatmap:** The heatmap provides a comprehensive overview of all contrasts/samples and top N TFs.

##### Datatables
Table of TF-activities.

##### Download
* **DoRothEA scores**: Download of TF activities in comma separated format and the figures that are currently showed.
* **Barplot for Sample**: Download Barplot of Sample/Contrast that is currently showed.
* **Barplot for TF**: Download Barplot of TFs that is currently showed.
* **Barplot for TF's network**: Download Network that is currently showed.

---

#### PROGENy

##### Control Widgets

* **Select Sample/Contrast:** Select contrast/sample of interest.
* **Select Pathway:** Select pathway to show in the scatter plot.

##### Figures
* **Heatmap:** The heatmap provides a comprehensive overview of all contrasts/samples and computed PROGENy-scores.
* **Bar:** The plot shows the activity of all paths for all given contrasts/samples.
* **Scatter:** The plot shows a scatter plot with marginal distribution (in the form of an arrangeGrob object) for the selected pathway and sample/contrast. The scatter plot has progeny weights as x-axis and the gene level stat used to compute progeny score as the y-axis. The marginal distribution of the gene level stats is displayed on the right of the plot to give visual support of the significance of each gene contributing to the progeny pathway score. The red and blue represent the positive and negative contribution of genes to the progeny pathway, respectively. For each gene contribution, 4 cases are possible, as the combinations of the sign of the gene level stat and the sign of the gene level weight. Positive weight will lead to a positive(blue)/negative(red) gene contribution if the gene level stat is positive/negative. Negative weight will lead to a negative(red)/positive(blue) gene contribution if the gene level stat is positive/negative.

##### Datatables
Table of PROGENy-scores.

##### Download
* **PROGENy scores and figures**: Download of PROGENy scores in comma separated format (csv) and the figures that are currently showed.
* **Scatter plot** Download of the scatter plot for a given pathway and sample/contrast.
* **Bar plot** Download of the bar plot for all pathways and sample/contrasts.
* **Heatmap** Download of the heatmap for all pathways and sample/contrasts.

---

#### KinAct

##### Control Widgets

* **Number of Kinases to display:** Show the top *n* activated and inhibited kinases (Default: 25).
* **Number of targets to display:** Show the top *n* targets of a selected kinase (Default: 5) 
* **Select Sample/Contrast:** Select contrast of interest.
* **Select Kinase:** Select kinase of interest. (Default: kinase with the highest activity).

##### Figures
* **Bar Kinase:** The plot shows the activity of the selected kinase for all given contrasts.
* **Bar Sample/Contrast:** The plot shows the top *n* regulated kinases for a given sample/contrast. Sample/contrast and number of shown kinases can be adjusted with corresponding widgets.
* **Network:** The plot shows the interactions of the selected kinases with the phosphosite's targets for the selected sample/contrast in a network. Blue nodes indicate over-expression, and red nodes indicate down-regulation. The color of the edges represent the effect of the kinase on its target (either activation or repression). The number of shown nodes can be changed by the corresponding widget.
* **Heatmap:** The heatmap provides a comprehensive overview of all contrasts/samples and top N kinases.

##### Datatables
Table of kinase activities.

##### Download
* **KinAct scores**: Download of kinase activities in comma separated format and the figures that are currently showed.
* **Barplot for Sample**: Download Barplot of Sample/Contrast that is currently showed.
* **Barplot for Kinase**: Download Barplot of Kinases that is currently showed.
* **Barplot for Kinase's network**: Download Network that is currently showed.

---

#### CARNIVAL

##### Control Widgets
###### CARNIVAL results visualisation
* **Focus on node:** Select a node to zoom in.
* **Hierarchical layout:** Get a hierarchical layout.

###### Enrichment analysis of CARNIVAL results
* **Select resource:** Select resource to extract the biological groups. A custom file can be upload or use any of the resources available through Omnipath.
* **Adjusted pValue:** Adjusted pValue to use as threshold to show the enriched results.
* **Paths/Signatures:** The number of significant Paths/Signatures to show in plots.
* **Genes:** The number of significant Genes to show in volcano plot.

##### Figures
* **Network:** CARNIVAL reconstructed network.
* **Bar:** The plot shows the pathways over the adjusted p-value in log scale. The cutoff for the adjusted p-value can be changed, as well as the nubmer of pathways to show, with the corresponding widget.
* **Volcano:** The plot shows the nodes of the reconstructed network. The colored dots indicate the pathway in which the genes are involved. The cutoff for the adjusted p-value, number of pathways and genes that are showed can be changed with the corresponding widget.

##### Datatables
Table of the geneset/pathway enritchment analysis.

##### Download
* **PEA results**: Download the gene set/pathway enritchment analysis in a comma separated format.
* **Volcanoplot**: Download the volcanoplot.
* **Barplot**: Download the lolypop plot.
* **CARNIVAL results** Download CARNIVAL results in and .rds format. The networks is also provided as _.csv_ format and the nodes attributes in a comma separated format. The last two files can be used to visualize the network in <a href="https://cytoscape.org" target="_blank">Cytoscape</a>.

---

#### COSMOS

##### Control Widgets
* **Focus on node:** Select a node to zoom in.

##### Figures
* **Network:** CARNIVAL reconstructed network.

##### Datatables
Table of the geneset/pathway enritchment analysis.

##### Download
* **COSMOS results** Download COSMOS results in and .rds format. The networks is also provided as _.csv_ format and the nodes attributes in a comma separated format. The last two files can be used to visualize the network in <a href="https://cytoscape.org" target="_blank">Cytoscape</a>.
