## Help page of the FUNKI application.
Dear User,

if your data files are sensitive or confidential we highly encourage you to run this app locally by typing 

```shiny::runGithub("ShinyFUNKI", "saezlab")``` 

in an R session. **Please note that this feature will be enabled as soon as this App is published.**

---

### Upload Data

All tools can be either applied on mouse or human data and require, independently of the omics technology, a gene expression object with HGNC/MGI symbols in rows and samples in columns. The output of a differential expression analysis can also be used.

DoRothEA and PROGENy can compute the respective activities for multiple contrast/samples in a single run. However, CARNIVAL only takes the first element of the matrix.

Details of each of the required parameters can be found by clicking in the **?** symbol.
Click on `Run DoRothEA`, `Run PROGENy` or `Run CARNIVAL`to start the computation.


---

### DoRothEA

#### Control Widgets

* **Number of Transcription Factors to display:** Show the top *n* activated and inhibited TFs (Default: 25).
* **Number of targets to display:** Show the top *n* targets of a selected TF (Default: 5) 
* **Select Sample/Contrast:** Select contrast of interest.
* **Select Transcription Factor:** Select TF of interest. (Default: TF with the highest activity).

#### Figures
* **Bar TF:** The plot shows the activity of the selected TF for all given contrasts.
* **Bar Sample/Contrast:** The plot shows the top *n* regulated TFs for a given sample/contrast. Sample/contrast and number of shown TFs can be adjusted with corresponding widgets.
* **Network:** The plot shows the TF-target interactions of the selected TF and the selected sample/contrast in a network. Green nodes indicate that the target is over-expressed and red nodes indicate that the target is down-regulated. The color of the edges represent the effect of the TF on its target (either activation or repression). The number of shown nodes can be changed by the corresponding widget.

#### Datatables
Table of TF-activities.

#### Download
* **Download DoRothEA scores and figures**: Download of TF activities in comma separated format and the figures that are currently showed.

---

### PROGENy

#### Control Widgets

* **Select Sample/Contrast:** Select contrast/sample of interest.
* **Select Pathway:** Select pathway to show in the scatter plot.

#### Figures
* **Heatmap:** The heatmap provides a comprehensive overview of all contrasts/samples and computed PROGENy-scores.
* **Bar:** The plot shows the activity of all paths for all given contrasts/samples.
* **Scatter:** The plot shows a scatter plot with marginal distribution (in the form of an arrangeGrob object) for the selected pathway and sample/contrast. The scatter plot has progeny weights as x-axis and the gene level stat used to compute progeny score as the y-axis. The marginal distribution of the gene level stats is displayed on the right of the plot to give visual support of the significance of each gene contributing to the progeny pathway score. The red and blue represent the positive and negative contribution of genes to the progeny pathway, respectively. For each gene contribution, 4 cases are possible, as the combinations of the sign of the gene level stat and the sign of the gene level weight. Positive weight will lead to a positive(blue)/negative(red) gene contribution if the gene level stat is positive/negative. Negative weight will lead to a negative(red)/positive(blue) gene contribution if the gene level stat is positive/negative.

#### Datatables
Table of PROGENy-scores.

#### Download
* **Download PROGENy scores and figures**: Download of PROGENy scores in comma separated format (csv) and the figures that are currently showed.
* **Download scatter plot** Download of the scatter plot for a given pathway and sample/contrast.
* **Download bar plot** Download of the bar plot for all pathways and sample/contrasts.
* **Download heatmap** Download of the heatmap for all pathways and sample/contrasts.

---

### CARNIVAL

#### Control Widgets
* **Focus on node:** Select a node to zoom in.
* **Path to TF:** Select a Transcription Factor (TF) so show all nodes that lead to the selected TF. 

#### Figures
* **Network:** CARNIVAL reconstructed network.
* **Bar:** The plot shows the pathways over the adjusted p-value in log scale. The cutoff for the adjusted p-value can be changed, as well as the nubmer of pathways to show, with the corresponding widget.
* **Volcano:** The plot shows the nodes of the reconstructed network. The colored dots indicate the pathway in which the genes are involved. The cutoff for the adjusted p-value, number of pathways and genes that are showed can be changed with the corresponding widget.

#### Datatables
Table of the geneset/pathway enritchment analysis.

#### Download
* **Download EA/Figures**: Download the gene set/pathway enritchment analysis in a comma separated format and the volcano and bar plots that are showed.
* **Download carnival results** Download CARNIVAL results in and .rds format. The networks is also provided in a .sif format and the nodes attributes in a comma separated format. The last to files can be used to visualize the network in <a href="https://cytoscape.org" target="_blank">Cytoscape</a>.

