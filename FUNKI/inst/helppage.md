## Help page of the FUNKI application.

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

### DoRothEA

#### Control Widgets

* **Number of Transcription Factors to display:** Show the top *n* activated and inhibited TFs (Default: 25).
* **Number of targets to display:** Show the top *n* targets of a selected TF (Default: 5) 
* **Select Sample/Contrast:** Select contrast of interest.
* **Select Transcription Factor:** Select TF of interest. (Default: TF with the highest activity).

#### Figures
* **Bar TF:** The plot shows the activity of the selected TF for all given contrasts.
* **Bar Sample/Contrast:** The plot shows the top *n* regulated TFs for a given sample/contrast. Sample/contrast and number of shown TFs can be adjusted with corresponding widgets.
* **Network:** The plot shows the TF-target interactions of the selected TF and the selected sample/contrast in a network. Blue nodes indicate that the target is over-expressed and red nodes indicate that the target is down-regulated. The color of the edges represent the effect of the TF on its target (either activation or repression). The number of shown nodes can be changed by the corresponding widget.
* **Heatmap:** The heatmap provides a comprehensive overview of all contrasts/samples and top N TFs.

#### Datatables
Table of TF-activities.

#### Download
* **DoRothEA scores**: Download of TF activities in comma separated format and the figures that are currently showed.
* **Barplot for Sample**: Download Barplot of Sample/Contrast that is currently showed.
* **Barplot for TF**: Download Barplot of TFs that is currently showed.
* **Barplot for TF's network**: Download Network that is currently showed.

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
* **PROGENy scores and figures**: Download of PROGENy scores in comma separated format (csv) and the figures that are currently showed.
* **Scatter plot** Download of the scatter plot for a given pathway and sample/contrast.
* **Bar plot** Download of the bar plot for all pathways and sample/contrasts.
* **Heatmap** Download of the heatmap for all pathways and sample/contrasts.

---

### KinAct

#### Control Widgets

* **Number of Kinases to display:** Show the top *n* activated and inhibited kinases (Default: 25).
* **Number of targets to display:** Show the top *n* targets of a selected kinase (Default: 5) 
* **Select Sample/Contrast:** Select contrast of interest.
* **Select Kinase:** Select kinase of interest. (Default: kinase with the highest activity).

#### Figures
* **Bar Kinase:** The plot shows the activity of the selected kinase for all given contrasts.
* **Bar Sample/Contrast:** The plot shows the top *n* regulated kinases for a given sample/contrast. Sample/contrast and number of shown kinases can be adjusted with corresponding widgets.
* **Network:** The plot shows the interactions of the selected kinases with the phosphosite's targets for the selected sample/contrast in a network. Blue nodes indicate over-expression, and red nodes indicate down-regulation. The color of the edges represent the effect of the kinase on its target (either activation or repression). The number of shown nodes can be changed by the corresponding widget.
* **Heatmap:** The heatmap provides a comprehensive overview of all contrasts/samples and top N kinases.

#### Datatables
Table of kinase activities.

#### Download
* **KinAct scores**: Download of kinase activities in comma separated format and the figures that are currently showed.
* **Barplot for Sample**: Download Barplot of Sample/Contrast that is currently showed.
* **Barplot for Kinase**: Download Barplot of Kinases that is currently showed.
* **Barplot for Kinase's network**: Download Network that is currently showed.

---

### CARNIVAL

#### Control Widgets
##### CARNIVAL results visualisation
* **Focus on node:** Select a node to zoom in.
* **Hierarchical layout:** Get a hierarchical layout.

##### Enrichment analysis of CARNIVAL results
* **Select resource:** Select resource to extract the biological groups. A custom file can be upload or use any of the resources available through Omnipath.
* **Adjusted pValue:** Adjusted pValue to use as threshold to show the enriched results.
* **Paths/Signatures:** The number of significant Paths/Signatures to show in plots.
* **Genes:** The number of significant Genes to show in volcano plot.

#### Figures
* **Network:** CARNIVAL reconstructed network.
* **Bar:** The plot shows the pathways over the adjusted p-value in log scale. The cutoff for the adjusted p-value can be changed, as well as the nubmer of pathways to show, with the corresponding widget.
* **Volcano:** The plot shows the nodes of the reconstructed network. The colored dots indicate the pathway in which the genes are involved. The cutoff for the adjusted p-value, number of pathways and genes that are showed can be changed with the corresponding widget.

#### Datatables
Table of the geneset/pathway enritchment analysis.

#### Download
* **PEA results**: Download the gene set/pathway enritchment analysis in a comma separated format.
* **Volcanoplot**: Download the volcanoplot.
* **Barplot**: Download the lolypop plot.
* **CARNIVAL results** Download CARNIVAL results in and .rds format. The networks is also provided as _.csv_ format and the nodes attributes in a comma separated format. The last two files can be used to visualize the network in <a href="https://cytoscape.org" target="_blank">Cytoscape</a>.

---

### COSMOS

#### Control Widgets
* **Focus on node:** Select a node to zoom in.

#### Figures
* **Network:** CARNIVAL reconstructed network.

#### Datatables
Table of the geneset/pathway enritchment analysis.

#### Download
* **COSMOS results** Download COSMOS results in and .rds format. The networks is also provided as _.csv_ format and the nodes attributes in a comma separated format. The last two files can be used to visualize the network in <a href="https://cytoscape.org" target="_blank">Cytoscape</a>.
