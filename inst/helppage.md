## Help page of the FUNKI application.
### Upload Data
All tools can be either applied on mouse or human data and require, independently of the omics technology, the output of a differential expression analysis. The table must contain the following comma separated columns (in arbitrary order): 
 
* `...`: Identifier of measured variable. `gene` for transcriptomics and `protein` for phosphoproteomics. Identifier must be encoded as symbols.
* `contrast`: Name of the computed contrast in the differential expression analysis.
* `logFC`: Effect size of the variable of interest.
* `adj.p.value`: p-value corrected for multiple hypothesis.

All other columns will be ignored.

All implemented tools can compute the respective activities for multiple contrast in a single run. Simply concatenate all tables (in the above described format) row-wise.

### DoRothEA
#### Before you start
We provide two different gene regulatory networks for the computation of TF-activities:

* **Coverage:** Contains all TF-target interactions. There is a confidence level assigned to each interaction. Filtering confidence level will remove interactions.
* **Quality:** Here each TF is assigned with a confidence level. The confidence level of a TF is inferred by its interaction confidence level. Filtering confidence level will remove regulons.

Click on `Run DoRothEA` to start the computation of TF-activities

#### Control Widgets

* **Select Contrast:** Select contrast of interest.
* **Select Confidence Level:** Select confidence levels which should be considered in the analysis (Default: A and B, changing will require re-computation of TF-activities). 
* **Number of dsiplayed TFs:** Show the top *n* activated and inhibited TFs (Default: 10).
* **Select TF:** Select TF of interest. (Default: TF with the highest activity).
* **Choose cutoff for adjusted p-value:** Genes with an adjusted p-value below this cutoff are considered to be differentially expressed (Default: 0.05).
* **Number of shown labels in Volcano plot/network:** Number of genes to be labelled in the volcano plot and number of nodes shown in the network (Default: 10).

#### Figures
* **Lollipop:** The plot shows the top *n* regulated TFs for a given contrast. Contrast and number of shown TFs can be adjusted with corresponding widgets.
* **Volcano:** The plot shows the targets of the selected TF and the selected contrast. Green dots represent significantly over-expressed and red dots represent significant down-regulated genes. The cutoff for the adjusted p-value can be changed with the corresponding widget.
* **Network:** The plot shows the TF-target interactions of the selected TF and the selected contrast in a network. Green nodes indicate that the target is over-expressed and red nodes indicate that the target is down-regulated. The color of the edges represent the effect of the TF on its target (either activation or repression). The number of shown nodes can be changed by the corresponding widget.
* **Heatmap:** The heatmap provides a comprehensive overview of all contrasts and computed TF-activities.
* **Bar:** The plot shows the activity of the selected TF for all given contrasts.

#### Datatables
Table of TF-activities.

#### Download
* **Download TF-activites**: Download of TF activities in [tidy](https://r4ds.had.co.nz/tidy-data.html) format
* **Download network file (.sif)** Download of the TF-target network for a given TF and contrast in .sif format. Can be used to visualize the network in [Cytoscape](https://cytoscape.org)
