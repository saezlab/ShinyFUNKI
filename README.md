## DoRothEA approach of FUNKI application for VRE

[`FUNKI`](https://github.com/saezlab/ShinyFUNKI) is a multi-omic functional integration and analysis platform. It provides a standardized pipeline to process and performs functional analysis on transcriptomic, proteomic, phosphoproteomic, and metabolomic datasets. The analysis can be performed both on a single type of omic data and on a multi-omic dataset by integrating them in supervised and unsupervised manners.

`FUNKI` is integrated into a Virtual Research Environment ([VRE](https://github.com/inab/openVRE)). `VRE` is an open-source cloud-based working environment that allows you to rapidly build your own computational platform. It offers:

- A user-friendly web-based interface that integrates a number of:
	- Analysis tools or pipelines.
	- Interfaces external data repositories.
	- Visualizers.
- A scalable backend for cloud computing compatible with `OCCI` middlewares like OpenNebula or OpenStack.

### DoRothEA approach

[`DoRothEA`](https://saezlab.github.io/dorothea/) is a gene regulatory network linking Transcription Factors (TFs) with their downstream targets. The unity of TF and its targets are referred to as regulon. Such gene regulatory networks allow the computation of TF-activities from gene expression data by enrichment analysis using the regulons as underlying gene sets. The network has been build integrating TF-target interactions from four different strategies: (i) manually curated interaction repositories, (ii) interactions derived from ChIP-seq binding data, (iii) in silico predictions of TF binding on gene promoters, and (iv) reverse-engineered regulons from large gene expression datasets. Based on the number of supporting evidence each interaction is accompanied by a confidence level ranging from high-confident (A) to low-confident (E).

In order to use the `FUNKI` application with the `DoRothEA` approach, you have to install first at least the core dependencies described in [INSTALL.md](https://github.com/lrodrin/ShinyFUNKI/blob/BSC_dorothea/INSTALL.md).
