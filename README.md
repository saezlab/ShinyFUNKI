## PROGENy approach of FUNKI application for VRE

[`FUNKI`](https://github.com/saezlab/ShinyFUNKI) is a multi-omic functional integration and analysis platform. It provides a standardized pipeline to process and performs functional analysis on transcriptomic, proteomic, phosphoproteomic, and metabolomic datasets. The analysis can be performed both on a single type of omic data and on a multi-omic dataset by integrating them in supervised and unsupervised manners.

`FUNKI` is integrated into a Virtual Research Environment ([VRE](https://github.com/inab/openVRE)). `VRE` is an open-source cloud-based working environment that allows you to rapidly build your own computational platform. It offers:

- A user-friendly web-based interface that integrates a number of:
	- Analysis tools or pipelines.
	- Interfaces external data repositories.
	- Visualizers.
- A scalable backend for cloud computing compatible with `OCCI` middlewares like OpenNebula or OpenStack.

### PROGENy approach

[`PROGENy`](https://saezlab.github.io/progeny/) is a linear model to infer pathway activity from gene expression data. Pathway activities are calculated based on consensus gene signatures obtained from perturbation experiments, that is, the footprint of pathway on gene expression.

In order to use the `FUNKI` application with the `PROGENy` approach, you have to install first at least the core dependencies described in [INSTALL.md](https://github.com/lrodrin/ShinyFUNKI/blob/BSC_progeny/INSTALL.md).