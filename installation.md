## Installation

FUNKI is accessible on [shinyapps](https://saezlab.shinyapps.io/funki/) or to run locally.

| :warning:        | Online version does NOT support CARNIVAL/COSMOS to be run with *cplex* or *cbc*. This software is licenced-based, so the LOCAL version of FUNKI is advided (previous installation of the selected software)      |
|---------------|:------------------------|

To run FUNKI locally, there are different options to fit all possible users: 

1. Run FUNKI directly from GitHub

In an R session, run the line below to launch FUNKI:

```shiny::runGitHub(repo = "ShinyFUNKI", username = "saezlab", subdir = "FUNKI")``` 

| :point_up:    | Remember to have all required packages installed beforehand! |
|---------------|:------------------------|

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


| :point_up:    | Remember to install *cplex* or *cbc* to use CARNIVAL/COSMOS with these softwares! |
|---------------|:------------------------|
