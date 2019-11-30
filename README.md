# Model Selection for: Social Intelligence and Social Networks Predict Group Collective Intelligence

This repository contains the code used during the model selection part of the analysis. The folders of the repository are:

- **data-raw** All the raw data (surveys) from the experiment. The folder only has symbolic links, meaning that
  to run the analyses you will need to request the data to the corresponding author of the paper.
  
- **data** Contains all the R scripts used to prepare the data for the analysis. Once executed, the generated
  datasets will be stored in the same place with the same name of the R script used to generate it.
  
- **analysis** R files used to anlyze the data. Includes all the steps for the model selection algorithm.

- **figures** R scripts to generate descriptive stats. Other figures that are generated during the analysis
  are also stored here.
  

  The **Makefile** is just a tool used to ease some repetitive tasks. Users are not required to use this to
  replicate the results from the paper.
