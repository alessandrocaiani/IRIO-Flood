The script SampleParams allows to create the sample of parameter combinations employed in the estimation process. 

The files main_EstimationModifiedLeontief and main_EstimationTraditionalLeontief launch the simulations on the 50000 parameter combinations to be tested and compute the associated loss function according to the MSM.

The folder resources contains the auxiliary materials required for running the code (with the only exception of the IO tables, which required authorization from IRPET).

The results folder contains the the results of the MSM procedure (.rds files), a script to run the model with the winning combination of parameters and a script to create the plots referring to the estimation phase of the paper.

The sensitivity_experiments folder contains the scripts and uxialiary files for the two sensitivity experiments presented in the paper, one related to the Covid-19 period estimation timespan (folder SensitivityCovidEstimation), and one to the Emilia Romagna 2023 flood application (folder SensitivityEmiliaFlood)
