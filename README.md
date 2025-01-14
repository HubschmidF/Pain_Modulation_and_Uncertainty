# Pain_Modulation_and_Uncertainty

Software scripts for the Pain Modulation and Uncertainty project. 
Contains the software scripts for the analyses and results reported in the main text of the "Reward-induced endogenous pain inhibition scales with action-outcome certainty in humans" manuscript (Not published).
The used stan implementation of the models is the one of Lei Zhang already used in Kreis & al. 2022, 2023 that is openly available (https://osf.io/6xab2/), therefore stan files are not given here.

## Datasets
Datasets are openly available on a dedicated OSF repository (https://osf.io/s2j9w/).

### Task_Data.csv
Dataframe of the raw data of the main task.

### Extracted_Quantities.csv
Dataframe with only active test trials of the main task as well as computationally modelled quantities, such as the state belief (HMM), the entropy of the state belief (HMM), the Bayesian surprise (HMM), the prediction-error (RL), as well as the extent of pain modulation in each test trial (estimated as in Desch & al. 2023).

### M5_Draws.xlsx
Posterior MCMC draws of the best fitting model (Diff-HMM) of Schlagenhauf & al. 2013. Those draws are used for posterior inference on parameters.

### Simulation folder
Contains the simulated data for the models with most free parameters, generating parameter values and recovered parameter values.

## References
1- Desch, S., Schweinhardt, P., Seymour, B., Flor, H., & Becker, S. (2023). Evidence for dopaminergic involvement in endogenous modulation of pain relief. eLife, 12, e81436. https://doi.org/10.7554/eLife.81436

2- Kreis, I., Zhang, L., Mittner, M., Syla, L., Lamm, C., & Pfuhl, G. (2023). Aberrant uncertainty processing is linked to psychotic-like experiences, autistic traits, and is reflected in pupil dilation during probabilistic learning. Cognitive, Affective, & Behavioral Neuroscience. https://doi.org/10.3758/s13415-023-01088-2

3- Kreis, I., Zhang, L., Moritz, S., & Pfuhl, G. (2022). Spared performance but increased uncertainty in schizophrenia: Evidence from a probabilistic decision-making task. Schizophrenia Research, 243, 414–423. https://doi.org/10.1016/j.schres.2021.06.038

4- Schlagenhauf, F., Huys, Q. J. M., Deserno, L., Rapp, M. A., Beck, A., Heinze, H.-J., Dolan, R., & Heinz, A. (2014). Striatal dysfunction during reversal learning in unmedicated schizophrenia patients. NeuroImage, 89, 171–180. https://doi.org/10.1016/j.neuroimage.2013.11.034
