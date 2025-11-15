# Scripts for growth function model analysis

This folder contains the four custom R scripts developed to run, compare and visualize growth function models from Capture-mark-recapture data 
(scripts 01 and 02) and individuals of known age (script 03), as well as to create graphs to compare consensus size-at-age estimates
with raw body size measures of individuals in multiyear monitoring programs (script 04).

The folder also contains the input data files required for aforementioned analyses, which can be used as templates to analyse other datasets.

The file 'Growth_recaptures.csv' contains a dataset of repeated body size measures (snout-to-vent length, SVL) of marked adults of 
10 amphibian species studied in a long-term monitoring program in central Spain.

The file 'models.csv' contains a list of different growth function models that can be applied to repeated body size measures of identified
individuals. These models correspond to four broad model formulations: von-Bertalanffy (coded as VB), Logistic, Schnute and Seasonal.

The file 'age_certain.csv' contains body size measures of individuals of known age, given the date of the measurement record and the estimated
date of metamorphosis.

The file 'captures_per_year.csv' contains repeated body size measures of marked individuals in multiyear monitoring programs.

The file 'specific_parameters.csv' contains a set of parameters to be used in the analyses coded in the R script. This file contains the 
following columns:
1) species: the name of the target species.
2) neg_gr_thr: a negative integer coding the maximum body size reduction (either real or as a result of a measurement error) allowed to maintain
   that individual in the dataset. If the threshold is exceeded in any of the recorded measures, the complete history record of the individual 
   is removed from the dataset.
3) max_dsvl_thr: the maximum positive difference in body size allowed for the species, to detect and remove measurement errors.
4) min_temp_thr: the minimum timespan allowed for the specified max_dsvl_thr, to detect and remove measurement errors.
5) Met_size: body size at metamorphosis.
6) tmax: a timespan (in days) that encompasses the maximum longevity of the species (better to overestimate than to underestimate).
7) Linf_init: the initial value for the asymptotic body size in growth models.
8) k_init: the initial value for the growth parameter in growth models.
9) C_init: the initial value for the specific C parameter in seasonal growth models.
10) ts_init: : the initial value for the specific ts parameter in seasonal growth models.
11) max_size_age: the maximum age used for visualization as horizontal axis in model graphs.
12) models_to_test: the capture-mark-recapture models to be tested, separated with a '+' character. Four possible models: von-Bertalanffy (coded as VB), Logistic, Schnute and Seasonal.
13) exclude_juvs: whether to exclude juveniles from capture-mark-recapture models in the model comparison step of script 01 (yes/no).
14) exclude_unknown_sex: whether to exclude individuals of unknown sex from capture-mark-recapture models in the model comparison step
    of script 01 (yes/no).
15) knownage_pops: analyse the study populations as either 'pooled' or 'separated' in the growth models fitted to individuals of
    known age (script 03).
16) knownage_sex: whether to analyse only 'males', only 'females', both sexes separated ('males+females') or both sexes mixed altogether
    ('pooled') in the growth models fitted to body size records of individuals of known age (script 03). Leave blank in no known age models
    are applied.
17) modelpred: the selected model for consensus estimates of body size measures at age, either capture-mark-recapture ('recaptures') or
    individuals of known age ('knownage'). Leave blank in no consensus estimates were obtained.
18) refpop: the name of the reference population to be used in the visualization of the consensus estimates and the raw body size measures
    of the monitoring program. Set 'pooled' if data were pooled for the construction of the selected model or leave blank if no consensus
    estimates were obtained.

See Sánchez-Montes et al. (2025) Integrative Zoology (Submitted) for further details.
