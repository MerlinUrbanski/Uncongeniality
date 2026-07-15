# Rethinking the Match: A Simulation-Based Assessment of Congeniality in Continuous Prediction Models

**Author:** Merlin Urbanski
**Program:** Methodology and Statistics for Behavioral, Biomedical, and Social Sciences (Research Master), Utrecht University
**Supervisors:** Dr. Maarten van Smeden (UMC Utrecht), Dr. Anne de Hond (UMC Utrecht), PhD Candidate Alex Carriero (UMC Utrecht)
**Host Institution:** Julius Center for Health Sciences and Primary Care, UMC Utrecht
**DOI:** [https://doi.org/10.31237/osf.io/n96rg_v1](https://doi.org/10.31237/osf.io/n96rg_v1)

---

## What this project is about

Medical datasets are almost never complete. When training a prediction model, researchers have to decide how to fill in missing values (imputation) and which algorithm to use for the actual prediction. My thesis asks a simple but underexplored question:

**Does it matter whether the imputation method and the prediction model "agree" with each other in their underlying assumptions?**

This alignment is called *congeniality*. It's a well-established concept in statistical inference (i.e. does my model give me an accurate estimate of an effect), but almost nobody had tested whether it also matters for **predictive accuracy** — which is the goal in most applied machine learning and clinical prediction settings.

## What I did

- Designed and ran a **large-scale simulation study** (40 scenarios, 100 iterations each) systematically varying missingness mechanisms (MCAR, MAR, MNAR), the shape of relationships between predictors (linear/quadratic), and correlation strength among predictors.
- Combined **5 imputation methods** (predictive mean matching, regression imputation, random forest imputation, and quadratic variants) with **2 prediction models** (linear regression, random forest) — 10 model combinations in total, spanning congenial, uncongenial, and intermediate pairings.
- Validated the simulation findings on **real-world clinical data** using the MIMIC-III ICU database (~37,000 patients), predicting blood urea nitrogen from routine lab values.
- Evaluated results using RMSE, R², and calibration curves, following the ADEMP framework for simulation study design and reporting.
- Built the full simulation and analysis pipeline in **R** (mice, randomForest, ggplot2), run on the UMC Utrecht High Performance Computing cluster.

## Key finding

Across every scenario and in the real-data case study, **congeniality did not affect predictive accuracy or calibration**. What did matter was the specific pairing of imputation method and prediction model — some combinations consistently worked better together regardless of whether they were formally "congenial." This suggests that guidance developed for parameter estimation doesn't automatically transfer to the prediction setting, and that model-combination choices in applied prediction modeling deserve their own evidence base.

## Abstract

**Introduction**
Missing data is a common challenge in medical research, and selecting an appropriate imputation method is crucial for accurate predictions. Congeniality refers to the alignment between the assumptions of the imputation model and the substantive prediction model. While this concept is well-understood in the context of parameter estimation, its implications for predictive performance and model calibration remain unclear.

**Methods**
We evaluated congenial and uncongenial model combinations across various scenarios, reflecting different relationships between predictors and the outcome. Our analysis focused on predictive accuracy and calibration in settings with continuous predictors and continuous outcomes. To illustrate these findings, we conducted a case study using the MIMIC-III dataset.

**Results**
Across all simulation scenarios, congeniality had no observable impact on the accuracy or calibration of model combinations. However, patterns from both the simulation study and the MIMIC-III case study suggested that interactions between the imputation model and the substantive prediction model can influence overall performance.

**Conclusion**
Accuracy and calibration are not determined by congeniality, while the combination of imputation and substantive prediction model matters.

## Repository contents

This repository is the Research Archive for the thesis and contains all code needed to reproduce the simulation study and the MIMIC-III case study, along with the full thesis text.

## Ethics

The simulation study and the MIMIC-III data example were approved by the Ethics Committee of the Faculty of Social and Behavioural Sciences, Utrecht University (FETC: 24-2006 and 24-2242).

## Contact

Questions are welcome — reach me at merlin.urbanski@gmail.com
