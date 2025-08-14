# validating-counterfactual-predictions

Replication files for:

Boyer, C., Dahabreh, I. J., & Steingrimsson, J. A. (2024). “Estimating and evaluating counterfactual prediction models”. arXiv. https://doi.org/10.48550/arXiv.2308.13026

# Abstract
Counterfactual prediction methods are required when a model will be deployed in a setting where treatment policies differ from the setting where the model was developed, or when a model provides predictions under hypothetical interventions to support decision-making. However, estimating and evaluating counterfactual prediction models is challenging because, unlike traditional (factual) prediction, one does not observe the potential outcomes for all individuals under all treatment strategies of interest. Here, we discuss how to estimate a counterfactual prediction model, how to assess the model's performance, and how to perform model and tuning parameter selection. We provide identification and estimation results for counterfactual prediction models and for multiple measures of counterfactual model performance, including loss-based measures, the area under the receiver operating characteristic curve, and the calibration curve. Importantly, our results allow valid estimates of model performance under counterfactual intervention even if the candidate prediction model is misspecified, permitting a wider array of use cases. We illustrate these methods using simulation and apply them to the task of developing a statin-naive risk prediction model for cardiovascular disease.

## Requirements

This project requires R (version 4.0 or higher) and the following R packages:

- `tidyverse` - Data manipulation and visualization
- `data.table` - Fast data manipulation
- `knitr` - Dynamic report generation  
- `kableExtra` - Enhanced table formatting
- `mgcv` - Generalized additive models
- `MASS` - Statistical functions and datasets
- `modelsummary` - Model summary tables
- `progressr` - Progress reporting
- `patchwork` - Combining plots

## Installation

1. Clone this repository or download the source code
2. Open the `validating-counterfactual-predictions.Rproj` file in RStudio
3. Install required packages by running:
   ```r
   source("1_code/0_packages.R")
   ```

## Repository structure

```
├── __master_run.R              # Main script to run all analyses
├── 1_code/                     # Analysis scripts
│   ├── 0_packages.R            # Package loading
│   ├── 1_functions.R           # Custom functions for simulations
│   ├── 2_run_sim_1.R           # Simulation study 1 (continuous outcomes)
│   ├── 3_run_sim_2.R           # Simulation study 2 (binary outcomes)  
│   ├── 4_load_app_data.R       # Load and prepare application data
│   ├── 5_fit_app_models.R      # Fit prediction models
│   └── 6_validate_app_models.R # Validate model performance
├── 2_tables/                  # Generated tables
├── 3_figures/                 # Generated figures
└── 4_manuscripts/             # Manuscript files
    ├── main.tex               # Main manuscript
    ├── supplement.tex         # Supplementary material
    └── *.bib                  # Bibliography files
```

## How to run

To reproduce all analyses from the paper:

1. **Run complete analysis pipeline:**
   ```r
   source("__master_run.R")
   ```

2. **Run individual components:**
   - Load packages: `source("1_code/0_packages.R")`
   - Load functions: `source("1_code/1_functions.R")`
   - Simulation 1: `source("1_code/2_run_sim_1.R")`
   - Simulation 2: `source("1_code/3_run_sim_2.R")`
   - Application data: `source("1_code/4_load_app_data.R")`
   - Model fitting: `source("1_code/5_fit_app_models.R")`
   - Model validation: `source("1_code/6_validate_app_models.R")`

**Note:** The application data analysis (scripts 4-6) requires access to the MESA dataset, which is not included in this repository due to data use agreements.

## Output

The analysis generates several outputs:

- **Simulation results:** Tables showing mean squared error (MSE) comparisons between weighted and unweighted estimators for correctly and incorrectly specified prediction models
- **Calibration plots:** Visual assessment of model calibration in `3_figures/calib.pdf`
- **Model performance metrics:** Counterfactual prediction performance measures including loss-based measures, AUC, and calibration curves
- **Manuscript files:** LaTeX source files for the main paper and supplement in `4_manuscripts/`

Key findings demonstrate that weighted estimators provide valid estimates of counterfactual model performance even when prediction models are misspecified, while unweighted estimators can be biased.

## Citation

```bibtex
@article{boyer2024estimating,
  title={Estimating and evaluating counterfactual prediction models},
  author={Boyer, Christopher B and Dahabreh, Issa J and Steingrimsson, Jon A},
  journal={arXiv preprint arXiv:2308.13026},
  year={2024},
  doi={10.48550/arXiv.2308.13026}
}
```

