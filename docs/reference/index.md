# Package index

## Model Training

Functions to train TabPFN models

- [`tab_pfn_classification()`](https://cedricbouffard.github.io/rtabpfn/reference/tab_pfn_classification.md)
  : Train a TabPFN classification model
- [`tab_pfn_regression()`](https://cedricbouffard.github.io/rtabpfn/reference/tab_pfn_regression.md)
  : Train a TabPFN regression model with enhanced predict options
- [`tab_pfn_unsupervised()`](https://cedricbouffard.github.io/rtabpfn/reference/tab_pfn_unsupervised.md)
  : Train a TabPFN Unsupervised Anomaly Detection Model

## Prediction

Functions for making predictions

- [`predict(`*`<tab_pfn>`*`)`](https://cedricbouffard.github.io/rtabpfn/reference/predict.tab_pfn.md)
  : Predict method for TabPFN models
- [`predict(`*`<tab_pfn_unsupervised>`*`)`](https://cedricbouffard.github.io/rtabpfn/reference/predict.tab_pfn_unsupervised.md)
  : Predict method for TabPFN Unsupervised models

## Model Interpretability

Functions for SHAP-based model explanations

- [`shap`](https://cedricbouffard.github.io/rtabpfn/reference/shap.md) :
  SHAP Values for TabPFN Models
- [`shap_values()`](https://cedricbouffard.github.io/rtabpfn/reference/shap_values.md)
  : Calculate SHAP Values for a TabPFN Model
- [`plot_shap_summary()`](https://cedricbouffard.github.io/rtabpfn/reference/plot_shap_summary.md)
  : Plot SHAP Summary
- [`plot_shap_dependence()`](https://cedricbouffard.github.io/rtabpfn/reference/plot_shap_dependence.md)
  : Plot SHAP Dependence Plot
- [`explain_prediction()`](https://cedricbouffard.github.io/rtabpfn/reference/explain_prediction.md)
  : Explain Individual Prediction with SHAP
- [`print(`*`<shap_explanation>`*`)`](https://cedricbouffard.github.io/rtabpfn/reference/print.shap_explanation.md)
  : Print method for SHAP explanation

## Anomaly Detection

Functions for unsupervised anomaly detection

- [`anomaly`](https://cedricbouffard.github.io/rtabpfn/reference/anomaly.md)
  : Unsupervised Anomaly Detection for TabPFN
- [`anomaly_scores()`](https://cedricbouffard.github.io/rtabpfn/reference/anomaly_scores.md)
  : Calculate Anomaly/Outlier Scores
- [`predict(`*`<tab_pfn_unsupervised>`*`)`](https://cedricbouffard.github.io/rtabpfn/reference/predict.tab_pfn_unsupervised.md)
  : Predict method for TabPFN Unsupervised models

## Setup and Configuration

Functions for setting up the Python environment

- [`setup_tabpfn()`](https://cedricbouffard.github.io/rtabpfn/reference/setup_tabpfn.md)
  : Configure TabPFN Python Environment
- [`check_shap_available()`](https://cedricbouffard.github.io/rtabpfn/reference/check_shap_available.md)
  : Check if SHAP is available
- [`check_unsupervised_available()`](https://cedricbouffard.github.io/rtabpfn/reference/check_unsupervised_available.md)
  : Check if Unsupervised Extension is available
- [`validate_tabpfn_env()`](https://cedricbouffard.github.io/rtabpfn/reference/validate_tabpfn_env.md)
  : Validate TabPFN Python Environment

## Print Methods

Print methods for model objects

- [`print(`*`<tab_pfn>`*`)`](https://cedricbouffard.github.io/rtabpfn/reference/print.tab_pfn.md)
  : Print method for TabPFN model specification
- [`print(`*`<tab_pfn_unsupervised>`*`)`](https://cedricbouffard.github.io/rtabpfn/reference/print.tab_pfn_unsupervised.md)
  : Print method for TabPFN Unsupervised models
- [`print(`*`<tab_pfn_fit>`*`)`](https://cedricbouffard.github.io/rtabpfn/reference/print.tab_pfn_fit.md)
  : Print method for fitted TabPFN model
- [`print(`*`<shap_explanation>`*`)`](https://cedricbouffard.github.io/rtabpfn/reference/print.shap_explanation.md)
  : Print method for SHAP explanation

## tidymodels Integration

parsnip model specification

- [`tab_pfn()`](https://cedricbouffard.github.io/rtabpfn/reference/tab_pfn.md)
  [`update(`*`<tab_pfn>`*`)`](https://cedricbouffard.github.io/rtabpfn/reference/tab_pfn.md)
  : TabPFN Model for tidymodels
- [`set_engine.tab_pfn()`](https://cedricbouffard.github.io/rtabpfn/reference/set_engine.tab_pfn.md)
  : Set the model engine for TabPFN
