# Pacman Behavioral Analyses

Analysis scripts (mostly in R) for the analysis of the pacman dataset. While [pacman_ieeg](https://github.com/bstavel/pacman_ieeg) has the python scripts for analyses that are (nearly) only brain data, these analyses include both pure behavioral analyses and both brain/behavior analyses.

## Pilot data behavior and validation

Basic analyses showing normative behavior in the pilot sample and validation that the timing and online task works as expected. See `normative behavior` files and `analysis/task_validation_sanity_checks/`

## Joint Modeling of Longitudinal and Survival Data

Method to dynamically predict avoidance behavior in the pacman data using continuous predictors-- including possible brain measures. See `analysis/survival/README.md` for details

## Frequency Encoding Analyses

Uses task behavior to predict neural responses via linear mixed effects models. See `analysis/frequency_analyses/README.md` for details

## Connectivity Analyses

Implements trial-by-trial theta correlations between different electrode pairs and compares to a null distribution. See `analysis/connectivity/README.md` for details

## Clinical Analyses

Analyses the prolific clinical sample recruited in Fall 2023. See `analysis/clinical/README.md` for details
