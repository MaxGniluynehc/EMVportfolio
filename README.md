# EMVportfolio

This R package solves the Exploratory Mean-Variance (EMV) portfolio optimization problem. 

## Package Architecture
All the functions of this package are written in R and stored in folder [R](https://github.com/MaxGniluynehc/EMVportfolio/tree/main/R), in particular, 
* [act_EMV_policy](https://github.com/MaxGniluynehc/EMVportfolio/blob/main/R/act_EMV_policy.R) and [act_MLE_policy](https://github.com/MaxGniluynehc/EMVportfolio/blob/main/R/act_MLE_policy.R) make investment decisions (i.e., take actions) following the MLE-based or RL-based trading policy, given the market scenario.  
* [getStockPath](https://github.com/MaxGniluynehc/EMVportfolio/blob/main/R/getStockPath.R) simulates the stock price paths from a Geometric Brownian Motion. 
* [getTerminalWealth](https://github.com/MaxGniluynehc/EMVportfolio/blob/main/R/getTerminalWealth.R) make the investments and compute the terminal wealth, following different trading policies.
* [show_terminalwealth](https://github.com/MaxGniluynehc/EMVportfolio/blob/main/R/show_terminalwealth.R) visualizes the trading performance of different polices, via plots and tables.
* [trainRLalg](https://github.com/MaxGniluynehc/EMVportfolio/blob/main/R/trainRLalg.R) trains the RL algorithm for the EMV trading policy. It can also visualize the training process via plots.

## Final Report
* [Final report](https://github.com/MaxGniluynehc/EMVportfolio/blob/main/STAT900_final_project_YulingMaxChen.pdf) This is the final report submitted to the STAT900 committee for evaluation. Any unclear definition, problem setup and simulation details in the Pacakge documentation can be found in the final report. 

## Installation 
To install this package, run the following in R: 
```{r}
devtools::install_github("MaxGniluynehc/EMVportfolio", ref="main", build_vignettes = TRUE, force = TRUE)
```

For more concrete examples and instructions of using this package, please refer to the vignette by running `vignette("EMVporfolio-handbook")` after installation.  

