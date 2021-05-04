# A Test For MCAR

This is an R package that helps you conduct missing complete at random test specifically designed for multivariate mixted-type data, the test can also apply to multivariate continuous and multivariate categorical data. 

# Introduction

We propose a test for MCAR that is focus on realized MCAR assumption as opposed to always MCAR. The work is motivated by the need to address the prevalent issues in missing data, especially for the difficulties in diagnostics of the MCAR test for multivariate mixed-type data. Little(1988) proposed a method for testing continuous scale data and Fuchs(1982) test can be extended to test for multivariate categorical data. However, there is no existing methodology to test for multivariate mixed-type data. A manuscript with full discussions of the theoretical properties and testability was written and submitted. The production of this work is an extension of a summer internship with Dr. Qian Shi from Mayo Clinic in 2019.  

# Installation

``` r
# Install package "devtools" for further package installation on Github
install.packages("devtools", dependencies=TRUE)
library(devtools)

# Install TestForMCAR with vignette from GitHub
devtools::install_github("YZRCstatcodes2021/TestForMCAR", build_vignettes = TRUE)

```
