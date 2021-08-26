# A Test For MCAR

This R package helps you conduct missing complete at random test specifically designed for multivariate mixed-type data, and the test can also apply to multivariate continuous and multivariate categorical data.

# Introduction

We propose a test for MCAR that is focused on realized MCAR assumption as opposed to always MCAR. The work is motivated by the need to address the prevalent issues in missing data, especially for the difficulties in diagnosing the MCAR test for multivariate mixed-type data. Little(1988) proposed a method for testing continuous scale data, and Fuchs's (1982) test can be extended to test for multivariate categorical data. However, there is no existing methodology to test for multivariate mixed-type data. A manuscript with full discussions of the theoretical properties and testability was written and submitted. The production of this work is an extension of a summer internship with Dr. Qian Shi from Mayo Clinic in 2019. A paper has been submitted for the proposed method.

# Installation

``` r
# Install package "devtools" for further package installation on Github
install.packages("devtools", dependencies=TRUE)
library(devtools)

# Install TestForMCAR with vignette from GitHub
devtools::install_github("YZRCstatcodes2021/TestForMCAR", build_vignettes = TRUE)

# Install complementary packages that used in the function MCAR.test
install.packages("R.utils", dependencies=TRUE)
install.packages("kSamples", dependencies=TRUE)

```

# MCAR.test Function

Usage:
``` r
MCAR.test(data, alpha=0.05, threshold.cat=12, var.type=NULL, warning=T)
``` 
Arguments:
``` r
data                    A dataframe of data matrix with missing data marked as NA

alpha                   Type-I-error rate for the hypothesis testing

threshold.cat           The threshold number that defines the cutoff between continuous and categorical. Default is 12, which means a variable 
                        with 12 or more levels is treated as continuous, and a variable with less than 12 levels is treated as categorical. 
                        threshold.cat will not work if var.type is specified. 

var.type                A vector of user-defined variable type. "1" denotes continuous and "2" denotes categorical. Default is NULL.

warning                 If warnings are given for chisq.test function, compute p-values with Monte Carlo simulation
``` 
Value:

Values were reported for the hypothesis testing results and unadjusted as well as adjusted p-values. Other multiple adjustment methods can be applied by the users
using unadjusted p-values.
``` r
var.type                "1" represents continuous data and "2" represents categorical data

anova.BH                ANOVA-type test statistic with Benjamini-Hochberg procedure for multiple adjustments

anova.Storey_alpha      ANOVA-type test statistic with Storey(2004) for multiple adjustments where the tunning parameter is set to the type-I-error rate

anova.Storey_bootstrap  ANOVA-type test statistic with Storey(2004) for multiple adjustments where the tunning parameter is choosen using bootstrap

AD.BH                   Multi-sample Anderson-Darling test statistic with Benjamini-Hochberg procedure for multiple adjustments

AD.Storey_alpha         Multi-sample Anderson-Darling test statistic with Storey(2004) for multiple adjustments where the tunning parameter is set to the type-I-error rate

AD.Storey_bootstrap     Multi-sample Anderson-Darling test statistic with Storey(2004) for multiple adjustments where the tunning parameter is choosen using bootstrap
``` 
# Examples

``` r
set.seed(321)

test.data = iris
test.data[sample(1:150,10),1] = NA
test.data[sample(1:150,10),3] = NA
test.data[sample(1:150,5),5] = NA

MCAR.test(test.data)
$var.type
[1] 1 1 1 1 2

$anova.BH
[1] "Do not reject the null"

$anova.Storey_alpha
[1] "Do not reject the null"

$anova.Storey_bootstrap
[1] "Do not reject the null"

$AD.BH
[1] "Do not reject the null"

$AD.Storey_alpha
[1] "Do not reject the null"

$AD.Storey_bootstrap
[1] "Do not reject the null"

$anova_unadjusted.pvalue
[1] 0.5396284 0.4704782 0.8694131 0.6462750 0.3196803

$AD_unadjusted.pvalue
[1] 0.5832500 0.4273800 0.9572500 0.3586200 0.3196803

$anova.BH.pvalue
[1] 0.8078438 0.8078438 0.8694131 0.8078438 0.8078438

$anova.Storey_alpha.pvalue
[1] 0.8078438 0.8078438 0.8694131 0.8078438 0.8078438

$anova.Storey_bootstrap.pvalue
[1] 0.8078438 0.8078438 0.8694131 0.8078438 0.8078438

$AD.BH.pvalue
[1] 0.7290625 0.7123000 0.9572500 0.7123000 0.7123000

$AD.Storey_alpha.pvalue
[1] 0.7290625 0.7123000 0.9572500 0.7123000 0.7123000

$AD.Storey_bootstrap.pvalue
[1] 0.7290625 0.7123000 0.9572500 0.7123000 0.7123000
``` 
