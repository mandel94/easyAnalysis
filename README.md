# easyAnalysis
An easy-to-use interface for the analysis of survey responses


---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)
```

# easyAnalysis

<!-- badges: start -->
<!-- badges: end -->

## WHAT
The goal of easyAnalysis is to provide an interface for implementing different types of analysis on qualitative data. The focus is on making the analysis as ease as possible, by abstracting away any low-level implementation and giving the analyst visual control over the analysis pipeline.
## WHY 
One: code-free! easyAnalysis makes the analysis independent of the analysts' programming skills;
Two: 'intelligence' is king! easyAnalysis helps to focus on the 'intelligence' part of the problem, using advanced analysis techniques without the need to worry about complex technical implementations;
Three: try and retry! Lengthy scripts tend to hinder the analysis flexibility and reproducibility. Since data analysis is a process of trial and error, reproducibility of the pipeline is a fundamental driver for extracting good insights. The visual interface of easyAnalysis provides all the reproducibility you need to elevate your skills as an insight chaser!  

## Installation

You can install the released version of easyAnalysis from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("easyAnalysis")
```


## Rules to follow 

1 The identifier should be the first column of the imported data frame; 
2 The application only support csv files. 


## Glossary
 ### MCA: 
Multiple Correspondence Analysis is a technique of analysis widely used in a variety of research fields (typically in social and behavioral sciences), as a way to find associations in categorical data. A typical application is with survey responses, in which each feature corresponds to one survey question, each one admitting a limited set of responses (just 'yes/no' for binary questions). 
MCA allows summarizing most of the variation in data with a limited set of factors called dimensions (like PCA, it is a way for reducing the dimensionality of the problem at hand). Each dimension is influenced by a subset of the original variable pool and represents a hidden phenomenon, that manifests itself in the form of an empirical association between the variables belonging to that subset. 
In brief, MCA can be a useful tool if we want to understand how responses from a survey are associated, and to extract a relevant insight from that association. For example, MCA could find an association in the response Sex: female and the response Entertainment: theater, and so on.

### Active variable: 
  in the context of multiple correspondence analysis, a
  variable is said to be active when it influences the resulting biplot coordinates. Active variables are the "construction materials" of the MCA 
  factors. 

### Supplementary variables: 
  in the context of multiple correspondence analysis,
  a variable is said to be supplementary when it does not influence the 
  resulting biplot coordinates. We might wish to include a variable as supplementary when we want to study how that variable can be explained in 
  terms of the factors being extracted, without influencing those factors. 

## Example

This is a basic example which shows you how to solve a common problem:

```{r example}
library(easyAnalysis)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so:

```{r cars}
summary(cars)
```

You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

```{r pressure, echo = FALSE}
plot(pressure)
```

In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN.

