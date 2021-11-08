
<!-- README.md is generated from README.Rmd. Please edit that file -->

# easyAnalysis

<!-- badges: start -->

<!-- badges: end -->

## WHAT?

The goal of easyAnalysis is to provide an interface for implementing
different types of analysis on qualitative data. The focus is on making
the analysis as ease as possible, by abstracting away any low-level
implementation and giving the analyst visual control over the analysis
pipeline. \#\# WHY? One: code-free\! easyAnalysis makes the analysis
independent of the analysts’ programming skills; Two: ‘intelligence’ is
king\! easyAnalysis helps to focus on the ‘intelligence’ part of the
problem, using advanced analysis techniques without the need to worry
about complex technical implementations; Three: try and retry\! Lengthy
scripts tend to hinder the analysis flexibility and reproducibility.
Since data analysis is a process of trial and error, reproducibility of
the pipeline is a fundamental driver for extracting good insights. The
visual interface of easyAnalysis provides all the reproducibility you
need to elevate your skills as an insight chaser\!

## Installation

You can install the released version of easyAnalysis from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("easyAnalysis")
```

## Things you should know

  - The **identifier** should be the **first column** of the imported
    data frame;
  - easyAnalysis supports only **.csv files**;
  - As for the data, easyAnalysis currently supports two types of data,
    coherently to the only type of analysis it can perform (MCA):
      - binary data (in the ‘1/0’ or ‘yes/no’ formats);
      - multi-categorical data.

## How to use easyAnalysis

Let’s explore the different sections of the user interface.

## Import

### **Select CSV file**

here you can browse your local system for a **CSV file** to load.

### Import

Click here to import into the application the CSV file you previously
uploaded.

### Select variables

Once the data has been imported, the list of variables will be available
here. Select all the variables you are interested in.

### Include selected variables

Click here and a table with imported data for the variables you selected
will display on the left side of the screen.

### Clear

Click here to clear the currently displayed table (only the ID\_CODE
will display)

### Remove rows with NA’s

Check this box if you want to remove all rows with at least one missing
value. This is a pretty brutal approach, as you are likely to lose a lot
of usable information. We recommend that you consider imputation methods
for filling missing values (easyAnalysis does not currently support
imputation methods in its interface).

# Glossary

## MCA: Multiple Correspondence Analysis is a technique of analysis widely used in a variety of research fields (typically in social and behavioral sciences), as a way to find associations in categorical data.

A typical application is with survey responses, in which each feature
corresponds to one survey question, each one admitting a limited set of
responses (just ‘yes/no’ for binary questions). It allows summarizing
most of the variation in data with a limited set of factors called
dimensions (like PCA, it is a way for reducing the dimensionality of the
problem at hand). Each dimension is influenced by a subset of the
original variable pool and represents a hidden phenomenon, that
manifests itself in the form of an empirical association between the
variables belonging to that subset. In brief, MCA can be a useful tool
if we want to understand how responses from a survey are associated, and
to extract a relevant insight from that association. For example, MCA
could find an association in the response Sex: female and the response
Entertainment: theater, and so on.

## Active variable:

in the context of multiple correspondence analysis, a variable is said
to be active when it influences the resulting biplot coordinates. Active
variables are the “construction materials” of the MCA factors.

## Supplementary variables:

in the context of multiple correspondence analysis, a variable is said
to be supplementary when it does not influence the resulting biplot
coordinates. We might wish to include a variable as supplementary when
we want to study how that variable can be explained in terms of the
factors being extracted, without influencing those factors.

## Cos2

In the context of MCA, cos2 is a value in the interval \[0, 1\]
associated with a given entity (a variable, or an individual), that
measures how important a particular dimension is for that entity. Each
entity is assigned multiple measures of cos2, one for each dimension
extracted from the MCA. Cos2 can be useful for interpreting the
dimensions of an MCA. In general, if variables are the entities, we can
interpret a dimension by looking at which entities have the strongest
cos2.

## Example

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.
