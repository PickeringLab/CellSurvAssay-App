CellSurvAssay-App
================

<img src="www/CellSurvAssay.png" width="200%" height="200%" />

## CellSurvAssay

CellSurvAssay consists of a couple of tools that can be used to perform
Clonogenic Survival Analysis in R very easily and efficiently. These two
tools are:

-   CellSurvAssay R package : This helps even beginner R users to
    perform the analysis in R, while maintaining the flexibility of a
    package. To know more details about the R package, visit
    [here](https://pickeringlab.github.io/CellSurvAssay/).

-   CellSurvAssay Shiny app : This is a web application that helps users
    with no experience in R to perform the analysis, in R. The app is
    based on the CellSurvAssay R package and can be accessed
    [here](https://pickeringlab.shinyapps.io/CellSurvAssay-App/).

## Purpose of the CellSurvAssay Shiny App

The CellSurvAssay Shiny web app uses the CellSurvAssay R package in the
background, which is built around the
[CFAssay](https://bioconductor.org/packages/release/bioc/html/CFAssay.html)
R package that can be used to perform Cell Survival Assay analysis in R.
However, the CellSurvAssay app has it’s own purposes and advantages:

-   it makes performing Clonogenic Survival Analysis in R incredibly
    user-friendly and efficient, even for users who have no experience
    in R and don’t have the luxury of time to learn it,

-   it arranges all the commonly used steps of clonogenic assay analysis
    in one location and automates the steps very similar to other
    available automated software,

-   it utilizes `ggplot()` to plot the cell suvival curves, and builds
    better quality figures than other available R packages,

-   it is less time consuming and more convenient for the user, as it
    accepts the raw data for the analysis, and calculates the plating
    efficiencies by itself,

-   it offers various method options for parameter estimation and
    calculating plating efficiencies, unlike most other available
    software tools, and

-   as R is being utilized, the methodology stays open and the results
    reproducible.
