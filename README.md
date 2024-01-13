# trident

The *trident* package dedicates to dental microwear texture analysis, allowing to further analyze the results using a variety of methods. 
It includes functions to measure microwear texture, to transform the data, to rank variables, as well as a complete shiny app.

# Installation

1 - Check that you have R > 4.0 and a compatible IDE (such as Rstudio) installed

2 - Windows users: check that you have a matching version of RTools installed

3 - Open the "InstallTrident.Rproj"

4 - Install all the dependencies:

```{r}
install.packages(c("car", "DescTools", "doSNOW", "dplyr", "DT", "factoextra", "FactoMineR", "foreach", "ggpubr", "ggplot2", "MASS", "nortest", "parallel", "picante", "plyr", "shiny", "shinyjs", "shinyFiles", "snow", "stats", "stringr", "utils"))
```

5 - Install the package from the tar.gz file in the project:

```{r}
install.packages("trident_1.3.8.tar.gz", repos = NULL, type = "source")
```

# Getting started with the shiny app

You can start using the shiny app using the following line in your R IDE:

```{r}
trident::trident.app()
```
There is also an updated documentation on this repository: [trident/supplementaryMaterial_02_userManual_26july2023.pdf](supplementaryMaterial_02_userManual_26july2023.pdf)

# Getting help

You can report issues here: [https://github.com/nialsiG/trident/issues](https://github.com/nialsiG/trident/issues)

# Changelog

## Version 1.3.8

Initial release of *trident*
