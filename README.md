# streamlineR
 
A package with a variety of functions made to streamline personal habits in R. 

## Installation

Package is installed using the devtools package:

```{r}
if (!require("devtools", quietly = TRUE))
    install.packages("devtools")

devtools::install_github("jrijn/streamlineR")
```

## Publish-ready theme

The function "publish" is a customized ggplot theme based on the "pubr" package. It also includes colorblind-safe color palettes.

It's super easy to use!

```{r}
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
geom_point() +
scale_color_manual(values = colorBlindGrey8) +
publish()
```

## Plotting chromatograms

To use the chromatogram functions, the package rawrr needs to be installed first:

```{r}
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("rawrr")

if (isFALSE(rawrr::.checkDllInMonoPath())){
  rawrr::installRawFileReaderDLLs()
}

rawrr::installRawrrExe()
```

After installing rawrr, plotting Orbitrap chromatograms is super easy:

```{r}
rawfile <- "C:/myfile.raw"
lc <- importChromatograms(rawfile)
plotChromatograms(lc)
```
