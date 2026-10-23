This is Frederik's personal package

Installation in R:
```{r}
devtools::install_github("f-bkr/fredverse")
```

<ins>Have a look at my Wrapper function for EffectLiteR and GORIC(A):</ins>
```{r}
set.seed(128)
  testdata <- data.from.causality.table()
  elrmod.test <- EffectLiteR::effectLite(y="Y", x="X", data=testdata)
  constrained.inference(elr.object = elrmod.test, estimand = c("adjmeans"))
```
