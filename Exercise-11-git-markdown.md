Exercise-11-git-markdown
================
MM
2024-12-20

## Downloading packages

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(dplyr)
library(broom)
library(haven)
x<-read_sav("data/ESS9e03_2.sav")
```

## Having a look at the dataset

``` r
head(x)
```

    ## # A tibble: 6 × 572
    ##   name      essround edition proddate    idno cntry      dweight pspwght pweight
    ##   <chr>        <dbl> <chr>   <chr>      <dbl> <chr+lbl>    <dbl>   <dbl>   <dbl>
    ## 1 ESS9e03_2        9 3.2     23.11.2023    27 AT [Austr…   0.581   0.218   0.302
    ## 2 ESS9e03_2        9 3.2     23.11.2023   137 AT [Austr…   1.06    0.413   0.302
    ## 3 ESS9e03_2        9 3.2     23.11.2023   194 AT [Austr…   1.38    2.27    0.302
    ## 4 ESS9e03_2        9 3.2     23.11.2023   208 AT [Austr…   0.993   0.386   0.302
    ## 5 ESS9e03_2        9 3.2     23.11.2023   220 AT [Austr…   0.377   1.03    0.302
    ## 6 ESS9e03_2        9 3.2     23.11.2023   254 AT [Austr…   1.48    0.576   0.302
    ## # ℹ 563 more variables: anweight <dbl>, nwspol <dbl+lbl>, netusoft <dbl+lbl>,
    ## #   netustm <dbl+lbl>, ppltrst <dbl+lbl>, pplfair <dbl+lbl>, pplhlp <dbl+lbl>,
    ## #   polintr <dbl+lbl>, psppsgva <dbl+lbl>, actrolga <dbl+lbl>,
    ## #   psppipla <dbl+lbl>, cptppola <dbl+lbl>, trstprl <dbl+lbl>,
    ## #   trstlgl <dbl+lbl>, trstplc <dbl+lbl>, trstplt <dbl+lbl>, trstprt <dbl+lbl>,
    ## #   trstep <dbl+lbl>, trstun <dbl+lbl>, vote <dbl+lbl>, prtvtcat <dbl+lbl>,
    ## #   prtvtdbe <dbl+lbl>, prtvtdbg <dbl+lbl>, prtvtgch <dbl+lbl>, …

## Including Plots

You can also embed plots, for example:

![](Exercise-11-git-markdown_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
