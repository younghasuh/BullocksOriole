Main code
================
6/18/2021

-   [Load libraries](#load-libraries)
-   [Import data](#import-data)
    -   [Attribute data](#attribute-data)
    -   [Spectra data](#spectra-data)
    -   [Visually check spectra](#visually-check-spectra)
        -   [Fix odd peaks](#fix-odd-peaks)
    -   [Average spectra](#average-spectra)
    -   [Smooth spectra](#smooth-spectra)
-   [Split orange and black patches](#split-orange-and-black-patches)
    -   [Extract orange patches](#extract-orange-patches)
    -   [Extract black patches](#extract-black-patches)
-   [Colorimetric variables](#colorimetric-variables)
    -   [Compare boxplots across specimen
        categories](#compare-boxplots-across-specimen-categories)
    -   [Check data](#check-data)
-   [Statistical analyses](#statistical-analyses)
    -   [H1. Have specimens faded over
        time?](#h1-have-specimens-faded-over-time)
        -   [1. Orange patches over time](#1-orange-patches-over-time)
        -   [2. Black patches over time](#2-black-patches-over-time)
    -   [H2. Have landuse changes affected Bullock’s
        orioles?](#h2-have-landuse-changes-affected-bullocks-orioles)
    -   [H3. Are color changes only in the hybrid
        zone?](#h3-are-color-changes-only-in-the-hybrid-zone)
-   [Avian visual models](#avian-visual-models)
    -   [Noise-weighted distances with the Receptor Noise
        Model](#noise-weighted-distances-with-the-receptor-noise-model)

The following analyses is conducted on spectra data for 119 specimens.
Sample sizes for each specimen category compose of the following:

| Bullock’s oriole |        |           | Baltimore oriole |        |
|------------------|--------|-----------|------------------|--------|
| Historic         | Modern | Reference | Historic         | Modern |
| 20               | 20     | 40        | 20               | 19     |

Bullock’s reference specimens come from the Museum of Vertebrate Zoology
(MVZ; n = 20) and the University of Washington Burke Museum (UWBM; n =
20), all from CA, NE, WA, OR, AZ.

**Set up**: integration time, 100 ms; 10 readings averaged per
recording; boxcar width 10

Each specimen has 5 patches measured (2 black 3 orange) and each patch
is measured 3 times; i.e. 15 measurements per specimen.

<figure>
<img src="pics.png" style="width:30.0%" alt="Fig 1. Patches measured on specimens." /><figcaption aria-hidden="true"><strong>Fig 1</strong>. Patches measured on specimens.</figcaption>
</figure>

<br>

### Load libraries

Using `pavo` as main, rest as data manipulation & organization

``` r
library(pavo)
library(data.table)
library(readxl)
library(tidyverse)
library(stringr)
library(knitr)
library(car)
library(ggpubr)
library(ggpmisc)
library(lme4)
library(cowplot)
```

Set ggplot themes for figures

``` r
mytheme <- theme(
  plot.title = element_text(size=20,face="bold",color="black"),      
  axis.text=element_text(size=16, color="black"),
  axis.title=element_text(size=18,face="bold",color="black"),
  axis.text.x=element_text(size=16, color="black"), 
  axis.text.y=element_text(size=16, color="black"),
  legend.text = element_text(size=16, color="black"),
  legend.title = element_text(size=16, color="black", face="bold"))

pal1 <- c("#ffa600", "#58508d")
pal2 <- c("#58508d", "#ffa600")
pal3 <- c("#635e50", "#e59500")
```

# Import data

## Attribute data

Load `specimen_info.xls` & filter out necessary info

``` r
attributes <- read_excel("oriole_specimen_info.xlsx")
knitr::kable(head(attributes))
```

| ID      | sp\_id          | cat        | Cat Num | Species | Code             | CollectionDate | State | Higher Geography                     | Specific Locality                                  |
|:--------|:----------------|:-----------|--------:|:--------|:-----------------|:---------------|:------|:-------------------------------------|:---------------------------------------------------|
| BA28299 | Balt\_Historic1 | Balt\_hist |       1 | 28299   | Baltimore Oriole | 1957-06-12     | NE    | United States, Nebraska, Polk County | Platte River, 3 miles South-Southwest Silver Creek |
| BA28300 | Balt\_Historic2 | Balt\_hist |       2 | 28300   | Baltimore Oriole | 1957-06-12     | NE    | United States, Nebraska, Polk County | Platte River, 3 miles South-Southwest Silver Creek |
| BA28301 | Balt\_Historic3 | Balt\_hist |       3 | 28301   | Baltimore Oriole | 1957-06-12     | NE    | United States, Nebraska, Polk County | Platte River, 3 miles South-Southwest Silver Creek |
| BA28302 | Balt\_Historic4 | Balt\_hist |       4 | 28302   | Baltimore Oriole | 1957-06-13     | NE    | United States, Nebraska, Polk County | Platte River, 3 miles South-Southwest Silver Creek |
| BA28304 | Balt\_Historic5 | Balt\_hist |       5 | 28304   | Baltimore Oriole | 1957-06-13     | NE    | United States, Nebraska, Polk County | Platte River, 3 miles South-Southwest Silver Creek |
| BA28305 | Balt\_Historic6 | Balt\_hist |       6 | 28305   | Baltimore Oriole | 1957-06-13     | NE    | United States, Nebraska, Polk County | Platte River, 3 miles South-Southwest Silver Creek |

``` r
attributes$date <- as.Date(attributes$CollectionDate, origin = "1899-12-30")

att <- attributes[,c("ID", "sp_id", "cat", "date", "Code")]
att$cat <- as.factor(att$cat)

# Just IDs and category
idcat <- att[,c("ID", "cat")]
```

<br>

## Spectra data

Finds and imports spectra files from a folder `allspec` and `burke`.
Contains replicated data for one missing measurement.

``` r
spec <- getspec("allspec", ext = "txt", lim = c(300, 700))  # 1485 files
spec <- as.rspec(spec)

burke <- getspec("burke", ext = "txt", lim = c(300, 700)) #300 files
burke_raw <- getspec("burke", ext = "txt", lim = c(300, 700)) #300 files
```

## Visually check spectra

``` r
plot(spec)
```

![**Fig 2a.** Spectra of specimens measured in
2019.](mainscript_files/figure-gfm/See%20other%20spectra-1.png)

Check spectra of Burke specimens

``` r
plot(burke)

plot(burke_raw)
```

![**Fig 2b.** Spectra of UWBM specimens measured in
2021.](mainscript_files/figure-gfm/See%20burke%20spectra-1.png)

``` r
png("burke_raw_spec.png", width = 800, height = 800, pointsize = 22, res = 100, bg = "transparent")
plot(burke_raw, col = spec2rgb(burke_raw), cex.lab = 1.5, cex.axis = 1.5)
dev.off()
```

    ## png 
    ##   2

### Fix odd peaks

Measurements for the Burke museum specimen have two peaks in the spectra
that occur for unknown reasons. They were replaced by deleting
reflectance at those wavelengths and interpolating the values from
before/after. The areas of concern seem to be 470-500 nm and 510-560 nm.

1.  Replace odd wavelengths with `NA`
2.  Use `as.rspec` to interpolate gaps

``` r
wl <- burke$wl

# replace with NA
burke$wl <- as.numeric(burke$wl)

burke[which(burke$wl > 470 & burke$wl < 500),] <- NA
burke[which(burke$wl > 515 & burke$wl < 560),] <- NA

burke[,1] <- wl # feed back in because it gets deleted

# Interpolate
burke <- as.rspec(burke)

# join 
allspec <- merge(spec, burke)
```

3.  Check new spectra

``` r
plot(burke)
```

![**Fig 2c.** Spectra of UWBM
specimens](mainscript_files/figure-gfm/See%20new%20burke%20spectra-1.png)

``` r
png("burke_corrected_spec.png", width = 800, height = 800, pointsize = 22, res = 100, bg = "transparent")
plot(burke, col = spec2rgb(burke_raw), cex.lab = 1.5, cex.axis = 1.5)
dev.off()
```

    ## png 
    ##   2

<br>

## Average spectra

Modify names for future processing.

## Smooth spectra

Suitable soothing parameter `span = 0.14` is the minimum amount of
smoothing to remove spectral noise.

``` r
# check which smoothing parameter to use
plotsmooth(m.allspec, minsmooth = 0.05, maxsmooth = 0.5, curves = 6, ask = FALSE, specnum = 6)
```

![**Fig 3.** Check smoothing
parameters](mainscript_files/figure-gfm/Smooth%20spectra-1.png)

``` r
# smooth spectra based on that
allspec.sm <- procspec(m.allspec, opt = "smooth", span = 0.14, fixneg = "zero") 
```

``` r
# spectra in their colors
plot(allspec.sm, col = spec2rgb(allspec.sm), cex.lab = 1.5, cex.axis = 1.5)
```

![**Fig 4.** Spectra based on their
color](mainscript_files/figure-gfm/All%20spectra%20in%20color-1.png)

<br>

# Split orange and black patches

### Extract orange patches

Subset then extract

``` r
allspec.orn <- subset(allspec.sm, "orn") #n=343
# average 3 measurements
allspec.orn.avg <- aggspec(allspec.orn, by = 3, FUN = mean, trim = FALSE) 
```

Get spetra

``` r
# get names from list
allspecimens <- names(allspec.orn.avg)
allds <- as.data.frame(allspecimens)

# change names to match
allds$ID <- gsub("MVZ_", "MVZ", allds$allspecimens)
idcat$ID2 <- gsub("MVZ.", "MVZ", idcat$ID)
idcat$ID2 <- gsub("UWBM.", "UWBM", idcat$ID2)
idcat$ID2 <- gsub("UWBMBU", "UWBM", idcat$ID2)


allds$ID2 <- gsub("\\_.*", "", allds$ID) #extract values before _
allds$patch <- gsub(".*_", "", allds$allspecimens) #extract values after _

idcat <- rbind(idcat, data.frame(ID = "wl", cat= "wl", ID2 = "wl")) # to match

# Check if specimen numbers match
# n_distinct(allds$ID2) #n=120
# n_distinct(ct$ID2) #n=120

# Create a new column with specimen type and patch
allds2 <- allds %>% 
  left_join(idcat, by = "ID2", keep=FALSE) 

# Create separate string for category list
allcategory <- allds2$cat
```

Plot average spectra

``` r
# plot by specimen
asoa <- allspec.orn.avg
colnames(asoa) <- allcategory

aggplot(asoa, by = allcategory, FUN.center = median, ylim = c(0, 65),
        alpha = 0.3, legend = F, cex.lab = 1.5, cex.axis = 1.5, lwd=2)
legend(290, 67, legend = c("BA historic", "BA modern", "BU historic", "BU modern", "BU reference"),
       col = c("red", "royalblue", "green", "purple", "orange", "yellow"), lty=1, cex=1.2,
       box.lty=0, lwd=2, bg="transparent")
```

![**Fig 5.** Spectra based on their specimen
category](mainscript_files/figure-gfm/Orange%20patches%20-%20plot%20average%20spectra%20by%20specimen-1.png)

Get Colorimetric variables

``` r
# use summary function
allsum.orn <- summary(allspec.orn.avg)
setDT(allsum.orn, keep.rownames = TRUE)[]

# convert MVZ names to be consistent with the specimen attribute data
allsum.orn$rn <- gsub("MVZ_", "MVZ.", allsum.orn$rn)
allid.orn <- do.call(rbind, strsplit(allsum.orn$rn, "\\_"))[, 1]
allsum.orn$ID <- allid.orn #specimen ID

# change UWBM to be consistent with the attribute data table
allsum.orn$ID <- gsub("UWBM", "UWBM.BU", allsum.orn$ID)

# Join spectra summary data with attribute data
alldat.orn <- allsum.orn %>%
  left_join(att, by = "ID", keep = F)

# Make just a species category
alldat.orn$sp <- gsub("\\_.*", "", alldat.orn$cat)

# Rename species category for future plotting
alldat.orn$spc <- factor(alldat.orn$sp, 
                         levels = c("Bull", "Balt"), 
                         labels = c("Bullock's", "Baltimore"))
```

#### Spectra split by species

``` r
# split by species
asoa.ba <- subset(asoa, "Balt")
asoa.bu <- subset(asoa, "Bull")

#remove bull-ref for now
asoa.bu2 = select(asoa.bu, 1:41)


allcat <- as.data.frame(allcategory)
allcat2 <- allcat %>% 
  filter(allcategory =="wl" | allcategory == "Balt_hist" | allcategory == "Balt_mod")
colnames(asoa.ba) <- allcat2$allcategory

allcat3 <- allcat %>% 
  filter(allcategory =="wl" | allcategory == "Bull_hist" | allcategory == "Bull_mod")
colnames(asoa.bu2) <- allcat3$allcategory


# plot & save
png("all_spectra_bullocks.png", width = 800, height = 800, pointsize = 22, res = 100, bg = "transparent")
aggplot(asoa.bu2, by = allcat3$allcategory, FUN.center = median, ylim = c(0, 65),
        alpha = 0.3, legend = F, cex.lab = 1.5, cex.axis = 1.5, lwd=2)
legend(290, 67, legend = c("Historic", "Contemporary"), col = c("red", "royalblue"), 
       lty=1, cex=1.2, box.lty=0, lwd=2, bg="transparent", title = "Bullock's oriole")
dev.off()
```

    ## png 
    ##   2

``` r
png("all_spectra_baltimore.png", width = 800, height = 800, pointsize = 22, res = 100, bg = "transparent")
aggplot(asoa.ba, by = allcat2$allcategory, FUN.center = median, ylim = c(0, 65),
        alpha = 0.3, legend = F, cex.lab = 1.5, cex.axis = 1.5, lwd=2)
legend(290, 67, legend = c("Historic", "Contemporary"), col = c("red", "royalblue"), 
       lty=1, cex=1.2, box.lty=0, lwd=2, bg="transparent", title = "Baltimore oriole")
dev.off()
```

    ## png 
    ##   2

### Extract black patches

Repeat steps above

``` r
allspec.blk <- subset(allspec.sm, "blk")

# average 3 measurements
allspec.blk.avg <- aggspec(allspec.blk, by = 2, FUN = mean, trim = FALSE) 

# Get colorimetric values
allsum.blk <- summary(allspec.blk.avg)
setDT(allsum.blk, keep.rownames = TRUE)[]

# convert MVZ and UWBM names to be consistent with the specimen attribute data
allsum.blk$rn <- gsub("MVZ_", "MVZ.", allsum.blk$rn)
allid.blk <- do.call(rbind, strsplit(allsum.blk$rn, "\\_"))[, 1]
allsum.blk$ID <- allid.blk #specimen ID
allsum.blk$ID <- gsub("UWBM", "UWBM.BU", allsum.blk$ID)


# Join spectra summary data with attribute data
alldat.blk <- allsum.blk %>%
  left_join(att, by = "ID", keep = F)

# Make just a species category
alldat.blk$sp <- gsub("\\_.*", "", alldat.blk$cat)

# Rename species category for future plotting
alldat.blk$spc <- factor(alldat.blk$sp, levels = c("Bull", "Balt"), labels = c("Bullock's", "Baltimore"))
```

<br>

# Colorimetric variables

List of variables of interest (from Pavo package; references available
there) - `B1`: total brightness; sum of relative reflectance over entire
spectral range (area under the curve) - `S9`: carotenoid chroma; (R700 -
R450)/R700 - `H3`: hue/wavelength at Rmid; sensitive to noisy spectra

### Compare boxplots across specimen categories

Orange patches

``` r
dat <- alldat.orn
plot(B1 ~ cat, data = dat, ylab = "Total brightness (B1)", xlab = "Specimen category")
```

![](mainscript_files/figure-gfm/Boxplot%20across%20specimens%20-%20orange-1.png)<!-- -->

``` r
plot(S9 ~ cat, data = dat, ylab = "Carotenoid chroma (S9)", xlab = "Specimen category")
```

![](mainscript_files/figure-gfm/Boxplot%20across%20specimens%20-%20orange-2.png)<!-- -->

``` r
plot(H3 ~ cat, data = dat, ylab = "Hue (H3)", xlab = "Specimen category")
```

![](mainscript_files/figure-gfm/Boxplot%20across%20specimens%20-%20orange-3.png)<!-- -->

Black patchess

``` r
dat <- alldat.blk
plot(B1 ~ cat, data = dat, ylab = "Total brightness (B1)", xlab = "Specimen category")
```

![](mainscript_files/figure-gfm/Boxplot%20across%20specimens%20-%20black-1.png)<!-- -->

### Check data

Histograms

``` r
# Oranges
hist(alldat.orn$B1)
```

![](mainscript_files/figure-gfm/Histograms-1.png)<!-- -->

``` r
hist(alldat.orn$B2)
```

![](mainscript_files/figure-gfm/Histograms-2.png)<!-- -->

``` r
hist(alldat.orn$S9)
```

![](mainscript_files/figure-gfm/Histograms-3.png)<!-- -->

``` r
hist(alldat.orn$H3)
```

![](mainscript_files/figure-gfm/Histograms-4.png)<!-- -->

``` r
# Black
hist(alldat.blk$B1)
```

![](mainscript_files/figure-gfm/Histograms-5.png)<!-- -->

Normality

``` r
# Oranges
qqPlot(alldat.orn$B1)
```

![](mainscript_files/figure-gfm/qqPlots-1.png)<!-- -->

    ## [1] 94 95

``` r
qqPlot(alldat.orn$B2)
```

![](mainscript_files/figure-gfm/qqPlots-2.png)<!-- -->

    ## [1] 94 95

``` r
qqPlot(alldat.orn$S9)
```

![](mainscript_files/figure-gfm/qqPlots-3.png)<!-- -->

    ## [1] 116  98

``` r
qqPlot(alldat.orn$H3)
```

![](mainscript_files/figure-gfm/qqPlots-4.png)<!-- -->

    ## [1] 50 43

``` r
# Black
qqPlot(alldat.blk$B1)
```

![](mainscript_files/figure-gfm/qqPlots-5.png)<!-- -->

    ## [1] 37 27

<br>

# Statistical analyses

## H1. Have specimens faded over time?

To test for specimen fauxing, look at colorimetric measures over time.

### 1. Orange patches over time

Check brightness, carotenoid chroma, hue

1.  Brightness

``` r
# Both species
lm_orn_both_b1 <- lm(B1 ~ date, data = alldat.orn)
summary(lm_orn_both_b1)
```

    ## 
    ## Call:
    ## lm(formula = B1 ~ date, data = alldat.orn)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2208.7  -423.0   111.5   558.7  1823.2 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 9.786e+03  7.279e+01  134.45   <2e-16 ***
    ## date        2.864e-03  5.620e-03    0.51    0.611    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 763.6 on 117 degrees of freedom
    ## Multiple R-squared:  0.002214,   Adjusted R-squared:  -0.006314 
    ## F-statistic: 0.2596 on 1 and 117 DF,  p-value: 0.6113

``` r
# Bullock's
lm_orn_BU_b1 <- lm(B1 ~ date, data = alldat.orn[which(alldat.orn$sp == "Bull"),])
summary(lm_orn_BU_b1)
```

    ## 
    ## Call:
    ## lm(formula = B1 ~ date, data = alldat.orn[which(alldat.orn$sp == 
    ##     "Bull"), ])
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2057.20  -515.73    87.37   565.12  1945.41 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 9.676e+03  9.139e+01 105.874   <2e-16 ***
    ## date        5.624e-03  6.931e-03   0.812     0.42    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 804.4 on 78 degrees of freedom
    ## Multiple R-squared:  0.008372,   Adjusted R-squared:  -0.004341 
    ## F-statistic: 0.6585 on 1 and 78 DF,  p-value: 0.4195

``` r
# Baltimore
lm_orn_BA_b1 <- lm(B1 ~ date, data = alldat.orn[which(alldat.orn$sp == "Balt"),])
summary(lm_orn_BA_b1)
```

    ## 
    ## Call:
    ## lm(formula = B1 ~ date, data = alldat.orn[which(alldat.orn$sp == 
    ##     "Balt"), ])
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1399.98  -304.34    99.23   363.49  1162.44 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.009e+04  1.126e+02   89.56   <2e-16 ***
    ## date        -1.176e-02  9.044e-03   -1.30    0.201    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 615.6 on 37 degrees of freedom
    ## Multiple R-squared:  0.04371,    Adjusted R-squared:  0.01786 
    ## F-statistic: 1.691 on 1 and 37 DF,  p-value: 0.2015

``` r
# Plot both
ggplot(alldat.orn, aes(x = date, y = B1, shape = spc, color = spc)) +
  geom_point(size = 2) +
  stat_smooth(method = lm, aes(fill = spc), se = T) +
  theme_classic() +
  mytheme +
  stat_fit_glance(method = "lm", label.x = "right", label.y = "bottom",
                        method.args = list(formula = y ~ x), size = 5, 
                        aes(label = sprintf('R^2~"="~%.3f~~italic(p)~"="~%.3f',
                                            stat(..r.squared..), stat(..p.value..))), 
                  parse = TRUE) + 
  labs(title = "Orange patches only", y = "Total brightness (B1)", x = "Collection date", 
       color = "Species", fill = "Species", shape = "Species") +
  scale_color_manual(values = pal1) +
  scale_fill_manual(values = pal1)
```

![](mainscript_files/figure-gfm/Orange%20-%20brightness%20x%20date-1.png)<!-- -->

2.  Carotenoid chroma

``` r
# Both species
lm_orn_both_s9 <- lm(S9 ~ date, data = alldat.orn)
summary(lm_orn_both_s9)
```

    ## 
    ## Call:
    ## lm(formula = S9 ~ date, data = alldat.orn)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.050172 -0.005326  0.002971  0.009081  0.023683 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 9.354e-01  1.319e-03 709.333   <2e-16 ***
    ## date        2.244e-07  1.018e-07   2.204   0.0295 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.01383 on 117 degrees of freedom
    ## Multiple R-squared:  0.03987,    Adjusted R-squared:  0.03166 
    ## F-statistic: 4.858 on 1 and 117 DF,  p-value: 0.02947

``` r
# Bullock's
lm_orn_BU_s9 <- lm(S9 ~ date, data = alldat.orn[which(alldat.orn$sp == "Bull"),])
summary(lm_orn_BU_s9)
```

    ## 
    ## Call:
    ## lm(formula = S9 ~ date, data = alldat.orn[which(alldat.orn$sp == 
    ##     "Bull"), ])
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.048236 -0.008618  0.003119  0.010649  0.025134 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 9.340e-01  1.647e-03 566.984   <2e-16 ***
    ## date        1.834e-07  1.249e-07   1.468    0.146    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0145 on 78 degrees of freedom
    ## Multiple R-squared:  0.0269, Adjusted R-squared:  0.01442 
    ## F-statistic: 2.156 on 1 and 78 DF,  p-value: 0.146

``` r
# Baltimore
lm_orn_BA_s9 <- lm(S9 ~ date, data = alldat.orn[which(alldat.orn$sp == "Balt"),])
summary(lm_orn_BA_s9)
```

    ## 
    ## Call:
    ## lm(formula = S9 ~ date, data = alldat.orn[which(alldat.orn$sp == 
    ##     "Balt"), ])
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.040891 -0.004822  0.002226  0.006510  0.020737 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 9.385e-01  2.208e-03 425.044   <2e-16 ***
    ## date        2.455e-07  1.773e-07   1.385    0.174    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.01207 on 37 degrees of freedom
    ## Multiple R-squared:  0.04929,    Adjusted R-squared:  0.02359 
    ## F-statistic: 1.918 on 1 and 37 DF,  p-value: 0.1743

``` r
# Plot both
ggplot(alldat.orn, aes(x = date, y = S9, shape = spc, color = spc)) +
  geom_point(size = 2) +
  stat_smooth(method = lm, aes(fill = spc), se = T) +
  theme_classic() +
  mytheme +
  stat_fit_glance(method = "lm", label.x = "right", label.y = "bottom",
                        method.args = list(formula = y ~ x), size = 5, 
                        aes(label = sprintf('R^2~"="~%.3f~~italic(p)~"="~%.3f',
                                            stat(..r.squared..), stat(..p.value..))), 
                  parse = TRUE) + 
  labs(title = "Orange patches only", y = "Carotenoid chroma (S9)", x = "Collection date", 
       color = "Species", fill = "Species", shape = "Species") +
  scale_color_manual(values = pal1) +
  scale_fill_manual(values = pal1)
```

![](mainscript_files/figure-gfm/Orange%20-%20chroma%20x%20date-1.png)<!-- -->

3.  Hue

``` r
# Both species
lm_orn_both_h3 <- lm(H3 ~ date, data = alldat.orn)
summary(lm_orn_both_h3)
```

    ## 
    ## Call:
    ## lm(formula = H3 ~ date, data = alldat.orn)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -18.3067  -3.7834  -0.3465   5.2359  14.6545 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 5.519e+02  6.902e-01 799.599   <2e-16 ***
    ## date        1.104e-04  5.329e-05   2.072   0.0405 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.24 on 117 degrees of freedom
    ## Multiple R-squared:  0.03538,    Adjusted R-squared:  0.02713 
    ## F-statistic: 4.291 on 1 and 117 DF,  p-value: 0.04051

``` r
# Bullock's
lm_orn_BU_h3 <- lm(H3 ~ date, data = alldat.orn[which(alldat.orn$sp == "Bull"),])
summary(lm_orn_BU_h3)
```

    ## 
    ## Call:
    ## lm(formula = H3 ~ date, data = alldat.orn[which(alldat.orn$sp == 
    ##     "Bull"), ])
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -16.5275  -6.3942  -0.4725   6.4996  14.2262 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 5.504e+02  8.871e-01 620.411   <2e-16 ***
    ## date        1.747e-04  6.728e-05   2.597   0.0112 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.808 on 78 degrees of freedom
    ## Multiple R-squared:  0.07961,    Adjusted R-squared:  0.06781 
    ## F-statistic: 6.746 on 1 and 78 DF,  p-value: 0.01122

``` r
# Baltimore
lm_orn_BA_h3 <- lm(H3 ~ date, data = alldat.orn[which(alldat.orn$sp == "Balt"),])
summary(lm_orn_BA_h3)
```

    ## 
    ## Call:
    ## lm(formula = H3 ~ date, data = alldat.orn[which(alldat.orn$sp == 
    ##     "Balt"), ])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -7.4545 -2.4008 -0.4553  2.0983  9.5450 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  5.562e+02  7.142e-01 778.706   <2e-16 ***
    ## date        -1.600e-04  5.735e-05  -2.789   0.0083 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.904 on 37 degrees of freedom
    ## Multiple R-squared:  0.1737, Adjusted R-squared:  0.1514 
    ## F-statistic:  7.78 on 1 and 37 DF,  p-value: 0.008299

``` r
# Plot both
ggplot(alldat.orn, aes(x = date, y = H3, shape = spc, color = spc)) +
  geom_point(size = 2) +
  stat_smooth(method = lm, aes(fill = spc), se = T) +
  theme_classic() +
  mytheme +
  stat_fit_glance(method = "lm", label.x = "right", label.y = "bottom",
                        method.args = list(formula = y ~ x), size = 5, 
                        aes(label = sprintf('R^2~"="~%.3f~~italic(p)~"="~%.3f',
                                            stat(..r.squared..), stat(..p.value..))), 
                  parse = TRUE) + 
  labs(title = "Orange patches only", y = "Hue (H3)", x = "Collection date", 
       color = "Species", fill = "Species", shape = "Species") +
  scale_color_manual(values = pal1) +
  scale_fill_manual(values = pal1)
```

![](mainscript_files/figure-gfm/Orange%20-%20hue%20x%20date-1.png)<!-- -->

### 2. Black patches over time

Check brightness

``` r
lm_black <- lm(B1 ~ date, data = alldat.blk)
summary(lm_black)
```

    ## 
    ## Call:
    ## lm(formula = B1 ~ date, data = alldat.blk)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1111.32  -276.65   -81.05   221.07  1402.48 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 1.914e+03  4.366e+01  43.837  < 2e-16 ***
    ## date        1.109e-02  3.371e-03   3.288  0.00133 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 458 on 117 degrees of freedom
    ## Multiple R-squared:  0.0846, Adjusted R-squared:  0.07678 
    ## F-statistic: 10.81 on 1 and 117 DF,  p-value: 0.001332

``` r
# plot
ggplot(alldat.blk, aes(x = date, y = B1, shape = spc, color = spc)) +
  geom_point(size = 2) +
  stat_smooth(method = lm, aes(fill = spc), se = T) +
  theme_classic() +
  mytheme +
  stat_fit_glance(method = "lm", label.x = "right", label.y = "bottom",
                        method.args = list(formula = y ~ x), size = 5, 
                        aes(label = sprintf('R^2~"="~%.3f~~italic(p)~"="~%.3f',
                                            stat(..r.squared..), stat(..p.value..))), 
                  parse = TRUE) + 
  labs(title = "Black patches only", y = "Brightness (B1)", x = "Collection date", 
       color = "Species", fill = "Species", shape = "Species") +
  scale_color_manual(values = pal1) +
  scale_fill_manual(values = pal1)
```

![](mainscript_files/figure-gfm/Black%20-%20brightness%20x%20date-1.png)<!-- -->

<br>

## H2. Have landuse changes affected Bullock’s orioles?

Compare reference vs. hybrid zone

``` r
# Select categories for comparison 
compare <- alldat.orn[which(alldat.orn$cat == "Bull_mod" | alldat.orn$cat == "Bull_hist" | alldat.orn$cat == "Balt_mod" | alldat.orn$cat == "Balt_hist"),]

# List for comparisons
comparelist <- list(c("Bull_hist", "Bull_mod"), c("Balt_hist", "Balt_mod"))


############
# for black patches
compare1 <- alldat.blk[which(alldat.blk$cat == "Bull_mod" | alldat.blk$cat == "Bull_hist" | alldat.blk$cat == "Balt_mod" | alldat.blk$cat == "Balt_hist"),]
```

1.  Brightness boxplots

``` r
b1orn <- ggplot(compare, aes(x = cat, y = B1, fill = sp)) +
  stat_boxplot(aes(x = cat, y = B1), geom = "errorbar", 
               position = position_dodge(width = .75), width = .5) +
  geom_boxplot(outlier.size = 1.5, position = position_dodge(width = .75), col = "black") +
  stat_compare_means(comparisons = comparelist, method = "t.test", label = "p.signif", size = 6) +
  theme_classic()+
  mytheme +
  labs(y = "Total brightness (B1)") +
  scale_fill_manual(values = pal1, name = "Species", 
                    limits = c("Bull", "Balt"), 
                    labels = c(expression(italic("Icterus bullockii")), 
                               expression(italic("Icterus galbula")))) +
  scale_x_discrete(name = "Specimen type", 
                   limits = c("Bull_hist", "Bull_mod", "Balt_hist", "Balt_mod"), 
                   labels = c("Historic", "Contemporary","Historic", "Contemporary")) +
  theme(legend.text.align = 0, axis.text.x = element_text(angle = 45, hjust=1))


# black
b1blk <- ggplot(compare1, aes(x = cat, y = B1, fill = sp)) +
  stat_boxplot(aes(x = cat, y = B1), geom = "errorbar", 
               position = position_dodge(width = .75), width = .5) +
  geom_boxplot(outlier.size = 1.5, position = position_dodge(width = .75), col = "black") +
  stat_compare_means(comparisons = comparelist, method = "t.test", label = "p.signif", size = 6) + 
  theme_classic()+ 
  mytheme +
  labs(y = "Total brightness (B1)") +
  scale_fill_manual(values = pal1, name = "Species", 
                    limits = c("Bull", "Balt"), 
                    labels = c(expression(italic("Icterus bullockii")), 
                               expression(italic("Icterus galbula")))) +
  scale_x_discrete(name = "Specimen type", 
                   limits = c("Bull_hist", "Bull_mod", "Balt_hist", "Balt_mod"), 
                   labels = c("Historic", "Contemporary","Historic", "Contemporary")) +
  theme(legend.text.align = 0) +
  ylim(900,4000)
b1blk
```

![](mainscript_files/figure-gfm/compare%20across%20bullocks%20groups%20-%20brightness-1.png)<!-- -->

``` r
ggsave("Figure4.png", plot=b1blk, width=200, height=125, units="mm", dpi=600, scale=T)
```

2.  Chroma

``` r
s9orn <- ggplot(compare, aes(x = cat, y = S9, fill = sp)) +
  stat_boxplot(aes(x = cat, y = S9), geom = "errorbar", 
               position = position_dodge(width = .75), width = .5) +
  geom_boxplot(outlier.size = 1.5, position = position_dodge(width = .75), col = "black") +
  stat_compare_means(comparisons = comparelist, method = "t.test", label = "p.signif", size = 6) + 
  theme_classic()+ 
  mytheme +
  labs(y = "Caronetoid chroma (S9)") +
  scale_fill_manual(values = pal1, name = "Species", 
                    limits = c("Bull", "Balt"), 
                    labels = c(expression(italic("Icterus bullockii")), 
                               expression(italic("Icterus galbula")))) +
  scale_x_discrete(name = "Specimen type", 
                   limits = c("Bull_hist", "Bull_mod", "Balt_hist", "Balt_mod"), 
                   labels = c("Historic", "Contemporary","Historic", "Contemporary")) +
  theme(legend.text.align = 0, axis.text.x = element_text(angle = 45, hjust=1))
s9orn
```

![](mainscript_files/figure-gfm/compare%20across%20bullocks%20groups%20-%20chroma-1.png)<!-- -->

3.  Hue

``` r
h3orn <- ggplot(compare, aes(x = cat, y = H3, fill = sp)) +
  stat_boxplot(aes(x = cat, y = H3), geom = "errorbar", 
               position = position_dodge(width = .75), width = .5) +
  geom_boxplot(outlier.size = 1.5, position = position_dodge(width = .75), col = "black") +
  stat_compare_means(comparisons = comparelist, method = "t.test", label = "p.signif", size = 6) + 
  theme_classic()+ 
  mytheme +
  labs(y = "Hue (H3)") +
  scale_fill_manual(values = pal1, name = "Species", 
                    limits = c("Bull", "Balt"), 
                    labels = c(expression(italic("Icterus bullockii")), 
                               expression(italic("Icterus galbula")))) +
  scale_x_discrete(name = "Specimen type", 
                   limits = c("Bull_hist", "Bull_mod", "Balt_hist", "Balt_mod"), 
                   labels = c("Historic", "Contemporary","Historic", "Contemporary")) +
  theme(legend.text.align = 0, legend.position = c(0.7, 0.15), 
        legend.background = element_rect(linetype="solid", colour ="lightgray"), 
        axis.text.x = element_text(angle = 45, hjust=1)) +
  ylim(530,573)

h3orn 
```

![](mainscript_files/figure-gfm/compare%20across%20bullocks%20groups%20-%20hue-1.png)<!-- -->

Plot all three together

``` r
#########
pg <- plot_grid(
  b1orn + theme(legend.position="none"),
  s9orn + theme(legend.position="none"),
  h3orn,
  align = 'vh',
  labels = c("A", "B", "C"),
  hjust = -1,
  nrow = 1, label_size = 16
)

plot_grid(pg)
```

![](mainscript_files/figure-gfm/compare%20across%20bullocks%20groups%20-%20all3-1.png)<!-- -->

``` r
ggsave("Figure3.png", plot=last_plot(), width=400, height=150, units="mm", dpi=600, scale=T)
```

<br>

## H3. Are color changes only in the hybrid zone?

``` r
# Set a new variable indicating hybrid zone or outside
alldat.orn$loc <- ifelse(alldat.orn$cat == "Bull_ref", "Outside", "Hybrid zone") 

# Separate Bullock's
bullocks <- alldat.orn[which(alldat.orn$sp == "Bull"),]
```

Linear models

``` r
lm1 <- lm(B1 ~ date*loc, data=bullocks)
summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = B1 ~ date * loc, data = bullocks)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1861.64  -626.36    62.42   591.49  1988.79 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     9811.91897  141.97455  69.110   <2e-16 ***
    ## date              -0.01558    0.01110  -1.403   0.1648    
    ## locOutside      -117.67770  189.01539  -0.623   0.5354    
    ## date:locOutside    0.03556    0.01441   2.467   0.0159 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 784.1 on 76 degrees of freedom
    ## Multiple R-squared:  0.08193,    Adjusted R-squared:  0.04569 
    ## F-statistic: 2.261 on 3 and 76 DF,  p-value: 0.08815

``` r
plot(lm1) 
```

![](mainscript_files/figure-gfm/H3%20models-1.png)<!-- -->![](mainscript_files/figure-gfm/H3%20models-2.png)<!-- -->![](mainscript_files/figure-gfm/H3%20models-3.png)<!-- -->![](mainscript_files/figure-gfm/H3%20models-4.png)<!-- -->

``` r
vif(lm1) 
```

    ##     date      loc date:loc 
    ## 2.701314 1.162218 2.475218

``` r
lm2 <- lm(S9 ~ date*loc, data=bullocks)
summary(lm2)
```

    ## 
    ## Call:
    ## lm(formula = S9 ~ date * loc, data = bullocks)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.036340 -0.003686  0.000129  0.006411  0.025940 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      9.400e-01  2.027e-03 463.689  < 2e-16 ***
    ## date             4.473e-07  1.585e-07   2.821 0.006102 ** 
    ## locOutside      -1.439e-02  2.699e-03  -5.333  9.6e-07 ***
    ## date:locOutside -7.704e-07  2.058e-07  -3.743 0.000351 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0112 on 76 degrees of freedom
    ## Multiple R-squared:  0.4347, Adjusted R-squared:  0.4123 
    ## F-statistic: 19.48 on 3 and 76 DF,  p-value: 1.822e-09

``` r
plot(lm2) 
```

![](mainscript_files/figure-gfm/H3%20models-5.png)<!-- -->![](mainscript_files/figure-gfm/H3%20models-6.png)<!-- -->![](mainscript_files/figure-gfm/H3%20models-7.png)<!-- -->![](mainscript_files/figure-gfm/H3%20models-8.png)<!-- -->

``` r
vif(lm2)
```

    ##     date      loc date:loc 
    ## 2.701314 1.162218 2.475218

``` r
lm3 <- lm(H3 ~ date*loc, data=bullocks)
summary(lm3)
```

    ## 
    ## Call:
    ## lm(formula = H3 ~ date * loc, data = bullocks)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -13.3607  -5.2640  -0.3296   3.9716  18.0050 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      5.488e+02  1.294e+00 423.991  < 2e-16 ***
    ## date             4.871e-04  1.012e-04   4.812 7.41e-06 ***
    ## locOutside       9.535e-01  1.723e+00   0.553    0.582    
    ## date:locOutside -5.401e-04  1.314e-04  -4.110 9.89e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.148 on 76 degrees of freedom
    ## Multiple R-squared:  0.2484, Adjusted R-squared:  0.2188 
    ## F-statistic: 8.374 on 3 and 76 DF,  p-value: 7.03e-05

``` r
plot(lm3) 
```

![](mainscript_files/figure-gfm/H3%20models-9.png)<!-- -->![](mainscript_files/figure-gfm/H3%20models-10.png)<!-- -->![](mainscript_files/figure-gfm/H3%20models-11.png)<!-- -->![](mainscript_files/figure-gfm/H3%20models-12.png)<!-- -->

``` r
vif(lm3)
```

    ##     date      loc date:loc 
    ## 2.701314 1.162218 2.475218

#### Plots

1.  Brightness

``` r
# Regression over time 
h3a <- ggplot(bullocks, aes(x = date, y = B1, shape = loc, color = loc)) +
  geom_point(size = 2) +
  stat_smooth(method = lm, aes(fill = loc), se = T) +
  theme_classic() +
  mytheme +
#  stat_fit_glance(method = "lm", 
#                        method.args = list(formula = y ~ x), size = 5, 
#                        aes(label = sprintf('R^2~"="~%.3f~~italic(p)~"="~%.3g',
#                                            stat(..r.squared..), stat(..p.value..))), 
#  parse = TRUE) + 
  labs(y = "Total brightness (B1)", x= "Collection date", color = "Location", 
       fill = "Location", shape = "Location") +
  scale_color_manual(values = pal3) +
  scale_fill_manual(values = pal3)

################
# black patches 
# not in manuscript 

bullblk <- alldat.blk[which(alldat.blk$sp == "Bull"),]

ggplot(bullblk, aes(x = date, y = B1)) +
  geom_point(size = 2) +
  stat_smooth(method = lm, se = T, col="black") +
  theme_classic() +
  mytheme +
#  stat_fit_glance(method = "lm", 
#                        method.args = list(formula = y ~ x), size = 5, 
#                        aes(label = sprintf('R^2~"="~%.3f~~italic(p)~"="~%.3g',
#                                            stat(..r.squared..), stat(..p.value..))),
#                        parse = TRUE) + 
  labs(y = "Total brightness (B1)", x= "Collection date") +
  scale_color_manual(values = pal3) +
  scale_fill_manual(values = pal3)
```

![](mainscript_files/figure-gfm/Compare%20across%20bullocks%20hybrid%20zone%20-%20brightness-1.png)<!-- -->

``` r
#ggsave("black_B1_time2.png", plot=last_plot(), width=200, height=125, units="mm", dpi=600, scale=T)
```

2.  Chroma

``` r
h3b <- ggplot(bullocks, aes(x = date, y = S9, shape = loc, color = loc)) +
  geom_point(size = 2) +
  stat_smooth(method = lm, aes(fill = loc), se = T) +
  theme_classic() +
  mytheme +
#  stat_fit_glance(method = "lm",
#                        method.args = list(formula = y ~ x), size = 5, 
#                        aes(label = sprintf('R^2~"="~%.3f~~italic(p)~"="~%.3g',
#                                            stat(..r.squared..), stat(..p.value..))), 
#                  parse = TRUE) + 
  labs(y = "Carotenoid chroma (S9)", x = "Collection date", color = "Location", 
       fill = "Location", shape = "Location") +
  scale_color_manual(values = pal3) +
  scale_fill_manual(values = pal3)
```

3.  Hue

``` r
h3c <- ggplot(bullocks, aes(x = date, y = H3, shape = loc, color = loc)) +
  geom_point(size = 2) +
  stat_smooth(method = lm, aes(fill = loc), se = T) +
  theme_classic() +
  mytheme +
#  stat_fit_glance(method = "lm", 
#                        method.args = list(formula = y ~ x), size = 5, 
#                        aes(label = sprintf('R^2~"="~%.3f~~italic(p)~"="~%.3g',
#                                            stat(..r.squared..), stat(..p.value..))), 
#                        parse = TRUE) + 
  labs(y = "Hue (H3)", x = "Collection date", color = "Location", 
       fill = "Location", shape = "Location") +
  scale_color_manual(values = pal3) +
  scale_fill_manual(values = pal3)
```

Merge

``` r
pg <- plot_grid(
  h3a + theme(legend.position="none"),
  h3b + theme(legend.position="none"),
  h3c + theme(legend.position="none"),
  align = 'vh',
  labels = c("A", "B", "C"),
  hjust = -1,
  nrow = 1, label_size = 16
)

leg <- get_legend(h3a + theme(legend.box.margin = margin(0,0,0,12)))

plot_grid(pg, leg, rel_widths = c(6, 0.8))
```

![](mainscript_files/figure-gfm/Merge%20all%20plots-1.png)<!-- -->

``` r
ggsave("Figure5.png", plot=last_plot(), width=400, height=125, units="mm", dpi=600, scale=T)
```

<br>

# Avian visual models

Using smoothed and averaged data: `allspec.orn.avg` and
`allspec.blk.avg`

## Noise-weighted distances with the Receptor Noise Model

``` r
vismod.orn <- vismodel(allspec.orn.avg,
  visual = "avg.uv", achromatic = "bt.dc", #blue tit double cone
  illum = "D65", relative = FALSE)
summary(vismod.orn)
```

    ## visual model options:
    ##  * Quantal catch: Qi 
    ##  * Visual system, chromatic: avg.uv 
    ##  * Visual system, achromatic: bt.dc 
    ##  * Illuminant: D65, scale = 1 (von Kries colour correction not applied) 
    ##  * Background: ideal 
    ##  * Transmission: ideal 
    ##  * Relative: FALSE

    ##        u                 s                 m                l         
    ##  Min.   :0.01557   Min.   :0.02199   Min.   :0.1942   Min.   :0.3884  
    ##  1st Qu.:0.02157   1st Qu.:0.03196   1st Qu.:0.2519   1st Qu.:0.4813  
    ##  Median :0.02436   Median :0.03633   Median :0.2831   Median :0.5077  
    ##  Mean   :0.02444   Mean   :0.03796   Mean   :0.2828   Mean   :0.5059  
    ##  3rd Qu.:0.02717   3rd Qu.:0.04156   3rd Qu.:0.3104   3rd Qu.:0.5308  
    ##  Max.   :0.03580   Max.   :0.06528   Max.   :0.3780   Max.   :0.5850  
    ##       lum        
    ##  Min.   :0.2202  
    ##  1st Qu.:0.2750  
    ##  Median :0.2931  
    ##  Mean   :0.2917  
    ##  3rd Qu.:0.3100  
    ##  Max.   :0.3500

``` r
vismod.orn
```

    ##                            u          s         m         l       lum
    ## BA28299_orn1      0.02796984 0.04674980 0.3066663 0.5569209 0.3209003
    ## BA28300_orn1      0.02791030 0.05063279 0.2778269 0.5269098 0.2993215
    ## BA28301_orn1      0.02047566 0.02977987 0.2449819 0.5119533 0.2749199
    ## BA28302_orn1      0.02362612 0.03791154 0.2180511 0.5373897 0.2714292
    ## BA28304_orn1      0.02695147 0.04102449 0.2722164 0.5602838 0.3044113
    ## BA28305_orn1      0.01944704 0.02842880 0.2796832 0.5564919 0.3036157
    ## BA28306_orn1      0.02371610 0.03918074 0.2472169 0.5309015 0.2836275
    ## BA28308_orn1      0.02527853 0.03957233 0.2694793 0.5531644 0.3011530
    ## BA28415_orn1      0.03579648 0.06528205 0.3107282 0.5487583 0.3244227
    ## BA28416_orn1      0.01557564 0.02712400 0.2576541 0.5151815 0.2801811
    ## BA28417_orn1      0.02010927 0.03121793 0.2672744 0.5656235 0.3011202
    ## BA28420_orn1      0.02201513 0.04070105 0.2964101 0.5378059 0.3085718
    ## BA28467_orn1      0.01959268 0.03067119 0.2318922 0.4716554 0.2571397
    ## BA28468_orn1      0.02381147 0.03532815 0.2503082 0.5272249 0.2835080
    ## BA28469_orn1      0.01945881 0.03145711 0.2319476 0.5219797 0.2725489
    ## BA28470_orn1      0.02428966 0.03680706 0.2646369 0.5850099 0.3075707
    ## BA28471_orn1      0.03064509 0.05256067 0.3036660 0.5358270 0.3142068
    ## BA28472_orn1      0.02367486 0.03449798 0.2798117 0.5185254 0.2937093
    ## BA28473_orn1      0.02934752 0.05103340 0.2838316 0.5132094 0.2976653
    ## BA28474_orn1      0.02728949 0.03787244 0.3071625 0.5421160 0.3144608
    ## BA56813_orn1      0.01980752 0.03580421 0.2765126 0.5127555 0.2900836
    ## BA56814_orn1      0.02135699 0.03461725 0.2414293 0.4376250 0.2514180
    ## BA56825_orn1      0.02132392 0.03216299 0.2438876 0.5653121 0.2911940
    ## BA56830_orn1      0.02538225 0.03488949 0.2896194 0.5561781 0.3101500
    ## BA56832_orn1      0.02658829 0.03795292 0.2281457 0.5365369 0.2770920
    ## BA56852_orn1      0.03062735 0.03624071 0.3462528 0.5637404 0.3380908
    ## BA57662_orn1      0.02507860 0.03284063 0.2250493 0.4637112 0.2528743
    ## BA57663_orn1      0.02750858 0.03559450 0.2806929 0.4935013 0.2874941
    ## BA57710_orn1      0.02540106 0.03737057 0.2950618 0.5325031 0.3057626
    ## BA57712_orn1      0.02941402 0.03906280 0.2837596 0.5061864 0.2931080
    ## BA57715_orn1      0.02377362 0.02997888 0.2615390 0.4860206 0.2748716
    ## BA57730_orn1      0.02727192 0.03496634 0.2708887 0.5307083 0.2945731
    ## BA57748_orn1      0.02768425 0.03333430 0.3024750 0.5231693 0.3053599
    ## BA57750_orn1      0.02127258 0.03104613 0.2712507 0.5030108 0.2844704
    ## BA57757_orn1      0.02638886 0.03254863 0.2856518 0.5680426 0.3116285
    ## BA57758_orn1      0.02019642 0.02667294 0.2718507 0.5050631 0.2843643
    ## BA57890_orn1      0.03029396 0.04206295 0.3251377 0.5301327 0.3196969
    ## BA57965_orn1      0.02317017 0.03126176 0.2660184 0.5199092 0.2871160
    ## BA58010_orn1      0.02652631 0.03844544 0.2689497 0.5274507 0.2931536
    ## BU26842_orn1      0.02055550 0.03176553 0.2637964 0.4581977 0.2680033
    ## BU26843_orn1      0.02989526 0.04480868 0.3178904 0.5443314 0.3221412
    ## BU26844_orn1      0.02435952 0.03860934 0.3601839 0.5077396 0.3265739
    ## BU26845_orn1      0.01944343 0.03235683 0.3358882 0.4737425 0.3038151
    ## BU26846_orn1      0.02563547 0.03621155 0.3780429 0.5411545 0.3444389
    ## BU26847_orn1      0.02577437 0.04136297 0.2521002 0.5245020 0.2837131
    ## BU26848_orn1      0.02162370 0.03608451 0.3571682 0.5397303 0.3342563
    ## BU26849_orn1      0.02568997 0.03761574 0.2944109 0.5001620 0.2956900
    ## BU26850_orn1      0.03394973 0.04731088 0.3473471 0.5277976 0.3309739
    ## BU26851_orn1      0.02352189 0.03211698 0.2532990 0.4863788 0.2712093
    ## BU26932_orn1      0.03214428 0.04759147 0.3707046 0.4942539 0.3303755
    ## BU26938_orn1      0.02392425 0.03915949 0.2868571 0.4993519 0.2910095
    ## BU26939_orn1      0.02151170 0.03595544 0.3013883 0.4858828 0.2931236
    ## BU26940_orn1      0.02629528 0.03735117 0.3575595 0.5071788 0.3251479
    ## BU26941_orn1      0.02814080 0.04091099 0.3254890 0.4843398 0.3053804
    ## BU26942_orn1      0.02226399 0.03744872 0.2746045 0.4696085 0.2771699
    ## BU26943_orn1      0.01613791 0.02925685 0.2616571 0.4869706 0.2734294
    ## BU26944_orn1      0.02070246 0.03074954 0.2156081 0.4695440 0.2476809
    ## BU26945_orn1      0.03031754 0.05168099 0.3564381 0.5269601 0.3347009
    ## BU26946_orn1      0.02458785 0.03551942 0.2958086 0.4544581 0.2812017
    ## BU56854_orn1      0.02625576 0.03139326 0.2834424 0.5017296 0.2897527
    ## BU57614_orn1      0.02346643 0.02788794 0.2507426 0.4977058 0.2733923
    ## BU57647_orn1      0.03070174 0.03894837 0.2870074 0.5143249 0.2965901
    ## BU57648_orn1      0.02948198 0.03324336 0.2237126 0.5258957 0.2686905
    ## BU57649_orn1      0.01794243 0.02552674 0.2037971 0.4825542 0.2443615
    ## BU57650_orn1      0.02889067 0.03501779 0.2516752 0.5519882 0.2902955
    ## BU57652_orn1      0.02904048 0.03195839 0.2606568 0.5600177 0.2958629
    ## BU57653_orn1      0.02612852 0.03056790 0.2548113 0.5423787 0.2883962
    ## BU57737_orn1      0.01691286 0.02198793 0.2281638 0.5045853 0.2627774
    ## BU57994_orn1      0.02706600 0.03632875 0.3174218 0.5564887 0.3225371
    ## BU58002_orn1      0.02626447 0.03181247 0.2303896 0.5178631 0.2687958
    ## BU58119_orn1      0.02877483 0.03523447 0.2846726 0.5111248 0.2940739
    ## BU58127_orn1      0.02575816 0.02955226 0.2306500 0.4873741 0.2606179
    ## BU58129_orn1      0.02629740 0.03188247 0.2766591 0.5195644 0.2920888
    ## BU58130_orn1      0.02233111 0.02752743 0.2059329 0.4768095 0.2458992
    ## BU58131_orn1      0.02567438 0.02807080 0.2227788 0.4450964 0.2442773
    ## BU58137_orn1      0.02466369 0.02770546 0.2433841 0.4487684 0.2549564
    ## BU58138_orn1      0.01557441 0.02727219 0.1992891 0.4416505 0.2305895
    ## BU58140_orn1      0.02743730 0.03526477 0.3004666 0.5173591 0.3026909
    ## BU58141_orn1      0.02689312 0.03195691 0.2770678 0.4872802 0.2814893
    ## MVZ_BU123442_orn1 0.01850014 0.03790981 0.2342114 0.4526627 0.2534928
    ## MVZ_BU136690_orn1 0.03248108 0.04643590 0.3519030 0.5838730 0.3500432
    ## MVZ_BU147393_orn1 0.02260356 0.04065882 0.3099909 0.4729095 0.2945274
    ## MVZ_BU147395_orn1 0.02088717 0.03764275 0.2888074 0.4718032 0.2834727
    ## MVZ_BU163024_orn1 0.02372045 0.03767827 0.3152666 0.5562934 0.3219474
    ## MVZ_BU163026_orn1 0.01721616 0.02856034 0.2423307 0.4928231 0.2676124
    ## MVZ_BU165823_orn1 0.01991413 0.02993584 0.2035702 0.4637613 0.2400153
    ## MVZ_BU26842_orn1  0.02247029 0.04033016 0.3029093 0.4766891 0.2922266
    ## MVZ_BU28167_orn1  0.02377104 0.03852595 0.3560773 0.5252397 0.3307880
    ## MVZ_BU34099_orn1  0.02184989 0.03161251 0.2119021 0.4434056 0.2394742
    ## MVZ_BU5182_orn1   0.01759971 0.03474942 0.2375780 0.4310673 0.2476265
    ## MVZ_BU57658_orn1  0.02180008 0.03620415 0.3137255 0.5226315 0.3102457
    ## MVZ_BU62167_orn1  0.02100451 0.03482360 0.2987441 0.5570055 0.3145327
    ## MVZ_BU65311_orn1  0.01747584 0.04138248 0.2875195 0.4889242 0.2878055
    ## MVZ_BU78245_orn1  0.01941314 0.03187771 0.2096335 0.3884183 0.2216850
    ## MVZ_BU88648_orn1  0.01615934 0.03311181 0.1942208 0.4097068 0.2201658
    ## MVZ_BU8940_orn1   0.02390707 0.05077865 0.3009464 0.4759848 0.2938056
    ## MVZ_BU8942_orn1   0.02449743 0.04173230 0.2896418 0.5165135 0.2992283
    ## MVZ_BU8943_orn1   0.03098834 0.06000429 0.3174131 0.4953682 0.3099498
    ## MVZ_BU95633_orn1  0.01959981 0.03581566 0.3200783 0.4730495 0.2969712
    ## UWBM104549_orn1   0.02840090 0.05135142 0.3244437 0.5006347 0.3119781
    ## UWBM109429_orn1   0.02362714 0.04624779 0.3174980 0.5369759 0.3177555
    ## UWBM122228_orn1   0.02377313 0.04128109 0.2831305 0.4789006 0.2836170
    ## UWBM122453_orn1   0.02967937 0.05754714 0.3721397 0.5165774 0.3410603
    ## UWBM122617_orn1   0.02230079 0.04218715 0.2955651 0.4989486 0.2952310
    ## UWBM37214_orn1    0.02045407 0.03804626 0.2830870 0.4652819 0.2788188
    ## UWBM41002_orn1    0.02618356 0.04387880 0.3153967 0.4844875 0.3014775
    ## UWBM42201_orn1    0.02611214 0.04804540 0.2927911 0.4300692 0.2751286
    ## UWBM48344_orn1    0.02162147 0.04560328 0.2668323 0.4415944 0.2654217
    ## UWBM50074_orn1    0.02445273 0.04829231 0.3592680 0.5286412 0.3354275
    ## UWBM53665_orn1    0.02440393 0.04047545 0.3585366 0.5439974 0.3379698
    ## UWBM55942_orn1    0.02890605 0.04650070 0.3088966 0.4996235 0.3036636
    ## UWBM55954_orn1    0.02421119 0.04595139 0.2838704 0.4700851 0.2819990
    ## UWBM59056_orn1    0.02399482 0.04627630 0.3470955 0.4847319 0.3166015
    ## UWBM59058_orn1    0.02686351 0.05231756 0.3230026 0.4799697 0.3050020
    ## UWBM59333_orn1    0.02652147 0.04683603 0.3274903 0.5076623 0.3142630
    ## UWBM79812_orn1    0.02756083 0.05705688 0.2823355 0.4629213 0.2820442
    ## UWBM86267_orn1    0.02172128 0.03452629 0.2378279 0.5288757 0.2742328
    ## UWBM88853_orn1    0.02362812 0.04208856 0.3469151 0.4961049 0.3189048
    ## UWBM91361_orn1    0.03030422 0.04557543 0.2605167 0.5012688 0.2805080

``` r
# Get color distances
vismod.orn.dist <- coldist(vismod.orn,
  noise = "neural", achromatic = TRUE, n = c(1, 2, 2, 4),
  weber = 0.1, weber.achro = 0.1)


# bootstrap color distances
cate <- allcategory[-1] # get list made from above but remove wl from vector

vm.orn.dist <- bootcoldist(vismod.orn, by = cate, n = c(1,2,2,4), weber=0.1, achromatic = FALSE)
#vm.orn.dist
```

``` r
# ggplot
# need to convert matrix into a usable data frame
vod <- as.data.frame(as.table(vm.orn.dist))
vod2 <- pivot_wider(vod, names_from = Var2, values_from = Freq)

# select comparisons
vod3 <- vod2 %>% 
  filter(Var1 == "Bull_hist-Bull_mod" | Var1 == "Balt_hist-Balt_mod" | 
         Var1 == "Balt_hist-Bull_hist" | Var1 == "Balt_mod-Bull_mod")

ggplot(vod3, aes(x=Var1, y=dS.mean)) +
  geom_errorbar(aes(ymin = dS.lwr, ymax = dS.upr), width=.3, 
                position = position_dodge(width=1), size=1) +
  geom_point(position = position_dodge(width=1), size=5) +
  theme_classic()+ 
  mytheme + 
  theme(text=element_text(family="sans")) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  coord_flip(clip="off") +
  labs(x=NULL, y = "Chromatic contrast (\u0394S)") +
  theme(panel.background = element_rect(fill = "transparent"), 
        axis.title.y = element_text(angle = 0, hjust = 0), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent"))+
  scale_x_discrete(limits = c("Balt_mod-Bull_mod", "Balt_hist-Bull_hist", 
                              "Balt_hist-Balt_mod", "Bull_hist-Bull_mod"), 
                   labels = c("Interspecific contemporary", "Interspecific historic",
                              expression(paste("Intraspecific ", italic("Icterus galbula"))),
                              expression(paste("Intraspecific ", italic("Icterus bullockii"))))) +
  geom_text(x=4.5, y=0.2, inherit.aes = F, label = "Centroid comparison", 
            check_overlap = T, hjust =1, fontface="bold", size=7)
```

![**Fig 6.** Chromatic contrasts (dS),
vertical](mainscript_files/figure-gfm/unnamed-chunk-1-1.png)

``` r
ggsave("chromatic_contrast.png", plot = last_plot(), width = 200, height = 80, 
       units = "mm", dpi = 300,  bg = "transparent")
```
