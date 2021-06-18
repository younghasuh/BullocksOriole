Main code
================
6/18/2021

  - [Load libraries](#load-libraries)
  - [Import data](#import-data)
      - [Attribute data](#attribute-data)
      - [Spectra data](#spectra-data)
      - [Visually check spectra](#visually-check-spectra)
          - [Fix odd peaks](#fix-odd-peaks)
      - [Average spectra](#average-spectra)
      - [Smooth spectra](#smooth-spectra)
  - [Split orange and black patches](#split-orange-and-black-patches)
      - [Extract orange patches](#extract-orange-patches)
      - [Extract black patches](#extract-black-patches)
  - [Colorimetric variables](#colorimetric-variables)
      - [Compare boxplots across specimen
        categories](#compare-boxplots-across-specimen-categories)
      - [Check data](#check-data)
  - [Statistical analyses](#statistical-analyses)
      - [H1. Have specimens faded over
        time?](#h1-have-specimens-faded-over-time)
          - [1. Orange patches over time](#1-orange-patches-over-time)
          - [2. Black patches over time](#2-black-patches-over-time)
      - [H2. Have landuse changes affected Bullock’s
        orioles?](#h2-have-landuse-changes-affected-bullocks-orioles)
      - [H3. Are color changes only in the hybrid
        zone?](#h3-are-color-changes-only-in-the-hybrid-zone)

The following analyses is conducted on spectra data for 119 specimens.
Sample sizes for each specimen category compose of the following:

| Bullock’s oriole |        |           | Baltimore oriole |        |
| ---------------- | ------ | --------- | ---------------- | ------ |
| Historic         | Modern | Reference | Historic         | Modern |
| 20               | 20     | 40        | 20               | 19     |

Bullock’s reference specimens come from the Museum of Vertebrate Zoology
(MVZ; n = 20) and the University of Washington Burke Museum (UWBM; n =
20).

**Set up**: integration time, 100 ms; 10 readings averaged per
recording; boxcar width 10

Each specimen has 5 patches measured (2 black 3 orange) and each patch
is measured 3 times; i.e. 15 measurements per specimen.

![**Fig 1**. Patches measured on specimens.](pics.png)

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

pal1 <- c("#58508d", "#ffa600")
```

# Import data

## Attribute data

Load `specimen_info.xls` & filter out necessary info

``` r
attributes <- read_excel("oriole_specimen_info.xlsx")
knitr::kable(head(attributes))
```

| ID      | sp\_id          | cat        | Cat Num | Species | Code             | CollectionDate | State | Higher Geography                     | Specific Locality                                  |
| :------ | :-------------- | :--------- | ------: | :------ | :--------------- | :------------- | :---- | :----------------------------------- | :------------------------------------------------- |
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
```

## Visually check spectra

``` r
plot(spec)
```

![**Fig 2a.** Spectra of specimens measured in
2019.](mainscript_files/figure-gfm/See%20other%20spectra-1.png)

``` r
plot(burke)
```

![**Fig 2b.** Spectra of UWBM specimens measured in
2021.](mainscript_files/figure-gfm/See%20burke%20spectra-1.png)

### Fix odd peaks

Measurements for the Burke museum specimen have two peaks in the spectra
that occur for unknown reasons. They were replaced by deleting
reflectance at those wavelengths and interpolating the values from
before/after. The areas of concern seem to be 470-500 nm and 510-560 nm.

1.  Replace odd wavelengths with `NA`
2.  Use `as.rspec` to interpolate gaps

<!-- end list -->

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

<!-- end list -->

``` r
plot(burke)
```

![**Fig 2c.** Spectra of UWBM
specimens](mainscript_files/figure-gfm/See%20new%20burke%20spectra-1.png)

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
legend(290, 67, legend = c("BA historic", "BA modern", "BU historic", "BU modern", "BU reference"), col = c("red", "royalblue", "green", "purple", "orange", "yellow"), lty=1, cex=1.2, box.lty=0, lwd=2, bg="transparent")
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
```

#### Spectra split by species

``` r
# split by species
asoa.ba <- subset(asoa, "Balt")
asoa.bu <- subset(asoa, "Bull")


allcat <- as.data.frame(allcategory)
allcat2 <- allcat %>% 
  filter(allcategory =="wl" | allcategory == "Balt_hist" | allcategory == "Balt_mod")
colnames(asoa.ba) <- allcat2$allcategory

allcat3 <- allcat %>% 
  filter(allcategory =="wl" | allcategory == "Bull_hist" | allcategory == "Bull_mod" | allcategory == "Bull_ref")
colnames(asoa.bu) <- allcat3$allcategory

# plot & save

#png("all_spectra_bullocks.png", width = 800, height = 800, pointsize = 22, res = 100, bg = "transparent")
aggplot(asoa.bu, by = allcat3$allcategory, FUN.center = median, ylim = c(0, 65),
        alpha = 0.3, legend = F, cex.lab = 1.5, cex.axis = 1.5, lwd=2)
legend(290, 67, legend = c("Historic", "Modern", "Reference"), col = c("red", "royalblue", "green"), lty=1, cex=1.2, box.lty=0, lwd=2, bg="transparent", title = "Bullock's oriole")
```

![](mainscript_files/figure-gfm/Split%20spectra%20by%20species-1.png)<!-- -->

``` r
#dev.off()

#png("all_spectra_baltimore.png", width = 800, height = 800, pointsize = 22, res = 100, bg = "transparent")
aggplot(asoa.ba, by = allcat2$allcategory, FUN.center = median, ylim = c(0, 65),
        alpha = 0.3, legend = F, cex.lab = 1.5, cex.axis = 1.5, lwd=2)
legend(290, 67, legend = c("Historic", "Modern"), col = c("red", "royalblue"), lty=1, cex=1.2, box.lty=0, lwd=2, bg="transparent", title = "Baltimore oriole")
```

![](mainscript_files/figure-gfm/Split%20spectra%20by%20species-2.png)<!-- -->

``` r
#dev.off()
```

### Extract black patches

Repeat steps above

``` r
allspec.blk <- subset(allspec.sm, "blk")

# average 3 measurements
allspec.blk.avg <- aggspec(allspec.blk, by = 2, FUN = mean, trim = FALSE) 

# Get colorimetric values
allsum.blk <- summary(allspec.blk.avg)
setDT(allsum.blk, keep.rownames = TRUE)[]

# convert MVZ names to be consistent with the specimen attribute data
allsum.blk$rn <- gsub("MVZ_", "MVZ.", allsum.blk$rn)

allid.blk <- do.call(rbind, strsplit(allsum.blk$rn, "\\_"))[, 1]

allsum.blk$ID <- allid.blk #specimen ID

allsum.blk$ID <- gsub("UWBM", "UWBM.BU", allsum.blk$ID)


# Join spectra summary data with attribute data
alldat.blk <- allsum.blk %>%
  left_join(att, by = "ID", keep = F)

# Make just a species category
alldat.blk$sp <- gsub("\\_.*", "", alldat.blk$cat)
```

<br>

# Colorimetric variables

List of variables of interest (from Pavo package; references available
there) - `B1`: total brightness; sum of relative reflectance over entire
spectral range (area under the curve) - `B2`: mean brightness; mean
relative reflectance over entire spectral range - `S1`: chroma; relative
contribution of a spectral range to the total brightness (B1) - `S9`:
carotenoid chroma; (R700 - R450)/R700 - `H3`: hue/wavelength at Rmid;
sensitive to noisy spectra

### Compare boxplots across specimen categories

Orange patches

``` r
dat <- alldat.orn
plot(B1~cat, data=dat, ylab = "Total brightness (B1)", xlab = "Specimen category")
```

![](mainscript_files/figure-gfm/Boxplot%20across%20specimens%20-%20orange-1.png)<!-- -->

``` r
plot(B2~cat, data=dat, ylab = "Mean brightness (B2)", xlab = "Specimen category") # same as above, just different y axis
```

![](mainscript_files/figure-gfm/Boxplot%20across%20specimens%20-%20orange-2.png)<!-- -->

``` r
plot(S9~cat, data=dat, ylab = "Carotenoid chroma (S9)", xlab = "Specimen category")
```

![](mainscript_files/figure-gfm/Boxplot%20across%20specimens%20-%20orange-3.png)<!-- -->

``` r
plot(H3~cat, data=dat, ylab = "Hue (H3)", xlab = "Specimen category")
```

![](mainscript_files/figure-gfm/Boxplot%20across%20specimens%20-%20orange-4.png)<!-- -->

Black patchess

``` r
dat <- alldat.blk
plot(B1~cat, data=dat, ylab = "Total brightness (B1)", xlab = "Specimen category")
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

![](mainscript_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

    ## [1] 94 95

``` r
qqPlot(alldat.orn$B2)
```

![](mainscript_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

    ## [1] 94 95

``` r
qqPlot(alldat.orn$S9)
```

![](mainscript_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

    ## [1] 116  98

``` r
qqPlot(alldat.orn$H3)
```

![](mainscript_files/figure-gfm/unnamed-chunk-1-4.png)<!-- -->

    ## [1] 50 43

``` r
# Black
qqPlot(alldat.blk$B1)
```

![](mainscript_files/figure-gfm/unnamed-chunk-1-5.png)<!-- -->

    ## [1] 37 27

<br>

# Statistical analyses

## H1. Have specimens faded over time?

To test for specimen fauxing, look at colorimetric measures over time.

### 1\. Orange patches over time

Check brightness, carotenoid chroma, hue

``` r
ggplot(alldat.orn, aes(x=date, y=B1, shape = sp, color=sp)) +
  geom_point(size=2) +
  stat_smooth(method = lm, aes(fill=sp), se = T) +
  theme_classic() +
  mytheme +
  stat_fit_glance(method = "lm", label.x="right", label.y="bottom",
                        method.args = list(formula = y ~ x), size = 5, 
                        aes(label = sprintf('R^2~"="~%.3f~~italic(p)~"="~%.2f',
                                            stat(..r.squared..),stat(..p.value..))), parse = TRUE) + 
  labs(title = "Orange patches only", y = "Total brightness (B1)", x= "Collection date")  +
  scale_color_manual(values= pal1, labels = c("Baltimore", "Bullock's")) +
  scale_fill_manual(values= pal1)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](mainscript_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### 2\. Black patches over time

Check brightness

``` r
plot(B1~date, data=alldat.blk, ylab = "Total brightness (B1)", xlab = "Specimen age", col = "#595959", main = "Black patches") 
```

![](mainscript_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

<br>

## H2. Have landuse changes affected Bullock’s orioles?

<br>

## H3. Are color changes only in the hybrid zone?