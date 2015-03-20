---
title: "ToothGrowth Analysis Project"
author: "Kaylee Walsh"
date: "March 20, 2015"
output: pdf_document
---

# Overview

The ToothGrowth data is the records of an experiment of the effect of Vitamin C on tooth growth in guinea pigs. The two supplements are orange juice (OJ) and ascorbic acid (VC) in three dose levels (0.5, 1, and 2 mg). We look at the effects between supplements and doses.

# Exploratory


```r
library(data.table)
data(ToothGrowth)
str(ToothGrowth)
```

```
## 'data.frame':	60 obs. of  3 variables:
##  $ len : num  4.2 11.5 7.3 5.8 6.4 10 11.2 11.2 5.2 7 ...
##  $ supp: Factor w/ 2 levels "OJ","VC": 2 2 2 2 2 2 2 2 2 2 ...
##  $ dose: num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
```

```r
summary(ToothGrowth$len)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     4.2    13.1    19.2    18.8    25.3    33.9
```

```r
hist(ToothGrowth$len)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

```r
dt <- as.data.table(ToothGrowth) ## I like to work in data.table over data.frame
```

The data has three columns: length of tooth growth after receiving treatment, supplement used, and the dosage administered. The spread of the length of teeth is 29.7. We see that the histogram is slightly skewed left, as hinted at since the median is greater than the mean. That might be hinting at overall growth between supplements and dosages. Now we break down and analyze which supplement/dosage pairing is most effective.

# Compare Doses, Fixed Supplements

## OJ Conclusions


```r
oj1 <- dt[supp == "OJ" & dose == 0.5, len]
oj2 <- dt[supp == "OJ" & dose == 1.0, len]
oj3 <- dt[supp == "OJ" & dose == 2.0, len]

t.test(oj1, oj2, paired = F)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  oj1 and oj2
## t = -5.049, df = 17.7, p-value = 8.785e-05
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -13.416  -5.524
## sample estimates:
## mean of x mean of y 
##     13.23     22.70
```

```r
t.test(oj2, oj3, paired = F)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  oj2 and oj3
## t = -2.248, df = 15.84, p-value = 0.0392
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -6.5314 -0.1886
## sample estimates:
## mean of x mean of y 
##     22.70     26.06
```

```r
t.test(oj1, oj3, paired = F)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  oj1 and oj3
## t = -7.817, df = 14.67, p-value = 1.324e-06
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -16.335  -9.325
## sample estimates:
## mean of x mean of y 
##     13.23     26.06
```

None of these confidence intervals contain zero which indicates that any increase in dosage of orange juice will lead to effective tooth growth.

## VC Conclusions


```r
vc1 <- dt[supp == "VC" & dose == 0.5, len]
vc2 <- dt[supp == "VC" & dose == 1.0, len]
vc3 <- dt[supp == "VC" & dose == 2.0, len]

t.test(vc1, vc2, paired = F)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  vc1 and vc2
## t = -7.463, df = 17.86, p-value = 6.811e-07
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -11.266  -6.314
## sample estimates:
## mean of x mean of y 
##      7.98     16.77
```

```r
t.test(vc2, vc3, paired = F)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  vc2 and vc3
## t = -5.47, df = 13.6, p-value = 9.156e-05
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -13.054  -5.686
## sample estimates:
## mean of x mean of y 
##     16.77     26.14
```

```r
t.test(vc1, vc3, paired = F)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  vc1 and vc3
## t = -10.39, df = 14.33, p-value = 4.682e-08
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -21.90 -14.42
## sample estimates:
## mean of x mean of y 
##      7.98     26.14
```

Again, none of these confidence intervals contain zero which indicates that any increase in dosage of vitamin C by ascorbic acid will lead to effective tooth growth.

Since both supplements show effective growth in tooth length, we can conclude increasing Vitamin C in the diet will lead to longer teeth. Now we compare which supplement is more effective at tooth growth.

# Compare Supplements, Fixed Doses

## Dosage of 0.5 mg Conclusions


```r
t.test(oj1, vc1, paired = F)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  oj1 and vc1
## t = 3.17, df = 14.97, p-value = 0.006359
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  1.719 8.781
## sample estimates:
## mean of x mean of y 
##     13.23      7.98
```

Since this interval does not contain zero, then we can conclude that at a dosage of 0.5 mg for both supplements, orange juice is more effective than ascorbic acid at tooth growth.

## Dosage of 1.0 mg Conclusions


```r
t.test(oj2, vc2, paired = F)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  oj2 and vc2
## t = 4.033, df = 15.36, p-value = 0.001038
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  2.802 9.058
## sample estimates:
## mean of x mean of y 
##     22.70     16.77
```

Since this interval does not contain zero, then we can conclude that at a dosage of 1.0 mg for both supplements, orange juice is more effective than ascorbic acid at tooth growth.

## Dosage of 2.0 mg Conclusions


```r
t.test(oj3, vc3, paired = F)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  oj3 and vc3
## t = -0.0461, df = 14.04, p-value = 0.9639
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -3.798  3.638
## sample estimates:
## mean of x mean of y 
##     26.06     26.14
```

Since this interval DOES contain zero, we can conclude that at the dosage of 2.0 mg for both supplements, they are approximately equivalent in effecacy for tooth growth.

# Results

Overall we can say that both supplements are effective at tooth growth, however when comparing supplements we see orange juice is more effective than ascorbic acid except for the dosage of 2.0 mg where they are equally effective. 

These tests were run under the assumptions that the observations were not paired, but statistically independent and that the variances were common.




