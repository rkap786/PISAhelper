This is code related to the paper, **"Differences in Time Usage as a Competing Hypothesis for Observed Group Differences in Accuracy with an Application to Observed Gender Differences in PISA Data"**, paper [here](https://onlinelibrary.wiley.com/doi/10.1111/jedm.12419). 

The code in this repository can be used to create the R package PISAhelper, which contains functions to recreate the results from the simulations in the paper -- the package can be used to simulate a dataset, calculate observed group score differences, and estimate group score differences due to capacity ($A_c$), and group score differences due to speed ($A_s$). These functions can also be used on empirical datasets. In the paper, the functions were using for PISA 2018 reading fluency and science items. 

### Installing the package
The package can be installed as under:

```
## Install necessary packages
list.of.packages=c('kdensity', 'MASS', 'splines', 'dplyr', 'fixest', 'devtools')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

### Install PISAhelper from github
library(devtools)
devtools::install_github("rkap786/PISAhelper", ref="main")
```

### Recreating simulation results from the paper
Please note important functions in the R sub-folder as under:
1. simdata(): generates data for simulation given group characteristics
2. integrate(): integrates group CAFs over their response time distributions
3. getcaf_deg.R: estimates flexible CAFs for each group using flexible b-splines. Default number of degrees used in b-splines is 2.
4. getcaf0_deg:  estimates flexible CAF, combined for both groups, using flexible b-splines. Default number of degrees used in b-splines is 2.

These functions can be used to recreate the simulation results from the paper as follows:

```


### Load packages
library(PISAhelper)
library(fixest)
library(dplyr)

### Generate data
set.seed(123)
speed.offset= 0.5
th.offset= 0.5 # higher ability group also has higher speed
rho= 0.25
N=1000
b.time= 0.5 #upward sloping CAF i.e., spending more time on the item increases probability of correst response
nitems= 45

x<-simdata(speed.offset=speed.offset,th.offset=th.offset,
           N=N,rho=rho, b.time=b.time, nitems=nitems)
######## generate data: each row represents an individual (variable id) responding to a given item (variable item),
######## individual's latent ability (th) and latent speed (tau), the group individual belongs to (group)
########  item difficulty (diff), observed item response time (rt), pvalue (pv), and observed response on the item (resp)


x1= x |> filter(group==1) ## Group 1 data
x2= x |> filter(group==2) ## Group 2 data
    
M<-by(x$resp,x$group,mean) # get observed differences in mean group scores 
A = M[[2]]- M[[1]] # the observed group mean score difference. Here given seed, observed group difference is 0.0778 

caf = getcaf_deg(x, deg=2) # Get CAF for each group
caf0= getcaf0_deg(x, deg=2) # Get overall CAF for both groups

## Group differences $A_c$:
caf.tmp= data.frame(t=caf[[2]][,1],yhat=caf[[2]][,2]) ## CAF for group 2
intdiff_th2= integrate(x$rt,caf.tmp) ## CAF for group 2 integrated over response time distribution for both groups
caf.tmp= data.frame(t=caf[[1]][,1],yhat=caf[[1]][,2]) ## CAF for group 1
intdiff_th1= integrate(x$rt,caf.tmp) ## CAF for group 1 integrated over response time distribution for both groups
ac = intdiff_th2 - intdiff_th1  #
# ac is 0.0929 i.e., group differences after adjusting for speed would increase, if higher ability group spent more time on items           

## Group differences $A_s$
caf.tmp = data.frame(t=caf0[,1],yhat=caf0[,2]) ## Overall CAF for both groups 
intdiff_t2h = integrate(x2$rt,caf.tmp) ## Overall CAF integrated over group 2's response time distribution
intdiff_t1h= integrate(x1$rt,caf.tmp) 
as= intdiff_t2h - intdiff_t1h ## Overall CAF integrated over group 2's response time distribution
# as is -0.0148 i.e., adjusted observed scores attributable to speed


```

The score differences A, $A_c$ and $A_s$ can also be directly retreived using the function scorediff() as follows:

```
scorediff(x) ## Returns A, A_c & A_s given data file x
## same information as above in one function

```
