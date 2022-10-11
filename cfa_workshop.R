# Clay Ford
# UVa Library StatLab
# Spring 2018
# CFA workshop

library(lavaan)
library(semPlot)


# Example: 2 latent factors -----------------------------------------------

# data that come with R
# Ability and Intelligence Tests
# Six tests were given to 112 individuals
# ?ability.cov
ability.cov
ability.cov$cov
ability.cov$n.obs
colnames(ability.cov$cov)
rownames(ability.cov$cov)

# specify a model using lavaan syntax

# model is specied inbetween single quotes: ''
# see ?model.syntax
# the letters are optional labels;
# Notice we can use comments in the model;

cfa.mod0 <- '
# 2-factor model
Verbal =~ general + reading + vocab
Reason =~ picture + blocks + maze
'

# Fit the model to ability.cov$cov
cfa.fit <- cfa(model = cfa.mod0, 
               sample.cov = ability.cov$cov, 
               sample.nobs = ability.cov$n.obs)
# summary of model
summary(cfa.fit, standardize = TRUE)

# fitted covariance matrix
fitted(cfa.fit)

# difference between observed and fitted covariance matrix
residuals(cfa.fit)
residuals(cfa.fit, type = "normalized")



# YOUR TURN!

# Data from Brown (2006), Ch 4. 

# A researcher has collected eight personality measures from a sample of 250
# psychotherapy outpatients. All variables measured on scales ranging from 0 to
# 32 (higher scores reflect higher levels of the assessed dimension). Below we
# read in the covariance matrix of these measures.

# Read in the covariance matrix:
psych.cov <- as.matrix(read.csv("https://github.com/clayford/CFA_workshop/raw/master/data/psychotherapy.csv"))
rownames(psych.cov) <- colnames(psych.cov)
psych.cov

# N1 = "anxiety" 
# N2 = "hostility"
# N3 = "depression"
# N4 = "self-consciousness"
# E1 = "warmth"
# E2 = "gregariousness"
# E3 = "assertiveness"
# E4 = "positive.emotions"

# Fit a two factor model where anxiety, hostility, depression, and
# self-consciousness load on a latent factor called "Neuroticism", and warmth,
# gregariousness, assertiveness, and positive emotions load on a factor called
# "Extraversion".

# 1. Specify the model using lavaan syntax; sample.nobs = 250. Use the
# column/row names (N1, N2, etc). Call it "pysch.mod"


# 2. fit the model using the cfa() function. Call it "psych.fit"


# 3. View the summary of psych.fit; use standardize = TRUE


# 4. Examine the fitted covariance matrix and the residuals


# Back to the slides.

# Understanding a CFA model -----------------------------------------------

# Derive some of the values in the summary output
summary(cfa.fit, standardize = TRUE)

# Std.lv values for marker variables:
# standard deviation of latent factor variances
sqrt(10.021)
sqrt(2.280)

# Std.lv for all other manifest variables:
# standard deviation of latent factor variances * unstandardized value
3.166 * 2.075
3.166 * 3.144

# Std.all values:
# standard deviation of latent factor variances * unstandardized value / obs SD of manifest variable
3.166 * 2.075 / sqrt(ability.cov$cov["reading", "reading"])
3.166 * 3.144 / sqrt(ability.cov$cov["vocab", "vocab"])



# Basic path diagrams -----------------------------------------------------

# Use semPaths() from the semPlot package

# default path diagram
# what = "std" ---> "display the standardized parameter estimate in edge labels."
semPaths(cfa.fit, what = "std")

# Not very pretty but functional; has weighted lines; marker variables have
# dashes; numbers hard to read; variables abbreviated

# turn off the colors and weights
semPaths(cfa.fit, 
         what = "std", 
         weighted = FALSE)           # weighted graph?

# a slightly different look
semPaths(cfa.fit, 
         what = "std", 
         weighted = FALSE,
         style = "lisrel")           # no double-headed selfloops

# a few more tweaks...
semPaths(cfa.fit, 
         what = "std",
         weighted = FALSE,           
         style = "lisrel",           
         nCharNodes = 0,             # Number of characters to abbreviate node labels
         edge.color = "black",       # A value indicating the color of all edges
         edge.label.cex = 1,         # Controls the font size of the edge labels (default = 0.8)
         fixedStyle = 1,             # make all lines have lty = 1 (solid line)
         asize = 2.4)                # Size of the arrowhead


# path diagram with unstandardized estimates
semPaths(cfa.fit, 
         what = "est",
         nCharNodes = 0,             # Number of characters to abbreviate node labels to
         edge.color = "black",       # A value indicating the color of all edges
         edge.label.cex = 1,         # Controls the font size of the edge labels (default = 0.8)
         fixedStyle = 1,             # make all lines have lty = 1 (solid line)
         asize = 2.4,                # Size of the arrowhead
         weighted = FALSE)           # weighted graph?


# path diagram with no fitted values
semPaths(cfa.fit, 
         what = "path",              # display the model without estimates
         nCharNodes = 0, 
         label.cex = 1.2, 
         edge.color = "black", 
         edge.label.cex = 1,
         fixedStyle = 1,
         asize = 2.4,
         style = "lisrel")


# Fitted values -----------------------------------------------------------

# Use the fitted function to generare the fitted covariance matrix
fitted(cfa.fit)

# The model for generating this matrix is LPL' + U
# We can extract L, P and U using the lavTech function with what = "est"
modMatrices <- lavTech(cfa.fit, what = "est")
L <- modMatrices$lambda  # loadings (unstandardized estimates)
P <- modMatrices$psi     # factor variance/covariance matrix
U <- modMatrices$theta   # manifest variables variance

# %*% means matrix multiplication
# t() means transpose
L %*% P %*% t(L) + U

# This is merely nice to know; you usually won't need to do this.


# YOUR TURN!

# 1. Create a path diagram of the psych.fit model. Try to interpret what it means.




# Back to the presentation

# Evaluating model fit ----------------------------------------------------

# goodness-of-fit measures

# Brown (2006) recommends at least looking at these
fitMeasures(cfa.fit, fit.measures = c("srmr","rmsea", "cfi", "tli"))

# want SRMR and RMSEA close to 0 (close to 0.08 and 0.06 or lower, respectively)
# want CFI and TLI close to 1 (close to 0.95 or higher)

# None of these look particularly good.

# Can also output goodness-of-fit measures in summary 
summary(cfa.fit,  fit.measures = TRUE)


# residuals
residuals(cfa.fit, type="normalized")

# Appears to be three areas of possible "strain"

# modification indices
modificationIndices(cfa.fit)

# Looks like adding "Reason =~ general" to our model might help

cfa.mod1 <- '
Verbal =~ general + reading + vocab
Reason =~ general + picture + blocks + maze
'

# Fit the model to ability.cov$cov
cfa.fit1 <- cfa(model = cfa.mod1, sample.cov = ability.cov$cov, sample.nobs = ability.cov$n.obs)

# summary of model
summary(cfa.fit1, standardize = TRUE)

# difference between observed and fitted covariance matrix
residuals(cfa.fit1, type = "normalized")

# Appears to be no areas of "strain"

# goodness-of-fit measures
fitMeasures(cfa.fit1, fit.measures = c("srmr","rmsea", "cfi", "tli"))

# want SRMR and RMSEA close to 0
# want CFI and TLI close to 1

# modification indices
modificationIndices(cfa.fit1)

# Add "picture ~~ maze" to model?

# path diagram of model
semPaths(cfa.fit1, 
         what = "std",
         style = "lisrel", 
         nCharNodes = 0,   
         edge.color = "black", 
         edge.label.cex = 1, 
         fixedStyle = 1, 
         asize = 2.4,
         weighted = FALSE)


# Let's look again at the modification indices for the original model
modificationIndices(cfa.fit)

# It appears that adding "reading ~~ vocab" to would improve model fit
# ie, estimate covariance between reading and vocab


cfa.mod2 <- '
Verbal =~ general + reading + vocab
Reason =~ picture + blocks + maze
reading ~~ vocab
'

# Fit the model to ability.cov$cov
cfa.fit2 <- cfa(model = cfa.mod2, sample.cov = ability.cov$cov, sample.nobs = ability.cov$n.obs)

# summary of model
# Minimum Function Test Statistic is much lower
summary(cfa.fit2, standardize = TRUE)

# difference between observed and fitted covariance matrix
residuals(cfa.fit2, type = "normalized")

# goodness-of-fit measures
fitMeasures(cfa.fit2, fit.measures = c("srmr","rmsea", "cfi", "tli"))

# want SRMR and RMSEA close to 0
# want CFI and TLI close to 1

# modification indices
modificationIndices(cfa.fit2)

# path diagram of model
semPaths(cfa.fit2, 
         what = "std",
         style = "lisrel", 
         nCharNodes = 0,   
         edge.color = "black", 
         edge.label.cex = 1, 
         fixedStyle = 1, 
         asize = 2.4,
         weighted = FALSE)

# EQUIVALENT CFA SOLUTIONS

# Brown (2006): "a quandary often faced by applied CFA researchers..."

# Notice these two different models produce virtually identical goodness of fit
# (with the same number of df) and predicted covariance matrices.

fitMeasures(cfa.fit1, fit.measures = c("chisq", "df", "srmr","rmsea", "cfi", "tli"))
fitMeasures(cfa.fit2, fit.measures = c("chisq", "df", "srmr","rmsea", "cfi", "tli"))

fitted(cfa.fit1)$cov - fitted(cfa.fit2)$cov

# Says Brown (2006): "models with fewer dfs have a greater number of
# alternative, equivalent solutions than more parsimonious models."

# Which is "better"? Which makes more sense?

# YOUR TURN

# 1. Check the fit measures of psych.fit


# 2. Check the modification indices of psych.fit


# 3. Fit a new model that includes a covariance parameter for N3 and N4. Call
# the new model "psych.mod2" and the new fit "psych.fit2".


# 4. View the summary. How do the estimates for N3 ~~ N4 compare to what was
# reported in the modification indices?




# Back to the presentation.


# A few more CFA topics ---------------------------------------------------

# Assessing Normality example
# data from Ch 9 of Brown
Data.not.norm <- read.csv("https://github.com/clayford/CFA_workshop/raw/master/data/cfa_non_ml.csv")
summary(Data.not.norm)

# pairwise scatter plots
pairs(Data.not.norm)
# add some jitter
pairs(lapply(Data.not.norm, jitter))

# for Mardia's Multivariate Normality Test
library(MVN)
result <- mvn(Data.not.norm, mvnTest = "mardia", multivariatePlot = "qq")
result

# univariate tests
# univariate nonnormality ensures multivariate non-normality.
lapply(Data.not.norm, shapiro.test)

# CFA for non-normal data
model <- '
  f1 =~ x1 + x2 + x3 + x4 + x5
'
# use estimator = "MLM"
fit.mlm <- cfa(model, data = Data.not.norm, estimator = "MLM")
summary(fit.mlm, fit.measures = TRUE)

# Notice we get both ML and MLM estimates for goodness-of-fit measures

# missing data example
# from Ch 9 of Brown
Data.miss <- read.csv("https://github.com/clayford/CFA_workshop/raw/master/data/cfamiss.csv")
summary(Data.miss)

# What percent of records have missing data?
mean(apply(Data.miss, 1, function(x)any(is.na(x))))

# analysis model
model <- '
  LV =~ s1 + s2 + s3 + s4
  s2 ~~ s4
'

# fit model as usual; records with missing data dropped
fit.ml <- cfa(model, data = Data.miss)
summary(fit.ml, fit.measures = TRUE)

# fit model with FIML
# Notice it also reports number of missing patterns
fit.fiml <- cfa(model, data = Data.miss, missing = "fiml")
summary(fit.fiml, fit.measures = TRUE)

# each row corresponds to a missing data pattern (1=observed, 0=missing)
lavInspect(fit.fiml, "patterns")

# proportion of observed data for pairs of variables
lavInspect(fit.fiml, "coverage")


# sample size planning
library(simsem)


# Specific hypothesized model
pop.model <- '
LV =~ 0.8*y1 + 0.7*y2 + 0.6*y3
LV ~~ 1*LV
y1 ~~ 0.4*y1
y2 ~~ 0.4*y2
y3 ~~ 0.4*y3'
# The analysis model we plan to fit
cfa.model <- '
LV =~ y1 + y2 + y3'

# simulate sample sizes from 10 to 30 by 10, simulating 50 datasets for each
# sample size.
ss.n <- sim(model = cfa.model, 
            n = rep(seq(10,30,10),50), 
            generate = pop.model, 
            std.lv=TRUE, 
            lavaanfun = "cfa")
pow <- getPower(ss.n)
findPower(pow, "N", 0.80) # NA means no sample size achieved 80% power
plotPower(ss.n, powerParam = c("LV=~y1", "LV=~y2", "LV=~y3"))



# Say we know we'll only get 18 subjects:
# simulate 100 replications of a sample of size 18
ss.n2 <- sim(nRep = 100, 
             model = cfa.model, 
             n = 18, 
             generate = pop.model, 
             std.lv=TRUE, 
             lavaanfun = "cfa")
getPower(ss.n2)
# nothing to plot


# Overidentification vs Underidentification

# With p variables we can estimate up to p(p+1)/2 parameters
nrow(ability.cov$cov)
p <- nrow(ability.cov$cov)
p * (p + 1) / 2

# Example of trying to estimate 21 parameters

mod.under <- '
Verbal =~ general + reading + vocab
Reason =~ general + picture + blocks + maze
general ~~ reading
general ~~ vocab
reading ~~ vocab
picture ~~ blocks
picture ~~ maze
blocks ~~ maze
general ~~ maze
'


# Fit the model to ability.cov$cov
# Notice the warning
cfa.fit.under <- cfa(model = mod.under, sample.cov = ability.cov$cov, sample.nobs = ability.cov$n.obs)

# summary of model
# No standard errors computed
# Degrees of freedom = 0
summary(cfa.fit.under, standardize = TRUE)

# diabolical path diagram of model with 21 parameters
semPaths(cfa.fit.under, 
         what = "est",
         style = "ram", 
         layout = "circle",
         nCharNodes = 0,   
         edge.color = "black", 
         edge.label.cex = 0.6, 
         fixedStyle = 1, 
         asize = 2.4,
         weighted = FALSE)


# Appendix ----------------------------------------------------------------

# Creating model with labels for first path diagram shown in presentation

cfa.model2 <- '
# loadings, or coefficients
Verbal =~ a*general + b*reading + c*vocab
Reason =~ d*picture + e*blocks + f*maze

# correlation between factors
Verbal ~~ g*Reason

# error variance
general ~~ u*general
reading ~~ v*reading
vocab ~~ w*vocab
picture ~~ x*picture
blocks ~~ y*blocks
maze ~~ z*maze

# factor variances
Verbal ~~ s*Verbal
Reason ~~ t*Reason
'

cfa.fit2 <- cfa(model = cfa.model2, sample.cov = ability.cov$cov, sample.nobs = 112)

# path diagram with no fitted values
# ie, the hypothesized model
semPaths(cfa.fit2, 
         #          style = "lisrel",
         nCharNodes = 0, 
         label.cex = 1.2, 
         edge.color = "black", 
         edge.label.cex = 1,
         fixedStyle = 1,
         asize = 2.4)
