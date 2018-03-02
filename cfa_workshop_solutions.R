# cfa workshop "your turn" solutions

# YOUR TURN!

# Data from Brown (2006), Ch 4. 

# A researcher has collected eight personality measures from a sample of 250
# psychotherapy outpatients. All variables measured on scales ranging from 0 to
# 32 (higher scores reflect higher levels of the assessed dimension). Below we
# read in the covariance matrix of these measures.

# Read in the covariance matrix:
psych.cov <- as.matrix(read.csv("http://people.virginia.edu/~jcf2d/data/psychotherapy.csv"))
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
psych.mod <- 'Neuroticism =~ N1 + N2 + N3 + N4
Extraversion =~ E1 + E2 + E3 + E4'

# 2. fit the model using the cfa() function. Call it "psych.fit"
psych.fit <- cfa(model = psych.mod, sample.cov = psych.cov, sample.nobs = 250)

# 3. View the summary of psych.fit; use standardize = TRUE
summary(psych.fit, standardize = TRUE)

# 4. Examine the fitted covariance matrix and the residuals
fitted(psych.fit)
residuals(psych.fit)


# YOUR TURN!

# 1. Create a path diagram of the psych.fit model. Try to interpret what it means.

semPaths(psych.fit, 
         what = "std",
         weighted = FALSE,           
         style = "lisrel",           
         nCharNodes = 5,             # Number of characters to abbreviate node labels
         edge.color = "black",       # A value indicating the color of all edges
         edge.label.cex = 1,         # Controls the font size of the edge labels (default = 0.8)
         fixedStyle = 1,             # make all lines have lty = 1 (solid line)
         asize = 2.4)                # Size of the arrowhead



# YOUR TURN

# 1. Check the fit measures of psych.fit
fitMeasures(psych.fit, fit.measures = c("srmr","rmsea", "cfi", "tli"))

# 2. Check the modification indices of psych.fit
modificationIndices(psych.fit)

# 3. Fit a new model that includes a covariance parameter for N3 and N4. Call
# the new model "psych.mod2" and the new fit "psych.fit2".
psych.mod2 <- 'Neuroticism =~ N1 + N2 + N3 + N4
Extraversion =~ E1 + E2 + E3 + E4
N3 ~~ N4'
psych.fit2 <- cfa(psych.mod2, sample.cov = psych.cov, sample.nobs = 250)

# 4. View the summary. How do the estimates for N3 ~~ N4 compare to what was
# reported in the modification indices?
summary(psych.fit2, standardize = TRUE)
fitMeasures(psych.fit, fit.measures = c("chisq")) - fitMeasures(psych.fit2, fit.measures = c("chisq"))


