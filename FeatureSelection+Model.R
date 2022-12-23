################################# Question 1 ###################################

# Load packages and data
library("dplyr")
library("gmodels")
library("car")
library("forcats")
library("flexclust")
library("gplots")
library("rpart")
library("rpart.plot")
library("nnet")
library("foreign")
library("corrplot")
library("randomForest")
library("pdp")
library("ggplot2")
library("effects")
source("BCA_functions_source_file.R")

# Read in data, deem empty strings as NAs
rrsp <- read.csv("vcRRSP.csv", stringsAsFactors = T, na.strings = "")

# Create 1:1 Estimation Sample and Validation Sample (without Holdout Sample)
rrsp$Sample <- create.samples(rrsp, est = 0.5, val = 0.5, rand.seed = 1)

# Mental Map
#          What are important determinants of whether an individual will
#          purchase RRSP?
#
#          -> Capability of RRSP purchases and Needs of an RRSP account:
#               Age; BALCHQ; BALSAV; TOTDEP; BALLOAN; BALLOC; BALMRGG; NEWLOC;
#               NEWMRGG; TXPOS; TXCHQ; TXWEB; TXTEL; avginc_1; avginv_1;
#
#          -> Intimacy with Vancity (why Vancity):
#               atmcrds; paydep; TOTDEP; TXBRAN; TXATM; TXPOS; TXCHQ; TXWEB; TXTEL;
#               TOTSERV; CH_NM_SERV; CH_NM_PRD;
#
#          -> Exposure to the product/sales (spontaneous purchase/pushed sales):
#               TXBRAN; TXTEL; TOTSERV; CH_NM_SERV; CH_NM_PRD; numrr_1; numcon_1;
#
#
#          Other Considerations (specific ones):
#
#          1. Income and age as given by the context (StatCan):
#          Important for 2 reasons. (1) People are typically earning most at their
#          mid stages of life. They want to take advantage of RRSP to defer tax
#          (the more you put into RRSP account, the less reported income is and
#          the less tax to pay). (2) They can defer tax for capital gains or 
#          appreciation if they invest using money in RRSP accounts.
#          
#          2. First-time home buyer plan: RRSP withdrawal for first time home
#          buyers are completely tax-free. Similarly, Lifelong Learning Plan
#          says that RRSP withdrawal to finance your training or education
#          is tax-free. (No available data, but can be one future improvement.)
#          
#          3. Occupation, Education, Financial Knowledge, Investment Experience
#          
#          4. Exposure to the product knowledge: the more often interacting
#          with Vancity, the more opportunity they are exposed to knowing the
#          benefits of RRSP and receiving sales/promotions from financial advisor.
#          The greater intimacy/relationship between the bank and consumer,
#          the more likely consumer will purchase RRSP with Vancity
#
#          5. Strong Liquidity (Cash Flow) and assets (Debts and Credits):
#          The more in-flow and less debts one has, the more likely he/she is
#          capable of or in need of an RRSP account.


# Examine and Clean Data


###### Check variable summary ######
variable.summary(rrsp)


###### Look for potential problems with NAs ######

# Particularly problematic variables (>3%): numrr_1, numcon_1, BALCHQ, BALSAV, BALLOC, BALMRGG, BALLOAN
# Somewhat problematic variables (<3% & >0%): CH_NM_SERV, CH_NM_PRD, valsegm, pcode, avginc_1, avginv_1, N_IND_INC_


# Why are numrr_1 and numcon_1 missing? Is it meaningful missing or due to non-reporting issues?

# The number of RRSP contributors and the number of RRSP contributions have the same
# missing rate of 8.5322896%, indicating that the reasons for missing these values
# are the same. One reason is that some of these records have their postal code missing.
# Therefore, they could not be linked to community-level data. We will omit these rows later.
# If we check the range of numrr_1, we find that the min is 20. So another possibility
# of missing values is that there is no RRSP contributor in this postal code
# neighborhood, and thus the number of contribution is N/A. Therefore, except
# those who do not have a postal code on file, it's reasonable to assume that
# these NAs are meaningful (i.e., no one holds RRSP in this community).

# Remove cases without postal codes to avoid misimputation of numcon_1 and numrr_1
# Also, by examining the data, we found that records without pcode tend to have
# many other features missing. So removing the 70 observations is a safe move.
rrsp <- rrsp[!is.na(rrsp$pcode),]

# Before we remove those records with postal code missing (1.3698630% or 70 records),
# we first impute zero for missing numrr_1 and numcon_1, indicating that there is no
# contributor in the area and thus no contribution. Note that there are also 5
# incidents where their community has 160 contributors but 0 contributions. Therefore,
# an option to distinguish these zeros from the imputed zeros is to add a dummy indicator
# variable. However, since there are only 5 such observations, we would not do it in this case.

rrsp$numcon_1 <- if_else(is.na(rrsp$numcon_1), 0L, rrsp$numcon_1)
rrsp$numrr_1 <- if_else(is.na(rrsp$numrr_1), 0L, rrsp$numrr_1)

# Why are Balance variables missing?

# If we check the ranges for these variables we can find that the minimal amount
# of BALCHQ, BALSAV, and BALLOC is 0, meaning that they have a CHE/SAV/LOC account
# but have a balance of 0. For those having NAs, it's reasonable to assume that
# they are meaningful missing:
# these individuals do not have a chequing/saving/credit line account. Similarly,
# a mortgage/loan valued 0 does not make sense (just close the account). So we
# assume that the NAs for BALLOAN and BALMRGG are meaningful  missing: these
# individuals do not have a loan or mortgage. Therefore, we can impute zero for
# these missing values but we will add dummy indicators to distinguish having
# an account or not.

rrsp$BALCHQ_IND <- if_else(is.na(rrsp$BALCHQ), 1, 0) # Add dummy variable first
rrsp$BALCHQ <- if_else(is.na(rrsp$BALCHQ),0,rrsp$BALCHQ) # Impute zero

rrsp$BALSAV_IND <- if_else(is.na(rrsp$BALSAV), 1, 0)
rrsp$BALSAV <- if_else(is.na(rrsp$BALSAV),0,rrsp$BALSAV)

rrsp$BALLOC_IND <- if_else(is.na(rrsp$BALLOC), 1, 0)
rrsp$BALLOC <- if_else(is.na(rrsp$BALLOC),0,rrsp$BALLOC)

rrsp$BALLOAN_IND <- if_else(is.na(rrsp$BALLOAN), 1, 0)
rrsp$BALLOAN <- if_else(is.na(rrsp$BALLOAN),0,rrsp$BALLOAN)

rrsp$BALMRGG_IND <- if_else(is.na(rrsp$BALMRGG), 1, 0)
rrsp$BALMRGG <- if_else(is.na(rrsp$BALMRGG),0,rrsp$BALMRGG)

# The variable pcode has a thin factor level of 1. It's statistically irrelevant.
# So remove it.
rrsp$pcode <- NULL

# The variable valsegm has a thin factor level of 55. Use the following codes
# to get an insight into this variable.
rrsp %>%
  group_by(valsegm) %>%
  summarise(num_row = length(valsegm))
# The results show that the level with 55 observations contains NAs. The NAs are
# most likely due to non-reporting issues and do not have a reasonable value
# for imputation. Moreover, they only constitute about 1% of total observations.
# And the records missing valsegm tend to have many NAs in other variables too.
# Therefore, we will remove these records later using na.omit().


# There is no unary variable.


###### Trivially related variables/Target Leakage ######

# Discussion:
# Could the variable valsegm potentially be a trivially related variable?
# Depending on whether an RRSP purchase would increase the rank or not.
# This possibility should be determined in the subsequent analysis. Happily,
# in later analysis we show that valsegm is not a trivially related variable.

# CH_NM_SERV and CH_NM_PRD are trivially related variables because RRSP purchases
# will cause changes in these two predictors.
rrsp$CH_NM_PRD <- NULL
rrsp$CH_NM_SERV <- NULL

# The variable TOTSERV is a trivially related variable because it is most likely
# measured after an individual has purchased an RRSP term, resulting an increase
# of 1 in TOTSERV. Another concern is whether people obtain more services as a
# result of an RRSP purchase (bundled sales)? Or, how we can determine if there
# is no causality other than just an increase of 1 from APURCH to TOTSERV?
#
# Before answering this question, we believe that TOTSERV should be a strong
# predictor of showing a customer's intimacy, exposure, product knowledge, and
# financial experience and more. For example, the more product a current client
# has, the more likely that the client would make an RRSP purchase. Additionally,
# since the business objective is to cross-sell to "EXISTING VANCITY CLIENTS", 
# it's likely that the existing clients have at least 1 product line. So it's
# likely that an increase by 1 in TOTSERV due to purchase would not change the
# fact of TOTSERV being a good predictor.
# 
# With these considerations in mind, we would like to test if, using some
# transformation, we can convert TOTSERV from a trivially related variable into
# a powerful predictor. To do this, we would assume that TOTSERV only increases
# by 1 if an individual purchases an RRSP, and that TOTSERV is reflecting
# only the purchase of RRSP terms.By this assumption, we can now form a new
# feature called TOTSERV.NEW to hold the values of "the total number of services
# and/or product lines an individual has BEFORE the purchase of an RRSP". The
# values are obtained by the following algorithm:
#
# If (purchase == Yes) && (TOTSERV >= 1), then (TOTSERV.NEW = TOTSERV - 1).
# If (purchase == Yes) && (TOTSERV < 1), something wrong but only 1 case, set 0
# If (purchase == NO), then (TOTSERV.NEW = TOTSERV)
#
rrsp$TOTSERV.NEW <- if_else(rrsp$APURCH == "Y",
                            if_else(rrsp$TOTSERV < 1, 0, rrsp$TOTSERV - 1),
                            rrsp$TOTSERV)



# The variable "unique" is a record-keeping identification number
row.names(rrsp) <- rrsp$unique
rrsp$unique <- NULL



# Gender takes two columns but can be reduced to one:
rrsp$gendf <- NULL # gendm = 0 means female, gendm = 1 means male

# The variable N_IND_INC_ is statistically irrelevant. Remove it.
rrsp$N_IND_INC_ <- NULL

# There are many variables that should be of a factor class (optional)
rrsp$paydep <- factor(rrsp$paydep)
rrsp$gendm <- factor(rrsp$gendm)
rrsp$atmcrd <- factor(rrsp$atmcrd)
rrsp$NEWLOC <- factor(rrsp$NEWLOC)
rrsp$NEWMRGG <- factor(rrsp$NEWMRGG)
rrsp$BALCHQ_IND <- factor(rrsp$BALCHQ_IND)
rrsp$BALSAV_IND <- factor(rrsp$BALSAV_IND)
rrsp$BALLOC_IND <- factor(rrsp$BALLOC_IND)
rrsp$BALLOAN_IND <- factor(rrsp$BALLOAN_IND)
rrsp$BALMRGG_IND <- factor(rrsp$BALMRGG_IND)

# glimpse(rrsp)
# variable.summary(rrsp)

###### Final Checks ######
sapply(select_if(rrsp,is.numeric),summary)
# Two observations have age = 0. Remove these two strange cases.
rrsp <- rrsp[rrsp$age != 0,]

# For variables that have a missing rate of 0.95~1.62%, removing these
# records does not lose as much good information because these are individuals
# with NAs across many variables. Using na.omit(), we only lose another 68
# observations, or about 1.35% of 5038 observations, which is acceptable.
# In total, we loss 70 (with missing pcode) + 68 (others) + 2 (aged 0) = 140 cases
# from the original dataset. or about 2.74% of 5110 observations, which is acceptable.
variable.summary(rrsp) # 5038 observations
rrsp2 <- na.omit(rrsp)
glimpse(rrsp2) # 4970 observations
variable.summary(rrsp2)


# Main task: test if TOTSERV.NEW is a trivially related variable.

# [10.3.4] Explore with Forests and Regression

###### Random Forest ######
# paste(names(rrsp2), collapse = " + ")
rrspForestAll <- randomForest(formula = APURCH ~ age + gendm + atmcrd + paydep +
                                BALCHQ + BALSAV + TOTDEP + BALLOAN + BALLOC +
                                BALMRGG + NEWLOC + NEWMRGG + TXBRAN + TXATM +
                                TXPOS + TXCHQ + TXWEB + TXTEL + TOTSERV.NEW +
                                valsegm + numrr_1 + numcon_1 + avginc_1 + avginv_1 +
                                BALCHQ_IND + BALSAV_IND + BALLOAN_IND +
                                BALLOC_IND + BALMRGG_IND,
                              data = filter(rrsp2, Sample =="Estimation"),
                              importance = TRUE,
                              ntree = 500, mtry = 4)
# Confusion Table
rrspForestAll[["confusion"]]
# Misclassification rate from the internal “out of bag” validation set is 0.2229455
# for “N” and 0.1841270 for “Y”. It means that the overall hit rate is 79.67% or
# an error rate of 20.33%. That’s a decent improvement over the 50% hit rate we
# would get without a model or, equivalently, the 50% response rate we would get
# if we had to randomly target individuals in a 50/50 database.

# Plot the relative importance of the variables:
varImpPlot(rrspForestAll,type = 2,
           main="rrspForestAll", # title
           cex =0.7) # font size

# The TOTSERV.NEW is strongly predictive, indicating some potential issues here.
# But we will just continue until the lift charts.

# For the APURCH target, we confirm that the target level being predicted is "Y".
levels(rrsp2$APURCH)

# PDP's
# decreasing at a decreasing rate
partial(rrspForestAll, pred.var = "TOTSERV.NEW", # target and predictor
        prob = TRUE, # probabilities on yaxis
        which.class = 2, # predict level 2, "Y"
        plot = TRUE, # generate plot
        rug = TRUE, # plot decile hashmarks
        plot.engine = "ggplot2")

# Extremely right-skewed! After trim, find increasing at decreasing rate
partial(rrspForestAll, pred.var = "TOTDEP", # target and predictor
        prob = TRUE, # probabilities on yaxis
        which.class = 2, # predict level 2, "Y"
        plot = TRUE, # generate plot
        rug = TRUE, # plot decile hashmarks
        plot.engine = "ggplot2")

# Extremely right-skewed! After trim, find increasing at slight decreasing rate
partial(rrspForestAll, pred.var = "BALCHQ", # target and predictor
        prob = TRUE, # probabilities on yaxis
        which.class = 2, # predict level 2, "Y"
        plot = TRUE, # generate plot
        rug = TRUE, # plot decile hashmarks
        plot.engine = "ggplot2")

# decreasing at an increasing rate
partial(rrspForestAll, pred.var = "age", # target and predictor
        prob = TRUE, # probabilities on yaxis
        which.class = 2, # predict level 2, "Y"
        plot = TRUE, # generate plot
        rug = TRUE, # plot decile hashmarks
        plot.engine = "ggplot2")

# Extremely right-skewed! After trim, find somehow U-shaped or linearly increasing
partial(rrspForestAll, pred.var = "avginc_1", # target and predictor
        prob = TRUE, # probabilities on yaxis
        which.class = 2, # predict level 2, "Y"
        plot = TRUE, # generate plot
        rug = TRUE, # plot decile hashmarks
        plot.engine = "ggplot2")

# Extremely right-skewed. After trim, find increasing at a decreasing rate
partial(rrspForestAll, pred.var = "TXBRAN", # target and predictor
        prob = TRUE, # probabilities on yaxis
        which.class = 2, # predict level 2, "Y"
        plot = TRUE, # generate plot
        rug = TRUE, # plot decile hashmarks
        plot.engine = "ggplot2")

# Extremely right-skewed, lots of fluctuation
partial(rrspForestAll, pred.var = "avginv_1", # target and predictor
        prob = TRUE, # probabilities on yaxis
        which.class = 2, # predict level 2, "Y"
        plot = TRUE, # generate plot
        rug = TRUE, # plot decile hashmarks
        plot.engine = "ggplot2")

# Extremely right-skewed
partial(rrspForestAll, pred.var = "numcon_1", # target and predictor
        prob = TRUE, # probabilities on yaxis
        which.class = 2, # predict level 2, "Y"
        plot = TRUE, # generate plot
        rug = TRUE, # plot decile hashmarks
        plot.engine = "ggplot2")

# Extremely right-skewed, inverted U shape
partial(rrspForestAll, pred.var = "TXCHQ", # target and predictor
        prob = TRUE, # probabilities on yaxis
        which.class = 2, # predict level 2, "Y"
        plot = TRUE, # generate plot
        rug = TRUE, # plot decile hashmarks
        plot.engine = "ggplot2")

# Extremely right-skewed, decreasing
partial(rrspForestAll, pred.var = "numrr_1", # target and predictor
        prob = TRUE, # probabilities on yaxis
        which.class = 2, # predict level 2, "Y"
        plot = TRUE, # generate plot
        rug = TRUE, # plot decile hashmarks
        plot.engine = "ggplot2")

# Trim the top 10%
# TOTDEP: Strong concavity or diminishing marginal returns
rrspForestAll.trim <- partial(rrspForestAll, pred.var = "TOTDEP",
                              prob = TRUE,
                              which.class = 2,
                              quantiles = TRUE, # prepare data trimming
                              probs = seq(from = 0.0, to = 0.9, by = 0.02), # of bottom 90%
                              plot= FALSE) # generate data, no plot
plotPartial(rrspForestAll.trim, # and pass data to plotting function
            rug = TRUE,
            train = filter(rrsp2, Sample == "Estimation"))

# BALCHQ: diminishing marginal returns
rrspForestAll.trim <- partial(rrspForestAll, pred.var = "BALCHQ",
                              prob = TRUE,
                              which.class = 2,
                              quantiles = TRUE, # prepare data trimming
                              probs = seq(from = 0.0, to = 0.9, by = 0.02), # of bottom 90%
                              plot= FALSE) # generate data, no plot
plotPartial(rrspForestAll.trim, # and pass data to plotting function
            rug = TRUE,
            train = filter(rrsp2, Sample == "Estimation"))

# avginc_1: Convexity (U shape)
rrspForestAll.trim <- partial(rrspForestAll, pred.var = "avginc_1",
                              prob = TRUE,
                              which.class = 2,
                              quantiles = TRUE, # prepare data trimming
                              probs = seq(from = 0.0, to = 0.9, by = 0.02), # of bottom 90%
                              plot= FALSE) # generate data, no plot
plotPartial(rrspForestAll.trim, # and pass data to plotting function
            rug = TRUE,
            train = filter(rrsp2, Sample == "Estimation"))

# TXBRAN: Increasing at decreasing rate
rrspForestAll.trim <- partial(rrspForestAll, pred.var = "TXBRAN",
                              prob = TRUE,
                              which.class = 2,
                              quantiles = TRUE, # prepare data trimming
                              probs = seq(from = 0.0, to = 0.9, by = 0.02), # of bottom 90%
                              plot= FALSE) # generate data, no plot
plotPartial(rrspForestAll.trim, # and pass data to plotting function
            rug = TRUE,
            train = filter(rrsp2, Sample == "Estimation"))

# avginv_1: Convexity (U shape)
rrspForestAll.trim <- partial(rrspForestAll, pred.var = "avginv_1",
                              prob = TRUE,
                              which.class = 2,
                              quantiles = TRUE, # prepare data trimming
                              probs = seq(from = 0.0, to = 0.9, by = 0.02), # of bottom 90%
                              plot= FALSE) # generate data, no plot
plotPartial(rrspForestAll.trim, # and pass data to plotting function
            rug = TRUE,
            train = filter(rrsp2, Sample == "Estimation"))

# Categorical variables:
# Only one categorical variables and is not strong but we can glimpse a bit
partial(rrspForestAll, pred.var = "valsegm",
        which.class = 2,
        plot = TRUE,
        rug = TRUE, plot.engine = "ggplot2",
        prob = TRUE)
# The plots of the effect of valsegm degrees indicate shows a decreasing
# pattern. Group E has the lowest effect and can be used as the base level
# for this variable.
# set the base levels for valsegm as E
rrsp2$valsegm <- relevel(rrsp2$valsegm, "E")
levels(rrsp2$valsegm)


###### Logistic Regression ######

# Correlation Matrix
# Select numeric columns only, then calculate and print correlation coefficients
corrMatrix <- cor(select_if(rrsp2, is.numeric)) # see ?dplyr::select_if
# temporarily reduce the number of output digits for easier inspection
options(digits = 2)
corrMatrix
options(digits = 7) # then reset output digits
# Visualize correlation
corrplot(corrMatrix,method="number",type="lower",
         diag = FALSE,number.cex = 0.5)

# Variable pairs with correlations of around 0.65 or greater (in absolute value)
# are worth keeping in mind when comparing the two models. There are only 2 such
# pairs (ignoring the TOTSERV variable): avginc/avginv and numrr/numcon. We would
# expect that the logistic model would not identify both numcon_1 and numrr_1 as
# highly significant, whereas in the random forest importance plot, they are 8th
# and 10th. Similarly, avginc_1 and avginv_1 would not appear both highly significant
# while they are the 5rd and 6th important variable.

# Make a table to highly correlated variables:
# numcon_1 x numrr_1      0.99
# avginv_1 x avginc_1     0.71


# Now we are ready to run the “maximal” logistic regression model

# Create a logistic regression model
rrspLogis <- glm(formula = APURCH ~ age + gendm + atmcrd + paydep + BALCHQ +
                  BALSAV + TOTDEP + BALLOAN + BALLOC + BALMRGG + NEWLOC +
                  NEWMRGG + TXBRAN + TXATM + TXPOS + TXCHQ + TXWEB + TXTEL +
                  TOTSERV.NEW + valsegm + numrr_1 +
                  numcon_1 + avginc_1 + avginv_1 + BALCHQ_IND + BALSAV_IND +
                  BALLOAN_IND + BALLOC_IND + BALMRGG_IND,
                data = filter(rrsp2, Sample =="Estimation"),
                family = binomial(logit))
# Print summary
summary(rrspLogis) # AIC = 1586.6

# Calculate and print McFadden R square (See Logistic Regression Chapter)
MR2 <- 1 - (rrspLogis$deviance / rrspLogis$null.deviance)
MR2.3 <- round(MR2,digits = 3)
print(paste("McFadden Rsquared: ",MR2.3)) # 0.559

# Check target variable level for logistic model
# Happily, since “N” precedes “Y” alphabetically, the base level 1 is “N” and
# thus the probabilities for level 2 “Y” are calculated, and we can interpret
# the signs of the coefficients relative to the probability of purchasing an RRSP.

# Run a stepwise regression using the "rrspLogis" model
rrspStep <- step(rrspLogis, direction = "both")
summary(rrspStep) # AIC = 1568.6

# McFadden R2
MR2.step <- 1 - (rrspStep$deviance / rrspStep$null.deviance)
MR2.step.3 <- round(MR2.step,digits = 3)
print(paste("McFadden Rsquared: ",MR2.step.3)) # 0.557


# Compare both models using cumulative Lift Chart
lift.chart(modelList = c("rrspLogis","rrspStep"),
           data = filter(rrsp2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022, type = "cumulative",
           sub = "Validation")
# Extraordinary fit. Problematic!

# Compare forests and stepwise logistic regression models
lift.chart(modelList = c("rrspStep","rrspForestAll"),
           data = filter(rrsp2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022, type = "cumulative",
           sub = "Validation")
# The lift charts show extraordinary fits, proving that TOTSERV is indeed a
# trivially related variable (i.e., target leakage). Therefore, we will remove
# this variable from both the forest and the regression model.


# Now we can remove both TOTSERV and TOTSERV.NEW from our subsequent modelling
# Rework and overwrite the previous models
rrspForestAll <- randomForest(formula = APURCH ~ age + gendm + atmcrd + paydep +
                                BALCHQ + BALSAV + TOTDEP + BALLOAN + BALLOC +
                                BALMRGG + NEWLOC + NEWMRGG + TXBRAN + TXATM +
                                TXPOS + TXCHQ + TXWEB + TXTEL + valsegm +
                                numrr_1 + numcon_1 + avginc_1 + avginv_1 +
                                BALCHQ_IND + BALSAV_IND + BALLOAN_IND +
                                BALLOC_IND + BALMRGG_IND,
                              data = filter(rrsp2, Sample =="Estimation"),
                              importance = TRUE,
                              ntree = 500, mtry = 4)

# Confusion Table
rrspForestAll[["confusion"]]
# Misclassification rate from the internal “out of bag” validation set is 0.39
# for “N” and 0.35 for “Y”. The overall hit rate is 63%, or equivalently, 
# the error rate is 37%. That’s only a 13% improvement over the 50% hit rate we
# would get without a model or the 50% response rate we would get
# if we had to randomly target individuals in a 50/50 database.

# Plot the relative importance of the variables:
varImpPlot(rrspForestAll,type = 2,
           main="rrspForestAll", # title
           cex =0.7) # font size
# Now the importance plot shows TOTDEP as the most predictive feature.

# Create a logistic regression model
rrspLogis <- glm(formula = APURCH ~ age + gendm + atmcrd + paydep + BALCHQ +
                   BALSAV + TOTDEP + BALLOAN + BALLOC + BALMRGG + NEWLOC +
                   NEWMRGG + TXBRAN + TXATM + TXPOS + TXCHQ + TXWEB + TXTEL +
                   valsegm + numrr_1 + numcon_1 + avginc_1 + avginv_1 +
                   BALCHQ_IND + BALSAV_IND + BALLOAN_IND + BALLOC_IND + BALMRGG_IND,
                 data = filter(rrsp2, Sample =="Estimation"),
                 family = binomial(logit))
# Print summary
summary(rrspLogis) # AIC = 3260.2

# Calculate and print McFadden R square (See Logistic Regression Chapter)
MR2 <- 1 - (rrspLogis$deviance / rrspLogis$null.deviance)
MR2.3 <- round(MR2,digits = 3)
print(paste("McFadden Rsquared: ",MR2.3)) # 0.074

# Run a stepwise regression using the "rrspLogis" model
rrspStep <- step(rrspLogis, direction = "both")
summary(rrspStep) # AIC = 3238.8

# McFadden R2
MR2.step <- 1 - (rrspStep$deviance / rrspStep$null.deviance)
MR2.step.3 <- round(MR2.step,digits = 3)
print(paste("McFadden Rsquared: ",MR2.step.3)) # 0.071


# Compare logistic and stepwise models using cumulative Lift Chart
lift.chart(modelList = c("rrspLogis","rrspStep"),
           data = filter(rrsp2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022, type = "cumulative",
           sub = "Validation")
# Results show that removing 15 variables (from 28 to 13) did not hurt predictability

# Compare forests and stepwise logistic regression models
lift.chart(modelList = c("rrspLogis","rrspStep","rrspForestAll"),
           data = filter(rrsp2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022, type = "cumulative",
           sub = "Validation")

# The forest is performing (much) better than the stepwise model on the
# validation set, indicating that the random forest is capturing some non-linear
# relationships that the logistic regression cannot. Now we need to examine why
# important variables in random forests are missing in Stepwise logistic regression.




###### Look for Improvement ######

###### Check for the needs of transformation ######
# Note: because the log of the mean does not equal the mean of the log transformed
# original variable, we will only transform those variables at individual level.

# Let's go back to the importance plot
varImpPlot(rrspForestAll,type = 2,
           main="rrspForestAll", # title
           cex =0.7) # font size
# Check what problems are embedded in the stepwise regression
summary(rrspStep)
rank((coef(summary(rrspStep))[,4])[-1])

# In the top 15 most important variable, only 7 are shared in common in both models:
# Check Appendix-Variable Selection

# Problems we need to engage with:
# 1. TOTDEP: not as significant as in the tree model (do log transformation)
#    This is seen in the PDP and/or plotmean.
# 2. avginc_1 and avginv_1: multicollinearity issue (do PCA)
#    This is seen in the correlation graph.
# 3. numcon_1 and numrr_1: multicollinearity issue  (do PCA)
#    This is seen in the correlation graph.
# 4. Why do some variables become so significant in stepwise model?
#    Try to find more non-linearities to make linear model...
#    In the following we try all BAL variables and TX variables.


# Problem 1:
#
# TOTDEP is ranked 1st in forest but has a p-value of just 0.012822 in regression.
# Check correlation?
corrplot(corrMatrix,method="number",type="lower",
         diag = FALSE,number.cex = 0.7)
# TOTDEP is not correlating with any variable.
# Check linearity?
# Trim the top 10%
# TOTDEP: Strong concavity or diminishing marginal returns
rrspForestAll.trim <- partial(rrspForestAll, pred.var = "TOTDEP",
                              prob = TRUE,
                              which.class = 2,
                              quantiles = TRUE, # prepare data trimming
                              probs = seq(from = 0.0, to = 0.9, by = 0.02), # of bottom 90%
                              plot= FALSE) # generate data, no plot
plotPartial(rrspForestAll.trim, # and pass data to plotting function
            rug = TRUE,
            train = filter(rrsp2, Sample == "Estimation"))

# Determine skewness - data is EXTREMELY right skewed. Plot of means may be less useful than PDP
hist(rrsp2$TOTDEP, col = "lightblue", breaks = 20)
# Try plot of means:
rrsp2$APURCH.NUM <- if_else(rrsp2$APURCH == "Y", 1, 0)
rrsp2$TOTDEP.Cat <- binVariable(rrsp2$TOTDEP, bins = 10,
                                      method = "proportions",
                                      labels = NULL)
plotmeans(APURCH.NUM ~ TOTDEP.Cat, data = rrsp2)
# Transform the variable TOTDEP by taking log, check if min value is 0
summary(rrsp2$TOTDEP)
# The summary shows that the minimum value of TOTDEP is 0. Add 1 to avoid log(0) problem
rrsp2$Log.TOTDEP <- log(rrsp2$TOTDEP + 1)
# Check Histogram, now it's normally distributed. We will use this variable later
hist(rrsp2$Log.TOTDEP, col = "lightblue", breaks = 20)
# In the next models, use logged TOTDEP.



# Problem 2,3: Multicollinearity
#
# Note that both numcon_1 and numrr_1 ranked 7th and 8th in random forest are
# both eliminated by the stepwise model because they are highly correlated (0.99).
# Similarly, avginc_1 is insignificant in the stepwise model but is ranked 4th in 
# the forest model. It may be because there is a strong correlation (0.71) between
# avginc_1 and avginv_1.
#
# We have 3 options:
# Option 1: we simply leave them to ANN,
# Option 2: remove one variable from each of these two highly correlated pairs,
# Option 3: do two PCA's to combine numcon_1+numrr_1 and avginc_1+avginv_1.
#
# PCAs retain the more information than the other two, so we will do PCAs on them
#
# Make sure to scale numcon_1 and numrr_1 by using "scale. = T"
PC.numcon_numrr <- prcomp(select(rrsp2,numcon_1,numrr_1), scale. = T)
summary(PC.numcon_numrr)
# PC1 explains 99.71% of variance.
# Add a new feature called PC.numcon.numrr to replace numcon_1 and numrr_1
rrsp2$PC.numcon.numrr <- PC.numcon_numrr$x[,1]

# Repeat on avginc/avginv
PC.avginc_avginv <- prcomp(select(rrsp2,avginc_1, avginv_1), scale = T)
summary(PC.avginc_avginv)
# PC1 explains 85.63% (so we lose 14.37% information) of variance.
# Replace avginc_1 and avginv_1 with PC1.
rrsp2$PC.avginc.avginv <- PC.avginc_avginv$x[,1]

# Check Non-Linearity of the 4 geo-demographical variables
par(mfrow = c(2,2))
rrsp2$avginc.Cat <- binVariable(rrsp2$avginc_1, bins = 6,
                                method = "proportions",
                                labels = NULL)
plotmeans(APURCH.NUM ~ avginc.Cat, data = rrsp2)

rrsp2$avginv.Cat <- binVariable(rrsp2$avginv_1, bins = 6,
                                method = "proportions",
                                labels = NULL)
plotmeans(APURCH.NUM ~ avginv.Cat, data = rrsp2)

rrsp2$numrr.Cat <- binVariable(rrsp2$numrr_1, bins = 6,
                                method = "proportions",
                                labels = NULL)
plotmeans(APURCH.NUM ~ numrr.Cat, data = rrsp2)

rrsp2$numcon.Cat <- binVariable(rrsp2$numcon_1, bins = 6,
                                method = "proportions",
                                labels = NULL)
plotmeans(APURCH.NUM ~ numcon.Cat, data = rrsp2)
par(mfrow = c(1,1))

# Problem 4: other numeric variables with possible non-linear relationships
#
# We are mainly interested in 5 BAL variables and 6 TX variables. We will take
# the log of all these predictors and do a trial and error.

# BAL variables
# Check if min values are 0's
sapply(rrsp2[,c(6:7,9:11)],summary)
# They are. Add 1 to avoid log(0) issue.
rrsp2$Log.BALCHQ <- log(rrsp2$BALCHQ + 1)
# hist(rrsp2$Log.BALCHQ, col = "lightblue", breaks = 20)
rrsp2$Log.BALSAV <- log(rrsp2$BALSAV + 1)
# hist(rrsp2$Log.BALSAV, col = "lightblue", breaks = 20)
rrsp2$Log.BALLOAN <- log(rrsp2$BALLOAN + 1)
# hist(rrsp2$Log.BALLOAN, col = "lightblue", breaks = 20)
rrsp2$Log.BALLOC <- log(rrsp2$BALLOC + 1)
# hist(rrsp2$Log.BALLOC, col = "lightblue", breaks = 20)
rrsp2$Log.BALMRGG <- log(rrsp2$BALMRGG + 1)
# hist(rrsp2$Log.BALMRGG, col = "lightblue", breaks = 20)

# TX variables
# Check if min values are 0. Add 1 if they do.
sapply(rrsp2[,c(14:19)],summary)
rrsp2$Log.TXBRAN <- log(rrsp2$TXBRAN + 1)
# hist(rrsp2$Log.TXBRAN, col = "lightblue", breaks = 20)
rrsp2$Log.TXATM <- log(rrsp2$TXATM + 1)
# hist(rrsp2$Log.TXATM, col = "lightblue", breaks = 20)
rrsp2$Log.TXPOS <- log(rrsp2$TXPOS + 1)
# hist(rrsp2$Log.TXPOS, col = "lightblue", breaks = 20)
rrsp2$Log.TXCHQ <- log(rrsp2$TXCHQ + 1)
# hist(rrsp2$Log.TXCHQ, col = "lightblue", breaks = 20)
rrsp2$Log.TXWEB <- log(rrsp2$TXWEB + 1)
# hist(rrsp2$Log.TXWEB, col = "lightblue", breaks = 20)
rrsp2$Log.TXTEL <- log(rrsp2$TXTEL + 1)
# hist(rrsp2$Log.TXTEL, col = "lightblue", breaks = 20)


###### Fixing the 4 problems ######

# Create 4 new logistic regression models

# Model rrspMixed1: Log.TOTDEP, PC of numcon/numrr, PC of avginc/avginv
rrspMixed1 <- glm(formula = APURCH ~ age + gendm + atmcrd + paydep + BALCHQ +
                   BALSAV + Log.TOTDEP + BALLOAN + BALLOC + BALMRGG + NEWLOC +
                   NEWMRGG + TXBRAN + TXATM + TXPOS + TXCHQ + TXWEB + TXTEL +
                   valsegm + PC.numcon.numrr + PC.avginc.avginv + BALCHQ_IND +
                   BALSAV_IND + BALLOAN_IND + BALLOC_IND + BALMRGG_IND,
                 data = filter(rrsp2, Sample =="Estimation"),
                 family = binomial(logit))
# Print summary
summary(rrspMixed1) # AIC = 3190.8

# Calculate and print McFadden R square (See Logistic Regression Chapter)
MR2.Mixed1 <- 1 - (rrspMixed1$deviance / rrspMixed1$null.deviance)
MR2.Mixed1.3 <- round(MR2.Mixed1,digits = 3)
print(paste("McFadden Rsquared: ",MR2.Mixed1.3)) # 0.093

# Run a stepwise regression using the "rrspMixed" model
rrspStep.Mixed1 <- step(rrspMixed1, direction = "both")
summary(rrspStep.Mixed1) # AIC = 3169

# McFadden R2
MR2.step.Mixed1 <- 1 - (rrspStep.Mixed1$deviance / rrspStep.Mixed1$null.deviance)
MR2.step.Mixed1.3 <- round(MR2.step.Mixed1,digits = 3)
print(paste("McFadden Rsquared: ",MR2.step.Mixed1.3)) # 0.087

# Leave performance comparison later...



# Model rrspMixed2: with log transformations given to the 5 BAL variables

rrspMixed2 <- glm(formula = APURCH ~ age + gendm + atmcrd + paydep + Log.BALCHQ +
                    Log.BALSAV + Log.TOTDEP + Log.BALLOAN + Log.BALLOC + Log.BALMRGG + NEWLOC +
                    NEWMRGG + TXBRAN + TXATM + TXPOS + TXCHQ + TXWEB + TXTEL +
                    valsegm + PC.numcon.numrr +
                    PC.avginc.avginv + BALCHQ_IND + BALSAV_IND +
                    BALLOAN_IND + BALLOC_IND + BALMRGG_IND,
                  data = filter(rrsp2, Sample =="Estimation"),
                  family = binomial(logit))
summary(rrspMixed2) # AIC = 3186.7 (Better than Mixed1)

# Calculate and print McFadden R square (See Logistic Regression Chapter)
MR2.Mixed2 <- 1 - (rrspMixed2$deviance / rrspMixed2$null.deviance)
MR2.Mixed2.3 <- round(MR2.Mixed2,digits = 3)
print(paste("McFadden Rsquared: ",MR2.Mixed2.3)) # 0.094

# Run a stepwise regression using the "rrspMixed2" model
rrspStep.Mixed2 <- step(rrspMixed2, direction = "both")
summary(rrspStep.Mixed2) # AIC = 3165.7 (Better than Step.Mixed1)

# McFadden R2
MR2.step.Mixed2 <- 1 - (rrspStep.Mixed2$deviance / rrspStep.Mixed2$null.deviance)
MR2.step.Mixed2.3 <- round(MR2.step.Mixed2,digits = 3)
print(paste("McFadden Rsquared: ",MR2.step.Mixed2.3)) # 0.091


# Model rrspMixed3: with log transformations given to the 6 TX variables

rrspMixed3 <- glm(formula = APURCH ~ age + gendm + atmcrd + paydep + Log.BALCHQ +
                    Log.BALSAV + Log.TOTDEP + Log.BALLOAN + Log.BALLOC +
                    Log.BALMRGG + NEWLOC + NEWMRGG + Log.TXBRAN + Log.TXATM +
                    Log.TXPOS + Log.TXCHQ + Log.TXWEB + Log.TXTEL +
                    valsegm + PC.numcon.numrr +
                    PC.avginc.avginv + BALCHQ_IND + BALSAV_IND +
                    BALLOAN_IND + BALLOC_IND + BALMRGG_IND,
                  data = filter(rrsp2, Sample =="Estimation"),
                  family = binomial(logit))
# Print summary
summary(rrspMixed3) # AIC = 3183 (Better than Mixed2)

# Calculate and print McFadden R square (See Logistic Regression Chapter)
MR2.Mixed3 <- 1 - (rrspMixed3$deviance / rrspMixed3$null.deviance)
MR2.Mixed3.3 <- round(MR2.Mixed3,digits = 3)
print(paste("McFadden Rsquared: ",MR2.Mixed3.3)) # 0.095

# Run a stepwise regression using the "rrspMixed3" model
rrspStep.Mixed3 <- step(rrspMixed3, direction = "both")
summary(rrspStep.Mixed3) # AIC = 3164.1

# McFadden R2
MR2.step.Mixed3 <- 1 - (rrspStep.Mixed3$deviance / rrspStep.Mixed3$null.deviance)
MR2.step.Mixed3.3 <- round(MR2.step.Mixed3,digits = 3)
print(paste("McFadden Rsquared: ",MR2.step.Mixed3.3)) # 0.093


# Model rrspMixed4: Selection of Transformation
#
# While the fit of the model is improving (lower AIC and higher McFadden R2),
# we also want to see if the changes in variable significance levels would suggest
# that some variables may not need to be transformed. We will compare the 
# absolute values of Pr(>|z|) of all predictor variables for the 3 Mixed models.

# Note that in the above, we have made the following improvements sequentially:
# From rrspLogis to rrspMixed1, we transformed TOTDEP, avginc/avginv, and numrr/numcon
# From rrspMixed1 to rrspMixed2, we transformed BAL variables.
# From rrspMixed2 to rrspMixed3, we transformed TX variables.


###### Make a table of comparison ######

p_comparison <- data.frame(Linear = c(as.data.frame((coef(summary(rrspLogis))[,4])[-1])[,1][1:26],NA,NA,
                                      as.data.frame((coef(summary(rrspLogis))[,4])[-1])[,1][27:31]),
                           Mixed1 = c(as.data.frame((coef(summary(rrspMixed1))[,4])[-1])[,1][1:22],NA,NA,NA,NA,
                                      as.data.frame((coef(summary(rrspMixed1))[,4])[-1])[,1][23:29]),
                           Mixed2 = c(as.data.frame((coef(summary(rrspMixed2))[,4])[-1])[,1][1:22],NA,NA,NA,NA,
                                      as.data.frame((coef(summary(rrspMixed2))[,4])[-1])[,1][23:29]),
                           Mixed3 = c(as.data.frame((coef(summary(rrspMixed3))[,4])[-1])[,1][1:22],NA,NA,NA,NA,
                                      as.data.frame((coef(summary(rrspMixed3))[,4])[-1])[,1][23:29]),
                           row.names = c(rownames(as.data.frame((coef(summary(rrspLogis))[,4])[-1]))[1:26],
                                         'PC.numcon.numrr','PC.avginc.avginv',
                                         rownames(as.data.frame((coef(summary(rrspLogis))[,4])[-1]))[27:31]))
# To check the effect of Logged TOTDEP and 2 PCA's, compare column 1 and 2;
# To check the effect of Logged BAL, compare column 2 and 3;
# To check the effect of Logged TX, compare column 3 and 4;
p_comparison

# Results
# The 3 variables which the log transformation did not improve are:
# BALLOAN, BALMRGG, TXATM
# So in the rrspMixed4, we will include their original values instead of logged ones.

# Note that the 2 PCA's were supposed to eliminate the multicollinearity issues.
# However, in all of the 3 stepwise models, the 2 PCs are rejected. Why? NOT due
# to correlation because if we check the correlation matrix below, the 2 PCs do not
# correlate with any feature (except for their children features). So does it mean
# that the 2 PCs are truly insignificant? If so, why are they showing important
# in the random forest all?
#
# Correlation Matrix
corrMatrix2 <- cor(select_if(rrsp2, is.numeric)[c(1:13,20,22,23)])
corrplot(corrMatrix2,method="number",type="lower",
         diag = FALSE,number.cex = 0.5)

# Build the model based on the variables we identified above:
rrspMixed4 <- glm(formula = APURCH ~ age + gendm + atmcrd + paydep + Log.BALCHQ +
                    Log.BALSAV + Log.TOTDEP + BALLOAN + Log.BALLOC + BALMRGG +
                    NEWLOC + NEWMRGG + Log.TXBRAN + TXATM + Log.TXPOS + Log.TXCHQ +
                    Log.TXWEB + Log.TXTEL + valsegm +
                    PC.numcon.numrr + PC.avginc.avginv + BALCHQ_IND + BALSAV_IND +
                    BALLOAN_IND + BALLOC_IND + BALMRGG_IND,
                  data = filter(rrsp2, Sample =="Estimation"),
                  family = binomial(logit))
summary(rrspMixed4) # AIC = 3182.5 (better than Mixed3)

# Calculate and print McFadden R square (See Logistic Regression Chapter)
MR2.Mixed4 <- 1 - (rrspMixed4$deviance / rrspMixed4$null.deviance)
MR2.Mixed4.3 <- round(MR2.Mixed4,digits = 3)
print(paste("McFadden Rsquared: ",MR2.Mixed4.3)) # 0.095

# Run a stepwise regression using the "rrspMixed4" model
rrspStep.Mixed4 <- step(rrspMixed4, direction = "both")
summary(rrspStep.Mixed4) # AIC = 3163.5 (Better than Mixed3)

# McFadden R2
MR2.step.Mixed4 <- 1 - (rrspStep.Mixed4$deviance / rrspStep.Mixed4$null.deviance)
MR2.step.Mixed4.3 <- round(MR2.step.Mixed4,digits = 3)
print(paste("McFadden Rsquared: ",MR2.step.Mixed4.3)) # 0.092


# Mixed5 is intended to further investigate into the nonlinearity of the 4
# demographical variables, using the categorized variables. NOT HELPING. REMOVE.
# rrspMixed5 <- glm(formula = APURCH ~ age + gendm + atmcrd + paydep + Log.BALCHQ +
#                     Log.BALSAV + Log.TOTDEP + BALLOAN + Log.BALLOC + BALMRGG +
#                     NEWLOC + NEWMRGG + Log.TXBRAN + TXATM + Log.TXPOS + Log.TXCHQ +
#                     Log.TXWEB + Log.TXTEL + valsegm + numrr.Cat +
#                     avginc.Cat + BALCHQ_IND + BALSAV_IND +
#                     BALLOAN_IND + BALLOC_IND + BALMRGG_IND,
#                   data = filter(rrsp2, Sample =="Estimation"),
#                   family = binomial(logit))
# summary(rrspMixed5) # AIC = 3189.1 (better than Mixed3)
# # Run a stepwise regression using the "rrspMixed5" model
# rrspStep.Mixed5 <- step(rrspMixed5, direction = "both")
# summary(rrspStep.Mixed5) # AIC = 3163.5 (Better than Mixed3)

###### Compare model results######

# Check lift charts on logistic regression
# Use a trueResp rate of 2.2%
lift.chart(modelList = c("rrspLogis","rrspMixed1","rrspMixed2","rrspMixed3","rrspMixed4"),
           data = filter(rrsp2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022, type = "cumulative",
           sub = "Validation")
# Very similar performance except Logis and Mixed1
# Look closer into Mixed2, Mixed3, and Mixed4
lift.chart(modelList = c("rrspMixed2","rrspMixed3","rrspMixed4"),
           data = filter(rrsp2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022, type = "cumulative",
           sub = "Validation")

# Check lift charts on stepwise regression
lift.chart(modelList = c("rrspStep","rrspStep.Mixed1","rrspStep.Mixed2",
                         "rrspStep.Mixed3","rrspStep.Mixed4"),
           data = filter(rrsp2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022, type = "cumulative",
           sub = "Validation")
# Look closer: StepMixed2 is worse
lift.chart(modelList = c("rrspStep.Mixed2","rrspStep.Mixed3","rrspStep.Mixed4"),
           data = filter(rrsp2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022, type = "cumulative",
           sub = "Validation")
# Look closer: StepMixed4 is slightly better and has lower AIC
lift.chart(modelList = c("rrspStep.Mixed3","rrspStep.Mixed4"),
           data = filter(rrsp2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022, type = "cumulative",
           sub = "Validation")

# Compare StepMixed3 and StepMixed4 with forest
lift.chart(modelList = c("rrspStep.Mixed3","rrspStep.Mixed4","rrspForestAll"),
           data = filter(rrsp2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022, type = "cumulative",
           sub = "Validation")
# The lift chart shows that rrspStep.Mixed4 appears to fit the validation sample
# just as good as rrspForestAll does. Good result!


# As a final check, use neural network to check for nonlinearities
# 4 node nnet using only the variables chosen by the Mixed4
coef(summary(rrspStep.Mixed4))
rrspNnet4 <- Nnet(formula = APURCH ~ age + atmcrd + paydep + Log.BALSAV +
                    Log.TOTDEP + Log.BALLOC + BALMRGG + Log.TXBRAN +
                    Log.TXTEL + valsegm + BALSAV_IND + BALLOAN_IND,
                 data = filter(rrsp2, Sample =="Estimation"),
                 decay = 0.15, size = 4)

# Compare stepwise, forest, and neural network model
lift.chart(modelList = c("rrspStep.Mixed4","rrspNnet4","rrspForestAll"),
           data = filter(rrsp2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022, type = "cumulative",
           sub = "Validation")
# Neural Network performs worse than the Stepwise regression and the forest, 
# meaning that there are no detectable patterns in the relations between
# the predictors that the stepwise regression used and the target but did not capture.

# Try neural network on all variables
rrspNnetAll <- Nnet(formula = APURCH ~ age + gendm + atmcrd + paydep + BALCHQ +
                      BALSAV + TOTDEP + BALLOAN + BALLOC + BALMRGG + NEWLOC +
                      NEWMRGG + TXBRAN + TXATM + TXPOS + TXCHQ + TXWEB + TXTEL +
                      valsegm + numrr_1 +
                      numcon_1 + avginc_1 + avginv_1 + BALCHQ_IND + BALSAV_IND +
                      BALLOAN_IND + BALLOC_IND + BALMRGG_IND,
                  data = filter(rrsp2, Sample =="Estimation"),
                  decay = 0.15, size = 4)

# Compare all four models
lift.chart(modelList = c("rrspStep.Mixed4","rrspNnet4","rrspNnetAll","rrspForestAll"),
           data = filter(rrsp2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022, type = "cumulative",
           sub = "Validation")

# The addition of variables to the neural network is even worse, indicating
# that the extra variables are overfitting. The random forest model is the best
# predictor. However, because we care about interpretation, the stepwise logistic
# regression is the best choice as it is a good predicting model and is clearly
# interpretable. The random forest on the other hand provides additional
# interpretation information on strongly predictive variables that the stepwise
# regression rejected due to some reasons:

#       What reasons? Answer using importance plot and summary of rrspStep.Mixed4.
#       Additionally, need to explain why some variables are important in forest,
#       but are rejected in the stepwise model. For example,the 2 PCs we discussed 
#       above. PDP's or plot of means may help.
#
# The following may help (comment out to use)
# corrplot(corrMatrix2,method="number",type="lower",
#          diag = FALSE,number.cex = 0.5)
# lift.chart(modelList = c("rrspStep.Mixed4","rrspForestAll"),
#            data = filter(rrsp2, Sample == "Validation"),
#            targLevel = "Y", trueResp = 0.022, type = "cumulative",
#            sub = "Validation")
# varImpPlot(rrspForestAll,type = 2,
#            main="rrspForestAll", # title
#            cex =0.7) # font size
# summary(rrspStep.Mixed4)

# Raw Estimated Probabilities added to data in ScoreRaw
rrsp2$ScoreRaw <- rawProbScore(model = "rrspStep.Mixed4",
                                   data = rrsp2,
                                   targLevel = "Y")


# Adjusted Estimated Probabilities, corrected for oversampling, in ScoreAdj
rrsp2$ScoreAdj <- adjProbScore(model = "rrspStep.Mixed4",
                                   data= rrsp2,
                                   targLevel = "Y",
                                   trueResp = 0.022)

# Rank Order - rank individuals in dataframe from best to worst, in ScoreRank
rrsp2$ScoreRank <- rankScore(model = "rrspStep.Mixed4",
                                 data = rrsp2,
                                 targLevel = "Y")

# Put customer ID's back as a variable
rrsp2$ID <- rownames(rrsp2)

# Verify the relationship between geodemographic variables and purchase behavior
# Found no obvious relationship, meaning that the reason why the random forest
# model found them to be important may not be due to their own impacts but because
# they contain some information contained in other predictors that predict better
# at an individual levels.
# scatterplot(x = rrsp2$ScoreRank, y = log(rrsp2$avginc_1+1),cex = 0.5)
# scatterplot(x = rrsp2$ScoreRank, y = log(rrsp2$avginv_1+1),cex = 0.5)
# scatterplot(x = rrsp2$ScoreRank, y = log(rrsp2$numrr_1+1),cex = 0.5)
# scatterplot(x = rrsp2$ScoreRank, y = log(rrsp2$numcon_1+1),cex = 0.5)


# For Question 2:

# Rank predictors by their significance (p-value)
coef <- data.frame(Variable = rownames(as.data.frame(coef(summary(rrspStep.Mixed4))[,4][-1])),
                   Coefficient = coef(summary(rrspStep.Mixed4))[,1][-1],
                   StdError = coef(summary(rrspStep.Mixed4))[,2][-1],
                   Zval = coef(summary(rrspStep.Mixed4))[,3][-1],
                   Pval = coef(summary(rrspStep.Mixed4))[,4][-1],
                   row.names = NULL)
coef <- coef[with(coef,order(coef$Pval)),]
row.names(coef) <- seq(1,15,1)
coef

# Plot effects of all variables
plot(allEffects(rrspStep.Mixed4), type="response")
# Plot effects of 5 key predictors
plot(effect("Log.TOTDEP",rrspStep.Mixed4),type = "response")
plot(effect("BALLOAN_IND",rrspStep.Mixed4),type = "response")
plot(effect("age",rrspStep.Mixed4),type = "response")
plot(effect("paydep",rrspStep.Mixed4),type = "response")
plot(effect("Log.BALLOC",rrspStep.Mixed4),type = "response")


# Relationship between purchase and loan holding
mosaicplot(table(rrsp2$APURCH,rrsp2$BALLOAN_IND),
           col = c("blue","green"),
           main = "Purchase vs Loan Holding",
           xlab = "Purchase?",
           ylab = "Hold Loan?")

# Relationship between purchase and payroll deposits
mosaicplot(table(rrsp2$APURCH,rrsp2$paydep),
           col = c("blue","green"),
           main = "Purchase vs Payroll Deposit Setup",
           xlab = "Purchase?",
           ylab = "Payroll Deposit?")

