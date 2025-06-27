#this script will
# 1. Show you how to calculate effect sizes for use in meta-analysis
# 2. Introduce you to different types of meta-analysis (fixed effects, random effects, multi-level)
# 3. Introduce you to subgroup and meta-regression analyses
# 4. Allow you to play around with some visualisation of results

# If you want to do a deep dive into the data and type of analyses we do in this practical
# you can read the paper that it is based on: https://doi.org/10.1111/gcb.17305
# feel free to cite it as much as you want in anything you publish ;) 

#install orchaRd if you don't already have it installed
devtools::install_github("daniel1noble/orchaRd", ref = "main", force = TRUE)

# First, we need to load the packages we will use

library(metafor) # for meta-analysis
library(orchaRd) # for visualisation
library(tidyverse) # for data manipulation and visualisations

# Next, we will load the data
#dataset of studies that looked at impact of drought on abundance of soil and litter invertebrates
abun_drought<-read.csv("abundance_drought_data.csv")

##########################################################
#1 - Calculating effect sizes#############################
##########################################################

# We will use the escalc function from the metafor package to calculate effect sizes
# We will calculate the effect sizes and effect size variance for the drought data first
# Here we calculate the log response ratio,  a common effect size measure 
# that can be converted into a proportional change and is therefore easy to understand

abun_drought_es <- escalc(measure = "ROM", 
                          m1i = disturbance_av, 
                          sd1i = dist_SD, 
                          n1i = dist_n, 
                          m2i = control_av, 
                          sd2i = control_SD, 
                          n2i = control_n, 
                          data = abun_drought)

# We can now look at the first few rows of the data to see the effect sizes - which are stored in the column yi
# You will see that some of these effect sizes are negative, which indicates 
# a decrease in abundance in the drought condition compared to the control
# and some are positive, which indicates an increase in abundance in the drought condition compared to the control
head(abun_drought_es$yi)

# We can also look at the variance of the effect sizes, which is stored in the column vi
# This variance is used to weight the effect sizes in the meta-analysis, the lower the variance
# the more reliable the effect size is considered to be
head(abun_drought_es$vi)

###############################################################
#2 - Meta-analysis##############################################
###############################################################

# We will now conduct a meta-analysis using the effect sizes we calculated

#First we will use a fixed effects model
abun_drought_fixed <- rma(yi = yi, vi = vi, data = abun_drought_es, method = "FE")

# We can look at the results of the meta-analysis
summary(abun_drought_fixed)
# a positive estimate indicates, on average, an increase in abundance


#Next we will use a random effects model
abun_drought_random <- rma(yi = yi, vi = vi, data = abun_drought_es, method = "REML")
# We can look at the results of the meta-analysis again
summary(abun_drought_random)

#what has changed from the fixed effects model?

# Next we can use a multi-level model - this is the kind of model we use
# when we have multiple effect sizes from the same study - which is very common 
# and is the case with this dataset
abun_drought_multilevel <- rma.mv(yi = yi, V = vi, random = ~ 1 | Study_ID, data = abun_drought_es)
# We can look at the results of the multi-level meta-analysis
summary(abun_drought_multilevel)
# What has changed from the random effects model?

####################################################################
# 3 - Subgroup and meta-regression analyses#########################
#####################################################################

# We can conduct a subgroup analysis to see if the effect sizes differ by a categorical variable
# For example we could look at how the effect of drought might vary by the body size of the invertebrates
# In this you just have to remember that microfauna = tiny things, mesofauna = medium sized things
# and macrofauna = relatively large things (although all of these animals are quite small in reality)

# We will do this by including a variable in the multi-level model
abun_drought_body_size <- rma.mv(yi = yi, V = vi, 
                                 random = ~ 1 | Study_ID, 
                                 mods = ~ Functional_group_size-1, 
                                 data = abun_drought_es)
# We can look at the results of the multi-level meta-analysis with the body size variable included
summary(abun_drought_body_size)

# Which groups are most affected by drought?

# We can also conduct a meta-regression to see if the effect sizes vary by a continuous variable
# For example we could look at how the effect of drought might vary by the intensity of the drought
# We will do this by including a variable in the multi-level model
abun_drought_intensity <- rma.mv(yi = yi, V = vi, 
                                  random = ~ 1 | Study_ID, 
                                  mods = ~ drought_intensity, 
                                  data = abun_drought_es)

summary(abun_drought_intensity)
# What is the relationship between drought intensity and effect size?

#we can also include both the body size and intensity variables in the same model
# to see whether the effect of drought intensity depends on the body size of the invertebrates
abun_drought_interaction <- rma.mv(yi = yi, V = vi, 
                             random = ~ 1 | Study_ID, 
                             mods = ~ Functional_group_size * drought_intensity-1, 
                             data = abun_drought_es)
summary(abun_drought_interaction)
# this model output looks quite complicated but it shows that mesofauna is affected by 
# differences in drought intensity more than microfauna and macrofauna



########################################################################
# 4 - Visualisation of results###########################################
#########################################################################

# We can visualise the results of the meta-analysis using a caterpillar plot
orchaRd::caterpillars(abun_drought_multilevel, mod="1", 
                      xlab = "Log response ratio",group="Study_ID",
                      colpoint = "black",colerrorbar = "black")

# this shows the effect sizes for each study, their confidence intervals
# and the overall average effect at the bottom as a red diamond
# if the edges of the diamond do not cross zero
# then the overall effect is statistically significant

# We can also visualise the results as an orchard plot
#first we need to save the model results
results<-mod_results(abun_drought_multilevel, mod="1",group="Study_ID")
#then we can plot them
orchard_plot(results, mod="1",xlab = "Log response ratio", group = "Study_ID")

# this shows the effect sizes for each study as a point, the size of which is relative to the
# weight that they have in the meta-analysis and the overall average as a blue point
# with error bars showing the confidence intervals and prediction intervals
# k refers to the number of effect sizes and in brackets is the number of studies used

# We can also visualise the results of the subgroup analysis
#first we need to have the model results
results_body_size <- mod_results(abun_drought_body_size, mod="Functional_group_size", group="Study_ID")
#then we can plot them
orchard_plot(results_body_size, mod="Functional_group_size", 
             xlab = "Log response ratio", group = "Study_ID")

#the interpretation of this plot is the same as the previous one
# can you see why mesofauna is most affected by drought now?

# we can also visualise the results of the meta-regression
#first we need to have the model results
results_intensity <- mod_results(abun_drought_intensity, mod="drought_intensity", group="Study_ID")
#then we can plot them
bubble_plot(results_intensity, mod="drought_intensity", 
             xlab = "Drought intensity", ylab="Log response ratio",group = "Study_ID")

#finally we can visualise the results of the interaction model
#first save the model results
results_interaction <- mod_results(abun_drought_interaction, 
                                    mod="drought_intensity", 
                                    group="Study_ID",
                                   by="Functional_group_size")
#then plot them
bubble_plot(results_interaction, group = "Study_ID",  mod = "drought_intensity", 
            xlab = "Drought intensity", legend.pos = "top.left")
