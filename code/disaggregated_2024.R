# ------------------------------------------------------------------------------

# Analyzing Disaggregated Data in R
# via {marginaleffects} and {clarify}
# Sakeef M. Karim
# CC-BY-SA 4.0

# PREAMBLE ---------------------------------------------------------------------

# For replication, clone the repository, un-annotate and run:

# renv::restore()

library(palmerpenguins)
library(tidyverse)
library(marginaleffects)
library(modelsummary)
library(clarify)
library(ggdist)
library(broom)

# LOADING THE DATA -------------------------------------------------------------

# Slightly modifying penguins dataframe from {palmerpenguins}

penguins_data <- penguins %>% mutate(# Treating "year" as a discrete variable:
                                     year = as_factor(year))

# BUILDING OUR MODEL -----------------------------------------------------------

# Binomial logistic regression model:

model <- penguins_data %>% glm(# Reversing factor order so that "1" is female:
                               fct_rev(sex) ~ 
                               # Interaction term:
                               flipper_length_mm * species +
                               # Year fixed-effects:
                               year, 
                               family = binomial, 
                               # For probit models, change argument above to 
                               # family = binomial(link = "probit"),
                               data = .)

# SUMMARIZING MODEL RESULTS ----------------------------------------------------

model %>% summary()

# HTML table via modelsummary

model %>% modelsummary()

# Tidied results:

model %>% tidy()

# Quick visual summary; zeroing-in on parameters of interest:

# Labels on plot:

var_labels <- rev(c("flipper_length_mm" = "Flipper Length (mm)", 
                    "flipper_length_mm:speciesGentoo" = 
                    "Flipper Length (mm) x Gentoo",
                    "flipper_length_mm:speciesChinstrap" = 
                    "Flipper Length (mm) x Chinstrap"))

modelplot(model,
          # Variable names (those of interest):
          coef_map = var_labels,
          # 99% confidence interval:
          conf_level = 0.99) + 
# Marking significance; line:
geom_vline(xintercept = 0, lty = "dotted") +
# Marking significance; colour scheme:
aes(colour = ifelse(p.value < 0.01, "Significant", "Not significant")) +
scale_colour_manual(values = c("grey", "#3f1f69")) +
labs(colour = "", 
     x = "Log Odds That Penguin Is Female\n (with 99% Confidence Intervals)") +
# Note: you can download IBM Plex Sans and Inconsolata via
# https://popagingdataviz.com/misc/fonts.zip
theme_bw(base_family = "IBM Plex Sans",
           base_size = 13)

# MAIN {marginaleffects} FUNCTIONS ---------------------------------------------

# Finding 25th/75th percentiles of flipper_length_mm:

perc_flipper <- penguins_data %>% 
                                   summarise(flipper_25 = 
                                             quantile(flipper_length_mm, 0.25, 
                                                      na.rm = TRUE),
                                             flipper_75 = 
                                             quantile(flipper_length_mm, 0.75, 
                                                      na.rm = TRUE))  

# Adjusted predictions, marginalized:

avg_predictions(model,
                variables = list(# unique = all levels of species variable:
                                 species = unique,
                                 # Setting flipper length to 25th/75th 
                                 # percentiles of flipper_length_mm:
                                 flipper_length_mm = 
                                 c(perc_flipper$flipper_25, 
                                  perc_flipper$flipper_75)))  

# Conditional predictions, visualized:

plot_predictions(model,
                 # Generating conditional predictions along two axes:
                 condition = c("flipper_length_mm", "species")) +
                labs(x = "Flipper Length (mm)",
                     y = "Probability That Penguin Is Female") +
                theme_bw(base_family = "IBM Plex Sans",
                         base_size = 13) 

# Average Marginal Effects (AMEs)

avg_slopes(model,
           # AME of which variable? Flipper length!
           variables = "flipper_length_mm",
           # Differences across subgroups:
           by = "species",
           # Counterfactual dataset - three times as long as original, with each
           # row getting "assigned" each level of "species".
           newdata = datagrid(species = unique,
                              grid_type = "counterfactual"))

# Visualizing AMEs

plot_slopes(model,
            variables = "flipper_length_mm",
            by = "species",
            newdata = datagrid(species = unique,
                               grid_type = "counterfactual")) +
            geom_hline(yintercept = 0, lty = "dotted") +
            theme_bw(base_family = "IBM Plex Sans",
                     base_size = 13) +
            labs(y = "Average Marginal Effect of Flipper Length on\n Probability of Penguin Being Female") +
            aes(colour = species) +
            theme(legend.position = "none") +
            # Flipping orientation for legibility:
            coord_flip()


# Average Contrasts

avg_comparisons(model, 
                variables = "flipper_length_mm",
                by = "species", 
                # Pairwise differences in unit-level predictions, marginalized
                hypothesis = "pairwise",
                # Counterfactual grid
                newdata = datagrid(species = unique,
                                   grid_type = "counterfactual"))

# Visualizing contrasts (vs base level)

plot_comparisons(model,
                 variables = "species",
                 condition = "flipper_length_mm",
                 newdata = datagrid(species = unique, 
                                    grid_type = "counterfactual")) +
                 geom_hline(yintercept = 0, lty = "dotted") +
                 labs(x = "Flipper Length (mm)") +
                 theme_bw(base_family = "IBM Plex Sans",
                          base_size = 13)


# SIMULATION-BASED INFERENCE ---------------------------------------------------

# via the {clarify} package:

# Generating 10,000 simulated values of coefficients (by sampling from 
# multivariate normal distribution)

set.seed(905)

sim_estimates <- sim(model, n = 10000)

# Generating predictions based on 10,000 simulations:          

sim_prediction <- sim_setx(sim_estimates, 
                           # Visualizing estimates for Adelies and Chinstraps
                           x = list(species = c("Adelie", "Chinstrap"),
                                    flipper_length_mm = 
                                    # Once again, constraining flipper length to
                                    # 25p and 75p.
                                    c(perc_flipper$flipper_25,
                                      perc_flipper$flipper_75)),
                           verbose = FALSE)

# Visualizing predicted values (based on simulation):

sim_prediction %>% plot() + 
                   theme_bw(base_family = "IBM Plex Sans",
                            base_size = 13) +
                   theme(legend.position = "top")

# Simulated AMEs via experimental `inference` function from {marginaleffects}:

model_dydx <- avg_slopes(model,
                         variables = "flipper_length_mm",
                         by = "species",
                         newdata = datagrid(species = unique,
                                            grid_type = "counterfactual"))

model_dydx %>% 
# Simulating estimates:
inferences(method = "simulation") %>% 
# Drawing rvar containing simulated arrays of AMEs
posterior_draws("rvar") %>% 
# Visualizing the simulated AMEs
ggplot(., aes(xdist = rvar, 
              y = species, 
              colour = species,
              fill = species)) +
# From ggdist package:
stat_slabinterval(alpha = 0.4) +
theme_bw(base_family = "IBM Plex Sans",
          base_size = 17) +
labs(x = "Average Marginal Effect of Flipper Length on\n Probability of Penguin Being Female",
     y = "")  +
geom_vline(xintercept = 0, lty = "dotdash")