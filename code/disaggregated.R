# PREAMBLE ----------------------------------------------------------------

# For replication, clone the repository, un-annotate and run:

# renv::restore()

# Data wrangling, visualization:

library(tidyverse)

# Visualizing adjusted model predictions/predictive margins:

library(ggeffects)

# Workhorse for estimating average marginal effects, contrasts and more:

library(marginaleffects)

# Generating quantities of substantive interest via Monte Carlo simulations:

library(clarify)

# Quickly produce regression tables:

library(gtsummary)
library(modelsummary)


# LOADING THE DATA --------------------------------------------------------

penguins <- palmerpenguins::penguins %>% mutate(year = as_factor(year))

# FITTING A (VERY) BASIC MODEL --------------------------------------------

basic_model <- lm(bill_length_mm ~ 
                  # Moderation:  
                  species*flipper_length_mm + 
                  # Basic controls:
                  sex + year, 
                  data = penguins) 

# Quick detour -- generating quick regression tables: 

# Via the modelsummary package:

basic_model %>% modelsummary(output = "table.docx")

# Via the gtsummary package:

basic_model %>% tbl_regression() %>% 
                as_flex_table() %>% 
                flextable::save_as_docx(path = "table.docx")

# GGEFFECTS ---------------------------------------------------------------

# Website for the ggeffects package:

# https://strengejacke.github.io/ggeffects/

# Adjusted predictions:

model_predictions <- ggpredict(basic_model, terms = c("flipper_length_mm", "species")) 

model_predictions %>% plot()

# Using ggplot2:

model_predictions_ggplot <- ggplot(model_predictions, aes(x = x, y = predicted, 
                                   fill = group,
                                   colour = group))

model_predictions_ggplot + geom_line() + 
                           geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
                           labs(x = "Flipper Length (mm)",
                                y = "Bill Length (mm") +
                           theme_minimal() +
                           theme(legend.title = element_blank())
              

# MARGINALEFFECTS ---------------------------------------------------------

# Website for the marginaleffects package:

# https://vincentarelbundock.github.io/marginaleffects/index.html

# Contrasts or comparisons:

# Comparing Chinstraps and Gentoos:

avg_comparisons(basic_model, variables = list(species = c("Chinstrap", "Gentoo")))

# Contrasts - while accounting for potential moderation effects:

# Differences across levels of flipper length:

comparisons(basic_model,
            variables = list(flipper_length_mm = c(180, 235)),
            newdata = datagrid(year = "2008",
                               species = c("Chinstrap", "Gentoo")))
            #hypothesis = "b1 = b2")

# Differences across species:

comparisons(basic_model,
            variables = list(species = c("Chinstrap", "Gentoo")),
            newdata = datagrid(year = "2008",
                               flipper_length_mm = c(180, 235)))
                               
# Average marginal effects (slope of the prediction function):

avg_slopes(basic_model,
           by = "species",
           variables = "flipper_length_mm")


# CLARIFY -----------------------------------------------------------------

# Website for clarify package:

# https://iqss.github.io/clarify/

# Seminal article by King, Tomz and Wittenberg:

# https://gking.harvard.edu/files/making.pdf

sim_estimates <- sim(basic_model)

# Simulation-baed predictions at representative values:

sim_prediction <- sim_setx(sim_estimates, x = list(species = c("Adelie", "Chinstrap"),
                                                   year = "2009",
                                                   flipper_length_mm = c(180, 235)),
                           verbose = FALSE)

# Summarizing results, expressing uncertainty:

sim_prediction %>% summary(level = 0.99)

# Visualizing results

sim_prediction %>% plot() + theme_minimal() + labs(x = "Simulated Parameter Estimates",
                                                   y = "")  + theme(legend.position = "bottom") +
                            scale_colour_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2")


# SEEMINGLY UNRELATED ESTIMATION IN R -------------------------------------

# See: https://www.rdocumentation.org/packages/systemfit/versions/1.1-28

# OR

# https://rdrr.io/cran/geepack

# For an example of how to replicate suest results in R, see:

# https://stats.stackexchange.com/questions/358795/how-to-test-significance-in-shift-of-a-variable-taken-an-other-variable-into-the/465215#465215
