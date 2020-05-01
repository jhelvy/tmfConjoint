library(tidyverse)
library(here)
library(mlogit)
library(cowplot)
library(janitor)
library(stringr)

# Load DOE from R
source(here::here('survey', 'pilot10', 'functions.R'))
doeAll <- readRDS(here::here(
    'survey', 'pilot10', 'survey', 'doe', 'doe.Rds'))
nResp <- as.numeric(names(doeAll))

# Define functions
getTempDf <- function(doeAll, size, hasCar = TRUE) {
    does <- doeAll[[as.character(size)]]
    doe <- does[['yes']]
    if (! hasCar) { doe <- does[['no']] }
    respIDs <- sample(seq(max(doe$respID)), size)
    tempDf <- doe %>% filter(respID %in% respIDs)
    numAlts <- max(tempDf$altID)
    tempDf$obsID <- rep(seq(nrow(tempDf) / numAlts), each = numAlts)
    # Assign random choices
    choices <- sample(seq(numAlts), max(tempDf$obsID), replace = TRUE)
    choices <- diag(numAlts)[choices,]
    tempDf$choice <- matrix(t(choices))
    return(tempDf)
}

recodeTempDf <- function(tempDf) {
    tempDf <- dummyCode(tempDf, vars = c(
        'numLegs', 'trip', 'leg1Mode', 'leg2Mode', 'tripTimeUnc')) %>%
        clean_names('lower_camel')
    # Convert the data to "mlogit" format:
    tempDf = mlogit.data(
        data    = tempDf,
        shape   = 'long',
        choice  = 'choice',
        alt.var = 'altId')
    return(tempDf)
}

# Get the standard errors out of the models
getSE <- function(models) {
    coefs1 <- coef(models[[1]])
    se <- as.data.frame(matrix(0, ncol=length(coefs1) + 1, nrow=length(nResp)))
    colnames(se) <- c('size', names(coefs1))
    for (i in 1:length(models)) {
        model <- models[[i]]
        size <- nrow(model$gradient) / 6
        se_vals <- sqrt(abs(diag(solve(model$hessian))))
        se[i, ] <- c(size, se_vals)
    }
    return(se)
}

formatSE <- function(df) {
    return(
        df %>%
            gather(coef, se, -c(size))
    )
}

# ---------------------------------------------------------------------------
# Baseline model

# Run models for different sample sizes
index <- 1
models <- list()
for (i in 1:length(nResp)) {
    size <- nResp[i]
    tempDf <- getTempDf(doeAll, size, hasCar = TRUE)
    tempDf <- recodeTempDf(tempDf)
    # Run the model:
    models[[i]] = mlogit(tempDf,
        formula = choice ~
            price + expressFee +
            leg1Time + leg2Time + leg3Time +
            transfer1Time + transfer2Time + transfer3Time +
            tripTimeUnc0_1 + tripTimeUnc0_2 +
            trip | 0)
}

# Save results
saveRDS(models,
    here::here('survey', 'pilot10', 'survey', 'samplesize', 'models',
               'baseline.Rds'))

# ---------------------------------------------------------------------------
# Interaction model

# Run models for different sample sizes
index <- 1
models <- list()
for (i in 1:length(nResp)) {
    size <- nResp[i]
    tempDf <- getTempDf(doeAll, size, hasCar = TRUE)
    tempDf <- recodeTempDf(tempDf)
    # Run the model:
    models[[i]] = mlogit(tempDf,
        formula = choice ~
        price + expressFee +
        leg1Time + leg2Time + leg3Time +
        transfer1Time + transfer2Time + transfer3Time +
        tripTimeUnc0_1 + tripTimeUnc0_2 +
        trip + time*totalTripTime | 0)
}

# Save results
saveRDS(models,
        here::here('survey', 'pilot10', 'survey', 'samplesize', 'models',
                   'interaction.Rds'))

# ---------------------------------------------------------------------------
# Mixed logit model
#
#     models[[i]] = mlogit(tempDf,
#         formula = choice ~
#             amount +
#             incentive_tax_deduction + incentive_sales_tax_exemption +
#             incentive_rebate_from_dealer + incentive_rebate_from_oem +
#             incentive_rebate_from_the_government +
#             timing_6_8_weeks + timing_3_months + timing_1_year | 0,
#         rpar = c(amount = 'n',
#             incentive_tax_deduction = 'n',
#             incentive_sales_tax_exemption = 'n',
#             incentive_rebate_from_dealer = 'n',
#             incentive_rebate_from_oem = 'n',
#             incentive_rebate_from_the_government = 'n',
#             timing_6_8_weeks = 'n',
#             timing_3_months = 'n',
#             timing_1_year = 'n'),
#         R = 100, halton = NA)

# ---------------------------------------------------------------------------
# Plot results

baseline <- readRDS(here::here(
    'survey', 'pilot10', 'survey', 'samplesize', 'models',
    'baseline.Rds')) %>%
    getSE() %>%
    formatSE()

baseline_plot <- ggplot(baseline, aes(x = size, y = se)) +
    geom_point(size = 2, alpha = 0.2) +
    theme_bw()


# comparison <- bind_rows(
#     mutate(baseline, model = 'baseline'),
#     mutate(linear_price, model = 'linear_price'),
#     mutate(interaction, model = 'interaction'),
#     mutate(mixed, model = 'mixed')) %>%
#     ggplot(aes(x = size, y = se, color = category)) +
#     geom_point() +
#     facet_wrap(~model, nrow = 1) +
#     theme_bw()

ggsave(here::here('survey', 'pilot10', 'survey', 'samplesize', 'plots',
                  'baseline.pdf'),
       baseline_plot, width = 5, height = 3)

