library(tidyverse)
library(here)
library(mlogit)
library(cowplot)
library(janitor)
library(stringr)

# Load DOE from R
doe_all <- readRDS(here::here(
    'survey', 'pilot9', 'survey', 'doe', 'doe.Rds'))
nResp <- as.numeric(names(doe_all))

# Define functions
getTempDf <- function(doeAll, size) {
    respIDs <- sample(seq(max(doeAll$respID)), size)
    tempDf <- doeAll %>% filter(respID %in% respIDs)
    tempDf$obsID <- rep(seq(nrow(tempDf) / 3), each = 3)
    # Assign random choices
    choices <- sample(seq(3), max(tempDf$obsID), replace = TRUE)
    choices <- diag(3)[choices,]
    tempDf$choice <- matrix(t(choices))
    return(tempDf)
}

recodeTempDf <- function(tempDf) {
    tempDf <- dummyCode(tempDf, vars = c(
        'incentive', 'amount', 'timing')) %>%
        clean_names()
    # Convert the data to "mlogit" format:
    tempDf = mlogit.data(
        data    = tempDf,
        shape   = 'long',
        choice  = 'choice',
        alt.var = 'alt_id')
    return(tempDf)
}

# Get the standard errors out of the models
getSE <- function(models) {
    coefs1 <- coef(models[[1]])
    se <- as.data.frame(matrix(0, ncol=length(coefs1) + 1, nrow=length(numResp)))
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
            gather(coef, se, -c(size)) %>%
            mutate(category = ifelse(
                str_detect(coef, 'amount'), 'amount', ifelse(
                str_detect(coef, 'incentive'), 'incentive', 'timing')))
    )
}

# ---------------------------------------------------------------------------
# Baseline model

# Run models for different sample sizes
numResp <- seq(100, 3000, 300)
index <- 1
models <- list()
for (i in 1:length(numResp)) {
    size <- numResp[i]
    tempDf <- getTempDf(doeAll, size)
    tempDf <- recodeTempDf(tempDf)
    # Run the model:
    models[[i]] = mlogit(tempDf, formula = choice ~
        amount_2000 + amount_3000 + amount_4000 + amount_5000 +
        amount_6000 + amount_7000 + amount_8000 + amount_9000 + amount_10000 +
        incentive_tax_deduction + incentive_sales_tax_exemption +
        incentive_rebate_from_dealer + incentive_rebate_from_oem +
        incentive_rebate_from_the_government +
        timing_6_8_weeks + timing_3_months + timing_1_year | 0)
}

# Save results
saveRDS(models, here::here('survey', 'samplesize', 'models', 'baseline.Rds'))

# ---------------------------------------------------------------------------
# Interaction model

# ---------------------------------------------------------------------------
# Mixed logit model

    models[[i]] = mlogit(tempDf, 
        formula = choice ~
            amount +
            incentive_tax_deduction + incentive_sales_tax_exemption +
            incentive_rebate_from_dealer + incentive_rebate_from_oem +
            incentive_rebate_from_the_government +
            timing_6_8_weeks + timing_3_months + timing_1_year | 0,
        rpar = c(amount = 'n',
            incentive_tax_deduction = 'n',
            incentive_sales_tax_exemption = 'n',
            incentive_rebate_from_dealer = 'n',
            incentive_rebate_from_oem = 'n',
            incentive_rebate_from_the_government = 'n',
            timing_6_8_weeks = 'n',
            timing_3_months = 'n',
            timing_1_year = 'n'),
        R = 100, halton = NA)

# ---------------------------------------------------------------------------
# Plot results

baseline <- readRDS(here::here(
    'survey', 'samplesize', 'models', 'baseline.Rds')) %>%
    getSE() %>%
    formatSE()
linear_price <- readRDS(here::here(
    'survey', 'samplesize', 'models', 'linear_price.Rds')) %>%
    getSE() %>%
    formatSE()
interaction <- readRDS(here::here(
    'survey', 'samplesize', 'models', 'interaction.Rds')) %>%
    getSE() %>%
    formatSE()
mixed <- readRDS(here::here(
    'survey', 'samplesize', 'models', 'mixed.Rds')) %>%
    getSE() %>%
    formatSE()

baseline_plot <- ggplot(baseline, aes(x = size, y = se, color = category)) +
    geom_point() +
    theme_bw()

linear_price_plot <- ggplot(linear_price, aes(x = size, y = se, color = category)) +
    geom_point() +
    theme_bw()

interaction_plot <- ggplot(interaction, aes(x = size, y = se, color = category)) +
    geom_point() +
    theme_bw()

mixed_plot <- ggplot(mixed, aes(x = size, y = se, color = category)) +
    geom_point() +
    theme_bw()

comparison <- bind_rows(
    mutate(baseline, model = 'baseline'),
    mutate(linear_price, model = 'linear_price'),
    mutate(interaction, model = 'interaction'),
    mutate(mixed, model = 'mixed')) %>%
    ggplot(aes(x = size, y = se, color = category)) +
    geom_point() +
    facet_wrap(~model, nrow = 1) +
    theme_bw()

ggsave(here::here('survey', 'samplesize', 'plots', 'baseline.pdf'),
       baseline_plot, width = 5, height = 3)

ggsave(here::here('survey', 'samplesize', 'plots', 'linear_price.pdf'),
       linear_price_plot, width = 5, height = 3)

ggsave(here::here('survey', 'samplesize', 'plots', 'interaction.pdf'),
       interaction_plot, width = 5, height = 3)

ggsave(here::here('survey', 'samplesize', 'plots', 'mixed.pdf'),
       mixed_plot, width = 5, height = 3)

ggsave(here::here('survey', 'samplesize', 'plots', 'comparison.pdf'),
       comparison, width = 11, height = 3)
