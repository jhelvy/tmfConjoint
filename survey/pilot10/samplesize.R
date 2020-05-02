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
getTempDf <- function(doeAll, size) {
    does <- doeAll[[as.character(size)]]
    doe_yes <- sample_frac(does[['yes']], 0.5)
    doe_no  <- sample_frac(does[['no']], 0.5)
    doe <- bind_rows(doe_yes, doe_no)
    doe <- addMetaData(doe, max(doe$altID), max(doe$qID))
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
        'numLegs', 'leg1Mode', 'leg2Mode', 'tripTimeUnc')) %>%
        clean_names('lower_camel') %>% 
        mutate(
            intTime1Car = leg1ModeCar*leg1Time, 
            intTime1CarExpress = leg1ModeCarExpress*leg1Time, 
            intTime1Taxi = leg1ModeUberTaxi*leg1Time, 
            intTime1Bus = leg1ModeBus*leg1Time, 
            intTime2Walk = leg2ModeWalk*leg2Time, 
            intTime2Taxi = leg2ModeUberTaxi*leg2Time, 
            intTime2Bus = leg2ModeBus*leg2Time
        )
    # Convert the data to "mlogit" format:
    tempDf = mlogit.data(
        data    = tempDf,
        shape   = 'long',
        choice  = 'choice',
        alt.var = 'altId')
    return(tempDf)
}

# Get the standard errors out of the models
getSE <- function(model, size) {
    se <- data.frame(
        size = nrow(model$gradient) / 6,
        se   = sqrt(abs(diag(solve(model$hessian)))))
    se$coef = row.names(se)
    row.names(se) <- NULL
    return(se)
}

# ---------------------------------------------------------------------------
# Run models

# Run models for different sample sizes
index <- 1
baseline <- list()
interaction <- list()
for (i in 1:length(nResp)) {
    size <- nResp[i]
    tempDf <- getTempDf(doeAll, size, hasCar = TRUE)
    tempDf <- recodeTempDf(tempDf)
    # Run the baseline model:
    model_baseline <- mlogit(tempDf, formula = choice ~
        price + expressFee +
        leg1ModeCar + leg1ModeCarExpress + leg1ModeUberTaxi + leg1ModeBus + 
        leg2ModeWalk + leg2ModeUberTaxi + leg2ModeBus +
        leg1Time + leg2Time + 
        transfer1Time + transfer2Time + 
        tripTimeUnc0_1 + tripTimeUnc0_2 | 0)
    # Run the interaction model:
    model_interaction <- mlogit(tempDf, formula = choice ~
        price + expressFee +
        leg1ModeCar + leg1ModeCarExpress + leg1ModeUberTaxi + leg1ModeBus + 
        leg2ModeWalk + leg2ModeUberTaxi + leg2ModeBus +
        leg1Time + leg2Time + 
        transfer1Time + transfer2Time + 
        tripTimeUnc0_1 + tripTimeUnc0_2 + 
        intTime1Car +
        intTime1CarExpress +
        intTime1Taxi +
        intTime1Bus +
        intTime2Walk +
        intTime2Taxi | 0)
    baseline[[i]] <- getSE(model_baseline)
    interaction[[i]] <- getSE(model_interaction)
}

# Save results
baseline <- do.call(rbind, baseline)
interaction <- do.call(rbind, interaction)
write_csv(baseline, here::here(
    'survey', 'pilot10', 'survey', 'samplesize', 'models', 'baseline.csv'))
write_csv(interaction, here::here(
    'survey', 'pilot10', 'survey', 'samplesize', 'models', 'interaction.csv'))

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

formatSE <- function(df) {
    df <- df %>% 
        mutate(category = case_when(
            coef %in% c('price', 'expressFee') ~ 'price', 
            str_detect(coef, 'Mode')           ~ 'mode',
            str_detect(coef, 'Time')           ~ 'time',
            TRUE                               ~ 'other',
        ))
    return(df)
}

baseline <- read_csv(here::here(
    'survey', 'pilot10', 'survey', 'samplesize', 'models',
    'baseline.csv')) %>% 
    formatSE()

interaction <- read_csv(here::here(
    'survey', 'pilot10', 'survey', 'samplesize', 'models',
    'interaction.csv')) %>% 
    formatSE()

baseline_plot <- ggplot(baseline, aes(x = size, y = se, color = category)) +
    geom_point(size = 1, alpha = 0.5) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    scale_x_continuous(breaks = nResp) +
    theme_minimal_hgrid()

interaction_plot <- ggplot(interaction, aes(x = size, y = se, color = category)) +
    geom_point(size = 1, alpha = 0.5) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    scale_x_continuous(breaks = nResp) +
    theme_minimal_hgrid()

comparison <- bind_rows(
    mutate(baseline, model = 'baseline'),
    mutate(interaction, model = 'interaction')) %>%
    ggplot(aes(x = size, y = se, color = category)) +
    geom_point(alpha = 0.5, size = 1) +
    facet_wrap(~model, nrow = 1) +
    theme_bw()

ggsave(here::here(
    'survey', 'pilot10', 'survey', 'samplesize', 'plots', 'baseline.pdf'),
    baseline_plot, width = 5, height = 3)

ggsave(here::here(
    'survey', 'pilot10', 'survey', 'samplesize', 'plots', 'interaction.pdf'),
    interaction_plot, width = 5, height = 3)

ggsave(here::here(
    'survey', 'pilot10', 'survey', 'samplesize', 'plots', 'comparison.pdf'),
    comparison, width = 7, height = 3)
