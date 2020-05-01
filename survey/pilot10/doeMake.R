source(here::here('survey', 'pilot9', 'functions.R'))

# Read in balanced full factorial
FF_bal_no <- readRDS(here::here(
    'survey', 'pilot9', 'survey', 'doe', 'ff_balanced_no.Rds'))
FF_bal_yes <- readRDS(here::here(
    'survey', 'pilot9', 'survey', 'doe', 'ff_balanced_yes.Rds'))

# Set number of respondents and respondent question
nResp     <- seq(500, 10000, 500) # Number of respondents
nAltsPerQ <- 3 # Number of alternatives per question
nQPerResp <- 6 # Number of questions per respondent

# Randomly sample from the FF_bal to evenly fit the desired sample size
doe <- list()
for (i in 1:length(nResp)) {
    size         <- nResp[i]
    nRowsPerResp <- nAltsPerQ*nQPerResp
    nRows        <- size*nRowsPerResp
    doeIDs_no    <- sample(x=seq(nrow(FF_bal_no)), size=nRows, replace=T)
    doeIDs_yes   <- sample(x=seq(nrow(FF_bal_yes)), size=nRows, replace=T)
    doe_no       <- FF_bal_no[doeIDs_no,]
    doe_yes      <- FF_bal_yes[doeIDs_yes,]
    # Make sure no two identical alts appear in one question
    doe[[i]] <- list(
        no  = removeDoubleAlts(doe_no, nAltsPerQ, nQPerResp),
        yes = removeDoubleAlts(doe_yes, nAltsPerQ, nQPerResp))
}

# Save does
names(doe) <- nResp
saveRDS(doe, here::here('survey', 'pilot9', 'survey', 'doe', 'doe.Rds'))
