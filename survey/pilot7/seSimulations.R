library(here)
source(here::here('survey', 'pilot7', 'functions.R'))
library(mlogit)
library(cowplot)

# Load DOE from github (same process as formr will do)
rootPath <- "https://raw.githubusercontent.com/jhelvy/tmfConjoint/master/survey/pilot7/survey/"
doeNoCarPath <- paste(rootPath, 'doeNoCar.csv', sep = '')
doeAllPath <- paste(rootPath, 'doeAll.csv', sep = '')
doeAll <- fread(doeAllPath)
doeNoCar <- fread(doeNoCarPath)

# Define functions
dummyCode = function(df, vars) {
    df = as.data.frame(df)
    nonVars = colnames(df)[which(! colnames(df) %in% vars)]
    # Keep the original variables and the order to restore later after merging
    df$order = seq(nrow(df))
    for (i in 1:length(vars)) {
        var      = vars[i]
        colIndex = which(colnames(df) == var)
        levels   = sort(unique(df[,colIndex]))
        mergeMat = as.data.frame(diag(length(levels)))
        mergeMat = cbind(levels, mergeMat)
        colnames(mergeMat) = c(var, paste(var, levels, sep='_'))
        df = merge(df, mergeMat)
    }
    # Restore the original column order
    new = colnames(df)[which(! colnames(df) %in% c(vars, nonVars))]
    df = df[c(nonVars, vars, new)]
    # Restore the original row order
    df = df[order(df$order),]
    row.names(df) = df$order
    df$order <- NULL
    return(df)
}

getTempDf <- function(doeAll, doeNoCar, size) {
    respIDs <- sample(seq(max(doeAll$respID)), size)
    tempDfAll <- doeAll[respID %in% respIDs]
    tempDfNoCar <- doeNoCar[respID %in% respIDs]
    tempDf <-  bind_rows(tempDfAll, tempDfNoCar)
    tempDf$obsID <- rep(seq(nrow(tempDf) / 3), each = 3)
    # Assign random choices
    choices <- sample(seq(3), max(tempDf$obsID), replace = TRUE)
    choices <- diag(3)[choices,]
    tempDf$choice <- matrix(t(choices))
    return(tempDf)
}

recodeTempDf <- function(tempDf) {
    tempDf <- dummyCode(tempDf, vars = c(
        'leg1Mode', 'leg2Mode', 'leg3Mode', 'numLegs'))
    # Rename columns
    colnames(tempDf) <- str_replace_all(colnames(tempDf), '\n', '')
    colnames(tempDf) <- str_replace_all(colnames(tempDf), ':', '')
    colnames(tempDf) <- str_replace_all(colnames(tempDf), '/', '')
    # Convert the data to "mlogit" format:
    tempDf = mlogit.data(
        data    = tempDf,
        shape   = 'long',
        choice  = 'choice',
        alt.var = 'altID')
    return(tempDf)
}

# ---------------------------------------------------------------------------

# Run models for different sample sizes
numResp <- seq(100, 3000, 300)
index <- 1
models <- list()
for (i in 1:length(numResp)) {
    size <- numResp[i]
    tempDf <- getTempDf(doeAll, doeNoCar, size)
    tempDf <- recodeTempDf(tempDf)
    # Run the model:
    models[[i]] = mlogit(tempDf, formula = choice ~
        price + expressFee + tripTimeUnc +
        totalLegTime + totalWaitTime +
        numLegs_2 + numLegs_3 +
        leg1Mode_Car + leg1Mode_CarExpress + leg1Mode_Taxi + leg1Mode_UberLyft +
        leg2Mode_Walk +
        leg3Mode_Taxi + leg3Mode_UberLyft | 0)
}

# Get the standard errors out of the models
coefs1 <- coef(models[[1]])
se <- as.data.frame(matrix(0, ncol=length(coefs1) + 1, nrow=length(numResp)))
colnames(se) <- c('size', names(coefs1))
for (i in 1:length(models)) {
    model <- models[[i]]
    se[i, ] <- c(2*numResp[i], sqrt(abs(diag(solve(model$hessian)))))
}

# Plot results 
se %>% 
    gather(coef, se, price:leg3Mode_UberLyft) %>% 
    ggplot(aes(x = size, y = se, color = coef)) + 
    geom_line() + 
    theme_cowplot() +
    facet_wrap(~coef)
