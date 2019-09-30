library(here)
source(here::here('survey', 'pilot7', 'functions.R'))
library(mlogit)

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

# Set the sample sizes
numResp <- seq(100, 3000, 300)
for (size in numResp) {
    tempDf <- getTempDf(doeAll, doeNoCar, size)
    tempDf <- dummyCode(tempDf, vars = c('leg1Mode', 'leg2Mode', 'leg3Mode'))
    # Convert the data to "mlogit" format:
    tempDf = mlogit.data(
        data    = tempDf,
        shape   = 'long',
        choice  = 'choice',
        alt.var = 'altID')
    # Run the model:
    model_linear = mlogit(tempDf, formula = choice ~
        leg1Mode_Car + leg1Mode_Car:\nExpress + leg1Mode_Taxi + 
        leg1Mode_Uber/Lyft + 
        leg2Mode_None + leg2Mode_Walk + 
        leg3Mode_None + leg3Mode_Taxi + leg3Mode_Uber/Lyft +
        leg1Time + leg2Time + leg3Time +
        transfer1Time + transfer31Time + transfer3Time +
        price + expressFee + tripTimeUnc | 0)   # Levels: 0, 1
    
    



    
}


# -----------------------------------------------------------------------------
# Estimate MNL linear model:



# View summary of results
# Check the 1st order condition: Is the gradient at the solution zero?
summary(model_linear)

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model_linear$hessian)$values
