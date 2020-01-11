source(here::here('survey', 'pilot8', 'functions.R'))
source(here::here('survey', 'pilot8', 'filterCases.R'))

# Set number of respondents and respondent question
nResp        <- 200 # Number of respondents
nAltsPerQ    <- 3 # Number of alternatives per question
nQPerResp    <- 6 # Number of questions per respondent

# Define global leg mode vars:
car     = 'Car'
express = 'Car:\nExpress'
uber    = 'Uber/Lyft'
taxi    = 'Taxi'
bus     = 'Bus'
walk    = 'Walk'
none    = 'None'
