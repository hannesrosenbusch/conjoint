

# test function -----------------------------------------------------------
source('C:/Users/hannesrosenbusch/Documents/introducing conjoint/conjoint app/hanneshelpers.R')

df = data.table::fread('C:/Users/hannesrosenbusch/Documents/introducing conjoint/ice cream conjoint/adminexport400.csv', data.table = F)
df = data.table::fread('C:/Users/hannesrosenbusch/Documents/introducing conjoint/attractiveness conjoint/adminexport.csv', data.table = F)
#df = df[,grepl('Welche dieser', colnames(df))] #select relevant columns

df = df[,grepl('attraktivste', colnames(df))] #select relevant columns
key = data.table::fread('C:/Users/hannesrosenbusch/Documents/introducing conjoint/ice cream conjoint/choice sets-2021-06-18.csv', data.table = F)
key = data.table::fread('C:/Users/hannesrosenbusch/Documents/introducing conjoint/attractiveness conjoint/ANALYSES CODES DO NOT DELETE.csv', data.table = F)

nr_profiles = 3
none_option = FALSE
r = importance_utility_ranking(df = df,key = key, nr_profiles = nr_profiles, none_option = none_option)
r[[1]]
r[[2]]
r[[3]]



