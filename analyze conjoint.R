

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




library(ChoiceModelR)
library(tidyr)
library(ggplot2)
key$Set = NULL
df$ID = 1:nrow(df)
nr_participants = nrow(df)

#long format with row per participant and set
df = df %>% pivot_longer(-ID, names_to = "set", values_to = "answer")
#correct set variable
df$set = substr(df$answer,1,nchar(df$answer)-1)

#format response variable
df$y = substr(df$answer,nchar(df$answer), nchar(df$answer))
df$y = tolower(df$y)
df$y = match(df$y, letters)
df$answer = NULL
df$set = rep(1:nrow(key), nrow(df)/nrow(key))
#set.seed(42); df$y[is.na(df$y)] = apply(df[is.na(df$y),] ,1, function(x){sample(df$y[!is.na(df$y) & df$set == x['set']], 1)}) #imputation


#format column and attribute names
colnames(key) = gsub("_a","",colnames(key));
colnames(key) = gsub("_b","",colnames(key));
if(nr_profiles > 2){colnames(key) = gsub("_c","",colnames(key))}
if(nr_profiles > 3){colnames(key) = gsub("_d","",colnames(key))}
colnames(key) =trimws(colnames(key))
attribute_names =trimws(unique(colnames(key)[colnames(key) != 'Set']))

nr_attributes = length(attribute_names)

#extract and count levels for attributes
sets = 1:nrow(key)
temp = key[,1:nr_attributes]
nr_levels = sapply(X = temp, FUN = function(x){length(unique(x))})
profile = sort(rep(1:nr_profiles, nrow(key)))
#put profiles of same set underneath each other (long format)
A_key = key[,1:nr_attributes]
B_key = key[,(nr_attributes +1): (2* nr_attributes)]
if(nr_profiles == 2){
  long_key = cbind(sets, profile, rbind(A_key, B_key))
}else if(nr_profiles == 3){C_key = key[,(2* nr_attributes + 1): (3* nr_attributes)]
long_key = cbind(sets, profile, rbind(A_key, B_key, C_key))
}else if(nr_profiles == 4){D_key = key[,(3* nr_attributes + 1): (4* nr_attributes)]
long_key = cbind(sets, profile, rbind(A_key, B_key, C_key, D_key))
}else{stop("Too few/many profiles per set")}

#go from all profiles to all profiles per participant in right format for choicemodelR
long_key = long_key[order(long_key$sets),]
long_df = long_key[rep(1:nrow(long_key), nr_participants),]
ID = sort(rep(df$ID, (nr_profiles)), decreasing = F)
long_df = cbind(ID, long_df)
#populate response variable in right way for choicemodelR
long_df$y = NA
long_df$ID_set = paste(long_df$ID, long_df$set, sep = "_")
past_ID_set = 0
for(i in 1:nrow(long_df)){
  current_ID_set = long_df$ID_set[i]
  if(current_ID_set  == past_ID_set){long_df$y[i] = 0
  }else{
    past_ID_set =  current_ID_set
    profile_choice = df$y[df$ID == long_df$ID[i] & df$set == long_df$sets[i]]
    
    long_df$y[i] = profile_choice}
  
}
long_df$ID_set = NULL
#long_df = long_df[!is.na(long_df$y),]

#numerical coding of attribute levels
colnames(long_df) = trimws(colnames(long_df))
long_df[,colnames(long_df) %in% attribute_names] = sapply(long_df[,colnames(long_df) %in% attribute_names], function(x){as.numeric(factor(x))})
#fit model with normal prior centered on zero with variance = 2
long_df = as.matrix(long_df)
xcoding = rep(0, nr_attributes)
mcmc = list(R = 4000, use = 3500)
options = list(none=none_option, save=TRUE, keep=1)
source('C:/Users/hannesrosenbusch/Documents/introducing conjoint/cust_choicemodelr.R')
out = cust_choicemodelr(long_df, xcoding, mcmc = mcmc, options = options)

#average across mcmc samples to obtain coefficients for each participant
estbetas = apply(out$betadraw,c(1,2),mean) 
# low_betas = apply(out$betadraw,c(1,2),function(x){quantile(x, 0.05)})
# high_betas = apply(out$betadraw,c(1,2),function(x){quantile(x, 0.95)})

#append coefficients for reference categories
#    myestbetas = cbind(estbetas[,1:3],0-apply(estbetas[,1:3],1,sum),estbetas[,4:5],0-apply(estbetas[,4:5],1,sum), estbetas[,6:8],0-apply(estbetas[,6:8],1,sum), estbetas[,9], 0-estbetas[,9])
#    high_betas = cbind(high_betas[,1:3],0-apply(high_betas[,1:3],1,sum),high_betas[,4:5],0-apply(high_betas[,4:5],1,sum), high_betas[,6:8],0-apply(high_betas[,6:8],1,sum), high_betas[,9], 0-high_betas[,9])
#    low_betas = cbind(low_betas[,1:3],0-apply(low_betas[,1:3],1,sum),low_betas[,4:5],0-apply(low_betas[,4:5],1,sum), low_betas[,6:8],0-apply(low_betas[,6:8],1,sum), low_betas[,9], 0-low_betas[,9])
prev_index = 0
reduced_nr_levels = cumsum(nr_levels -1)
betas_per_attribute = c()
for(i in 1:length(reduced_nr_levels)){
  betas_non_ref = estbetas[,(prev_index+1):reduced_nr_levels[i]]
  if((prev_index+1)==reduced_nr_levels[i]){
    beta_ref =  0 - betas_non_ref
  }else{
    beta_ref = 0 - apply(betas_non_ref,1,sum)} 
  betas_attr = cbind(betas_non_ref, beta_ref )
  betas_per_attribute = cbind(betas_per_attribute, betas_attr)
  prev_index = reduced_nr_levels[i]}

#average coefficients across participants
betas = apply(betas_per_attribute, 2, mean)
names(betas) = NULL
# highs = apply(high_betas, 2, mean)
# lows = apply(low_betas, 2, mean)

#assign attribute and level name to coefficient
all_levels = c()
all_attributes = c()
for(attr in attribute_names){
  attr_levels = sort(unique(A_key[,attr]), decreasing = F)
  all_levels = c(all_levels, attr_levels)
  all_attributes = c(all_attributes, rep(attr, length(attr_levels)))}


result = as.data.frame(cbind(all_levels, betas, all_attributes))
result$betas = as.numeric(result$betas)
# result$highs = as.numeric(result$highs)
# result$lows = as.numeric(result$lows)

#prep dfs for plotting
importance = c()
for(attr in attribute_names){
  mini_b = min(result$betas[result$all_attributes == attr])
  result$betas[result$all_attributes == attr] = result$betas[result$all_attributes == attr] - mini_b + 0.1
  # result$highs[result$all_attributes == attr] = result$highs[result$all_attributes == attr] - mini_b + 0.1
  # result$lows[result$all_attributes == attr] = result$lows[result$all_attributes == attr] - mini_b + 0.1
  maxi_b = max(result$betas[result$all_attributes == attr])
  importance = c(importance, maxi_b)}
importance_df = as.data.frame(cbind(attribute_names, importance))
importance_df$importance = as.numeric(importance_df$importance)
importance_df$importance = importance_df$importance /sum(importance_df$importance) *100
Encoding(result$all_attributes) = "UTF-8"
Encoding(result$all_levels) = "UTF-8"
Encoding(importance_df$attribute_names) = "UTF-8"

#plot importance
importance_plot = ggplot(importance_df) + 
  geom_bar(aes(x = reorder(attribute_names, -1*importance),y = importance, fill = attribute_names), stat = "identity", show.legend = FALSE) +
  theme_bw() + labs(x = element_blank(), y = "Importance", title = "Importance of different attributes") + theme(text = element_text(size = 18))

#plot utilities
utility_plot = ggplot(result) + 
  geom_bar(aes(x = reorder(all_levels, betas), y = betas, fill = all_attributes), stat = "identity", show.legend = FALSE) +  
  #geom_line(aes(x = reorder(all_levels, betas), y = betas, group = 1)) +
  #geom_errorbar(aes(x = reorder(all_levels, betas), ymin = lows, ymax = highs), width = 0.3) +
  facet_wrap(~all_attributes,  scales = "free_x") +
  theme_bw() + labs(x = element_blank(), y = "Utilities", title = "Utilities of specific features")+ theme(text = element_text(size = 18))

#make utility ranking
l1 = split(result$all_levels, result$all_attributes)
all_profiles = expand.grid(l1)
all_profiles = data.frame(lapply(all_profiles,  as.character))
sum(result$betas[result$all_levels %in% all_profiles[1,]])
all_profiles["Utility"] = apply(all_profiles, 1, function(x){
  sum(result$betas[result$all_levels %in% x])})
all_profiles = all_profiles[order(all_profiles$Utility, decreasing = T),]
Rank = 1:nrow(all_profiles)
all_profiles = cbind(Rank, all_profiles)
rownames(all_profiles) = NULL


