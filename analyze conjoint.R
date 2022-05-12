

# test function -----------------------------------------------------------
source('C:/Users/hannesrosenbusch/Documents/introducing conjoint/conjoint app/hanneshelpers.R')

#df = data.table::fread('C:/Users/hannesrosenbusch/Documents/introducing conjoint/ice cream conjoint/adminexport400.csv', data.table = F)
#df = data.table::fread('C:/Users/hannesrosenbusch/Documents/introducing conjoint/attractiveness conjoint/adminexport.csv', data.table = F)
df = data.table::fread('C:/Users/hannesrosenbusch/Documents/introducing conjoint/schoko conjoint/ChocoConjoint.csv', data.table = F)
df[,grepl('15. Welches dieser', colnames(df))] = NULL; df = df[,grepl('Welches dieser', colnames(df))] #select relevant columns

#df = df[,grepl('Welche dies', colnames(df))] #select relevant columns
#key = data.table::fread('C:/Users/hannesrosenbusch/Documents/introducing conjoint/ice cream conjoint/choice sets-2021-06-18.csv', data.table = F)
#key = data.table::fread('C:/Users/hannesrosenbusch/Documents/introducing conjoint/attractiveness conjoint/ANALYSES CODES DO NOT DELETE.csv', data.table = F)
key = data.table::fread('C:/Users/hannesrosenbusch/Documents/introducing conjoint/schoko conjoint/ANALYSES CODES DO NOT DELETE.csv', data.table = F)

nr_profiles = 3
# r = importance_utility_ranking(df = df,key = key, nr_profiles = nr_profiles, none_option = none_option)
# r[[1]]
# r[[2]]
# r[[3]]
  
library(tidyr)
library(ggplot2)
key$Set = NULL
df$ID = 1:nrow(df)
nr_participants = nrow(df)
#long format with row per participant and set
df = df %>% pivot_longer(-ID, names_to = "set", values_to = "answer")

#format response variable
df$y = substr(df$answer,nchar(df$answer), nchar(df$answer))
df$y = tolower(df$y)
if("e" %in% df$y){none_option=T;df$y[df$y == "e"] = "d"
}else{none_option = F}

df$y = match(df$y, letters)
df$answer = NULL
df$set = rep(1:nrow(key), nrow(df)/nrow(key))
#set.seed(42); df$y[is.na(df$y)] = apply(df[is.na(df$y),] ,1, function(x){sample(df$y[!is.na(df$y) & df$set == x['set']], 1)})


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

#numerical coding of attribute levels
colnames(long_df) = trimws(colnames(long_df))
long_df[,colnames(long_df) %in% attribute_names] = sapply(long_df[,colnames(long_df) %in% attribute_names], function(x){as.numeric(factor(x))})
#fit model with normal prior centered on zero with variance = 2
long_df = as.matrix(long_df)
xcoding = rep(0, nr_attributes)
mcmc = list(R = 4000, use = 3000) 
options = list(none=none_option, save=TRUE, keep=1)

out = cust_choicemodelr(long_df, xcoding, mcmc = mcmc, options = options)

betas = colMeans(out$betawrite)
cumu_nr_levels = cumsum(nr_levels)
#prep dfs for plotting
importance = c()
prev_index = 0
all_attributes = c()
all_levels = c()
for(i in 1:length(cumu_nr_levels)){
  mini_b = min(betas[(prev_index+1):(cumu_nr_levels[i])])
  maxi_b = max(betas[(prev_index+1):(cumu_nr_levels[i])])
  
  importance = c(importance, rep(maxi_b - mini_b, nr_levels[i]))
  
  all_attributes = c(all_attributes, rep(attribute_names[i], nr_levels[i]))
  attr_levels = sort(unique(A_key[,attribute_names[i]]), decreasing = F)
  all_levels = c(all_levels, attr_levels)
  prev_index = cumu_nr_levels[i]}

if(none_option){all_attributes = c(all_attributes, "None"); importance = c(importance, betas[length(betas)]); all_levels = c(all_levels, "None")}


plotting_df = as.data.frame(cbind(all_attributes, all_levels, importance, betas))
plotting_df$betas = as.numeric(plotting_df$betas)

plotting_df$importance = as.numeric(plotting_df$importance)
plotting_df$importance = plotting_df$importance /sum(plotting_df$importance[plotting_df$all_attributes != "None" & !duplicated(plotting_df$all_attributes)]) *100

Encoding(plotting_df$all_levels) = Encoding(plotting_df$all_attributes) = "UTF-8"

#plot importance
importance_plot = ggplot(plotting_df[!duplicated(plotting_df$all_attributes) & plotting_df$all_attributes != "None",]) + 
  geom_bar(aes(x = reorder(all_attributes, -1*importance),y = importance, fill = reorder(all_attributes, -1*importance)), stat = "identity", show.legend = FALSE) +
  theme_bw() + labs(x = element_blank(), y = "Importance", title = "Importance of different attributes") + theme(text = element_text(size = 18))



#plot utilities
utility_plot = ggplot(plotting_df[plotting_df$all_attributes != "None",]) + 
  geom_bar(aes(x = reorder(all_levels, -betas), y = betas, fill = all_attributes), stat = "identity", show.legend = FALSE) +  
  #geom_line(aes(x = reorder(all_levels, betas), y = betas, group = 1)) +
  #geom_errorbar(aes(x = reorder(all_levels, betas), ymin = lows, ymax = highs), width = 0.3) +
  facet_wrap(~all_attributes,  scales = "free_x") +
  geom_hline(aes(yintercept = 0))+
  theme_bw() + labs(x = element_blank(), y = "Utilities", title = "Utilities of specific features")+ theme(text = element_text(size = 18))

#make utility ranking
l1 = split(plotting_df$all_levels, plotting_df$all_attributes)
all_profiles = expand.grid(l1)
all_profiles = data.frame(lapply(all_profiles,  as.character))
all_profiles$None = NULL
all_profiles["Utility"] = apply(all_profiles, 1, function(x){
  round(sum(plotting_df$betas[plotting_df$all_levels %in% x]), 3)})

all_profiles = all_profiles[order(all_profiles$Utility, decreasing = T),]
if(tolower(plotting_df$all_levels[nrow(plotting_df)]) == "none"){
  all_profiles = rbind(c(rep("-", (ncol(all_profiles)-1)), round(as.numeric(betas[length(betas)]), 3)), all_profiles)
}
all_profiles$Utility = as.numeric(all_profiles$Utility)
Rank = nrow(all_profiles) - rank(all_profiles$Utility - min(all_profiles$Utility)) + 1
all_profiles = cbind(Rank, all_profiles)


#coefficient comparisons


avg_betas_draws = apply(out$betadraw,c(2,3),mean) 
# avg_betas = apply(avg_betas_draws,1,mean) 
# low_betas = apply(avg_betas_draws,1,function(x){quantile(x, 0.05)})
# high_betas = apply(avg_betas_draws,1,function(x){quantile(x, 0.95)})
#append coefficients for reference categories
#    myestbetas = cbind(estbetas[,1:3],0-apply(estbetas[,1:3],1,sum),estbetas[,4:5],0-apply(estbetas[,4:5],1,sum), estbetas[,6:8],0-apply(estbetas[,6:8],1,sum), estbetas[,9], 0-estbetas[,9])
#    high_betas = cbind(high_betas[,1:3],0-apply(high_betas[,1:3],1,sum),high_betas[,4:5],0-apply(high_betas[,4:5],1,sum), high_betas[,6:8],0-apply(high_betas[,6:8],1,sum), high_betas[,9], 0-high_betas[,9])
#    low_betas = cbind(low_betas[,1:3],0-apply(low_betas[,1:3],1,sum),low_betas[,4:5],0-apply(low_betas[,4:5],1,sum), low_betas[,6:8],0-apply(low_betas[,6:8],1,sum), low_betas[,9], 0-low_betas[,9])
prev_index = 1
prev_r_index = 0
reduced_nr_levels = cumsum(nr_levels -1)
cs_nrlevels = cumsum(nr_levels)
within_attribute_beta_comparisons = data.frame(matrix(ncol = 0, nrow = 1))
between_attribute_importance_comparisons = data.frame(matrix(ncol = 0, nrow = mcmc$use))
for(i in 1:length(nr_levels)){
  
  betas_non_ref = avg_betas_draws[(prev_r_index+1):(reduced_nr_levels[i]),] #2, 3500
  
  if((prev_r_index+1)==reduced_nr_levels[i]){
    beta_ref =  0 - betas_non_ref                                       #3500
    
  }else{
    beta_ref = 0 - apply(betas_non_ref,2,sum)}
  
  
  betas_attr = rbind(betas_non_ref, beta_ref )
  between_attribute_importance_comparisons[attribute_names[i]] = apply(betas_attr, 2, function(x){max(x)-min(x)})
  start = ifelse(i == 1, 1, cs_nrlevels[i-1]+1)
  rownames(betas_attr) = all_levels[start:(cs_nrlevels[i])]
  comparisons = combn(rownames(betas_attr), 2)
  
  for(c in 1:ncol(comparisons)){
    comp = comparisons[,c]
    percen = round(sum(betas_attr[comp[1],] > betas_attr[comp[2],])/ncol(betas_attr) *100, 1)
    if(percen < 50){
      percen = 100 - percen
      within_attribute_beta_comparisons[paste(comp[2], ">", comp[1])] = paste0(percen, "%")
    }else{
      within_attribute_beta_comparisons[paste(comp[1], ">", comp[2])] = paste0(percen, "%")
  }}
  
  prev_r_index = reduced_nr_levels[i]
  prev_index = cs_nrlevels[i]}


imp_comparisons = combn(colnames(between_attribute_importance_comparisons), 2)
between_attribute_importance_comparisons_df = data.frame(matrix(ncol = 0, nrow = 1))
for(i in 1:ncol(imp_comparisons)){
  
  comp = imp_comparisons[,i]
  percen = round(sum(between_attribute_importance_comparisons[,comp[1]] > between_attribute_importance_comparisons[,comp[2]])/nrow(between_attribute_importance_comparisons) *100, 1)
  if(percen < 50){
    percen = 100 - percen
  between_attribute_importance_comparisons_df[paste(comp[2], ">", comp[1])] = paste0(percen, "%")
  }else{
    between_attribute_importance_comparisons_df[paste(comp[1], ">", comp[2])] = paste0(percen, "%")
  }
  
}
between_attribute_importance_comparisons_df = t(between_attribute_importance_comparisons_df)
Encoding(rownames(between_attribute_importance_comparisons_df )) = "UTF-8"
colnames(between_attribute_importance_comparisons_df) = "Probability of higher importance"
within_attribute_beta_comparisons = t(within_attribute_beta_comparisons)
Encoding(rownames(within_attribute_beta_comparisons)) = "UTF-8"
colnames(within_attribute_beta_comparisons) = "Probability of superiority"