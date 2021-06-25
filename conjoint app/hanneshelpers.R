resample_without_creating_duplicates = function(piles, three = T){
  r_seed = 42
  pile1 = piles[[1]]
  best_pile2 = piles[[2]]
  if(three){best_pile3 = piles[[3]]}
  
  #resample pile1 
  set.seed(r_seed);pile1 = pile1[sample(nrow(pile1)),]
  
  #resample best_pile2 and pile 3
  duplicates_within_sets = T
  if(three){
    while(duplicates_within_sets){
      r_seed = r_seed + 1
      set.seed(r_seed); best_pile2 = best_pile2[sample(nrow(best_pile2)),]
      r_seed = r_seed + 1
      set.seed(r_seed);best_pile3 = best_pile3[sample(nrow(best_pile3)),]
      duplicates_within_sets = any(rowSums(pile1==best_pile3) == ncol(pile1)) | any(rowSums(pile1==best_pile2) == ncol(pile1)) | any(rowSums(best_pile2==best_pile3) == ncol(pile1))
    }

    randomized_piles = list(pile1, best_pile2, best_pile3)
  }else{
    while(duplicates_within_sets){
      r_seed = r_seed + 1
      set.seed(r_seed); best_pile2 = best_pile2[sample(nrow(best_pile2)),]
      duplicates_within_sets = any(rowSums(pile1==best_pile2) == ncol(pile1)) 
    }
    randomized_piles = list(pile1, best_pile2)
  }
  
  return(randomized_piles)
}

mix_match = function(pile1, third_pile = T){
  pile1[] <- lapply(pile1, as.character)
  #make pile2
  smallest_overlap = 99999
  pile2 = pile1
  
  for(column in colnames(pile1)){
    vals = sort(unique(pile1[,column]))
    perms = DescTools::Permn(vals)
    for(p in 1:nrow(perms)){
      rec_vals = perms[p,]
      for(i in 1:length(vals)){
        pile2[pile1[column] == vals[i],column] = rec_vals[i]
      }
      overlap = nrow(intersect(pile1, pile2))
      if(overlap < smallest_overlap){smallest_overlap = overlap; best_pile2 = pile2}
      if(overlap == 0){break}
    }
  }
  piles = list(pile1, best_pile2)
  #make pile 3
  if(third_pile){
    pile3 = pile2
    smallest_overlap = 99999
    
    for(column in colnames(pile1)){
      vals = sort(unique(pile1[,column]))
      perms = DescTools::Permn(vals)
      for(p in 1:nrow(perms)){
        rec_vals = perms[p,]
        for(i in 1:length(vals)){
          pile3[pile1[column] == vals[i],column] = rec_vals[i]
        }
        overlap = nrow(intersect(pile1, pile3)) + nrow(intersect(pile2, pile3))
        if(overlap < smallest_overlap){smallest_overlap = overlap; best_pile3 = pile3}
        if(overlap == 0){break}
      }
    }
    piles = c(piles, list(best_pile3))
  }
  piles = resample_without_creating_duplicates(piles, third_pile)
  
  return(piles)
}


plot_set = function(sets, set_number = 1, aest1, aest2, aest3, imagepath){
sets['Set'] = NULL
colnames(sets) = gsub('_a', '', colnames(sets));colnames(sets) = gsub('_b', '', colnames(sets));colnames(sets) = gsub('_c', '', colnames(sets))
  
elements1 = sets[set_number,1:(ncol(sets)/3)]
elements1=elements1[,order(ncol(elements1):1)]
colnames(elements1) = paste(colnames(elements1), ":", "")

if(!is.na(imagepath)){jp = readJPEG(imagepath)
                      g = rasterGrob(jp, interpolate=TRUE)
}else{g = NA}

set1 = ggplot() +  
  geom_text(aes(x = aest1['gap']/10, y = 1:ncol(elements1),
                label = unlist(elements1[1,]), hjust = 0,fontface = "bold"), size = aest1['font_size_vals']) +
  geom_text( aes(x = 0, y = 1:ncol(elements1), 
                label = colnames(elements1)), hjust = 1, size = aest3['font_size_keys']) +
  theme_bw()+ 
  scale_y_continuous(breaks = NULL, limits = c(-1*aest1['bottom_buffer'],ncol(elements1)+1+aest1['top_buffer'])) + 
  scale_x_continuous(breaks = NULL, limits = c( -0.6- aest1['left_buffer'],1 )) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  {
    if(!all(is.na(g))){annotation_custom(g, xmin=aest1['left_pic'], xmax=aest1['right_pic'], ymin=aest1['bottom_pic'], ymax=aest1['top_pic'])}
  }

elements2 = sets[set_number,((ncol(sets)/3)+1):(2*ncol(sets)/3)]
colnames(elements2) = paste(colnames(elements2), ":", "")
elements2=elements2[,order(ncol(elements2):1)]

set2 = ggplot() +  
  geom_text(aes(x = aest2['gap']/10, y = 1:ncol(elements2), 
                label = unlist(elements2[1,]), hjust = 0,fontface = "bold"), size = aest2['font_size_vals']) +
  geom_text(aes(x = 0, y = 1:ncol(elements2), 
                label = colnames(elements2)), hjust = 1, size = aest2['font_size_keys']) +
  theme_bw()+ 
  scale_y_continuous(breaks = NULL, limits = c(-1*aest2['bottom_buffer'],ncol(elements2)+1+aest2['top_buffer'])) + 
  scale_x_continuous(breaks = NULL, limits = c(-0.6-  aest2['left_buffer'],1)) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())  +{
    if(!all(is.na(g))){annotation_custom(g, xmin=aest2['left_pic'], xmax=aest2['right_pic'], ymin=aest2['bottom_pic'], ymax=aest2['top_pic'])}
  }

elements3 = sets[set_number,((2*ncol(sets)/3) +1):ncol(sets)]
colnames(elements3) = paste(colnames(elements3), ":", "")
# colnames(elements3) = gsub('.2','',colnames(elements3))
elements3=elements3[,order(ncol(elements3):1)]
set3 = ggplot() +  
  geom_text(aes(x = aest3['gap']/10, y = 1:ncol(elements3), 
                label = unlist(elements3[1,]), hjust = 0,fontface = "bold"), size = aest3['font_size_vals']) +
  geom_text(aes(x = 0, y = 1:ncol(elements3), 
                label = colnames(elements3)), hjust = 1, size = aest3['font_size_keys']) +
  theme_bw()+ 
  scale_y_continuous(breaks = NULL, limits = c(-1*aest3['bottom_buffer'],ncol(elements3)+1+aest3['top_buffer'])) + 
  scale_x_continuous(breaks = NULL, limits = c(-0.6-  aest3['left_buffer'],1)) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + {
    if(!all(is.na(g))){annotation_custom(g, xmin=aest3['left_pic'], xmax=aest3['right_pic'], ymin=aest3['bottom_pic'], ymax=aest3['top_pic'])}
  }
list(set1,set2,set3)

}


get_a_png_plot <- function(sets, set_number, aest1, aest2, aest3, profile){
  if(profile == "a"){
  p = plot_set(sets, set_number, aest1, aest2, aest3)[[1]]
  }else if(profile == "b"){
    p= plot_set(sets, set_number, aest1, aest2, aest3)[[2]]
  }else if(profile == "c"){
    p= plot_set(sets, set_number, aest1, aest2, aest3)[[3]]
  }else{stop("profile isnt a, b, or c")}
  p

}


#https://rdrr.io/cran/ChoiceModelR/man/choicemodelr.html  R = total number of iterations of the Markov chain Monte Carlo use = the number of iterations to be used in parameter estimation

importance_utility_ranking = function(df, key, nr_profiles, none_option){
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
  mcmc = list(R = 4000, use = 3500) 
  options = list(none=FALSE, save=TRUE, keep=1)
  out = choicemodelr(long_df, xcoding, mcmc = mcmc, options = options)
  
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
  head(all_profiles)
  return(list(importance_plot, utility_plot, all_profiles))
}

