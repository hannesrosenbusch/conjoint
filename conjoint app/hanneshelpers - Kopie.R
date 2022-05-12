load("all_designchecks.RData")


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


plot_set = function(sets, set_number = 1, profile_number, aest, decorpath, none_text, imgs){
sets['Set'] = NULL
colnames(sets) = gsub('_a', '', colnames(sets));colnames(sets) = gsub('_b', '', colnames(sets));colnames(sets) = gsub('_c', '', colnames(sets))

elements = sets[set_number, ((profile_number-1) * ncol(sets)/3 + 1):(profile_number * ncol(sets)/3)]
elements=elements[,order(ncol(elements):1)]

if(sum(grepl("Image", colnames(elements))) > 0){
  imgpath = imgs[elements$Image]
  if(substr(imgpath, nchar(imgpath)-3, nchar(imgpath)) == ".png"){
    pn = readPNG(imgpath)
    g = rasterGrob(pn, interpolate=TRUE)
  }else if(substr(imgpath, nchar(imgpath)-3, nchar(imgpath)) == ".jpg" | substr(imgpath, nchar(imgpath)-4, nchar(imgpath)) == ".jpeg"){
    jp = readJPEG(imgpath)
    g = rasterGrob(jp, interpolate=TRUE)
  }else{stop("weird input format")}
}else if(!is.na(decorpath)){#this else if for imgs vs decor or let user overwrite?
 if(substr(decorpath, nchar(decorpath)-3, nchar(decorpath)) == ".png"){
   pn = readPNG(decorpath)
   g = rasterGrob(pn, interpolate=TRUE)
 }else if(substr(decorpath, nchar(decorpath)-3, nchar(decorpath)) == ".jpg" | substr(decorpath, nchar(decorpath)-4, nchar(decorpath)) == ".jpeg"){
  jp = readJPEG(decorpath)
  g = rasterGrob(jp, interpolate=TRUE)
 }else{stop("weird input format")}
}else{g = NA}


colnames(elements) = paste0(colnames(elements), ":")

profile_plot= ggplot() +  
  geom_text(aes(x = aest['gap']/10, y = 1:ncol(elements),
                label = unlist(elements[1,]), hjust = 0,fontface = "bold"), size = aest['font_size_vals']) +
  geom_text( aes(x = 0, y = 1:ncol(elements), 
                label = colnames(elements)), hjust = 1, size = aest['font_size_keys']) +
  theme_bw()+ 
  scale_y_continuous(breaks = NULL, limits = c(-1*aest['bottom_buffer'],ncol(elements)+1+aest['top_buffer'])) + 
  scale_x_continuous(breaks = NULL, limits = c( -0.6- aest['left_buffer'],1 )) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  {
    if(!all(is.na(g))){annotation_custom(g, xmin=aest['left_pic'], xmax=aest['right_pic'], ymin=aest['bottom_pic'], ymax=aest['top_pic'])}
  }



none_plot = ggplot() +  
  geom_text(aes(x = 0, y = 2, 
                label = none_text, fontface = "bold"), size = aest['font_size_vals']) +
  theme_bw()+ 
  scale_y_continuous(breaks = NULL, limits = c(0,4)) + 
  scale_x_continuous(breaks = NULL, limits = c(-1,1)) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 

list(profile_plot,none_plot)

}


importance_utility_ranking = function(df, key, nr_profiles, none_option){
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
  print(nrow(key))
  print(nrow(df))
  df$set = rep(1:nrow(key), nrow(df)/nrow(key))
  #set.seed(42); df$y[is.na(df$y)] = apply(df[is.na(df$y),] ,1, function(x){sample(df$y[!is.na(df$y) & df$set == x['set']], 1)})
  
  print("A")
  #format column and attribute names
  colnames(key) = gsub("_a","",colnames(key));
  colnames(key) = gsub("_b","",colnames(key));
  if(nr_profiles > 2){colnames(key) = gsub("_c","",colnames(key))}
  if(nr_profiles > 3){colnames(key) = gsub("_d","",colnames(key))}
  colnames(key) =trimws(colnames(key))
  attribute_names =trimws(unique(colnames(key)[colnames(key) != 'Set']))
  print(attribute_names)
  print("B")
  nr_attributes = length(attribute_names)
  
  #extract and count levels for attributes
  sets = 1:nrow(key)
  temp = key[,1:nr_attributes]
  nr_levels = sapply(X = temp, FUN = function(x){length(unique(x))})
  profile = sort(rep(1:nr_profiles, nrow(key)))
  print("C")
  #put profiles of same set underneath each other (long format)
  print(nr_attributes)
  print(nr_profiles)
  print(ncol(key))
  A_key = key[,1:nr_attributes]
  print("C1")
  B_key = key[,(nr_attributes +1): (2* nr_attributes)]
  print("C2")
  if(nr_profiles == 2){
    long_key = cbind(sets, profile, rbind(A_key, B_key))
  }else if(nr_profiles == 3){C_key = key[,(2* nr_attributes + 1): (3* nr_attributes)]
  print("C3")
  long_key = cbind(sets, profile, rbind(A_key, B_key, C_key))
  }else if(nr_profiles == 4){D_key = key[,(3* nr_attributes + 1): (4* nr_attributes)]
  long_key = cbind(sets, profile, rbind(A_key, B_key, C_key, D_key))
  }else{stop("Too few/many profiles per set")}
  print("D")
  #go from all profiles to all profiles per participant in right format for choicemodelR
  long_key = long_key[order(long_key$sets),]
  long_df = long_key[rep(1:nrow(long_key), nr_participants),]
  ID = sort(rep(df$ID, (nr_profiles)), decreasing = F)
  long_df = cbind(ID, long_df)
  print("E")
  #populate response variable in right way for choicemodelR
  long_df$y = NA
  long_df$ID_set = paste(long_df$ID, long_df$set, sep = "_")
  past_ID_set = 0
  print("F")
  for(i in 1:nrow(long_df)){
    current_ID_set = long_df$ID_set[i]
    if(current_ID_set  == past_ID_set){long_df$y[i] = 0
    }else{
      past_ID_set =  current_ID_set
      profile_choice = df$y[df$ID == long_df$ID[i] & df$set == long_df$sets[i]]
      
      long_df$y[i] = profile_choice}
  }
  long_df$ID_set = NULL
  print("G")
  #numerical coding of attribute levels
  colnames(long_df) = trimws(colnames(long_df))
  long_df[,colnames(long_df) %in% attribute_names] = sapply(long_df[,colnames(long_df) %in% attribute_names], function(x){as.numeric(factor(x))})
  #fit model with normal prior centered on zero with variance = 2
  long_df = as.matrix(long_df)
  xcoding = rep(0, nr_attributes)
  mcmc = list(R = 4000, use = 3000) 
  options = list(none=none_option, save=TRUE, keep=1)
  print("H")
  #untilhere
  out = cust_choicemodelr(long_df, xcoding, mcmc = mcmc, options = options)
  print("I")
  betas = colMeans(out$betawrite)
  cumu_nr_levels = cumsum(nr_levels)
  #prep dfs for plotting
  importance = c()
  prev_index = 0
  all_attributes = c()
  all_levels = c()
  print("J")
  for(i in 1:length(cumu_nr_levels)){
    mini_b = min(betas[(prev_index+1):(cumu_nr_levels[i])])
    maxi_b = max(betas[(prev_index+1):(cumu_nr_levels[i])])
    
    importance = c(importance, rep(maxi_b - mini_b, nr_levels[i]))
    
    all_attributes = c(all_attributes, rep(attribute_names[i], nr_levels[i]))
    attr_levels = sort(unique(A_key[,attribute_names[i]]), decreasing = F)
    all_levels = c(all_levels, attr_levels)
    prev_index = cumu_nr_levels[i]}
  print("K")
  if(none_option){all_attributes = c(all_attributes, "None"); importance = c(importance, betas[length(betas)]); all_levels = c(all_levels, "None")}
  
  
  plotting_df = as.data.frame(cbind(all_attributes, all_levels, importance, betas))
  plotting_df$betas = as.numeric(plotting_df$betas)
  
  plotting_df$importance = as.numeric(plotting_df$importance)
  plotting_df$importance = plotting_df$importance /sum(plotting_df$importance[plotting_df$all_attributes != "None" & !duplicated(plotting_df$all_attributes)]) *100
  
  Encoding(plotting_df$all_levels) = Encoding(plotting_df$all_attributes) = "UTF-8"
  print("L")
  #plot importance
  importance_plot = ggplot(plotting_df[!duplicated(plotting_df$all_attributes) & plotting_df$all_attributes != "None",]) + 
    geom_bar(aes(x = reorder(all_attributes, -1*importance),y = importance, fill = all_attributes), stat = "identity", show.legend = FALSE) + # fill = reorder(all_attributes, -1*importance)), stat =
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
  print("M")
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
  
  return(list(importance_plot, utility_plot, all_profiles, out$betawrite, plotting_df, within_attribute_beta_comparisons, between_attribute_importance_comparisons_df))
}

market_simulator = function(selected_profiles, betawrite, plotting_df){
  #market simulator
  
  #selected_profiles = all_profiles[107,];betawrite = out$betawrite #for testing function outside app
  one_hot_vector = apply(selected_profiles, 1, function(x){plotting_df$all_levels %in% x})
  participant_scores = betawrite %*% one_hot_vector #none beta; out$betawrite
  market_simulation = apply(participant_scores, 1, function(x){which.max(x)})
  market_simulation = table(market_simulation)
  market_simulation = market_simulation / sum(market_simulation) * 100
  names(market_simulation) = paste("Rank", selected_profiles$Rank)
  
  pie(market_simulation, 
      labels = paste0(names(market_simulation), " (",round(market_simulation), "%)"),
      edges = 500,
      main = "Artificial market simulation")
}

cust_choicemodelr <-function(data, xcoding, demos, prior, mcmc, constraints, options) {
  set.seed(42)
  callStop = function(message) { stop(message, call. = FALSE) }
  
  if (missing(data)) { callStop("data argument required") }
  if (ncol(data) < 5) { callStop("there are not enough columns in data to specify ID, Set, Alt, X variable(s), and y") }
  natts = ncol(data) - 4
  if (missing(xcoding)) { callStop("xcoding argument required") }
  if (length(xcoding) != natts) { callStop("length of xcoding is not equal to number of attributes") }
  if (any(xcoding != 0 & xcoding != 1)) { callStop("xcoding must contain only values 0 and 1") }
  npw = sum(xcoding == 0)
  ID = unique(data[,1])
  nunits = length(ID)
  drawdelta = ifelse(missing(demos), FALSE, TRUE)
  if (drawdelta) {
    if (nrow(demos) != nunits) { callStop("number of rows in demos does not equal number of units") }
    nz = ncol(demos)
    demos = t(t(demos) - colMeans(demos))
  }
  if (missing(options)) {
    none = FALSE
    save = FALSE
    restart = FALSE
    keep = 10
    wgt = 5
  }
  else {
    none = ifelse(is.null(options$none), FALSE, options$none)
    save = ifelse(is.null(options$save), FALSE, options$save)
    restart = ifelse(is.null(options$restart), FALSE, options$restart)
    keep = ifelse(is.null(options$keep), 10, options$keep)
    wgt = ifelse(is.null(options$wgt), 5, options$wgt)
    if (wgt < 1 | wgt > 10) { callStop("wgt must be in the range 1 to 10") }
  }
  
  maxsets = max(data[,2])
  maxalts = max(data[,3]) + ifelse(none, 1, 0)
  
  nsets = rep(0, nunits)
  nalts = matrix(0, nrow = nunits, ncol = maxsets)
  for (i in 1:nunits) {
    temp = data[data[,1] == ID[i],]
    nsets[i] = max(temp[,2])
    for (j in 1:nsets[i]) {
      nalts[i, j] = max(temp[temp[,2] == j, 3]) + ifelse(none, 1, 0)
    }
  }
  
  setsmap = (nalts > 0)
  altsmap = matrix(FALSE, nrow = sum(nsets), ncol = maxalts)
  index = 1
  nalts = t(nalts) # TO GET AROUND R COLUMN FILL
  for (i in 1:length(nalts)) {
    if (nalts[i] != 0) {
      altsmap[index, 1:nalts[i]] = TRUE
      index = index + 1
    }
  }
  
  avgalts = mean(nalts[nalts > 0])
  avgsets = mean(nsets)
  nalts = colSums(nalts)
  
  info = list(nunits = nunits,
              maxsets = maxsets,
              maxalts = maxalts,
              nsets = nsets,
              nalts = nalts,
              setsmap = setsmap,
              altsmap = altsmap)	
  
  if (all(data[!(data[,3] == 1), ncol(data)] == 0)) { share = FALSE }
  else { share = TRUE }
  
  if (share) {
    Xt = data[, c(-1, -2, -3, -ncol(data)), drop = FALSE]
    y = matrix(0, nrow = maxalts, ncol = sum(nsets))
    y[t(altsmap)] = data[, ncol(data)]
    y = t(y)
    ytab = colSums(y > 0); ytab = rbind(ytab, round(ytab / sum(ytab) * 100, 2))
    y = y / rowSums(y) * wgt
  }
  else {
    if (none) {
      Xt = NULL
      for (i in 1:nunits) {
        for (j in 1:nsets[i]) {
          ind = (data[,1] == ID[i]) & (data[,2] == j)
          Xt = rbind(Xt, cbind(data[ind, c(-1, -2, -3, -ncol(data)), drop = FALSE], 0), c(rep(0, natts), 1))
        }
      }
    }
    else { Xt = data[, c(-1, -2, -3, -ncol(data)), drop = FALSE] }
    y = data[data[,3] == 1, ncol(data)]
    
    if (any(y[!is.na(y)] > maxalts) | any(y[!is.na(y)] < 1)) { callStop(paste("invalid values of y present in data - values must be 1 to ", 
                                                                              maxalts, sep = "")) }
    ytab = table(y); ytab = rbind(ytab, round(ytab / sum(ytab) * 100, 2))
  }
  
  nlevels = c(rep(0, natts))
  for (i in 1:natts) {
    if (xcoding[i] == 0) {
      nlevels[i] = length(unique(Xt[,i]))
      if (any(unique(Xt[,i]) == 0)) { nlevels[i] = nlevels[i] - 1 }
    }
    else { nlevels[i] = 1 }
  }
  npar = sum(nlevels) - npw + ifelse(none, 1, 0)
  X = matrix(0, nrow = nrow(Xt), ncol = npar)
  Xind = 1
  if (none) { nn = Xt[, ncol(Xt)] == 0 }
  else { nn = rep(TRUE, nrow(Xt)) }
  for (i in 1:natts) {
    if (xcoding[i] == 1) {
      X[nn, Xind] = Xt[nn, i] - mean(unique(Xt[nn, i]))
      Xind = Xind + 1
    }
    else {
      for (j in 1:(nlevels[i] - 1)) {
        X[nn, Xind] = (Xt[nn, i] == j) * 1 - (Xt[nn, i] == nlevels[i]) * 1
        Xind = Xind + 1
      }
    }
  }
  if (none) { X[, npar] = Xt[, ncol(Xt)] }
  
  effectsmap = matrix(FALSE, nrow = natts, ncol = npar)
  index = 1
  for (i in 1:natts) {
    count = 1
    repeat {
      effectsmap[i, index] = TRUE
      index = index + 1
      count = count + 1
      if (count > max(1, nlevels[i] - 1)) { break }
    }
  }
  if (none) { effectsmap = rbind(effectsmap, c(rep(FALSE, npar - 1), TRUE)) }
  
  if (missing(prior)) { prior = list() }
  if (is.null(prior$mubar)) { mubar = matrix(rep(0, npar), nrow = 1) }
  else {
    mubar = matrix(prior$mubar, nrow = 1)
    if (ncol(mubar) != npar) { callStop("ncol(mubar) does not equal npar") }
  }
  if (is.null(prior$Amu)) { Amu = matrix(0.01, ncol = 1) }
  else {
    Amu = matrix(prior$Amu, ncol = 1)
    if (any(dim(Amu) != c(1, 1))) { callStop("Amu must be a 1 x 1 matrix") }
  }
  if (is.null(prior$df)) { df = 5 }
  else {
    df = prior$df
    if (df < 2) { callStop("invalid value of df - must be >= 2") }
  }
  nu = npar + df
  if (is.null(prior$v)) {
    v = 2
  }
  else {
    v = prior$v
    if (v <= 0) { callStop("invalid v - must be >= 0") }
  }
  if (is.null(prior$Ad) & drawdelta) { Ad = 0.01 * diag(npar * nz) }
  else if (drawdelta) {
    Ad = prior$Ad
    if (any(dim(Ad) != c(npar * nz, npar * nz))) { callStop("Ad must be of dimensions (npar * nz) x (npar * nz)") }
  }
  if (is.null(prior$deltabar) & drawdelta) { deltabar = rep(0, nz * npar) }
  else if (drawdelta) {
    deltabar = prior$deltabar
    if (length(deltabar) != nz * npar) { callStop("deltabar must be of length npar * nz") }
  }
  
  V = matrix(0, nrow = npar, ncol = npar)
  v.ind = 1
  for (i in 1:natts) {
    if (nlevels[i] == 1) {
      V[v.ind, v.ind] = 1
      v.ind = v.ind + 1
    }
    else {
      pcov = -1 / nlevels[i]
      pvar = (nlevels[i] - 1) / nlevels[i]
      temp = pvar * diag(nlevels[i] - 1)
      temp[upper.tri(temp) | lower.tri(temp)] = pcov
      
      V[v.ind:(v.ind + nlevels[i] - 2), v.ind:(v.ind + nlevels[i] - 2)] = temp
      v.ind = v.ind + nlevels[i] - 1
    }
  }
  if (none) { V[v.ind, v.ind] = 1 }
  V = V * nu * v
  
  if (missing(mcmc)) { callStop("mcmc argument required") }
  if (is.null(mcmc$s)) {
    s = 0.1
    adjust.s = TRUE
  }
  else {
    s = mcmc$s
    adjust.s = FALSE
  }
  if (is.null(mcmc$R)) { callStop("element R of mcmc required") }
  R = mcmc$R
  if (is.null(mcmc$use)) { use = min(0.5 * R, 10000) }
  else {
    use = mcmc$use
    if (use > R) { callStop("use must be <= R") }
  }
  
  constrain = ifelse(missing(constraints), FALSE, TRUE)
  if (constrain) {
    if (length(constraints) != natts) { callStop("length(constraints) must equal number of attributes") }
    for (i in 1:natts) {
      if (any(dim(constraints[[i]]) != c(nlevels[i], nlevels[i]))) {
        callStop("dim(constraints[[i]]) must equal nlevels[i] x nlevels[i]")
      }
      if (any(constraints[[i]] != -1 & constraints[[i]] != 0 & constraints[[i]] != 1)) {
        callStop("constraints matrices must only contain -1, 0, and 1 elements")
      }
    }
  }
  
  fR = 0
  if (restart) {
    if (!any(list.files() == "restart.txt")) {
      callStop("file restart.txt does not exist in the working directory")
    }
    f.in = scan("restart.txt", nlines = 1, skip = 0, quiet = TRUE)
    fR = f.in[1]
    fUnits = f.in[2]
    fPar = f.in[3]
    s = f.in[4]
    fCons = f.in[5]
    fDem = f.in[6]
    items = 1
    ltr = fUnits
    if (fCons == 1) { 
      items = items + 1 
      ltr = c(ltr, fUnits)	
    } 
    if (fDem == 1) { 
      items = items + 1 
      ltr = c(ltr, 1)
    }
    if (fUnits != nunits) { callStop("number of units in restart file does not match data file") }	
    if (fPar != npar) { callStop("number of parameters in restart file does not match data file") }
    if (fCons != as.numeric(constrain)) { callStop("restart file and function arguments do not agree on constraints being present") }
    if (fDem != as.numeric(drawdelta)) { callStop("restart file and function arguments do not agree on demographics being present") }
    
    fIndex = 1
    fCount = 1
    repeat {
      if (fCount > items) { break }
      f.in = scan("restart.txt", nlines = ltr[fCount], skip = fIndex, quiet = TRUE)
      switch(fCount,
             lb <- matrix(f.in, ncol = fPar, byrow = TRUE),
             lbc <- matrix(f.in, ncol = fPar, byrow = TRUE),
             ld <- c(f.in)
      )
      fIndex = fIndex + ltr[fCount]
      fCount = fCount + 1
    }
  }
  
  if (save) {
    if (drawdelta) { deltadraw = matrix(0, nrow = floor(use / keep), ncol = nz * npar) }
    betadraw = array(0, dim = c(nunits, npar, floor(use / keep)))
    compdraw = NULL
    loglike = rep(0, floor(use / keep))
  }
  if (constrain) {
    if (save) { betadraw.c = array(0, dim = c(nunits, npar, floor(use / keep))) }
    if (restart) { oldbetas.c = lbc }
    else { oldbetas.c = matrix(0, nrow = nunits, ncol = npar) }
  }
  betaout = matrix(0, nrow = nunits, ncol = npar)
  if (drawdelta) { 
    if (restart) { olddelta = ld } 
    else { olddelta = rep(0, nz * npar) }
  }
  if (restart) { oldbetas = lb }
  else { oldbetas = matrix(0, nrow = nunits, ncol = npar) }
  oldll = rep(0, nunits)
  oldcomp = NULL
  
  muplot = matrix(0, nrow = R, ncol = npar + npw)
  xplot = (1 + fR):(R + fR)
  
  #
  # DEFINE NEEDED FUNCTIONS
  #
  getLLMnl = function (beta, y, X, info) {
    nunits = info$nunits
    nalts = info$maxalts
    nsets = info$maxsets
    
    map.sets = t(info$setsmap)
    map.alts = t(info$altsmap)
    
    
    tmpXbeta = X * beta[rep(1:nunits, info$nalts),]
    
    tmpXbeta = rowSums(tmpXbeta)
    
    Xbeta = matrix(0, nrow = nalts, ncol = sum(info$nsets))
    
    Xbeta[map.alts] = tmpXbeta
    
    # SHARE DATA
    if (is.matrix(y)) {
      Xbeta[map.alts] = exp(Xbeta[map.alts])
      denom = colSums(Xbeta)
      probs = (t(Xbeta) / denom) ^ y
      tmpProbs = apply(probs, 1, prod)
      probs = matrix(1, nrow = nsets, ncol = nunits)
      probs[map.sets] = tmpProbs
      
      ll = log(apply(probs, 2, prod))
    }
    
    # DISCRETE DATA
    else {
      ind = cbind(y, 1:sum(info$nsets))
      
      xby = matrix(0, nrow = nsets, ncol = nunits)
      xby[map.sets] = Xbeta[ind]
      Xbeta[map.alts] = exp(Xbeta[map.alts])
      denom = matrix(0, nrow = nsets, ncol = nunits)
      denom[map.sets] = log(colSums(Xbeta))
      hannestemp = xby - denom
      
      ll = colSums(hannestemp, na.rm = TRUE)
      
    }
    
    return(ll)
  }
  
  getLndMvn = function (x, mu, rooti) {
    npar = ncol(x)
    #
    # WITH COVARIATES
    #	
    if (is.matrix(mu)) { z = (x - mu) %*% rooti }
    #
    # NO COVARIATES
    #
    else { z = crossprod(t(x) - mu, rooti) }
    
    logs = -(npar / 2) * log(2 * pi) - 0.5 * rowSums(z * z) + sum(log(diag(rooti)))
    
    return(logs)
  }
  
  drawDelta = function (x, y, comps, deltabar, Ad) {
    yy = t(t(y) - comps$mu)
    sig = tcrossprod(comps$rooti)
    
    xtx = crossprod(x) %x% sig
    xty = matrix(sig %*% crossprod(yy, x), ncol = 1)
    
    cov = chol2inv(chol(xtx + Ad))
    
    return(cov %*% (xty + Ad %*% deltabar) + t(chol(cov)) %*% rnorm(length(deltabar)))
  }
  
  mnlRwMetropOnce = function (y, X, oldbeta, oldll, s, inc.root, betabar, rootpi, info, constraints, oldbeta.c) {
    stay = 0
    nunits = info$nunits
    npar = ncol(oldbeta)
    
    increment = s * crossprod(matrix(rnorm(nunits * npar), ncol = nunits), inc.root)
    newbeta = oldbeta + increment
    
    if (!missing(constraints)) {
      newbeta.c = constrainBetas(newbeta, constraints)
      newll = getLLMnl(newbeta.c, y, X, info)
    }
    else {
      newll = getLLMnl(newbeta, y, X, info)
    }
    newlpost = newll + getLndMvn(newbeta, betabar, rootpi)
    ldiff = newlpost - oldll - getLndMvn(oldbeta, betabar, rootpi)
    alpha = exp(ldiff)
    alpha[alpha > 1] = 1
    unif = runif(nunits)
    unif[alpha == 1] = 0
    
    good = (unif <= alpha)
    betadraw = oldbeta
    betadraw[good,] = newbeta[good,]
    if (!missing(constraints)) {
      betadraw.c = oldbeta.c
      betadraw.c[good,] = newbeta.c[good,]
    }
    oldll[good] = newll[good]
    
    stay = sum(!good)
    
    if (!missing(constraints)) {
      return(list(betadraw = betadraw, betadraw.c = betadraw.c, stay = stay, oldll = oldll))
    }
    else {
      return(list(betadraw = betadraw, stay = stay,  oldll = oldll))
    }
  }
  
  constrainBetas = function (betas, constraints) {
    newlength = 0
    oldlength = 0
    for (h in 1:length(constraints)) {
      if (all(constraints[[h]] == 0)) {
        if (ncol(constraints[[h]]) == 1) {
          newlength = 1
        }
        else {
          newlength = ncol(constraints[[h]]) - 1
        }
        oldlength = oldlength + newlength
        next
      }
      else if (ncol(constraints[[h]]) == 1) {
        newlength = 1
        betat = betas[,oldlength + 1]
        if (constraints[[h]] == 1) {
          betat[betat < 0] = 0
        }
        if (constraints[[h]] == -1) {
          betat[betat > 0] = 0
        }
        betas[,oldlength + 1] = betat
      }
      else {
        newlength = ncol(constraints[[h]]) - 1
        betat = betas[,(oldlength + 1):(oldlength + newlength), drop = FALSE]
        betat = cbind(betat, -1 * rowSums(betat))
        repeat {
          bad = FALSE
          for (i in 1:(nrow(constraints[[h]]) - 1)) {
            for (j in (i + 1):ncol(constraints[[h]])) {
              if (constraints[[h]][i, j] == 1) {
                change = (betat[,i] < betat[,j])
                if (sum(change) > 0) {
                  betat[change, j] = betat[change, i]
                  betat[change,] = betat[change,] - rowMeans(betat[change,,drop = FALSE])
                  bad = TRUE
                }
              }
              else if (constraints[[h]][i, j] == -1) {
                change = (betat[,i] > betat[,j])
                if (sum(change) > 0) {
                  betat[change, i] = betat[change, j]
                  betat[change,] = betat[change,] - rowMeans(betat[change,,drop = FALSE])
                  bad = TRUE
                }
              }
            }
          }
          if (!bad) { break }
        }
        betas[,(oldlength + 1):(oldlength + newlength)] = betat[,1:newlength]
      }
      oldlength = oldlength + newlength
    }
    return(betas)
  }
  
  rGibbs = function (y, betabar, A, nu, V) {
    temp = rmultireg(y, matrix(rep(1, nrow(y)), ncol = 1), betabar, A, nu, V)
    comps = list(mu = as.vector(temp$B), rooti = backsolve(chol(temp$Sigma), diag(ncol(temp$Sigma))))
    
    return(comps)
  }
  
  rmultireg = function (Y, X, Bbar, A, nu, V) {
    n = nrow(Y)
    m = ncol(Y)
    k = ncol(X)
    RA = chol(A)
    W = rbind(X, RA)
    Z = rbind(Y, RA %*% Bbar)
    IR = backsolve(chol(crossprod(W)), diag(k))
    Btilde = crossprod(t(IR)) %*% crossprod(W, Z)
    S = crossprod(Z - W %*% Btilde)
    rwout = rwishart(nu + n, chol2inv(chol(V + S)))
    B = Btilde + IR %*% matrix(rnorm(m * k), ncol = m) %*% t(rwout$CI)
    
    return(list(B = B, Sigma = rwout$IW))
  }
  
  rwishart = function (nu, V) {
    m = nrow(V)
    df = (nu + nu - m + 1) - (nu - m + 1):nu
    if (m > 1) {
      T = diag(sqrt(rchisq(c(rep(1, m)), df)))
      T[lower.tri(T)] = rnorm((m * (m + 1)/2 - m))
    }
    else {
      T = sqrt(rchisq(1, df))
    }
    U = chol(V)
    C = t(T) %*% U
    CI = backsolve(C, diag(m))
    
    return(list(W = crossprod(C), IW = crossprod(t(CI)), C = C, CI = CI))
  }
  
  getEffectsCodedParameters = function(parms, map, xcoding) {
    out = NULL
    # FOR THE NON-CODED NONE PARAMETER
    if (nrow(map) > length(xcoding)) { xcoding = c(xcoding, 1) }
    for (i in 1:nrow(map)) {
      if (xcoding[i] == 0) {
        out = cbind(out, parms[,map[i,], drop = FALSE], -1 * rowSums(parms[,map[i,], drop = FALSE]))
      }
      else {
        out = cbind(out, parms[,map[i,], drop = FALSE])
      }
    }
    return(out)
  }
  
  fsh = function() {
    if (Sys.info()[1] == "Windows") { flush.console() }
    return()
  }
  
  #
  # OPEN CONNECTION FOR WRITING TO LOG FILE
  #
  on.exit(sink())
  sink("RLog.txt", append = TRUE, type = "output", split = TRUE)
  
  #
  # PRINT TO CONSOLE AND LOG
  #
  if (restart) {
    cat("Restarting from previous ", fR, "-iteration model estmation...", sep = "", fill = TRUE)
    cat("", fill = TRUE)
  }
  cat("                    Logit Data                    ", fill = TRUE)
  cat("==================================================", fill = TRUE)
  cat("Attribute       Type         Levels", fill = TRUE)
  cat("-----------------------------------", fill = TRUE)    
  for (i in 1:natts) {
    cat(sprintf("%-12s   %-12s   %2i", paste("Attribute", i), 
                ifelse(xcoding[i] == 0, "Part Worth", "Linear"), nlevels[i]), fill = TRUE)
  } 
  cat("", fill = TRUE)
  cat(npar, " parameters to be estimated", ifelse(none, " (including 'None').", "."), sep = "", fill = TRUE)
  cat("", fill = TRUE)
  cat(nunits, "total units.", fill = TRUE)
  cat("Average of", round(avgalts, 1), "alternatives in each of", round(avgsets, 1), "sets per unit.", fill = TRUE)
  cat(ifelse(share, sum(ytab[1,]), sum(nsets)), ifelse(share, " expanded", ""), " tasks in total.", sep = "", fill = TRUE)
  cat("", fill = TRUE)
  cat("Table of choice data pooled across units:", fill = TRUE)
  cat("Choice  Count   Pct.", fill = TRUE)
  cat("--------------------", fill = TRUE)
  for (i in 1:maxalts) {
    cat(sprintf("%4i    %-6i %s", i, ytab[1,i], paste(ytab[2,i],'%', sep = "")), 
        fill = TRUE)
  }	
  cat("", fill = TRUE)
  cat("      MCMC Inference for Hierarchical Logit       ", fill = TRUE)
  cat("==================================================", fill = TRUE)
  cat("Total Iterations:         ", R + fR, fill = TRUE)
  cat("Draws used in estimation: ", use, fill = TRUE)
  cat("Units:                    ", nunits, fill = TRUE)
  cat("Parameters per unit:      ", npar, fill = TRUE)
  if (share) {
    cat("Task weight:              ", wgt, fill = TRUE)
  }
  cat("Constraints ", ifelse(constrain, "", "not "), "in effect.", fill = TRUE, sep = "")
  cat("Draws ", ifelse(save, "are to be ", "not "), "saved.", fill = TRUE, sep = "")
  cat("Prior degrees of freedom: ", df, fill = TRUE)
  cat("Prior variance:           ", v, fill = TRUE)
  cat("", fill = TRUE)
  cat("MCMC Iteration Beginning...", fill = TRUE)
  fsh()
  
  itime = proc.time()[3]
  acceptr.t = 0
  
  for (rep in 1:R) {
    set.seed(rep)
    if (drawdelta) {
      mgout = rGibbs(oldbetas - demos %*% t(matrix(olddelta, ncol = nz)), mubar, Amu, nu, V)
      oldcomp = mgout
      olddelta = drawDelta(demos, oldbetas, oldcomp, deltabar, Ad)
    }
    else {	
      mgout = rGibbs(oldbetas, mubar, Amu, nu, V)
      oldcomp = mgout
    }
    
    if (rep == 1) {
      if (constrain) {
        oldll = getLLMnl(oldbetas.c, y, X, info)
      }
      else {
        oldll = getLLMnl(oldbetas, y, X, info)
      }
    }
    
    rootpi = oldcomp$rooti
    inc.root = chol(chol2inv(chol(tcrossprod(rootpi))))
    if (drawdelta) {
      betabar = t(matrix(olddelta, ncol = nz) %*% t(demos) + oldcomp$mu)
    }
    else {
      betabar = oldcomp$mu
    }		
    
    if (constrain) {
      metropout = mnlRwMetropOnce(y, X, oldbetas, oldll, s, inc.root, betabar, rootpi, info, constraints, oldbetas.c)
      oldbetas.c = metropout$betadraw.c
    }
    else {
      metropout = mnlRwMetropOnce(y, X, oldbetas, oldll, s, inc.root, betabar, rootpi, info)
    }
    oldbetas = metropout$betadraw
    oldll = metropout$oldll
    
    #
    # CALCULATE GOODNESS-OF-FIT MEASUREMENTS
    #
    if (share) {
      RLH = exp(mean(oldll))^(1 / (avgsets * wgt))
      PctCert = (mean(oldll) - log(1 / avgalts) * avgsets * wgt) / (-log(1 / avgalts) * avgsets * wgt)
    }
    else {
      RLH = exp(mean(oldll))^(1 / avgsets)
      PctCert = (mean(oldll) - log(1 / avgalts) * avgsets) / (-log(1 / avgalts) * avgsets)
    }
    AvgVar = mean(diag(crossprod(inc.root)))
    RMS = sqrt(mean(oldbetas ^ 2))
    
    if (rep == 1) {
      RLH.a = RLH
      PctCert.a = PctCert
      AvgVar.a = AvgVar
      RMS.a = RMS
    }
    else {
      RLH.a = 0.99 * RLH.a + 0.01 * RLH
      PctCert.a = 0.99 * PctCert.a + 0.01 * PctCert
      AvgVar.a = 0.99 * AvgVar.a + 0.01 * AvgVar
      RMS.a = 0.99 * RMS.a + 0.01 * RMS
    }
    
    #
    # ADJUST SCALING PARAMETER TO TRY AND KEEP ACCEPTANCE RATE ~30%
    #
    acceptr = nunits - metropout$stay
    acceptr.t = acceptr.t + acceptr
    if (adjust.s) {
      if (acceptr / nunits < 0.3) { s = s * 0.9 }
      else if (acceptr / nunits > 0.3) { s = s * 1.1 }
    }
    acceptr = 0
    
    #
    # PREPARE MU VALUES FOR PLOTTING
    #
    mutemp = matrix(oldcomp$mu, nrow = 1)
    muplot[rep,] = getEffectsCodedParameters(mutemp, effectsmap, xcoding)
    
    newplot = FALSE
    if (rep == 1) {
      yl = c(-1, 1)
      matplot(0, 0, type = "l", col = 1:8, lty = 1, ylim = yl, xlim = c(1, R + fR), xlab = "Rep", ylab = "Mu")
    }
    if (rep %% 100 == 0) {
      #
      # PLOT MU
      #
      if (max(muplot[(rep - 99):rep,]) >= yl[2]) {
        yl[2] = ceiling(max(muplot[(rep - 99):rep,]))
        newplot = TRUE
      }
      if (min(muplot[(rep - 99):rep,]) <= yl[1]) {
        yl[1] = floor(min(muplot[(rep - 99):rep,]))
        newplot = TRUE
      }
      if (newplot) {
        matplot(xplot[1:rep], muplot[1:rep,], type = "l", col = 1:8, lty = 1, ylim = yl, xlim = c(1, R + fR), xlab = "Rep", ylab = "Mu")
      }
      else {
        matplot(xplot[(rep - 100):rep], muplot[(rep - 100):rep,], type = "l", col = 1:8, lty = 1, add = TRUE)
      }
      
      #
      # UPDATE PROGRESS
      #
      ctime = proc.time()[3]
      timetoend = ((ctime - itime) / rep) * (R + 1 - rep)
      if (rep == 100) {
        cat("Iteration ", "Acceptance  ", "RLH    ", "Pct. Cert.  ", "Avg. Var.  ", "RMS    ", "Time to End", fill = TRUE)
      }
      cat(sprintf("%9.0i  %-5.3f        %-5.3f   %-5.3f        %-5.2f       %-5.2f   %-6s",
                  rep + fR, acceptr.t / (100 * nunits), RLH.a, PctCert.a, AvgVar.a, RMS.a, 
                  paste(timetoend %/% 60, ifelse(round(timetoend %% 60) > 9, ":", ":0"), round(timetoend %% 60), 
                        sep = "")), fill = TRUE)
      fsh()
      acceptr.t = 0
    }
    
    if (rep > R - use) {
      if (save) {
        mkeep = (rep - (R - use)) / keep
        if (rep %% keep == 0) {
          betadraw[,,mkeep] = oldbetas
          if (constrain) { betadraw.c[,,mkeep] = oldbetas.c }
          if (drawdelta) { deltadraw[mkeep,] = olddelta }
          loglike[mkeep] = sum(oldll)
          compdraw[[mkeep]] = oldcomp
        }
      }
      if (constrain) {
        betaout = betaout + oldbetas.c
      }
      else {
        betaout = betaout + oldbetas
      }
    }		
  }
  
  ctime = proc.time()[3]
  cat("", fill = TRUE)
  cat("Total Time Elapsed: ", (ctime - itime) %/% 60, ifelse(round((ctime - itime) %% 60) > 9, ":", ":0"), 
      round((ctime - itime) %% 60), fill = TRUE, sep = "")
  cat("", fill = TRUE)
  
  #
  # WRITE RESTART FILE
  #	
  write.table(cbind(R + fR, nunits, npar, s, as.numeric(constrain), as.numeric(drawdelta)), "restart.txt", sep = " ", row.names = FALSE, col.names = FALSE)
  write.table(oldbetas, "restart.txt", sep = " ", row.names = FALSE, col.names = FALSE, append = TRUE)
  if (constrain) {
    write.table(oldbetas.c, "restart.txt", sep = " ", row.names = FALSE, col.names = FALSE, append = TRUE)
  }
  if (drawdelta) {
    write.table(t(olddelta), "restart.txt", sep = " ", row.names = FALSE, col.names = FALSE, append = TRUE)
  }
  
  #
  #
  # WRITE RESPONDENT BETAS TO CSV FILE
  #
  betaout = betaout / use
  betawrite = getEffectsCodedParameters(betaout, effectsmap, xcoding) #aaa cbind(matrix(ID, ncol = 1), getEffectsCodedParameters(betaout, effectsmap, xcoding))
  betanames = c() #aaa  "ID"
  for (i in 1:natts) {
    for (j in 1:nlevels[i]) {
      betanames = c(betanames, paste("A", i, "B", j, sep = ""))
    }
  }
  if (none) { betanames = c(betanames, "NONE") }
  cat("Writing estimated unit-level betas to Rbetas.csv in the working directory", fill = TRUE)
  cat("", fill=TRUE)
  write.table(betawrite, file = "RBetas.csv", sep = ",", col.names = betanames, row.names = FALSE, qmethod = "double") 			
  
  if (save) { 
    switch(1 + 1 * constrain + 2 * drawdelta,
           return(list(betadraw = betadraw, compdraw = compdraw, loglike = loglike, betawrite = betawrite)),
           return(list(betadraw = betadraw, betadraw.c = betadraw.c, compdraw = compdraw, loglike = loglike, betawrite = betawrite)),
           return(list(betadraw = betadraw, deltadraw = deltadraw, compdraw = compdraw, loglike = loglike, betawrite = betawrite)),
           return(list(betadraw = betadraw, betadraw.c = betadraw.c, deltadraw = deltadraw, compdraw = compdraw, loglike = loglike, betawrite = betawrite))
    )
  }
  else { return(NULL) }		 
}


bigger_design = function(x, current_design){
  d = as.integer(unlist(strsplit(current_design, "x")))
  if(length(d) == length(x)){
    return(all(x >= d))
  }else(return(FALSE))
}

current_and_alternative_designs = function(data){
  data = data[,order(colSums(data==""), decreasing = T)]
  data_list = as.list(data)
  data_list = removeListElemComplete(data_list, "")
  ddd = oa.design(factor.names = data_list, columns = "min3", seed = 42)
  colnames(ddd) = gsub("X.", "", colnames(ddd) )
  
  current_design = paste(sort(apply(data, 2, function(x){length(x) - sum(x == "")})), collapse = "x")

  if(nrow(ddd) <= all_designchecks[current_design]){
    current = data.frame(c(current_design),c(nrow(ddd)))
    colnames(current) = c("Design", "#Sets")
    return(list(current,NULL, "Good design!"))
  }else{

  
  if(current_design %in% names(all_designchecks)){

    coded_dcheck = strsplit(names(all_designchecks), "x")
    integer_dcheck = lapply(coded_dcheck,  as.integer)
    better_designs = as.data.frame(unlist(all_designchecks[which(unlist(lapply(integer_dcheck, bigger_design, current_design = current_design)) & all_designchecks <= 48)]))
    better_designs = cbind(rownames(better_designs), better_designs)#
    colnames(better_designs) = c("Design", "#Sets")
    if(all_designchecks[current_design] <= 32){
      message = "Good design!" 
    }else if(nrow(better_designs) > 0){
      message = "Larger, optimized designs available!"
    }else{message = "Design size threatens data quality"}
    
    current = as.data.frame(all_designchecks[current_design])
    current = cbind(names(all_designchecks[current_design]), as.integer(all_designchecks[current_design]))
    colnames(current) = c("Design", "#Sets")
    return(list(current,better_designs, message))
  }else{return(list(NULL, NULL, "Questionable design size"))}
}}

removeListElemComplete = function(inlist, elem_remove) {
  removeListElem <- function(inlist,elem_remove){
    outlist = lapply(inlist,setdiff,elem_remove)
    outlist[lengths(outlist) > 0]
  }
  outlist = lapply(inlist, removeListElem, elem_remove = elem_remove)
  outlist[lengths(outlist) > 0]
}
load("all_designchecks.RData")


resample_without_creating_duplicates = function(piles, three = T){
  set.seed(42)
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
  set.seed(42)
  
  pile1[] <- lapply(pile1, as.character)

  #make pile2
  smallest_overlap = 99999
  pile2 = pile1
  
  for(column in colnames(pile1)){
    
    vals = unique(pile1[,column]) #removed sort
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
      vals = unique(pile1[,column]) #removed sort
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


plot_set = function(sets, set_number = 1, profile_number, aest, decorpath, none_text, imgs){
  set.seed(42)
sets['Set'] = NULL
colnames(sets) = gsub('_a', '', colnames(sets));colnames(sets) = gsub('_b', '', colnames(sets));colnames(sets) = gsub('_c', '', colnames(sets))

elements = sets[set_number, ((profile_number-1) * ncol(sets)/3 + 1):(profile_number * ncol(sets)/3)]
elements=elements[,order(ncol(elements):1)]

if(sum(grepl("Image", colnames(elements))) > 0){
  imgpath = imgs[elements$Image]
  if(substr(imgpath, nchar(imgpath)-3, nchar(imgpath)) == ".png"){
    pn = readPNG(imgpath)
    g = rasterGrob(pn, interpolate=TRUE)
  }else if(substr(imgpath, nchar(imgpath)-3, nchar(imgpath)) == ".jpg" | substr(imgpath, nchar(imgpath)-4, nchar(imgpath)) == ".jpeg"){
    jp = readJPEG(imgpath)
    g = rasterGrob(jp, interpolate=TRUE)
  }else{stop("weird input format")}
}else if(!is.na(decorpath)){#this else if for imgs vs decor or let user overwrite?
 if(substr(decorpath, nchar(decorpath)-3, nchar(decorpath)) == ".png"){
   pn = readPNG(decorpath)
   g = rasterGrob(pn, interpolate=TRUE)
 }else if(substr(decorpath, nchar(decorpath)-3, nchar(decorpath)) == ".jpg" | substr(decorpath, nchar(decorpath)-4, nchar(decorpath)) == ".jpeg"){
  jp = readJPEG(decorpath)
  g = rasterGrob(jp, interpolate=TRUE)
 }else{stop("weird input format")}
}else{g = NA}


colnames(elements) = paste0(colnames(elements), ":")

profile_plot= ggplot() +  
  geom_text(aes(x = aest['gap']/10, y = 1:ncol(elements),
                label = unlist(elements[1,]), hjust = 0,fontface = "bold"), size = aest['font_size_vals']) +
  geom_text( aes(x = 0, y = 1:ncol(elements), 
                label = colnames(elements)), hjust = 1, size = aest['font_size_keys']) +
  theme_bw()+ 
  scale_y_continuous(breaks = NULL, limits = c(-1*aest['bottom_buffer'],ncol(elements)+1+aest['top_buffer'])) + 
  scale_x_continuous(breaks = NULL, limits = c( -0.6- aest['left_buffer'],1 )) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  {
    if(!all(is.na(g))){annotation_custom(g, xmin=aest['left_pic'], xmax=aest['right_pic'], ymin=aest['bottom_pic'], ymax=aest['top_pic'])}
  }



none_plot = ggplot() +  
  geom_text(aes(x = 0, y = 2, 
                label = none_text, fontface = "bold"), size = aest['font_size_vals']) +
  theme_bw()+ 
  scale_y_continuous(breaks = NULL, limits = c(0,4)) + 
  scale_x_continuous(breaks = NULL, limits = c(-1,1)) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 

list(profile_plot,none_plot)

}


importance_utility_ranking = function(df, key, nr_profiles, none_option){
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
  print(nrow(key))
  print(nrow(df))
  df$set = rep(1:nrow(key), nrow(df)/nrow(key))
  #set.seed(42); df$y[is.na(df$y)] = apply(df[is.na(df$y),] ,1, function(x){sample(df$y[!is.na(df$y) & df$set == x['set']], 1)})
  
  print("A")
  #format column and attribute names
  colnames(key) = gsub("_a","",colnames(key));
  colnames(key) = gsub("_b","",colnames(key));
  if(nr_profiles > 2){colnames(key) = gsub("_c","",colnames(key))}
  if(nr_profiles > 3){colnames(key) = gsub("_d","",colnames(key))}
  colnames(key) =trimws(colnames(key))
  attribute_names =trimws(unique(colnames(key)[colnames(key) != 'Set']))
  print(attribute_names)
  print("B")
  nr_attributes = length(attribute_names)
  
  #extract and count levels for attributes
  sets = 1:nrow(key)
  temp = key[,1:nr_attributes]
  nr_levels = sapply(X = temp, FUN = function(x){length(unique(x))})
  profile = sort(rep(1:nr_profiles, nrow(key)))
  print("C")
  #put profiles of same set underneath each other (long format)
  print(nr_attributes)
  print(nr_profiles)
  print(ncol(key))
  A_key = key[,1:nr_attributes]
  print("C1")
  B_key = key[,(nr_attributes +1): (2* nr_attributes)]
  print("C2")
  if(nr_profiles == 2){
    long_key = cbind(sets, profile, rbind(A_key, B_key))
  }else if(nr_profiles == 3){C_key = key[,(2* nr_attributes + 1): (3* nr_attributes)]
  print("C3")
  long_key = cbind(sets, profile, rbind(A_key, B_key, C_key))
  }else if(nr_profiles == 4){D_key = key[,(3* nr_attributes + 1): (4* nr_attributes)]
  long_key = cbind(sets, profile, rbind(A_key, B_key, C_key, D_key))
  }else{stop("Too few/many profiles per set")}
  print("D")
  #go from all profiles to all profiles per participant in right format for choicemodelR
  long_key = long_key[order(long_key$sets),]
  long_df = long_key[rep(1:nrow(long_key), nr_participants),]
  ID = sort(rep(df$ID, (nr_profiles)), decreasing = F)
  long_df = cbind(ID, long_df)
  print("E")
  #populate response variable in right way for choicemodelR
  long_df$y = NA
  long_df$ID_set = paste(long_df$ID, long_df$set, sep = "_")
  past_ID_set = 0
  print("F")
  for(i in 1:nrow(long_df)){
    current_ID_set = long_df$ID_set[i]
    if(current_ID_set  == past_ID_set){long_df$y[i] = 0
    }else{
      past_ID_set =  current_ID_set
      profile_choice = df$y[df$ID == long_df$ID[i] & df$set == long_df$sets[i]]
      
      long_df$y[i] = profile_choice}
  }
  long_df$ID_set = NULL
  print("G")
  #numerical coding of attribute levels
  colnames(long_df) = trimws(colnames(long_df))
  long_df[,colnames(long_df) %in% attribute_names] = sapply(long_df[,colnames(long_df) %in% attribute_names], function(x){as.numeric(factor(x))})
  #fit model with normal prior centered on zero with variance = 2
  long_df = as.matrix(long_df)
  xcoding = rep(0, nr_attributes)
  mcmc = list(R = 4000, use = 3000) 
  options = list(none=none_option, save=TRUE, keep=1)
  print("H")
  #untilhere
  out = cust_choicemodelr(long_df, xcoding, mcmc = mcmc, options = options)
  print("I")
  betas = colMeans(out$betawrite)
  cumu_nr_levels = cumsum(nr_levels)
  #prep dfs for plotting
  importance = c()
  prev_index = 0
  all_attributes = c()
  all_levels = c()
  print("J")
  for(i in 1:length(cumu_nr_levels)){
    mini_b = min(betas[(prev_index+1):(cumu_nr_levels[i])])
    maxi_b = max(betas[(prev_index+1):(cumu_nr_levels[i])])
    
    importance = c(importance, rep(maxi_b - mini_b, nr_levels[i]))
    
    all_attributes = c(all_attributes, rep(attribute_names[i], nr_levels[i]))
    attr_levels = sort(unique(A_key[,attribute_names[i]]), decreasing = F)
    all_levels = c(all_levels, attr_levels)
    prev_index = cumu_nr_levels[i]}
  print("K")
  if(none_option){all_attributes = c(all_attributes, "None"); importance = c(importance, betas[length(betas)]); all_levels = c(all_levels, "None")}
  
  
  plotting_df = as.data.frame(cbind(all_attributes, all_levels, importance, betas))
  plotting_df$betas = as.numeric(plotting_df$betas)
  
  plotting_df$importance = as.numeric(plotting_df$importance)
  plotting_df$importance = plotting_df$importance /sum(plotting_df$importance[plotting_df$all_attributes != "None" & !duplicated(plotting_df$all_attributes)]) *100
  
  Encoding(plotting_df$all_levels) = Encoding(plotting_df$all_attributes) = "UTF-8"
  print("L")
  
  #plot importance
  importance_plot = ggplot(plotting_df[!duplicated(plotting_df$all_attributes) & plotting_df$all_attributes != "None",]) + 
    geom_bar(aes(x = reorder(all_attributes, -1*importance),y = importance, fill = all_attributes), stat = "identity", show.legend = FALSE) + # fill = reorder(all_attributes, -1*importance)), stat =
    geom_text(aes(x = reorder(all_attributes, -1*importance),y = importance, label=paste0(round(importance, 2), "%", sep=""), vjust=-0.25)) +
    theme_bw() + labs(x = element_blank(), y = "Importance", title = "Importance of different attributes") + theme(text = element_text(size = 18))
  
  
  #plot utilities
  utility_plot = ggplot(plotting_df[plotting_df$all_attributes != "None",]) + 
    geom_bar(aes(x = reorder(all_levels, -betas), y = betas, fill = all_attributes), stat = "identity", show.legend = FALSE) +
    geom_text(aes(x = reorder(all_levels, -betas), y = betas, label=round(betas, 4), vjust=0.25)) +
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
  print("M")
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
  
  return(list(importance_plot, utility_plot, all_profiles, out$betawrite, plotting_df, within_attribute_beta_comparisons, between_attribute_importance_comparisons_df))
}

market_simulator = function(selected_profiles, betawrite, plotting_df){
  #market simulator
  
  #selected_profiles = all_profiles[107,];betawrite = out$betawrite #for testing function outside app
  one_hot_vector = apply(selected_profiles, 1, function(x){plotting_df$all_levels %in% x})
  participant_scores = betawrite %*% one_hot_vector #none beta; out$betawrite
  market_simulation = apply(participant_scores, 1, function(x){which.max(x)})
  market_simulation = table(market_simulation)
  market_simulation = market_simulation / sum(market_simulation) * 100
  names(market_simulation) = paste("Rank", selected_profiles$Rank)
  
  pie(market_simulation, 
      labels = paste0(names(market_simulation), " (",round(market_simulation), "%)"),
      edges = 500,
      main = "Artificial market simulation")
}

cust_choicemodelr <-function(data, xcoding, demos, prior, mcmc, constraints, options) {
  set.seed(42)
  callStop = function(message) { stop(message, call. = FALSE) }
  
  if (missing(data)) { callStop("data argument required") }
  if (ncol(data) < 5) { callStop("there are not enough columns in data to specify ID, Set, Alt, X variable(s), and y") }
  natts = ncol(data) - 4
  if (missing(xcoding)) { callStop("xcoding argument required") }
  if (length(xcoding) != natts) { callStop("length of xcoding is not equal to number of attributes") }
  if (any(xcoding != 0 & xcoding != 1)) { callStop("xcoding must contain only values 0 and 1") }
  npw = sum(xcoding == 0)
  ID = unique(data[,1])
  nunits = length(ID)
  drawdelta = ifelse(missing(demos), FALSE, TRUE)
  if (drawdelta) {
    if (nrow(demos) != nunits) { callStop("number of rows in demos does not equal number of units") }
    nz = ncol(demos)
    demos = t(t(demos) - colMeans(demos))
  }
  if (missing(options)) {
    none = FALSE
    save = FALSE
    restart = FALSE
    keep = 10
    wgt = 5
  }
  else {
    none = ifelse(is.null(options$none), FALSE, options$none)
    save = ifelse(is.null(options$save), FALSE, options$save)
    restart = ifelse(is.null(options$restart), FALSE, options$restart)
    keep = ifelse(is.null(options$keep), 10, options$keep)
    wgt = ifelse(is.null(options$wgt), 5, options$wgt)
    if (wgt < 1 | wgt > 10) { callStop("wgt must be in the range 1 to 10") }
  }
  
  maxsets = max(data[,2])
  maxalts = max(data[,3]) + ifelse(none, 1, 0)
  
  nsets = rep(0, nunits)
  nalts = matrix(0, nrow = nunits, ncol = maxsets)
  for (i in 1:nunits) {
    temp = data[data[,1] == ID[i],]
    nsets[i] = max(temp[,2])
    for (j in 1:nsets[i]) {
      nalts[i, j] = max(temp[temp[,2] == j, 3]) + ifelse(none, 1, 0)
    }
  }
  
  setsmap = (nalts > 0)
  altsmap = matrix(FALSE, nrow = sum(nsets), ncol = maxalts)
  index = 1
  nalts = t(nalts) # TO GET AROUND R COLUMN FILL
  for (i in 1:length(nalts)) {
    if (nalts[i] != 0) {
      altsmap[index, 1:nalts[i]] = TRUE
      index = index + 1
    }
  }
  
  avgalts = mean(nalts[nalts > 0])
  avgsets = mean(nsets)
  nalts = colSums(nalts)
  
  info = list(nunits = nunits,
              maxsets = maxsets,
              maxalts = maxalts,
              nsets = nsets,
              nalts = nalts,
              setsmap = setsmap,
              altsmap = altsmap)	
  
  if (all(data[!(data[,3] == 1), ncol(data)] == 0)) { share = FALSE }
  else { share = TRUE }
  
  if (share) {
    Xt = data[, c(-1, -2, -3, -ncol(data)), drop = FALSE]
    y = matrix(0, nrow = maxalts, ncol = sum(nsets))
    y[t(altsmap)] = data[, ncol(data)]
    y = t(y)
    ytab = colSums(y > 0); ytab = rbind(ytab, round(ytab / sum(ytab) * 100, 2))
    y = y / rowSums(y) * wgt
  }
  else {
    if (none) {
      Xt = NULL
      for (i in 1:nunits) {
        for (j in 1:nsets[i]) {
          ind = (data[,1] == ID[i]) & (data[,2] == j)
          Xt = rbind(Xt, cbind(data[ind, c(-1, -2, -3, -ncol(data)), drop = FALSE], 0), c(rep(0, natts), 1))
        }
      }
    }
    else { Xt = data[, c(-1, -2, -3, -ncol(data)), drop = FALSE] }
    y = data[data[,3] == 1, ncol(data)]
    
    if (any(y[!is.na(y)] > maxalts) | any(y[!is.na(y)] < 1)) { callStop(paste("invalid values of y present in data - values must be 1 to ", 
                                                                              maxalts, sep = "")) }
    ytab = table(y); ytab = rbind(ytab, round(ytab / sum(ytab) * 100, 2))
  }
  
  nlevels = c(rep(0, natts))
  for (i in 1:natts) {
    if (xcoding[i] == 0) {
      nlevels[i] = length(unique(Xt[,i]))
      if (any(unique(Xt[,i]) == 0)) { nlevels[i] = nlevels[i] - 1 }
    }
    else { nlevels[i] = 1 }
  }
  npar = sum(nlevels) - npw + ifelse(none, 1, 0)
  X = matrix(0, nrow = nrow(Xt), ncol = npar)
  Xind = 1
  if (none) { nn = Xt[, ncol(Xt)] == 0 }
  else { nn = rep(TRUE, nrow(Xt)) }
  for (i in 1:natts) {
    if (xcoding[i] == 1) {
      X[nn, Xind] = Xt[nn, i] - mean(unique(Xt[nn, i]))
      Xind = Xind + 1
    }
    else {
      for (j in 1:(nlevels[i] - 1)) {
        X[nn, Xind] = (Xt[nn, i] == j) * 1 - (Xt[nn, i] == nlevels[i]) * 1
        Xind = Xind + 1
      }
    }
  }
  if (none) { X[, npar] = Xt[, ncol(Xt)] }
  
  effectsmap = matrix(FALSE, nrow = natts, ncol = npar)
  index = 1
  for (i in 1:natts) {
    count = 1
    repeat {
      effectsmap[i, index] = TRUE
      index = index + 1
      count = count + 1
      if (count > max(1, nlevels[i] - 1)) { break }
    }
  }
  if (none) { effectsmap = rbind(effectsmap, c(rep(FALSE, npar - 1), TRUE)) }
  
  if (missing(prior)) { prior = list() }
  if (is.null(prior$mubar)) { mubar = matrix(rep(0, npar), nrow = 1) }
  else {
    mubar = matrix(prior$mubar, nrow = 1)
    if (ncol(mubar) != npar) { callStop("ncol(mubar) does not equal npar") }
  }
  if (is.null(prior$Amu)) { Amu = matrix(0.01, ncol = 1) }
  else {
    Amu = matrix(prior$Amu, ncol = 1)
    if (any(dim(Amu) != c(1, 1))) { callStop("Amu must be a 1 x 1 matrix") }
  }
  if (is.null(prior$df)) { df = 5 }
  else {
    df = prior$df
    if (df < 2) { callStop("invalid value of df - must be >= 2") }
  }
  nu = npar + df
  if (is.null(prior$v)) {
    v = 2
  }
  else {
    v = prior$v
    if (v <= 0) { callStop("invalid v - must be >= 0") }
  }
  if (is.null(prior$Ad) & drawdelta) { Ad = 0.01 * diag(npar * nz) }
  else if (drawdelta) {
    Ad = prior$Ad
    if (any(dim(Ad) != c(npar * nz, npar * nz))) { callStop("Ad must be of dimensions (npar * nz) x (npar * nz)") }
  }
  if (is.null(prior$deltabar) & drawdelta) { deltabar = rep(0, nz * npar) }
  else if (drawdelta) {
    deltabar = prior$deltabar
    if (length(deltabar) != nz * npar) { callStop("deltabar must be of length npar * nz") }
  }
  
  V = matrix(0, nrow = npar, ncol = npar)
  v.ind = 1
  for (i in 1:natts) {
    if (nlevels[i] == 1) {
      V[v.ind, v.ind] = 1
      v.ind = v.ind + 1
    }
    else {
      pcov = -1 / nlevels[i]
      pvar = (nlevels[i] - 1) / nlevels[i]
      temp = pvar * diag(nlevels[i] - 1)
      temp[upper.tri(temp) | lower.tri(temp)] = pcov
      
      V[v.ind:(v.ind + nlevels[i] - 2), v.ind:(v.ind + nlevels[i] - 2)] = temp
      v.ind = v.ind + nlevels[i] - 1
    }
  }
  if (none) { V[v.ind, v.ind] = 1 }
  V = V * nu * v
  
  if (missing(mcmc)) { callStop("mcmc argument required") }
  if (is.null(mcmc$s)) {
    s = 0.1
    adjust.s = TRUE
  }
  else {
    s = mcmc$s
    adjust.s = FALSE
  }
  if (is.null(mcmc$R)) { callStop("element R of mcmc required") }
  R = mcmc$R
  if (is.null(mcmc$use)) { use = min(0.5 * R, 10000) }
  else {
    use = mcmc$use
    if (use > R) { callStop("use must be <= R") }
  }
  
  constrain = ifelse(missing(constraints), FALSE, TRUE)
  if (constrain) {
    if (length(constraints) != natts) { callStop("length(constraints) must equal number of attributes") }
    for (i in 1:natts) {
      if (any(dim(constraints[[i]]) != c(nlevels[i], nlevels[i]))) {
        callStop("dim(constraints[[i]]) must equal nlevels[i] x nlevels[i]")
      }
      if (any(constraints[[i]] != -1 & constraints[[i]] != 0 & constraints[[i]] != 1)) {
        callStop("constraints matrices must only contain -1, 0, and 1 elements")
      }
    }
  }
  
  fR = 0
  if (restart) {
    if (!any(list.files() == "restart.txt")) {
      callStop("file restart.txt does not exist in the working directory")
    }
    f.in = scan("restart.txt", nlines = 1, skip = 0, quiet = TRUE)
    fR = f.in[1]
    fUnits = f.in[2]
    fPar = f.in[3]
    s = f.in[4]
    fCons = f.in[5]
    fDem = f.in[6]
    items = 1
    ltr = fUnits
    if (fCons == 1) { 
      items = items + 1 
      ltr = c(ltr, fUnits)	
    } 
    if (fDem == 1) { 
      items = items + 1 
      ltr = c(ltr, 1)
    }
    if (fUnits != nunits) { callStop("number of units in restart file does not match data file") }	
    if (fPar != npar) { callStop("number of parameters in restart file does not match data file") }
    if (fCons != as.numeric(constrain)) { callStop("restart file and function arguments do not agree on constraints being present") }
    if (fDem != as.numeric(drawdelta)) { callStop("restart file and function arguments do not agree on demographics being present") }
    
    fIndex = 1
    fCount = 1
    repeat {
      if (fCount > items) { break }
      f.in = scan("restart.txt", nlines = ltr[fCount], skip = fIndex, quiet = TRUE)
      switch(fCount,
             lb <- matrix(f.in, ncol = fPar, byrow = TRUE),
             lbc <- matrix(f.in, ncol = fPar, byrow = TRUE),
             ld <- c(f.in)
      )
      fIndex = fIndex + ltr[fCount]
      fCount = fCount + 1
    }
  }
  
  if (save) {
    if (drawdelta) { deltadraw = matrix(0, nrow = floor(use / keep), ncol = nz * npar) }
    betadraw = array(0, dim = c(nunits, npar, floor(use / keep)))
    compdraw = NULL
    loglike = rep(0, floor(use / keep))
  }
  if (constrain) {
    if (save) { betadraw.c = array(0, dim = c(nunits, npar, floor(use / keep))) }
    if (restart) { oldbetas.c = lbc }
    else { oldbetas.c = matrix(0, nrow = nunits, ncol = npar) }
  }
  betaout = matrix(0, nrow = nunits, ncol = npar)
  if (drawdelta) { 
    if (restart) { olddelta = ld } 
    else { olddelta = rep(0, nz * npar) }
  }
  if (restart) { oldbetas = lb }
  else { oldbetas = matrix(0, nrow = nunits, ncol = npar) }
  oldll = rep(0, nunits)
  oldcomp = NULL
  
  muplot = matrix(0, nrow = R, ncol = npar + npw)
  xplot = (1 + fR):(R + fR)
  
  #
  # DEFINE NEEDED FUNCTIONS
  #
  getLLMnl = function (beta, y, X, info) {
    nunits = info$nunits
    nalts = info$maxalts
    nsets = info$maxsets
    
    map.sets = t(info$setsmap)
    map.alts = t(info$altsmap)
    
    
    tmpXbeta = X * beta[rep(1:nunits, info$nalts),]
    
    tmpXbeta = rowSums(tmpXbeta)
    
    Xbeta = matrix(0, nrow = nalts, ncol = sum(info$nsets))
    
    Xbeta[map.alts] = tmpXbeta
    
    # SHARE DATA
    if (is.matrix(y)) {
      Xbeta[map.alts] = exp(Xbeta[map.alts])
      denom = colSums(Xbeta)
      probs = (t(Xbeta) / denom) ^ y
      tmpProbs = apply(probs, 1, prod)
      probs = matrix(1, nrow = nsets, ncol = nunits)
      probs[map.sets] = tmpProbs
      
      ll = log(apply(probs, 2, prod))
    }
    
    # DISCRETE DATA
    else {
      ind = cbind(y, 1:sum(info$nsets))
      
      xby = matrix(0, nrow = nsets, ncol = nunits)
      xby[map.sets] = Xbeta[ind]
      Xbeta[map.alts] = exp(Xbeta[map.alts])
      denom = matrix(0, nrow = nsets, ncol = nunits)
      denom[map.sets] = log(colSums(Xbeta))
      hannestemp = xby - denom
      
      ll = colSums(hannestemp, na.rm = TRUE)
      
    }
    
    return(ll)
  }
  
  getLndMvn = function (x, mu, rooti) {
    npar = ncol(x)
    #
    # WITH COVARIATES
    #	
    if (is.matrix(mu)) { z = (x - mu) %*% rooti }
    #
    # NO COVARIATES
    #
    else { z = crossprod(t(x) - mu, rooti) }
    
    logs = -(npar / 2) * log(2 * pi) - 0.5 * rowSums(z * z) + sum(log(diag(rooti)))
    
    return(logs)
  }
  
  drawDelta = function (x, y, comps, deltabar, Ad) {
    yy = t(t(y) - comps$mu)
    sig = tcrossprod(comps$rooti)
    
    xtx = crossprod(x) %x% sig
    xty = matrix(sig %*% crossprod(yy, x), ncol = 1)
    
    cov = chol2inv(chol(xtx + Ad))
    
    return(cov %*% (xty + Ad %*% deltabar) + t(chol(cov)) %*% rnorm(length(deltabar)))
  }
  
  mnlRwMetropOnce = function (y, X, oldbeta, oldll, s, inc.root, betabar, rootpi, info, constraints, oldbeta.c) {
    stay = 0
    nunits = info$nunits
    npar = ncol(oldbeta)
    
    increment = s * crossprod(matrix(rnorm(nunits * npar), ncol = nunits), inc.root)
    newbeta = oldbeta + increment
    
    if (!missing(constraints)) {
      newbeta.c = constrainBetas(newbeta, constraints)
      newll = getLLMnl(newbeta.c, y, X, info)
    }
    else {
      newll = getLLMnl(newbeta, y, X, info)
    }
    newlpost = newll + getLndMvn(newbeta, betabar, rootpi)
    ldiff = newlpost - oldll - getLndMvn(oldbeta, betabar, rootpi)
    alpha = exp(ldiff)
    alpha[alpha > 1] = 1
    unif = runif(nunits)
    unif[alpha == 1] = 0
    
    good = (unif <= alpha)
    betadraw = oldbeta
    betadraw[good,] = newbeta[good,]
    if (!missing(constraints)) {
      betadraw.c = oldbeta.c
      betadraw.c[good,] = newbeta.c[good,]
    }
    oldll[good] = newll[good]
    
    stay = sum(!good)
    
    if (!missing(constraints)) {
      return(list(betadraw = betadraw, betadraw.c = betadraw.c, stay = stay, oldll = oldll))
    }
    else {
      return(list(betadraw = betadraw, stay = stay,  oldll = oldll))
    }
  }
  
  constrainBetas = function (betas, constraints) {
    newlength = 0
    oldlength = 0
    for (h in 1:length(constraints)) {
      if (all(constraints[[h]] == 0)) {
        if (ncol(constraints[[h]]) == 1) {
          newlength = 1
        }
        else {
          newlength = ncol(constraints[[h]]) - 1
        }
        oldlength = oldlength + newlength
        next
      }
      else if (ncol(constraints[[h]]) == 1) {
        newlength = 1
        betat = betas[,oldlength + 1]
        if (constraints[[h]] == 1) {
          betat[betat < 0] = 0
        }
        if (constraints[[h]] == -1) {
          betat[betat > 0] = 0
        }
        betas[,oldlength + 1] = betat
      }
      else {
        newlength = ncol(constraints[[h]]) - 1
        betat = betas[,(oldlength + 1):(oldlength + newlength), drop = FALSE]
        betat = cbind(betat, -1 * rowSums(betat))
        repeat {
          bad = FALSE
          for (i in 1:(nrow(constraints[[h]]) - 1)) {
            for (j in (i + 1):ncol(constraints[[h]])) {
              if (constraints[[h]][i, j] == 1) {
                change = (betat[,i] < betat[,j])
                if (sum(change) > 0) {
                  betat[change, j] = betat[change, i]
                  betat[change,] = betat[change,] - rowMeans(betat[change,,drop = FALSE])
                  bad = TRUE
                }
              }
              else if (constraints[[h]][i, j] == -1) {
                change = (betat[,i] > betat[,j])
                if (sum(change) > 0) {
                  betat[change, i] = betat[change, j]
                  betat[change,] = betat[change,] - rowMeans(betat[change,,drop = FALSE])
                  bad = TRUE
                }
              }
            }
          }
          if (!bad) { break }
        }
        betas[,(oldlength + 1):(oldlength + newlength)] = betat[,1:newlength]
      }
      oldlength = oldlength + newlength
    }
    return(betas)
  }
  
  rGibbs = function (y, betabar, A, nu, V) {
    temp = rmultireg(y, matrix(rep(1, nrow(y)), ncol = 1), betabar, A, nu, V)
    comps = list(mu = as.vector(temp$B), rooti = backsolve(chol(temp$Sigma), diag(ncol(temp$Sigma))))
    
    return(comps)
  }
  
  rmultireg = function (Y, X, Bbar, A, nu, V) {
    n = nrow(Y)
    m = ncol(Y)
    k = ncol(X)
    RA = chol(A)
    W = rbind(X, RA)
    Z = rbind(Y, RA %*% Bbar)
    IR = backsolve(chol(crossprod(W)), diag(k))
    Btilde = crossprod(t(IR)) %*% crossprod(W, Z)
    S = crossprod(Z - W %*% Btilde)
    rwout = rwishart(nu + n, chol2inv(chol(V + S)))
    B = Btilde + IR %*% matrix(rnorm(m * k), ncol = m) %*% t(rwout$CI)
    
    return(list(B = B, Sigma = rwout$IW))
  }
  
  rwishart = function (nu, V) {
    m = nrow(V)
    df = (nu + nu - m + 1) - (nu - m + 1):nu
    if (m > 1) {
      T = diag(sqrt(rchisq(c(rep(1, m)), df)))
      T[lower.tri(T)] = rnorm((m * (m + 1)/2 - m))
    }
    else {
      T = sqrt(rchisq(1, df))
    }
    U = chol(V)
    C = t(T) %*% U
    CI = backsolve(C, diag(m))
    
    return(list(W = crossprod(C), IW = crossprod(t(CI)), C = C, CI = CI))
  }
  
  getEffectsCodedParameters = function(parms, map, xcoding) {
    out = NULL
    # FOR THE NON-CODED NONE PARAMETER
    if (nrow(map) > length(xcoding)) { xcoding = c(xcoding, 1) }
    for (i in 1:nrow(map)) {
      if (xcoding[i] == 0) {
        out = cbind(out, parms[,map[i,], drop = FALSE], -1 * rowSums(parms[,map[i,], drop = FALSE]))
      }
      else {
        out = cbind(out, parms[,map[i,], drop = FALSE])
      }
    }
    return(out)
  }
  
  fsh = function() {
    if (Sys.info()[1] == "Windows") { flush.console() }
    return()
  }
  
  #
  # OPEN CONNECTION FOR WRITING TO LOG FILE
  #
  on.exit(sink())
  sink("RLog.txt", append = TRUE, type = "output", split = TRUE)
  
  #
  # PRINT TO CONSOLE AND LOG
  #
  if (restart) {
    cat("Restarting from previous ", fR, "-iteration model estmation...", sep = "", fill = TRUE)
    cat("", fill = TRUE)
  }
  cat("                    Logit Data                    ", fill = TRUE)
  cat("==================================================", fill = TRUE)
  cat("Attribute       Type         Levels", fill = TRUE)
  cat("-----------------------------------", fill = TRUE)    
  for (i in 1:natts) {
    cat(sprintf("%-12s   %-12s   %2i", paste("Attribute", i), 
                ifelse(xcoding[i] == 0, "Part Worth", "Linear"), nlevels[i]), fill = TRUE)
  } 
  cat("", fill = TRUE)
  cat(npar, " parameters to be estimated", ifelse(none, " (including 'None').", "."), sep = "", fill = TRUE)
  cat("", fill = TRUE)
  cat(nunits, "total units.", fill = TRUE)
  cat("Average of", round(avgalts, 1), "alternatives in each of", round(avgsets, 1), "sets per unit.", fill = TRUE)
  cat(ifelse(share, sum(ytab[1,]), sum(nsets)), ifelse(share, " expanded", ""), " tasks in total.", sep = "", fill = TRUE)
  cat("", fill = TRUE)
  cat("Table of choice data pooled across units:", fill = TRUE)
  cat("Choice  Count   Pct.", fill = TRUE)
  cat("--------------------", fill = TRUE)
  for (i in 1:maxalts) {
    cat(sprintf("%4i    %-6i %s", i, ytab[1,i], paste(ytab[2,i],'%', sep = "")), 
        fill = TRUE)
  }	
  cat("", fill = TRUE)
  cat("      MCMC Inference for Hierarchical Logit       ", fill = TRUE)
  cat("==================================================", fill = TRUE)
  cat("Total Iterations:         ", R + fR, fill = TRUE)
  cat("Draws used in estimation: ", use, fill = TRUE)
  cat("Units:                    ", nunits, fill = TRUE)
  cat("Parameters per unit:      ", npar, fill = TRUE)
  if (share) {
    cat("Task weight:              ", wgt, fill = TRUE)
  }
  cat("Constraints ", ifelse(constrain, "", "not "), "in effect.", fill = TRUE, sep = "")
  cat("Draws ", ifelse(save, "are to be ", "not "), "saved.", fill = TRUE, sep = "")
  cat("Prior degrees of freedom: ", df, fill = TRUE)
  cat("Prior variance:           ", v, fill = TRUE)
  cat("", fill = TRUE)
  cat("MCMC Iteration Beginning...", fill = TRUE)
  fsh()
  
  itime = proc.time()[3]
  acceptr.t = 0
  
  for (rep in 1:R) {
    set.seed(rep)
    if (drawdelta) {
      mgout = rGibbs(oldbetas - demos %*% t(matrix(olddelta, ncol = nz)), mubar, Amu, nu, V)
      oldcomp = mgout
      olddelta = drawDelta(demos, oldbetas, oldcomp, deltabar, Ad)
    }
    else {	
      mgout = rGibbs(oldbetas, mubar, Amu, nu, V)
      oldcomp = mgout
    }
    
    if (rep == 1) {
      if (constrain) {
        oldll = getLLMnl(oldbetas.c, y, X, info)
      }
      else {
        oldll = getLLMnl(oldbetas, y, X, info)
      }
    }
    
    rootpi = oldcomp$rooti
    inc.root = chol(chol2inv(chol(tcrossprod(rootpi))))
    if (drawdelta) {
      betabar = t(matrix(olddelta, ncol = nz) %*% t(demos) + oldcomp$mu)
    }
    else {
      betabar = oldcomp$mu
    }		
    
    if (constrain) {
      metropout = mnlRwMetropOnce(y, X, oldbetas, oldll, s, inc.root, betabar, rootpi, info, constraints, oldbetas.c)
      oldbetas.c = metropout$betadraw.c
    }
    else {
      metropout = mnlRwMetropOnce(y, X, oldbetas, oldll, s, inc.root, betabar, rootpi, info)
    }
    oldbetas = metropout$betadraw
    oldll = metropout$oldll
    
    #
    # CALCULATE GOODNESS-OF-FIT MEASUREMENTS
    #
    if (share) {
      RLH = exp(mean(oldll))^(1 / (avgsets * wgt))
      PctCert = (mean(oldll) - log(1 / avgalts) * avgsets * wgt) / (-log(1 / avgalts) * avgsets * wgt)
    }
    else {
      RLH = exp(mean(oldll))^(1 / avgsets)
      PctCert = (mean(oldll) - log(1 / avgalts) * avgsets) / (-log(1 / avgalts) * avgsets)
    }
    AvgVar = mean(diag(crossprod(inc.root)))
    RMS = sqrt(mean(oldbetas ^ 2))
    
    if (rep == 1) {
      RLH.a = RLH
      PctCert.a = PctCert
      AvgVar.a = AvgVar
      RMS.a = RMS
    }
    else {
      RLH.a = 0.99 * RLH.a + 0.01 * RLH
      PctCert.a = 0.99 * PctCert.a + 0.01 * PctCert
      AvgVar.a = 0.99 * AvgVar.a + 0.01 * AvgVar
      RMS.a = 0.99 * RMS.a + 0.01 * RMS
    }
    
    #
    # ADJUST SCALING PARAMETER TO TRY AND KEEP ACCEPTANCE RATE ~30%
    #
    acceptr = nunits - metropout$stay
    acceptr.t = acceptr.t + acceptr
    if (adjust.s) {
      if (acceptr / nunits < 0.3) { s = s * 0.9 }
      else if (acceptr / nunits > 0.3) { s = s * 1.1 }
    }
    acceptr = 0
    
    #
    # PREPARE MU VALUES FOR PLOTTING
    #
    mutemp = matrix(oldcomp$mu, nrow = 1)
    muplot[rep,] = getEffectsCodedParameters(mutemp, effectsmap, xcoding)
    
    newplot = FALSE
    if (rep == 1) {
      yl = c(-1, 1)
      matplot(0, 0, type = "l", col = 1:8, lty = 1, ylim = yl, xlim = c(1, R + fR), xlab = "Rep", ylab = "Mu")
    }
    if (rep %% 100 == 0) {
      #
      # PLOT MU
      #
      if (max(muplot[(rep - 99):rep,]) >= yl[2]) {
        yl[2] = ceiling(max(muplot[(rep - 99):rep,]))
        newplot = TRUE
      }
      if (min(muplot[(rep - 99):rep,]) <= yl[1]) {
        yl[1] = floor(min(muplot[(rep - 99):rep,]))
        newplot = TRUE
      }
      if (newplot) {
        matplot(xplot[1:rep], muplot[1:rep,], type = "l", col = 1:8, lty = 1, ylim = yl, xlim = c(1, R + fR), xlab = "Rep", ylab = "Mu")
      }
      else {
        matplot(xplot[(rep - 100):rep], muplot[(rep - 100):rep,], type = "l", col = 1:8, lty = 1, add = TRUE)
      }
      
      #
      # UPDATE PROGRESS
      #
      ctime = proc.time()[3]
      timetoend = ((ctime - itime) / rep) * (R + 1 - rep)
      if (rep == 100) {
        cat("Iteration ", "Acceptance  ", "RLH    ", "Pct. Cert.  ", "Avg. Var.  ", "RMS    ", "Time to End", fill = TRUE)
      }
      cat(sprintf("%9.0i  %-5.3f        %-5.3f   %-5.3f        %-5.2f       %-5.2f   %-6s",
                  rep + fR, acceptr.t / (100 * nunits), RLH.a, PctCert.a, AvgVar.a, RMS.a, 
                  paste(timetoend %/% 60, ifelse(round(timetoend %% 60) > 9, ":", ":0"), round(timetoend %% 60), 
                        sep = "")), fill = TRUE)
      fsh()
      acceptr.t = 0
    }
    
    if (rep > R - use) {
      if (save) {
        mkeep = (rep - (R - use)) / keep
        if (rep %% keep == 0) {
          betadraw[,,mkeep] = oldbetas
          if (constrain) { betadraw.c[,,mkeep] = oldbetas.c }
          if (drawdelta) { deltadraw[mkeep,] = olddelta }
          loglike[mkeep] = sum(oldll)
          compdraw[[mkeep]] = oldcomp
        }
      }
      if (constrain) {
        betaout = betaout + oldbetas.c
      }
      else {
        betaout = betaout + oldbetas
      }
    }		
  }
  
  ctime = proc.time()[3]
  cat("", fill = TRUE)
  cat("Total Time Elapsed: ", (ctime - itime) %/% 60, ifelse(round((ctime - itime) %% 60) > 9, ":", ":0"), 
      round((ctime - itime) %% 60), fill = TRUE, sep = "")
  cat("", fill = TRUE)
  
  #
  # WRITE RESTART FILE
  #	
  write.table(cbind(R + fR, nunits, npar, s, as.numeric(constrain), as.numeric(drawdelta)), "restart.txt", sep = " ", row.names = FALSE, col.names = FALSE)
  write.table(oldbetas, "restart.txt", sep = " ", row.names = FALSE, col.names = FALSE, append = TRUE)
  if (constrain) {
    write.table(oldbetas.c, "restart.txt", sep = " ", row.names = FALSE, col.names = FALSE, append = TRUE)
  }
  if (drawdelta) {
    write.table(t(olddelta), "restart.txt", sep = " ", row.names = FALSE, col.names = FALSE, append = TRUE)
  }
  
  #
  #
  # WRITE RESPONDENT BETAS TO CSV FILE
  #
  betaout = betaout / use
  betawrite = getEffectsCodedParameters(betaout, effectsmap, xcoding) #aaa cbind(matrix(ID, ncol = 1), getEffectsCodedParameters(betaout, effectsmap, xcoding))
  betanames = c() #aaa  "ID"
  for (i in 1:natts) {
    for (j in 1:nlevels[i]) {
      betanames = c(betanames, paste("A", i, "B", j, sep = ""))
    }
  }
  if (none) { betanames = c(betanames, "NONE") }
  cat("Writing estimated unit-level betas to Rbetas.csv in the working directory", fill = TRUE)
  cat("", fill=TRUE)
  write.table(betawrite, file = "RBetas.csv", sep = ",", col.names = betanames, row.names = FALSE, qmethod = "double") 			
  
  if (save) { 
    switch(1 + 1 * constrain + 2 * drawdelta,
           return(list(betadraw = betadraw, compdraw = compdraw, loglike = loglike, betawrite = betawrite)),
           return(list(betadraw = betadraw, betadraw.c = betadraw.c, compdraw = compdraw, loglike = loglike, betawrite = betawrite)),
           return(list(betadraw = betadraw, deltadraw = deltadraw, compdraw = compdraw, loglike = loglike, betawrite = betawrite)),
           return(list(betadraw = betadraw, betadraw.c = betadraw.c, deltadraw = deltadraw, compdraw = compdraw, loglike = loglike, betawrite = betawrite))
    )
  }
  else { return(NULL) }		 
}


bigger_design = function(x, current_design){
  d = as.integer(unlist(strsplit(current_design, "x")))
  if(length(d) == length(x)){
    return(all(x >= d))
  }else(return(FALSE))
}

current_and_alternative_designs = function(data){
  data = data[,order(colSums(data==""), decreasing = T)]
  data_list = as.list(data)
  data_list = removeListElemComplete(data_list, "")
  ddd = oa.design(factor.names = data_list, columns = "min3", seed = 42)
  colnames(ddd) = gsub("X.", "", colnames(ddd) )
  
  current_design = paste(sort(apply(data, 2, function(x){length(x) - sum(x == "")})), collapse = "x")
  print("h1")
  if(nrow(ddd) <= all_designchecks[current_design]){
    print("h2")
    current = data.frame(c(current_design),c(nrow(ddd)))
    colnames(current) = c("Design", "#Sets")
    return(list(current,NULL, "Good design!"))
  }else{
    print("h3")

  
  if(current_design %in% names(all_designchecks)){

    coded_dcheck = strsplit(names(all_designchecks), "x")
    integer_dcheck = lapply(coded_dcheck,  as.integer)
    better_designs = as.data.frame(unlist(all_designchecks[which(unlist(lapply(integer_dcheck, bigger_design, current_design = current_design)) & all_designchecks <= 48)]))
    better_designs = cbind(rownames(better_designs), better_designs)#
    colnames(better_designs) = c("Design", "#Sets")
    if(all_designchecks[current_design] <= 32){
      message = "Good design!" 
    }else if(nrow(better_designs) > 0){
      message = "Larger, optimized designs available!"
    }else{message = "Design size threatens data quality"}
    
    current = as.data.frame(all_designchecks[current_design])
    current = cbind(names(all_designchecks[current_design]), as.integer(all_designchecks[current_design]))
    colnames(current) = c("Design", "#Sets")
    return(list(current,better_designs, message))
  }else{return(list(NULL, NULL, "Questionable design size"))}
}}

removeListElemComplete = function(inlist, elem_remove) {
  removeListElem <- function(inlist,elem_remove){
    outlist = lapply(inlist,setdiff,elem_remove)
    outlist[lengths(outlist) > 0]
  }
  outlist = lapply(inlist, removeListElem, elem_remove = elem_remove)
  outlist[lengths(outlist) > 0]
}
