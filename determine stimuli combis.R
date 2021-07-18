set.seed(42)
library(conjoint) #version 1.41
library(dplyr)
library(ggplot2)
nr_att_and_lev = 6
attributes_levels = list()
  for(l in 1:nr_att_and_lev){
  attributes_levels[[l]] = paste0(letters[l], 1:nr_att_and_lev)
}
attributes_levels = as.data.frame(attributes_levels, col.names = LETTERS[1:nr_att_and_lev])
#source("C:/Users/hannesrosenbusch/Documents/introducing conjoint/conjoint app/hanneshelpers.R")
load("C:/Users/hannesrosenbusch/Documents/introducing conjoint/designcheck.RData") 
designcheck4 = list()
for(as in 3:nrow(attributes_levels)){
  for(bs in 3:nrow(attributes_levels)){ 
    for(cs in 4:nrow(attributes_levels)){ 
      for(ds in 5:nrow(attributes_levels)){ 
        for(see in 1:10){

            sdes = sort(c(as,bs, cs, ds))
            nr_cards = 2*sum(sdes)
            
            current_design = paste(as.character(sdes), collapse = "x")
            if(!current_design %in% names(designcheck4)){ 
              #print(nr_cards)
              print(current_design)
             exper= expand.grid(attributes_levels[1:as,1], attributes_levels[1:bs,2], attributes_levels[1:cs,3], attributes_levels[1:ds,4])
             print(nrow(exper))
            design2=caFactorialDesign(data=exper,type="orthogonal", seed = 42)
            
            print(nrow(design2))}
                      }
        #designcheck4[current_design]= nrow(design2)
            }}}}
  beepr:beep()

#save(designcheck4, file = "designcheck4.RData") 

  load("C:/Users/hannesrosenbusch/Documents/introducing conjoint/designcheck2.RData") 
  load("C:/Users/hannesrosenbusch/Documents/introducing conjoint/designcheck3.RData") 
  load("C:/Users/hannesrosenbusch/Documents/introducing conjoint/designcheck4.RData") 
  load("C:/Users/hannesrosenbusch/Documents/introducing conjoint/designcheck.RData") 
  designchecks = c(designcheck2[order(names(designcheck2))],designcheck3[order(names(designcheck3))], designcheck4[order(names(designcheck4))], designcheck[order(names(designcheck))])
  save(designchecks, file = "designchecks.Rdata")


  
  
  


bigger_design = function(x, current_design){
  d = as.integer(unlist(strsplit(current_design, "x")))
  if(length(d) == length(x)){
    return(all(x >= d))
  }else(return(FALSE))
}

current_and_alternative_designs = function(data){
  current_design = paste(sort(apply(test, 2, function(x)length(x) - sum(is.na(x)))), collapse = "x")
  if(current_design %in% names(designchecks)){
  coded_dcheck = strsplit(names(designchecks), "x")
  integer_dcheck = lapply(coded_dcheck,  as.integer)
  better_designs = as.data.frame(unlist(designchecks[which(unlist(lapply(integer_dcheck, bigger_design, current_design = current_design)) & designchecks <= 48)]))
  colnames(better_designs) = c("#Sets")
  if(designchecks[current_design] <= 32){
    message = "Good design!"
  }else if(nrow(better_designs) > 0){
    message = "Larger, optimized designs available!"
  }else{message = "Design size threatens data quality"}
  return(list(designchecks[current_design],better_designs, message))
  }else{return(list(NULL, NULL, "Questionable design size"))}
}


load("designchecks.Rdata")
test = data.frame(matrix(data = 1:4, nrow = 4, ncol = 3))
test[4,3] = ""
test[3:4,1] = ""


current_and_alternative_designs(test)
dm_list = as.list(test)
dm_list = removeListElemComplete(dm_list, "")
ddd = oa.design(nlevels = c(2,4,3))
h = nrow(test) - colSums(test=="")
ddd = oa.design(factor.names = dm_list, columns = "min3")

attributes(ddd)$design.info$type

for(i in 1:nrow(designs)){

experiment<-expand.grid( attributes_levels[1:i, 1:j] )
design = paste(rep(i, j))
print(design)
}

design2=caFactorialDesign(data=experiment,type="orthogonal", seed = 42)#federov exchange algorithm
colnames(design2) = c(  "price",
                        "RAM",
                        "weight",
                        "display")
# mix and match -----------------------------------------------------------
#https://d1wqtxts1xzle7.cloudfront.net/41021413/desgncbc-with-cover-page.pdf?Expires=1623244036&Signature=drTInogMRjeoKCkkzAX-zz9Hwc-jrz872E7mcDnp8kw484eNEeawNDduatez20OHqDHmeXjdBZ0YRmV-7nFm5YGRAseRKS6d1IC3brZpiCdqlsEaY2pxyz1pVTA~5RvFjM7EZ23Us2JcklLi70DIhniHdsl-aAbmeTNxhV8AK5Fz9f2idkdIS6kG7Y9YKYbNenK0VJadYMMyoAnEt5g8Vzkh4iuFyilUiICPk~AIZdEW4aMw1R-zsZ4xUE4MrE7UGHk2OE6tDAWcXsQgRDwK8sbfTowq8L3G9nENwg7vks3CA9UeL8I5Gdn0~n~wFMIOLacwcldYjJOSOrk4JwhdHA__&Key-Pair-Id=APKAJLOHF5GGSLRBV4ZA

resample_without_creating_duplicates = function(piles, three = T){
  pile1 = piles[[1]]
  best_pile2 = piles[[2]]
  if(three){best_pile3 = piles[[3]]}

  #resample pile1 
  pile1 = pile1[sample(nrow(pile1)),]
  
  #resample best_pile2 and pile 3
  duplicates_within_sets = T
  if(three){
    while(duplicates_within_sets){
      best_pile2 = best_pile2[sample(nrow(best_pile2)),]
      best_pile3 = best_pile3[sample(nrow(best_pile3)),]
      duplicates_within_sets = any(rowSums(pile1==best_pile3) == ncol(pile1)) | any(rowSums(pile1==best_pile2) == ncol(pile1)) | any(rowSums(best_pile2==best_pile3) == ncol(pile1))
    }
    #add choice_set variables to pile 2 and 3
    pile1$choice_set = best_pile2$choice_set = best_pile3$choice_set = 1:nrow(best_pile3)
    randomized_piles = list(pile1, best_pile2, best_pile3)
  }else{
    while(duplicates_within_sets){
      best_pile2 = best_pile2[sample(nrow(best_pile2)),]
      duplicates_within_sets = any(rowSums(pile1==best_pile2) == ncol(pile1)) 
    }
    #add choice_set variables to pile 2
    best_pile2$choice_set = 1:nrow(best_pile2)
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
  if(third_pile){print('third pile active')
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



d= mix_match(design2, T)
a= d[[1]]
b = d[[2]]
c = d[[3]]
# intersect(a, b);intersect(a, c);intersect(b, c)
# any(rowSums(a==c) == ncol(a)) | any(rowSums(a==b) == ncol(a)) | any(rowSums(b==c) == ncol(a))

plot_set = function(sets, set_nr, three){
  sets["Choice Set"] = NULL
  if(three){
    
  
    
  }else{
    
  }
  
}

elements = a[1,]
colnames(elements) = paste(colnames(elements), ":")
ggplot() +  geom_text(aes(x = 0, y = 1:ncol(elements), 
                        label = unlist(elements[1,]), hjust = 0,fontface = "bold"),size =10) +
            geom_text(aes(x = -0.5, y = 1:ncol(elements), 
                        label = colnames(elements)), hjust = 0) +
  theme_bw()+ 
  scale_y_continuous(breaks = NULL, limits = c(0,ncol(elements)+1)) + scale_x_continuous(breaks = NULL, limits = c(-1,1)) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

 