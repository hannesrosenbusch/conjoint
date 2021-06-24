set.seed(42)
library(conjoint) #version 1.41
library(dplyr)
library(ggplot2)
price = as.factor(c('300$', '500$', '700$'))
RAM = as.factor(c('4GB', '6GB', '8GB'))
weight = as.factor(c('1KG', '2KG', '3KG'))
display = as.factor(c('14inch', '16inch', '18inch'))
#self_description = as.factor(c('1', '2'))
# d = cbind.data.frame(attractiveness,
#                  expression,
#                  social_identity,
#                  self_description)

experiment<-expand.grid(
  price,
  RAM,
  weight,
  display
  )

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

 