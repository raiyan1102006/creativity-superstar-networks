require("stm")
require("parallel")
require(gtools)
require(stringr)

# params
k_list = c(20,28,30,40,50,60)
frex_weight_list = c(0.25,0.50,0.75)

for (k in k_list){
  # load data
  loadfilename <- paste("data/model_k_",toString(k),".rda", sep="")
  load(loadfilename) # loads the 'mod' variable for the given k
  
  for (frex_weight in frex_weight_list){
    # extract concepts
    frexconcepts <- labelTopics(mod, topics = 1:k, n = 500, frexweight = frex_weight)
    a <- frexconcepts$frex
    
    # save
    filename <- paste("data/frexconcepts_k",toString(k),"_frex",toString(frex_weight),".rda", sep="")
    save(a, file = filename)
  }
}

