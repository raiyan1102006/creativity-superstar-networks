require("stm")
require("parallel")
require(gtools)
require(stringr)

# Load preprocessed data
out <- readRDS(file = "data/obj_all_abstracts_year.rds")

# sweep over k
klist <- c(20,28,30,40,50,60)

for (k in klist) {
  filename <- paste("data/model_k_", toString(k), ".rda", sep="")
  mod <- stm(documents = out$documents, vocab = out$vocab, K = k,
                 prevalence =~ s(year),
                 data = out$meta, init.type = "Spectral")
  save(mod, file=filename)
}


# Save wordclouds
load("data/model_k_40.rda") # loads the 'mod' variable for k=40

for (topicid in 1:40){
  filename <- paste("plots/wordcloud40/topic_",toString(topicid),".pdf", sep="")
  pdf(file=filename)
  cloud(mod, topic = topicid)
  dev.off()
}



#plot(mod, type = "summary", xlim = c(0, 0.3))