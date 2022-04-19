require("stm")
require("parallel")
require(gtools)
require(stringr)

# Load preprocessed data
out <- readRDS(file = "data/obj_all_abstracts_year.rds")

# Run searchK
klist = c(5,7,10,12,15,18,20,23,25,28,30,33,35,40,45,50,60,70,80,90,100)
storage <- searchK(documents=out$documents, vocab=out$vocab, K = klist, 
                   prevalence =~ s(year),
                   data = out$meta)

# save results
# storage_5_10_20.rda -> klist = c(5,10,20)
# storage_all.rda -> klist = c(5,7,10,12,15,18,20,23,25,28,30,33,35,40,45,50,60,70,80,90,100)
save(storage, file="data/storage_all.rda") 
#load("data/storage_all.rda") # how to load it later

# plot results
my_plot<-function(x, ...){
  oldpar <- par(no.readonly=TRUE)
  g <- x$results
  par(mfrow=c(2,2),mar=c(4,4,4,4),oma=c(2,2,2,2))
  
  if(!is.null(g$semcoh)){
    plot(g$K,g$semcoh,type="p", main="Semantic Coherence", xlab="Number of Topics (K)", ylab="Semantic Coherence")
    lines(g$K,g$semcoh,lty=1,col=1 ) 
  }
  
  plot(g$K,g$exclus,type="p", main="Exclusivity", xlab="Number of Topics (K)", ylab="Exclusivity")
  lines(g$K,g$exclus,lty=1,col=1 )  
  
  plot(g$K,g$heldout,type="p", main="Held-Out Likelihood", xlab="Number of Topics (K)", ylab="Held-Out Likelihood")
  lines(g$K,g$heldout,lty=1,col=1)
  
  plot(g$K,g$residual,type="p", main="Residuals", xlab="Number of Topics (K)", ylab="Residuals")
  lines(g$K,g$residual,lty=1,col=1 )

  
  title("Diagnostic Values by Number of Topics", outer=TRUE)  
  par(oldpar)
}

my_plot(storage)

