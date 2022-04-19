require("stm")
require("parallel")
require(gtools)
require(stringr)


# Load data
data <- read.csv("data/all_abstracts_year.csv")
processed <- textProcessor(data$abstract, metadata = data)

#The default setting lower.thresh=1 in prepDocuments means that words which appear 
#in only one document will be dropped. 
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

# save in R format
saveRDS(out, file = "data/obj_all_abstracts_year.rds")

# save in python readable JSON format
library(jsonlite)
l = toJSON(out, pretty = TRUE, auto_unbox = TRUE)
write(l, "data/obj_all_abstracts_year.json")
