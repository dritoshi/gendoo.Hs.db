# check select method for GendooGene class"

test_select <- function() {
  my.keys    <- c(1, 2, 3)
  my.cols    <- c("_id", "gene_id")
  my.keytype <- c("_id") 
  
  results <- select(GendooGene, keys = my.keys, cols = my.cols, keytype = my.keytype)

  checkEquals(results[,1], c(1, 2, 3))
  checkEquals(results[,2], c("A1BG", "A2M", "A2MP"))
}
