# check select method for GendooGene class"

test_select_gene <- function() {
  my.keys    <- c(1, 2, 3)
  my.cols    <- c("_id", "gene_id", "gene_symbol")
  my.keytype <- c("_id") 
  
  results <- select(GendooGene, keys = my.keys, cols = my.cols, keytype = my.keytype)

  checkEquals(results[,1], c(1, 2, 3))
  checkEquals(results[,2], c("1", "2", "3"))
  checkEquals(results[,3], c("A1BG", "A2M", "A2MP"))
}

test_select_mesha <- function() {
  my.keys    <- c(1)
  my.cols    <- c("gene_id", "mesh")
  my.keytype <- c("gene_id")

  results <- select(GendooMeSHA, keys = my.keys, cols = my.cols, keytype = my.keytype)

  checkEquals(ncol(results), 2)
  checkEquals(nrow(results), 16)
}
