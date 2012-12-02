\name{GendooGene}
\alias{GendooGene}
\title{The GendooGene for gendoo.Hs}
\description{
  GendooGene is an R object that contains Entrez Gene ID and its annotations.
}
\details{
  GendooGene provides Entrez Gene IDs with its annotations, 
  Gene ID, symbol, description, synonyms and gene type.
  You can retrive these data from the object using by select method.
  See examples.
}
\examples{
  keytypes(GendooGene)
  cols(GendooGene)

  my.keytype <- c("gene_id")
  gene_ids <- keys(GendooGene, keytype = my.keytype)

  ## search
  my.keys <- c(1)
  my.cols <- c("gene_id", "gene_symbol", "gene_description",
              "gene_synonyms", "gene_type")
  select(GendooGene, keys = my.keys, cols = my.cols, keytype = my.keytype)
}
\keyword{datasets}
