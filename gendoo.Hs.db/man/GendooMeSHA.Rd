\name{GendooMeSHA}
\alias{GendooMeSHA}
\alias{GendooMeSHB}
\alias{GendooMeSHC}
\alias{GendooMeSHD}
\alias{GendooMeSHG}
\alias{GendooMeSHS}
\title{The GendooMeSH for gendoo.Hs}
\description{
  GendooMeSH is an R object that contains mapping table of Entrez Gene
  ID to MeSH ID.
}
\details{
  GendooMeSH provides mapping table for Entrez Gene ID to MeSH ID.
  You can retrive these data from the object using by select method.
  See examples.

  A: Anatomy
  B: Organisms
  C: Diseases
  D: Drugs and chemicals
  G: Biological phenomena
  S: Substance Names
}
\examples{
  keytypes(GendooMeSHA)
  cols(GendooMeSHA)

  my.keytype <- c("gene_id")
  gene_ids <- keys(GendooMeSHA, keytype = my.keytype)

  ## search
  my.keys <- c(1, 2, 3)
  my.cols <- c("gene_id", "mesh")
  my.mesh <- select(GendooMeSHA, keys = my.keys, cols = my.cols, keytype
  = my.keytype)
  head(my.mesh)
}
\keyword{datasets}
