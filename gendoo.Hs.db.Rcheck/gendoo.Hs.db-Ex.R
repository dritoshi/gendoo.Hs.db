pkgname <- "gendoo.Hs.db"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('gendoo.Hs.db')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("GendooGene")
### * GendooGene

flush(stderr()); flush(stdout())

### Name: GendooGene
### Title: The GendooGene for gendoo.Hs
### Aliases: GendooGene
### Keywords: datasets

### ** Examples

  keytypes(GendooGene)
  cols(GendooGene)

  my.keytype <- c("gene_id")
  gene_ids <- keys(GendooGene, keytype = my.keytype)

  ## search
  my.keys <- c(1)
  my.cols <- c("gene_id", "gene_symbol", "gene_description",
              "gene_synonyms", "gene_type")
  my.genes <- select(GendooGene, keys = my.keys, cols = my.cols, keytype
  = my.keytype)
  head(my.genes)



cleanEx()
nameEx("GendooMeSHA")
### * GendooMeSHA

flush(stderr()); flush(stdout())

### Name: GendooMeSHA
### Title: The GendooMeSH for gendoo.Hs
### Aliases: GendooMeSHA GendooMeSHB GendooMeSHC GendooMeSHD GendooMeSHG
###   GendooMeSHS
### Keywords: datasets

### ** Examples

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



cleanEx()
nameEx("gendoo.HsBASE")
### * gendoo.HsBASE

flush(stderr()); flush(stdout())

### Name: gendoo.Hs.db
### Title: Gendoo Human Database package
### Aliases: gendoo.Hs.db gendoo.Hs
### Keywords: datasets

### ** Examples

  ls("package:gendoo.Hs.db")



cleanEx()
nameEx("gendoo.Hs_dbconn")
### * gendoo.Hs_dbconn

flush(stderr()); flush(stdout())

### Name: gendoo.Hs_dbconn
### Title: Collect information about the package annotation DB
### Aliases: gendoo.Hs gendoo.Hs_dbconn gendoo.Hs_dbfile gendoo.Hs_dbschema
###   gendoo.Hs_dbInfo
### Keywords: utilities datasets

### ** Examples

  ## Count the number of rows in the "metadata" table:
  dbGetQuery(gendoo.Hs_dbconn(), "SELECT COUNT(*) FROM metadata")

  ## The connection object returned by gendoo.Hs_dbconn() was
  ## created with:
  dbConnect(SQLite(), dbname=gendoo.Hs_dbfile(), cache_size=64000,
  synchronous=0)

  gendoo.Hs()

  gendoo.Hs_dbschema()

  gendoo.Hs_dbInfo()

  gendoo.Hs_dbfile()



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
