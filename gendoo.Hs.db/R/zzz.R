datacache <- new.env(hash = TRUE, parent = emptyenv())

## database
gendoo.Hs          <- function() get("dbshow",   envir = datacache)()
gendoo.Hs_dbconn   <- function() get("dbconn",   envir = datacache)
gendoo.Hs_dbfile   <- function() get("dbfile",   envir = datacache)
gendoo.Hs_dbschema <- function() get("dbschema", envir = datacache)()
gendoo.Hs_dbInfo   <- function() get("dbInfo",   envir = datacache)()

.onLoad <- function(libname, pkgname)
{
  ##
  ## DB connection
  ##
  require("methods", quietly = TRUE)

  ## dbfile
  dbfile <- system.file("extdata", "gendoo.Hs.db.sqlite",
                        package = pkgname, lib.loc = libname)
  assign("dbfile", dbfile, envir = datacache)

  ## dbconn 
  driver <- dbDriver("SQLite")
  db <- dbfile
  dbconn <- dbConnect(driver, db)
  assign("dbconn", dbconn, envir = datacache)

  ## dbshow
  dbshow <- function() {
    cat("Load database.\n")
  }
  assign("dbshow", dbshow, envir = datacache)

  ## dbschema
  dbschema <- function() cat(dbGetQuery(dbconn, "SELECT * FROM sqlite_master;")$sql)
  assign("dbschema", dbschema, envir = datacache)

  ## dbInfo
  dbInfo <- function() dbGetQuery(dbconn, "SELECT * FROM METADATA;")
  assign("dbInfo", dbInfo, envir = datacache)

  ns <- asNamespace(pkgname)

  ##
  ## Definition of Classes
  ##

  ## Make class
  class.template <- 'setClass("CLASSNAME",
    representation(name = "character"),
    prototype(name = "CLASSNAME")
  )'
  class.sub.name <- c("Gene", "MeSHA", "MeSHB", "MeSHC", "MeSHD", "MeSHG", "MeSHS")
  sapply(class.sub.name, function(class.sub.name) {
    class.name <- paste0("Gendoo", class.sub.name)
    new.class  <- gsub("CLASSNAME", class.name, class.template)
    eval(parse(text = new.class))
  })

  ##
  ## Definition of Methods
  ##
  
  ## GendooGene
  # cols
  setMethod("cols",
    "GendooGene",
    function(x) {
      return(
        c("gene_id", "gene_symbol", "gene_description",
          "gene_synonyms", "gene_type")
      )
    }
  )
  # keytypes
  setMethod("keytypes",
    "GendooGene",
    function(x) {
      return(
        c("gene_id", "gene_symbol", "gene_description",
          "gene_synonyms", "gene_type")
      )
    }
  )
  # keys
  setMethod("keys",
    "GendooGene",
    function(x, keytype){
      query <- paste0("SELECT ", keytype, " FROM genes;")
      k     <- dbGetQuery(gendoo.Hs_dbconn(), query)
      return(k)
    }
  )
  # select
  setMethod("select", "GendooGene",
    function(x, keys, cols, keytype) {
      if (length(cols) > 1) {
        c <- cols[1]
	for (i in 2:(length(cols))){
  	  c <- paste(c, cols[i], sep = ",")
	}
      } else {
        c <- cols
      }
      
      keys <- paste0('"', keys, '"')
      ke   <- paste(keytype, keys, sep ="=")
      
      kee <- c()
      if (length(ke) > 1)  {
        if(length(ke) >= 1000) {
          ke_loc <- div(1:length(ke), ceiling(length(keys)/500))
          for(j in 1:ceiling(length(keys)/500)){
            kee[j] <- paste(ke[ke_loc[[j]]], sep="",collapse=" OR ")
          }
        } else {          
          kee <- paste(ke, sep="", collapse=" OR ")
        }          
      } else {
        kee <- ke
      }

      # SQL
      kk <- c()
      for(i in 1:length(kee)){
        query <- paste0("SELECT ", c, " FROM genes WHERE ", kee[i], ";")
        k <- dbGetQuery(gendoo.Hs_dbconn(), query)        
        kk <- rbind(kk,k)
      }
      return(unique(kk))
    }
  )

  ## MeSHx
  # vector dividing function
  div <- function(x,d=1) {
    y <- list()
    delta <- ceiling(length(x) / d)
    for(i in 1:d){
      y[[i]] <- as.vector(na.omit(x[((i-1)*delta+1):(i*delta)]))
    }
    return(y)
  }
  # cols
  method.template <- 'setMethod("cols",
    "CLASSNAME",
    function(x) { return(c("gene_id", "mesh")) }
  )'
  class.sub.name <- c("MeSHA", "MeSHB", "MeSHC", "MeSHD", "MeSHG", "MeSHS")
  sapply(class.sub.name, function(class.sub.name) {
    class.name <- paste0("Gendoo", class.sub.name)
    new.method <- gsub("CLASSNAME", class.name, method.template)
    eval(parse(text = new.method))
  })
  # keytypes
  method.template <- 'setMethod("keytypes",
    "CLASSNAME",
    function(x) { return(c("gene_id", "mesh")) }
  )'
  class.sub.name <- c("MeSHA", "MeSHB", "MeSHC", "MeSHD", "MeSHG", "MeSHS")
  sapply(class.sub.name, function(class.sub.name) {
    class.name <- paste0("Gendoo", class.sub.name)
    new.method <- gsub("CLASSNAME", class.name, method.template)
    eval(parse(text = new.method))
  })
  # keys
  method.template <- 'setMethod("keys",
    "CLASSNAME",
    function(x, keytype){
      query <- paste0("SELECT ", keytype, " FROM CLASSSUBNAME;")
      k     <- dbGetQuery(gendoo.Hs_dbconn(), query)
      return(k)
    }
  )'
  class.sub.name <- c("MeSHA", "MeSHB", "MeSHC", "MeSHD", "MeSHG", "MeSHS")
  sapply(class.sub.name, function(class.sub.name) {
    class.name <- paste0("Gendoo", class.sub.name)
    new.method <- gsub("CLASSNAME",    class.name,     method.template)
    new.method <- gsub("CLASSSUBNAME", class.sub.name, new.method)
    eval(parse(text = new.method))
  })
  # select
  method.template <- 'setMethod("select", "CLASSNAME",
    function(x, keys, cols, keytype) {
      if (length(cols) > 1) {
        c <- cols[1]
	for (i in 2:(length(cols))){
  	  c <- paste(c, cols[i], sep = ",")
	}
      } else {
        c <- cols
      }
      keys <- paste0(\'"\', keys, \'"\')
      ke <- paste(keytype, keys, sep ="=")
      if (length(ke) > 1)  {
        kee <- ke[1]
	for (i in 2:(length(ke))){
  	  kee <- paste(kee, ke[i], sep = " OR ")
	}
    } else{
      kee <- ke
    }
      query <- paste0("SELECT ", c, " FROM CLASSSUBNAME WHERE ", kee)
      k <- dbGetQuery(gendoo.Hs_dbconn(), query)
      return(k)
    }
  )'
  class.sub.name <- c("MeSHA", "MeSHB", "MeSHC", "MeSHD", "MeSHG", "MeSHS")
  sapply(class.sub.name, function(class.sub.name) {
    class.name <- paste0("Gendoo", class.sub.name)
    new.method <- gsub("CLASSNAME",    class.name,     method.template)
    new.method <- gsub("CLASSSUBNAME", class.sub.name, new.method)
    eval(parse(text = new.method))
  })
  
  ## Export class
  export.template <- '
    CLASSNAME <- new("CLASSNAME")
    assign("CLASSNAME", CLASSNAME, envir = ns)
    namespaceExport(ns, "CLASSNAME")'
  class.sub.name <- c("Gene", "MeSHA", "MeSHB", "MeSHC", "MeSHD", "MeSHG", "MeSHS")
  sapply(class.sub.name, function(class.sub.name) {
    class.name <- paste0("Gendoo", class.sub.name)
    new.export <- gsub("CLASSNAME", class.name, export.template)
    eval(parse(text = new.export))
  })
  
}

.onUnload <- function(libpath)
{
  dbDisconnect(gendoo.Hs_dbconn())
}
