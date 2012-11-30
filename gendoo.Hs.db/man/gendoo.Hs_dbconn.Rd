\name{gendoo.Hs_dbconn}

\alias{gendoo.Hs}
\alias{gendoo.Hs_dbconn}
\alias{gendoo.Hs_dbfile}
\alias{gendoo.Hs_dbschema}
\alias{gendoo.Hs_dbInfo}

\title{Collect information about the package annotation DB}

\description{
  Some convenience functions for getting a connection object to (or collecting
  information about) the package annotation DB.
}

\usage{
  gendoo.Hs()
  gendoo.Hs_dbconn()
  gendoo.Hs_dbfile()
  gendoo.Hs_dbschema()
  gendoo.Hs_dbInfo()
}

\details{
  \code{gendoo.Hs} returns the mata data (character string) of the
  package annotation DB.
  
  \code{gendoo.Hs_dbconn} returns a connection object to the
  package annotation DB.  IMPORTANT: Don't call
  \code{\link[DBI:dbConnect]{dbDisconnect}} on the connection object
  returned by \code{gendoo.Hs_dbconn}.

  \code{gendoo.Hs_dbfile} returns the path (character string) to the
  package annotation DB (this is an SQLite file).

  \code{gendoo.Hs_dbschema} prints the schema definition of the
  package annotation DB.

  \code{gendoo.Hs_dbInfo} prints other information about the package
  annotation DB.
}

\value{
  \code{gendoo.Hs}: none (invisible \code{NULL}).

  \code{gendoo.Hs_dbconn}: a DBIConnection object representing an
  open connection to the package annotation DB.

  \code{gendoo.Hs_dbfile}: a character string with the path to the
  package annotation DB.

  \code{gendoo.Hs_dbschema}: none (invisible \code{NULL}).

  \code{gendoo.Hs_dbInfo}: none (invisible \code{NULL}).
}

\seealso{
  \code{\link[DBI:dbSendQuery]{dbGetQuery}},
  \code{\link[DBI:dbConnect]{dbConnect}},
  \code{\link[AnnotationDbi:AnnDbObj-class]{dbshow}},
  \code{\link[AnnotationDbi:AnnDbObj-class]{dbconn}},
  \code{\link[AnnotationDbi:AnnDbObj-class]{dbfile}},
  \code{\link[AnnotationDbi:AnnDbObj-class]{dbschema}},
  \code{\link[AnnotationDbi:AnnDbObj-class]{dbInfo}}
}

\examples{
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
}

\keyword{utilities}
\keyword{datasets}
