% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster.by.distribution.R
\name{cluster.by.distribution}
\alias{cluster.by.distribution}
\title{cluster.by.distribution}
\usage{
cluster.by.distribution(x, group_column, target_columns, by = 0.1,
  defined.k = numeric(0), clara.metric = "euclidean",
  clusgap.B = 400)
}
\arguments{
\item{x}{data frame with a grouping variable, and at least one numeric variable to be used for comparing distribution}

\item{group_column}{column name of x which defines the group}

\item{target_columns}{column name of x which will be used to compare distribution}

\item{by}{bin size to be used in binned.dist.matrix, Default: 0.1}

\item{defined.k}{if not given, number of medoids is calculated in get.clara.object, Default: numeric(0)}

\item{clara.metric}{metric parameter given to cluster::clara Default: 'euclidean'}

\item{clusgap.B}{metric parameter given to cluster::clusGap, Default: 400}
}
\value{
list
}
\description{
The objective of clustering is to find how similar the distribution of data is, among different groups.
}
\details{
list(data=data.frame(),clara=function(){},silinfo=function(){},clustering=function(){})
}
\examples{
\dontrun{
if(interactive()){
 cluster.by.distribution(random.tibble,'label',c('rand'))
 }
}
}
\seealso{
\code{\link[dplyr]{group_by_all}},\code{\link[dplyr]{group_keys}},\code{\link[dplyr]{mutate}}
}
