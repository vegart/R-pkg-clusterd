#' @title get.clara.object
#' @description obtain clara object defined in cluster::clara using distance matrix obtained from binned.dist.matrix(...).
#' @param is.k.predefined if TRUE, uses defined.k as the number of medoids for clustering
#' @param target.matrix  distance matrix obtained from binned.dist.matrix(...)
#' @param defined.k if numeric(0) optimal number of medoids are calculated using cluster::clusGap and cluster::maxSE, Default: numeric(0)
#' @param clara.metric metric parameter given to cluster::clara Default: 'euclidean'
#' @param clusgap.B metric parameter given to cluster::clusGap, Default: 400
#' @return clara
#' @details list(clara,silinfo,clustering)
#' @examples
#' \dontrun{
#' if(interactive()){
#'  get.clara.object(F,binned.dist.matrix(iris,'Species','Sepal.Length'))
#'  }
#' }
#' @seealso
#'  \code{\link[cluster]{clusGap}},\code{\link[cluster]{clara}}
#'  \code{\link[dplyr]{reexports}},\code{\link[dplyr]{arrange}}
#' @rdname get.clara.object
#' @export
#' @importFrom cluster clusGap clara maxSE
#' @importFrom dplyr as_tibble arrange
get.clara.object <-
function(
  is.k.predefined
  ,target.matrix
  ,defined.k = numeric(0)
  ,clara.metric='euclidean'
  ,clusgap.B = 400
){
  tryCatch({
    target.k <- 0

    if(is.k.predefined)
    {
      warning(paste0('WILL USE PREDEFINED NUMBER OF K FROM defined.k PARAM=',paste0(defined.k)))
      target.k <- defined.k
    }
    else
    {
      target.gap <- cluster::clusGap(target.matrix,FUNcluster= cluster::clara, K.max=min(c(nrow(target.matrix)-1,20)), B=clusgap.B)
      target.k <- cluster::maxSE(target.gap$Tab[, "gap"], target.gap$Tab[, "SE.sim"], method="Tibs2001SEmax")

      if(target.k < 2){
        warning("UNABLE TO DETERMINE OPTIMAL NUMBER OF CLUSTERS. WILL RESORT TO CREATING 2 CLUSTERS.")
        target.k <- 2
      }
    }

    target.clara <- target.matrix %>% cluster::clara(.,metric = clara.metric,k=target.k)

    target.silinfo <- (target.clara %>% summary) $silinfo $ widths %>%
      cbind(row_names=rownames(.)) %>%
      dplyr::as_tibble(.) %>%
      dplyr::arrange(cluster,desc(sil_width))

    target.clustering  <- target.clara $ clustering

    list(
      clara = function(){target.clara}
      ,silinfo = function(){target.silinfo}
      ,clustering = function(){target.clustering}
    )
  })
}
