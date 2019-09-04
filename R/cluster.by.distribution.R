#' @title cluster.by.distribution
#' @description The objective of clustering is to find how similar the distribution of data is, among different groups.
#' @param x data frame with a grouping variable, and at least one numeric variable to be used for comparing distribution
#' @param group_column column name of x which defines the group
#' @param target_columns column name of x which will be used to compare distribution
#' @param by bin size to be used in binned.dist.matrix, Default: 0.1
#' @param defined.k if not given, number of medoids is calculated in get.clara.object, Default: numeric(0)
#' @param clara.metric metric parameter given to cluster::clara Default: 'euclidean'
#' @param clusgap.B metric parameter given to cluster::clusGap, Default: 400
#' @return list
#' @details list(data=data.frame(),clara=function(){},silinfo=function(){},clustering=function(){})
#' @examples
#' \dontrun{
#' if(interactive()){
#'  cluster.by.distribution(random.tibble,'label',c('rand'))
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{group_by_all}},\code{\link[dplyr]{group_keys}},\code{\link[dplyr]{mutate}}
#' @rdname cluster.by.distribution
#' @export
#' @importFrom dplyr group_by_at group_split mutate
cluster.by.distribution <-
function(
  x,
  group_column,
  target_columns,
  by=0.1,
  defined.k = numeric(0),
  clara.metric='euclidean',
  clusgap.B = 400
){
  target.matrix <- binned.dist.matrix(x,group_column,target_columns,by=by)
  clara.object <- get.clara.object(
    is.k.predefined = (defined.k %>% length > 0)
    ,target.matrix
    ,defined.k = defined.k
    ,clara.metric=clara.metric
    ,clusgap.B=clusgap.B
  )

  labeled.x <- x %>%  dplyr::group_by_at(group_column) %>% dplyr::group_split() %>%
    lapply(function(z){
      z %>% dplyr::mutate(
        clustered = clara.object$clustering() %>% .[which(names(.) == unique(z[[group_column]]))]
      )
    }) %>% Reduce(rbind,.)

  list(
    list(data=labeled.x)
    ,clara.object
  ) %>% unlist(recursive=F)
}