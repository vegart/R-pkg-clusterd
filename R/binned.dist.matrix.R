#' @title binned.dist.matrix
#' @description historical binning method to obtain distance vector for different group value
#' @param x data frame with a grouping variable, and at least one numeric variable to be used for comparing distribution
#' @param group_column column name of x which defines the group
#' @param target_columns column name of x which will be used to compare distribution
#' @param by bin size to be used in binned.dist.matrix, Default: 0.1
#' @return matrix(nrow=length(unique(x[[group_column]])),ncol=as.integer(length(1/by)) + 1)
#' @details matrix element value contains proportion of data contained in the bin region, and is scaled afterwards. NA and Inf values are eliminated.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  binned.dist.matrix(iris,'Species','Sepal.Length')
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{tally_}},\code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{filter_all}},\code{\link[dplyr]{all_vars}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}}
#' @rdname binned.dist.matrix
#' @export
#' @importFrom dplyr select_ mutate_all mutate filter_if all_vars filter select as_tibble everything
binned.dist.matrix <-
function(
  x,
  group_column,
  target_columns,
  by=0.1
){
  target.label <- x[[group_column]]

  stopifnot(
    inherits(x,"data.frame"),
    !is.null(target.label)
  )

  target.attribute.names <- strsplit(target_columns,split = ",")[[1]]
  target.attribute.text <- paste0(
    "c("
    ,target.attribute.names %>% sapply(function(z){paste0("'",z,"'")}) %>% paste0(collapse=",")
    ,")"
  )

  is.multi.attribute.selected <- tryCatch({
    x <- x %>%
      dplyr::select_(
        target.attribute.text
      ) %>%
      dplyr::mutate_all(as.numeric) %>%
      dplyr::mutate(
        target.label = target.label
      )
    TRUE
  },error=function(e){FALSE}
  )

  stopifnot(is.multi.attribute.selected)

  x.rm.na <-  x %>%
    dplyr::filter_if(is.numeric,dplyr::all_vars(!is.na(.) & !is.infinite(.)))

  if(nrow(x) != nrow(x.rm.na)){
    warning(paste0(nrow(x) - nrow(x.rm.na)," OUT OF ",nrow(x)," ROWS WITH ANY NA OR INF [",target_columns,"] VALUES WERE ELIMINATED"))
    x <- x.rm.na
  }
  ###########################################################################################
  ## DEDUCE DISTANCE VECTOR BASED ON PROPORTION OF VALUES
  ###########################################################################################
  quantile.dist.vector <- function(
    x,
    by=0.1
  ){
    stopifnot(by <= 1)

    get.proportioned.length <- function(target,lower,upper){
      if(target %>% length == 0)
      {
        NA
      }else
      {
        if(lower==upper)
        {
          0
        }else
        {
          (target[which(target >= lower & target < upper)] %>% length) * (upper - lower)
        }
      }
    }

    target.vector <- x %>% as.numeric

    all.quantiles <- seq(from=by,to=1,by=by)

    all.quantiles %>% sapply(function(each.quantile){

      quant.value.lower <- quantile(target.vector,probs=max(each.quantile-by,0),type=1,na.rm = TRUE)
      quant.value.upper <- quantile(target.vector,probs=each.quantile,type=1,na.rm = TRUE)

      quant.proportion <- get.proportioned.length(target.vector,quant.value.lower,quant.value.upper) / (target.vector %>% length)

      quant.proportion *100
    }) %>% scale(center = FALSE) %>% as.numeric
  }

  target.matrix <- target.attribute.names %>% lapply(function(v){
    unique(target.label) %>% lapply(function(z){
      res <- x %>% dplyr::filter(target.label==z) %>% dplyr::select(v) %>% unlist %>% quantile.dist.vector(by=by)
    }) %>%
    Reduce(rbind,.)
  }) %>% Reduce(cbind,.)

  rownames(target.matrix) <- unique(target.label)

  target.matrix <- target.matrix[
    target.matrix %>% dplyr::as_tibble(.) %>%
      dplyr::mutate(
        row.names = rownames(target.matrix)
      ) %>%
      dplyr::select(row.names,dplyr::everything()) %>%
      apply(.,1,function(z){
        !is.element(TRUE,is.na(z))
      }) %>% which,]

  zero.columns.index <- which(target.matrix %>% colSums() == 0)

  if(zero.columns.index %>% length > 0){
    warning(
      paste0(zero.columns.index %>% length," ZERO-COLUMN INDICES WERE ELIMINATED : ",paste0(zero.columns.index,collapse = ","))
    )
    target.matrix <- target.matrix[,-zero.columns.index]
  }
  target.matrix
}
