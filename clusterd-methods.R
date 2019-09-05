####################################################################################################
## DATA
####################################################################################################
random.tibble <- (
  function(){
    runif.10000_1_100 <- runif(n = 10000,min = 1,max=30)
    runif.5000_4_400 <- runif(n = 5000,min = 5,max=10)
    runif.2500_3_300 <- runif(n = 2500,min = 3,max=20)

    rnorm.10000_0_1 <- rnorm(n = 10000,mean = 0,sd=1)
    rnorm.5000_3_3 <- rnorm(n = 5000,mean = 3,sd=3)
    rnorm.2500_5_5 <- rnorm(n = 2500,mean = 5,sd=5)

    rchisq.10000_0_2 <- rchisq(10000, df = 0, ncp = 2.)
    rchisq.5000_3_4 <- rchisq(5000, df = 3, ncp = 4.)
    rchisq.2500_1_1 <- rchisq(2500, df = 1, ncp = 1.)

    random.list <- list(
      runif.element= list(
        data.frame(label="runif.10000_1_100",rand=runif.10000_1_100),
        data.frame(label="runif.5000_4_400",rand=runif.5000_4_400),
        data.frame(label="runif.2500_3_300",rand=runif.2500_3_300)
      ),
      rnorm.element = list(
        data.frame(label="rnorm.10000_0_1",rand=rnorm.10000_0_1),
        data.frame(label="rnorm.5000_3_3",rand=rnorm.5000_3_3),
        data.frame(label="rnorm.2500_5_5",rand=rnorm.2500_5_5)
      ),
      rchisq.element = list(
        data.frame(label="rchisq.10000_0_2",rand=rchisq.10000_0_2),
        data.frame(label="rchisq.5000_3_4",rand=rchisq.5000_3_4),
        data.frame(label="rchisq.2500_1_1",rand=rchisq.2500_1_1)
      )
    )

    random.tibble <- Reduce(rbind,unlist(random.list,recursive = FALSE))
    random.tibble
  }
)()
####################################################################################################
## SUGGESTS
# magrittr(>=1.5)
####################################################################################################
# magrittr::`%>%`
####################################################################################################
## IMPORTS
####################################################################################################
###################################################
## cluster(>=2.0.9)
###################################################
# cluster::clusGap
# cluster::maxSE
# cluster::clara
###################################################
## dplyr(>=0.8.1)
###################################################
# dplyr::select_
# dplyr::mutate_all
# dplyr::mutate
# dplyr::filter_if
# dplyr::select
# dplyr::filter
# dplyr::as_tibble
# dplyr::arrange
# dplyr::group_by_at
# dplyr::group_split
# dplyr::all_vars
# dplyr::everything
####################################################################################################
`%>%` <- function (lhs, rhs)
{
  parent <- parent.frame()
  env <- new.env(parent = parent)

  split_chain <- getFromNamespace("split_chain", "magrittr")
  wrap_function <- getFromNamespace("wrap_function", "magrittr")
  freduce <- getFromNamespace("freduce", "magrittr")
  is_placeholder <- getFromNamespace("is_placeholder", "magrittr")
  is_compound_pipe <- getFromNamespace("is_compound_pipe", "magrittr")

  chain_parts <- split_chain(match.call(), env = env)
  pipes <- chain_parts[["pipes"]]
  rhss <- chain_parts[["rhss"]]
  lhs <- chain_parts[["lhs"]]
  env[["_function_list"]] <- lapply(1:length(rhss), function(i) wrap_function(rhss[[i]],
                                                                              pipes[[i]], parent))
  env[["_fseq"]] <- `class<-`(eval(quote(function(value) freduce(value,
                                                                 `_function_list`)), env, env), c("fseq", "function"))
  env[["freduce"]] <- freduce
  if (is_placeholder(lhs)) {
    env[["_fseq"]]
  }
  else {
    env[["_lhs"]] <- eval(lhs, parent, parent)
    result <- withVisible(eval(quote(`_fseq`(`_lhs`)), env,
                               env))
    if (is_compound_pipe(pipes[[1L]])) {
      eval(call("<-", lhs, result[["value"]]), parent,
           parent)
    }
    else {
      if (result[["visible"]])
        result[["value"]]
      else invisible(result[["value"]])
    }
  }
}
####################################################################################################
binned.dist.matrix <- function(
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

  target.attribute.names <- unlist(strsplit(target_columns,split = ","))
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
####################################################################################################
get.clara.object <- function(
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
####################################################################################################
cluster.by.distribution <- function(
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
####################################################################################################