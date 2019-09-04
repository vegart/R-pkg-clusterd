#' @title random.tibble
#' @description ROWS GENERATED FROM UNIFORM,NORMAL,CHI-SQUARE DISTRIBUTION WITH DIFFERING PARAMETERS COMBINED AS SINGLE DATAFRAME.
#' @format A data frame with 52500 rows and 2 variables:
#' \describe{
#'   \item{\code{label}}{integer grouping label}
#'   \item{\code{rand}}{double value genrated from runif,rnorm, or rchisq}
#'}
#' @details NULL
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