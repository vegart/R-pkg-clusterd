{
  require(ggpubr)
  require(clusterd)
  require(dplyr)

  rm(.Random.seed)
  dev.off()

  list(
    len = 100000,
    factor.len = 10
  ) %>%
    {
      tibble(
        val=rnorm(n = .[['len']],0,1),
        label=sample(paste0('label_',sample(seq_len(10),size = .[['factor.len']],replace = T)),size = .[['len']],replace = T)
      )
    }
} %>%
  mutate(label = as.factor(label)) %>%
  clusterd::binned.dist.matrix(group_column ='label',target_columns = 'val') %>%
  t %>% as_tibble %>%
  mapply(FUN=function(z,each.name){
    tibble(value=z,label=each.name) %>%
      mutate(
        rnum = seq_len(nrow(.))
      )
  },names(.),SIMPLIFY = F) %>%
  Reduce(rbind,.) %>%
  clusterd::cluster.by.distribution(
    group_column = 'label',
    target_columns = c('value'),
    by=0.01
  ) %>%
  {
    require(factoextra)
    require(gridExtra)

    clara.p <- fviz_cluster(.$clara())
    line.p <- ggline(.$data,x='rnum',y='value',color = 'label',fill='label')
    dense.p <- ggdensity(.$data,x='value',color='label',fill='label')

    grid.arrange(grobs=list(clara.p,line.p,dense.p))
    .
  }


