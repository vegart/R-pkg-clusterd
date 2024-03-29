`%>%` <-
function (lhs, rhs)
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
