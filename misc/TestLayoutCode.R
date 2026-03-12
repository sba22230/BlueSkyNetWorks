library(tictoc)
tic()
res <- rxExec(
  layout_exec,
  graph = g,
  directed = TRUE,
  seed = 22230,
  layout_type = rxElemArg(c(
    "drl"
    #,"drl_fast"
    #,"fr"
    #,"graphopt"
    #,"lgl"
    #,"kk"
  )),
  execObjects = c("connStr","layout_exec")
)
toc()
tic()
res1 <- rxExec(
  layout_exec,
  graph = g,
  directed = TRUE,
  seed = 22230,
  layout_type = rxElemArg(c(
    #"drl"
    "drl_fast"
    #,"fr"
    #,"graphopt"
    #,"lgl"
    #,"kk"
  )),
  execObjects = c("connStr","layout_exec")
)
toc()
tic()
res2 <- rxExec(
  layout_exec,
  graph = g,
  directed = TRUE,
  seed = 22230,
  layout_type = rxElemArg(c(
    #"drl"
    #,"drl_fast"
    "fr"
    #,"graphopt"
    #,"lgl"
    #,"kk"
  )),
  execObjects = c("connStr","layout_exec")
)
toc()
tic()
res3 <- rxExec(
  layout_exec,
  graph = g,
  directed = TRUE,
  seed = 22230,
  layout_type = rxElemArg(c(
    #"drl"
    #,"drl_fast"
    #,"fr"
    "graphopt"
    #,"lgl"
    #,"kk"
  )),
  execObjects = c("connStr","layout_exec")
)
toc()
tic()
res4 <- rxExec(
  layout_exec,
  graph = g,
  directed = TRUE,
  seed = 22230,
  layout_type = rxElemArg(c(
    #"drl"
    #,"drl_fast"
    #,"fr"
    #,"graphopt"
    "lgl"
    #,"kk"
  )),
  execObjects = c("connStr","layout_exec")
)
toc()
tic()
res5 <- rxExec(
  layout_exec,
  graph = g,
  directed = TRUE,
  seed = 22230,
  layout_type = rxElemArg(c(
    #"drl"
    #,"drl_fast"
    #,"fr"
    #,"graphopt"
    #,"lgl"
    "kk"
  )),
  execObjects = c("connStr","layout_exec")
)
toc()
