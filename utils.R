adicionar_arte_horst <- function(img, ext = ".png", legenda_complementar = ""){
  cat(paste0(
"![](https://github.com/allisonhorst/stats-illustrations/blob/main/rstats-artwork/", img, ext, "?raw=true){fig-align='center'}

::: footer
Arte por [Allison Horst](https://mobile.twitter.com/allison_horst)", legenda_complementar, "
:::
"
  ))
}
