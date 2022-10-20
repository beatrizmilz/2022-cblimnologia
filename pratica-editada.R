# https://rstudio.cloud/content/4773874
# https://beatrizmilz.github.io/2022-cblimnologia/
# https://beamilz.com/posts/2021-03-29-desvendando-erros/pt/


# IMPORTAÇÃO
# importar vários arquivos ------------------------------------------------
library(dplyr)

# dplyr::filter() # com :: nao é necessário usar library()
# stats::filter()





# usando import dataset: lembrar de copiar o código!
# library(readxl)
# dados_exemplo <- read_excel("dados-brutos/RelatorioQualidadeAguasSuperficiais_clorofila-a_2008-2012.xlsx")
# View(dados_exemplo)

# dica: use aspas e tab
# "dados-brutos/RelatorioQualidadeAguasSuperficiais_PT_2018-2022.xlsx"


# importando um arquivo
infoaguas_exemplo <- readxl::read_xlsx("dados-brutos/RelatorioQualidadeAguasSuperficiais_clorofila-a_2008-2012.xlsx")

# vendo as colunas
dplyr::glimpse(infoaguas_exemplo)

# descobrindo o caminho de todos os arquivos

fs::dir_ls("dados-brutos/")


arquivos <- fs::dir_ls("dados-brutos/", glob = "*.xlsx")




# precisamos aplicar a função readxl::read_xlsx para todos eles!

# usar purrr::map() !
# exemplo: 

vetor <- 1:5

# aplicar a funcao sqrt() em todos os valores de 'vetor'
purrr::map(vetor, sqrt)


temp <- purrr::map(arquivos, readxl::read_xlsx) |> 
  dplyr::bind_rows()

# pipe: %>% , |>
# 1:5 |> sum()


# usar para importar:
dados_brutos_infoaguas <- arquivos |> 
  purrr::map(readxl::read_xlsx) |> 
  dplyr::bind_rows()

dplyr::glimpse(dados_brutos_infoaguas)

# LIMPEZA
# renomear colunas --------------------------------------------------------

infoaguas_renomeado <- dados_brutos_infoaguas |> 
  janitor::clean_names()

dplyr::glimpse(infoaguas_renomeado)


# Criar novas colunas a partir dos dados ----------------------------------

# o valor, a variável que mais temos interesse, está em formato de texto!

infoaguas_renomeado$valor


infoaguas_renomeado$valor

# usar as.numeric não funciona.
as.numeric(infoaguas_renomeado$valor)


# isso custa 1.000,34 / 1000.34

# vamos conhecer as funções de parse!


readr::parse_number(infoaguas_renomeado$valor,
                    locale = readr::locale(decimal_mark = ",", grouping_mark = "."))




# exemplo:
readr::parse_number(infoaguas_renomeado$valor,
                    locale = readr::locale(decimal_mark = ",", grouping_mark = "."))


# estrutura de uma função
some_dois_numeros <- function(x, y){
  # o que eu quero que a funcao faça??
  
  x + y
}

some_dois_numeros(1, 10)


parse_number_br <- function(x){
  readr::parse_number(x,
                      locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
  
}

parse_number_br("855,3600000000")
class(parse_number_br("855,3600000000"))

# Exemplo da dúvida da Giovanna:
funcao_da_gio <- function(x, y){
  (x*100)/y
}

funcao_da_gio(x = 5, y = 10)

infoaguas_renomeado |> 
  dplyr::mutate(
    cod_interaguas = as.numeric(cod_interaguas),
    altitude = as.numeric(altitude),
    resultado_funcao_gio = funcao_da_gio(x = cod_interaguas, y = altitude)) |> View()



Sys.Date()
"20/10/2022"
readr::parse_date("20/10/2022", format = "%d/%m/%Y")

# ?readr::parse_date


# vamos criar uma funcao!

parse_number_br <- function(x) {
  readr::parse_number(x,
                      locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
  
}

parse_date_br <- function(x) {
  readr::parse_date(x, format = "%d/%m/%Y")
}

parse_date_br("15/02/1993")


# |> pipe nativo - vem com o R base atual
# %>% pipe - vem com o tidyverse/dplyr..


# exemplo simples de mutate! usando a função que criamos
infoaguas_renomeado |> 
  dplyr::mutate(valor_corrigido = parse_number_br(valor)) |> View()

infoaguas <- infoaguas_renomeado |> 
  dplyr::mutate(
    valor = parse_number_br(valor),
    dplyr::across(
      # .cols = argumento das colunas
      .cols = c("periodo_de", "periodo_ate", "data_coleta", "inicio_operacao"),
      # .fns = argumento da funcao
      .fns = parse_date_br
    ),
    lat = -parzer::parse_lat(latitude),
    long = -parzer::parse_lon(longitude)
  ) 

dplyr::glimpse(infoaguas)


# como transformar várias colunas de uma vez? arrumar colunas de data!
infoaguas <- infoaguas_renomeado |>
  dplyr::mutate(valor = parse_number_br(valor),
                dplyr::across(.cols = c("inicio_operacao", "data_coleta", "periodo_de", "periodo_ate"),
                              .fns = parse_date_br),
                latitude = -parzer::parse_lat(latitude),
                longitude = -parzer::parse_lon(longitude)) 





# wide --------------------------------------------------------------------

# https://tidyr.tidyverse.org/reference/pivot_wider.html

infoaguas_wide <- infoaguas |> 
  dplyr::select(codigo_ponto, data_coleta, parametro, valor) |> 
  tidyr::pivot_wider(names_from = parametro, values_from = valor) |> 
  janitor::clean_names()


infoaguas_long <- infoaguas_wide |>
  tidyr::pivot_longer(
    cols = c("fosforo_total", "clorofila_a"),
    names_to = "parametro",
    values_to = "valor"
  )


infoaguas_long |> View()

# mutate ---------------------------------------------------------------

# https://www.sciencedirect.com/science/article/abs/pii/S0925857413003091

calcular_tsi_tsr <- function(TP, CHLA){
  tsi_tp <- 10 * (6 - ((-0.27637 * SciViews::ln(TP) + 1.329766) / SciViews::ln(2)))
  
  tsi_chla <- 10 * (6 - ((-0.2512 * SciViews::ln(CHLA) + 0.842257) / SciViews::ln(2)
  ))
  
  tsi_tsr <- (tsi_tp + tsi_chla) / 2
  
  tsi_tsr
                       
}


infoaguas_wide |> 
  tidyr::drop_na(fosforo_total, clorofila_a) |> 
  dplyr::mutate(iet = calcular_tsi_tsr(TP = fosforo_total, CHLA = clorofila_a)) |> View()




# https://dplyr.tidyverse.org/reference/case_when.html


calcular_classe_tsi <- function(tsi_tsr){
  tsi_tsr_class <- dplyr::case_when(
    tsi_tsr <= 51.1 ~ "Ultraoligotrophic",
    tsi_tsr >= 51.2 & tsi_tsr < 53.2 ~ "Oligotrophic",
    tsi_tsr >= 53.2 & tsi_tsr < 55.8 ~ "Mesotrophic",
    tsi_tsr >= 55.8 & tsi_tsr < 58.2 ~ "Eutrophic",
    tsi_tsr >= 58.2 & tsi_tsr < 59.1 ~ "Supereutrophic",
    tsi_tsr >= 59.1 ~ "Hypereutrophic"
  )
  
  tsi_tsr_class
}



infoaguas_iet <- infoaguas_wide |> 
  tidyr::drop_na(fosforo_total, clorofila_a) |> 
  dplyr::mutate(tsi_tsr = calcular_tsi_tsr(TP = fosforo_total, CHLA = clorofila_a),
                tsi_tsr_class = calcular_classe_tsi(tsi_tsr)) 




# broom

resultado_modelo <- lm(formula = fosforo_total ~ clorofila_a, data = infoaguas_wide) |> 
  broom::tidy()

resultado_modelo$p.value[1]

resultado_modelo$p.value[2]

# vis ---------------------------------------------------------------------


# Se der tempo: 
infoaguas |>
  dplyr::distinct(codigo_ponto, latitude, longitude) |>
  leaflet::leaflet() |>
  leaflet::addProviderTiles(provider = "OpenStreetMap") |>
  leaflet::addAwesomeMarkers(
    lng = ~ longitude,
    lat = ~ latitude,
    popup = ~ codigo_ponto
  )


infoaguas_iet |> 
  distinct(codigo_ponto, tsi_tsr_class)

# uma visualização de dados

library(ggplot2)

infoaguas |> 
  dplyr::filter(parametro == "Clorofila-a") |> 
  ggplot() +
  geom_line(aes(x = data_coleta, y = valor)) +
  facet_wrap(vars(codigo_ponto)) +
  theme_bw()


# DUVIDA: COLOCAR MAIS DE UM PARAMETRO?

# infoaguas |> 
#   ggplot() +
#   geom_line(aes(x = data_coleta, y = valor)) +
#   facet_grid(codigo_ponto ~ parametro) +
#   theme_bw()


grafico_clorofila <- infoaguas |> 
  dplyr::filter(parametro == "Clorofila-a") |> 
  ggplot() +
  geom_line(aes(x = data_coleta, y = valor)) +
  facet_wrap(vars(codigo_ponto), ncol = 1) +
  theme_bw()

grafico_fosforo <- infoaguas |> 
  dplyr::filter(parametro == "Fósforo Total") |> 
  ggplot() +
  geom_line(aes(x = data_coleta, y = valor)) +
  facet_wrap(vars(codigo_ponto), ncol = 1) +
  theme_bw()



# install.packages("patchwork")
library(patchwork)

grafico_clorofila + grafico_fosforo


# quantas linhas existem para cada categoria em parametro?

infoaguas |> dplyr::count(parametro)



# http://pnqa.ana.gov.br/Publicacao/RESOLUCAO_CONAMA_n_357.pdf
  

