# IMPORTAÇÃO
# importar vários arquivos ------------------------------------------------

# importando um arquivo
infoaguas_exemplo <- readxl::read_xlsx("dados-brutos/RelatorioQualidadeAguasSuperficiais_clorofila-a_2008-2012.xlsx")

# vendo as colunas
dplyr::glimpse(infoaguas_exemplo)

# descobrindo o caminho de todos os arquivos
arquivos <- fs::dir_ls("dados-brutos/", glob = "*.xlsx")


# precisamos aplicar a função readxl::read_xlsx para todos eles!

# usar purrr::map() !
# exemplo: 

vetor <- 1:5

# aplicar a funcao sqrt() em todos os valores de 'vetor'
purrr::map(vetor, sqrt)

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

# usar as.numeric não funciona.
as.numeric(infoaguas_renomeado$valor)


# vamos conhecer as funções de parse!

# exemplo:
readr::parse_number(infoaguas_renomeado$valor,
                    locale = readr::locale(decimal_mark = ",", grouping_mark = "."))

# vamos criar uma funcao!

parse_number_br <- function(x) {
  readr::parse_number(x,
                      locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
  
}

parse_date_br <- function(x) {
  readr::parse_date(x, format = "%d/%m/%Y")
}



# exemplo simples de mutate! usando a função que criamos
infoaguas_renomeado |> 
  dplyr::mutate(valor_corrigido = parse_number_br(valor)) |> View()


# como transformar várias colunas de uma vez? arrumar colunas de data!
infoaguas <- infoaguas_renomeado |>
  dplyr::mutate(valor = parse_number_br(valor),
                dplyr::across(.cols = c("inicio_operacao", "data_coleta", "periodo_de", "periodo_ate"),
                              .fns = parse_date_br),
                latitude = -parzer::parse_lat(latitude),
                longitude = -parzer::parse_lon(longitude)) 





# wide --------------------------------------------------------------------

infoaguas_wide <- infoaguas |> 
  dplyr::select(codigo_ponto, valor, data_coleta, parametro) |> 
  #dplyr::group_by(codigo_ponto, data_coleta) |> 
  tidyr::pivot_wider(names_from = parametro, values_from = valor) |> 
  janitor::clean_names()


infoaguas_long <- infoaguas_wide |>
  tidyr::pivot_longer(
    cols = c("fosforo_total", "clorofila_a"),
    names_to = "parametro",
    values_to = "valor"
  )


# mutate ---------------------------------------------------------------

calcular_tsi_tsr <- function(TP, CHLA){
  tsi_tp <- 10 * (6 - ((-0.27637 * SciViews::ln(TP) + 1.329766) / SciViews::ln(2)))
  
  tsi_chla <- 10 * (6 - ((-0.2512 * SciViews::ln(CHLA) + 0.842257) / SciViews::ln(2)
  ))
  
  tsi_tsr <- (tsi_tp + tsi_chla) / 2
  
  tsi_tsr
                       
}

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

lm(formula = fosforo_total ~ clorofila_a, data = infoaguas_wide) |> 
  broom::tidy()



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


# uma visualização de dados

library(ggplot2)

infoaguas |> 
  dplyr::filter(parametro == "Clorofila-a") |> 
  ggplot() +
  geom_line(aes(x = data_coleta, y = valor, color = codigo_ponto)) +
  facet_wrap(vars(codigo_ponto)) +
  theme_bw()



# http://pnqa.ana.gov.br/Publicacao/RESOLUCAO_CONAMA_n_357.pdf
  

