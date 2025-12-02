make_panel_san <- function(){
  dados <- dplyr::bind_rows(pnadc(), pof(), pnad2013(), pnad2009(), pnad2004()) |>
    dplyr::mutate(
      ano = forcats::fct(ano, levels = c("PNAD 2004", "PNAD 2009", "PNAD 2013", "POF 2017/2018", "PNADC 2023", "PNADC 2024")),
      faixa_idade = dplyr::case_when(
        idade >= 0 & idade <= 4 ~ "0 a 4 anos",    
        idade >= 5 & idade <= 9 ~ "5 a 9 anos",     
        idade >= 10 & idade <= 13 ~ "10 a 13 anos",   
        idade >= 14 & idade <= 19 ~ "14 a 19 anos",   
        idade >= 20 & idade <= 24 ~ "20 a 24 anos",   
        idade >= 25 & idade <= 29 ~ "25 a 29 anos",   
        idade >= 30 & idade <= 34 ~ "30 a 34 anos",   
        idade >= 35 & idade <= 39 ~ "35 a 39 anos",   
        idade >= 40 & idade <= 44 ~ "40 a 44 anos",   
        idade >= 45 & idade <= 49 ~ "45 a 49 anos",   
        idade >= 50 & idade <= 54 ~ "50 a 54 anos",   
        idade >= 55 & idade <= 59 ~ "55 a 59 anos",   
        idade >= 60 & idade <= 64 ~ "60 a 64 anos",   
        idade >= 65 & idade <= 69 ~ "65 a 69 anos",   
        idade >= 70 & idade <= 74 ~ "70 a 74 anos",   
        idade >= 75 & idade <= 79 ~ "75 a 79 anos",   
        idade >= 80 ~ "80 anos ou mais"),
      faixa_escolaridade = dplyr::if_else(faixa_escolaridade == "Não determinado", NA, faixa_escolaridade),
      san = dplyr::if_else(san == "Tem morador menor de 18 anos e sem declaração de segurança alimentar", NA, san))
  
  saveRDS(dados, "data/clean_data/painel_san.RDS")
  
  return(dados)
}