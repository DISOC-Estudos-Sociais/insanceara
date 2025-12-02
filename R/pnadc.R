pnadc <- function(data = "data/clean_data/pnadc.RDS"){
  cat("=== INICIANDO PROCESSAMENTO PNAD CONTÍNUA ===\n")
  dados <- readr::read_rds(data) |> 
    dplyr::select(Ano, UF, Estrato,
                  # situação do domicílio e peso
                  V1022, V1028,
                  # sexo, idade e raça
                  V2007, V2009, V2010,
                  # condição no domicílio, idade, escolaridade, renda
                  VD2002, VD2006, VD3004, VDI5009,
                  # segurança alimentar
                  SD17001
    ) |> 
    dplyr::mutate(Estrato=stringr::str_sub(Estrato, 1, 4),
                  Ano = dplyr::if_else(Ano == 2023, "PNADC 2023", "PNADC 2024")) |> 
    dplyr::rename(ano = Ano,
                  uf = UF,
                  estrato = Estrato,
                  situacao = V1022,
                  peso = V1028,
                  sexo = V2007,
                  idade = V2009,
                  raca = V2010,
                  posicao = VD2002,
                  faixa_idade = VD2006,
                  faixa_escolaridade = VD3004,
                  faixa_renda = VDI5009,
                  san = SD17001)
  saveRDS(dados, "data/clean_data/pnadc_clean.RDS")
  cat("✓ PNAD Contínua processada e salva\n\n")
  return(dados)
}