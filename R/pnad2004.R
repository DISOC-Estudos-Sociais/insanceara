pnad2004 <- function(){
  cat("=== INICIANDO PROCESSAMENTO PNAD 2004 ===\n")
  dados_dom <- read_pnada(microdata = "data/raw_data/DOM2004.TXT", input_txt = "data/raw_data/input_Dom2004.txt") |> 
    pnada_labeller(dictionary.file = "data/raw_data/Dicionário de variáveis de domicílios - 2004.xls")
  
  dados <- read_pnada(microdata = "data/raw_data/PES2009.TXT", input_txt = "data/raw_data/input_Pes2004.txt") |> 
    pnada_labeller(dictionary.file = "data/raw_data/Dicionário de variáveis de pessoas - 2009.xls") |> 
    dplyr::left_join(dados_dom)
  
  rm(dados_dom)
  
  dados <- dados |>
    dplyr::select(UF, V4602,
                  # situação do domicílio e peso
                  V4105, V4611,
                  # sexo, idade e raça
                  V0302, V8005, V0404,
                  # condição no domicílio, renda
                  V0401, V4622,
                  # segurança alimentar
                  V4623A) |>
    dplyr::mutate(ano="PNAD 2004", .before = 1) |> 
    dplyr::rename(uf = UF,
                  estrato = V4602,
                  situacao = V4105,
                  peso = V4611,
                  sexo = V0302,
                  idade = V8005,
                  raca = V0404,
                  posicao = V0401,
                  faixa_renda = V4622,
                  san = V4623A) |>
    dplyr::mutate(situacao = forcats::fct_collapse(situacao,
                                                   "Urbana" = c("URBANA - Cidade ou vila, área urbanizada", "URBANA - Cidade ou vila, área não urbanizada", "URBANA - Área urbana isolada"),
                                                   "Rural" = c("RURAL - Zona rural exclusive aglomerado rural", "RURAL - Aglomerado rural, isolado, povoado", "RURAL - Aglomerado rural de extensão urbana", "RURAL - Aglomerado rural, isolado, outros aglomerados", "RURAL - Aglomerado rural, isolado, núcleo")),
                  sexo = dplyr::if_else(sexo == "Feminino", "Mulher", "Homem"),
                  raca=dplyr::if_else(raca=="Sem declaração", "Ignorado", raca),
                  posicao = forcats::fct_recode(posicao,
                                                "Pessoa responsável"="Pessoa de referência",
                                                "Cônjuge ou companheiro(a)"="Cônjuge",
                                                "Filho(a)"="Filho",
                                                "Agregado(a)"="Agregado",
                                                "Empregado(a) doméstico(a)"="Empregado doméstico"),
                  faixa_renda = forcats::fct_collapse(faixa_renda,
                                                      "Mais de ¼ até ½ salário mínimo" = "Mais de ¼ até ½",
                                                      "Mais de ½ até 1 salário mínimo" = "Mais de ½ até 1",
                                                      "Mais de 1 até 2 salários mínimos" = "Mais de  1 até   2",
                                                      "Mais de 2 até 3 salários mínimos" = "Mais de  2 até  3",
                                                      "Mais de 3 até 5 salários mínimos" = "Mais de 3 até 5",
                                                      "Mais de 5 salários mínimos" = "Mais de 5 salários",
                                                      "Ignorado"=c("Sem rendimento", "Sem declaração")),
                  san = forcats::fct_collapse(san,
                                              "Segurança alimentar"=c("Não tem morador menor de 18 anos e tem segurança alimentar", "Tem morador menor de 18 anos e tem segurança alimentar"),
                                              "Insegurança alimentar leve"=c("Tem morador menor de 18 anos e insegurança alimentar leve", "Não tem morador menor de 18 anos e insegurança alimentar leve"),
                                              "Insegurança alimentar moderada"=c("Não tem morador menor de 18 anos e insegurança alimentar moderada", "Tem morador menor de 18 anos e insegurança alimentar moderada"),
                                              "Insegurança alimentar grave"=c("Tem morador menor de 18 anos e insegurança alimentar grave", "Não tem morador menor de 18 anos e insegurança alimentar grave")))
  readr::write_rds(dados, "data/clean_data/pnad2004.RDS")
  cat("✓ PNAD processada e salva\n\n")
  return(dados)
}