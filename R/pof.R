pof <- function(data_domicilio="data/raw_data/DOMICILIO.txt", data_morador = "data/raw_data/MORADOR.txt", input="data/raw_data/Leitura dos Microdados - SAS.txt", dicionario= "data/raw_data/Dicionários de váriaveis.xls"){
  cat("=== INICIANDO PROCESSAMENTO POF 2017/2018 ===\n")
  dados_domicilio <- read_pof(data_domicilio, input) |> 
    pof_labeller(dicionario, sheet = "Domicílio")
  
  dados <- read_pof(data_morador, input) |> 
    pof_labeller(dicionario, sheet = "Morador") |> 
    dplyr::left_join(dados_domicilio)
  
  rm(dados_domicilio)
  
  dados <- dados |> 
    dplyr::select(UF, ESTRATO_POF,
                  TIPO_SITUACAO_REG, PESO_FINAL,
                  V0404, V0403, V0405,
                  V0306, NIVEL_INSTRUCAO,
                  V6199) |> 
    dplyr::mutate(ano="POF 2017/2018", .before = 1,
                  ESTRATO_POF = as.character(ESTRATO_POF)) |> 
    dplyr::rename(uf = UF,
                  estrato = ESTRATO_POF,
                  situacao = TIPO_SITUACAO_REG,
                  peso = PESO_FINAL,
                  sexo = V0404,
                  idade = V0403,
                  raca = V0405,
                  posicao = V0306,
                  faixa_escolaridade = NIVEL_INSTRUCAO,
                  san = V6199) |>
    dplyr::mutate(situacao=dplyr::if_else(situacao == "Urbano", "Urbana", "Rural"),
                  raca=dplyr::if_else(raca=="Sem declaração", "Ignorado", raca),
                  faixa_escolaridade = forcats::fct_recode(faixa_escolaridade,
                                                           "Fundamental incompleto ou equivalente" = "Ensino Fundamental Incompleto",
                                                           "Médio completo ou equivalente" = "Ensino Médio Completo",        
                                                           "Médio incompleto ou equivalente" = "Ensino Médio Incompleto",
                                                           "Sem instrução e menos de 1 ano de estudo" = "Sem instrução",
                                                           "Fundamental completo ou equivalente" = "Ensino Fundamental Completo",
                                                           "Superior incompleto ou equivalente" = "Ensino Superior Incompleto",
                                                           "Superior completo" = "Ensino Superior Completo"),
                  posicao=forcats::fct_collapse(posicao,
                                                "Pessoa responsável" = "Pessoa de referência da UC",            
                                                "Cônjuge ou companheiro(a)" = c("Cônjuge ou companheiro(a) do mesmo sexo", "Cônjuge ou companheiro(a) de sexo diferente"),     
                                                "Filho(a)" = c("Filho(a) somente da pessoa de referência", "Filho(a) da pessoa de referência e do cônjuge"),                      
                                                "Neto(a)" = "Neto(a)",                       
                                                "Pai, mãe, padrasto ou madrasta" = "Pai, mãe, padrasto ou madrasta",
                                                "Outro parente" = "Outro parente",                
                                                "Irmão ou irmã" = "Irmão ou irmã",                
                                                "Bisneto(a)" = "Bisneto(a)",                    
                                                "Avô ou avó" = "Avô ou avó",                    
                                                "Sogro(a)" = "Sogro(a)",                      
                                                "Genro ou nora" = "Genro ou nora",                 
                                                "Convivente" = "Convivente",                    
                                                "Agregado(a)" = "Agregado(a)",                   
                                                "Enteado(a)" = "Filho(a) somente do cônjuge",                    
                                                "Empregado(a) doméstico(a)" = "Parente do(a) empregado(a) doméstico(a)"),
                  san = forcats::fct_recode(san,
                                            "Segurança alimentar" = "Segurança",
                                            "Insegurança alimentar leve" = "Insegurança leve",
                                            "Insegurança alimentar moderada" = "Insegurança moderada",
                                            "Insegurança alimentar grave" = "Insegurança grave"))
  readr::write_rds(dados, "data/clean_data/pof.RDS")
  cat("✓ POF processada e salva\n\n")
  return(dados)
}