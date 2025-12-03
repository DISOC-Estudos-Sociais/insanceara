dados_svy_san_ce <- function(df, var){
  df <- df |>
    filter(posicao == "Pessoa responsável" & uf == "Ceará" & ano %in% c("PNADC 2023", "PNADC 2024")) |>
    as_survey_design(weights = peso) |>
    group_by(ano, {{ var }}, san) |>
    summarise(
      n = survey_total(),
      .groups = "drop"
    ) |>
    group_by(ano, san) |>
    mutate(
      populacao = sum(n),
      proporcao = n / populacao
    ) |>
    ungroup() 
  
  return(df)
}
