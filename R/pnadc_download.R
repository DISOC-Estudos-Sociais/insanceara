pnadc_download <- function(){
  
  baixar_base_trimestre <- function(ano, variaveis, trimestre = 4) {
    get_pnadc(year = ano, vars = variaveis, topic = trimestre, design = FALSE)
  }
  
  # Definir variáveis selecionadas: PNAD Contínua
  variaveis_selecionadas <- c(
    "Capital", "RM_RIDE", "V1022", "V2007", "V2009", "V2010",
    "VD2002", "VD2003", "VD2004", "VD2006", "VD3004", "VD3005",
    "VD3006", "VD4001", "VD4002", "VD4003", "VD4004A", "VD4005",
    "VD4007", "VD4008", "VD4009", "VD4010", "VD4011", "VD4012",
    "VD4013", "VD4014", "VD4015", "VD4016", "VD4017", "VD4018",
    "VD4019", "VD4020", "VDI4022", "VD4023", "VD4030", "VD4031",
    "VD4032", "VD4033", "VD4034", "VD4035", "VD4036", "VD4037",
    "VDI4046", "VDI4047", "VDI4048", "VDI4052", "VDI5001", "VDI5002",
    "VDI5003", "VDI5004", "VDI5005", "VDI5006", "VDI5007", "VDI5008",
    "VDI5009", "VDI5010", "VDI5011", "VDI5012",
    "S17001", "S17002", "S17003", "S17004", "S17005", "S17006", 
    "S17007", "S17008", "S17009", "S17010", "S17011", "S17012", 
    "S17013", "S17014", "SD17001"
  )
  
  # Pipeline principal. Seleciona o 4 semestre, que contem informações sobre segurança alimentar.
  dados <- 2023:2024 |> 
    map_df(~baixar_base_trimestre(.x, variaveis_selecionadas, 4)) |> 
    identity()
  
  saveRDS(dados, "data/clean_data/pnadc.RDS")
}

