read_pof <- function(microdata, input_txt, vars=NULL) {
  library(magrittr)
  X1 = X2 = X3 = start = end = NULL
  input <- suppressWarnings(suppressMessages({
    readr::read_table(input_txt, col_names=FALSE) %>% 
      subset(substr(X1, 1, 1) == "@") %>%
      dplyr::mutate(
        type = ifelse(substr(X3, 1, 1) == "$", "c", "d"), 
        start = as.numeric(gsub("@", "", X1)), 
        X3 = as.integer(chartr("$", " ", X3)), 
        end = start + X3 - 1
      )
  }))
  # Filtrar linhas baseado no tipo de planilha
  if (microdata == "data/raw_data/CONDICOES_VIDA.txt") {
    input <- input[398:452, ]
  } else if (microdata == "data/raw_data/DOMICILIO.txt") {
    input <- input[1:38, ]
  } else if (microdata == "data/raw_data/MORADOR.txt") {
    input <- input[39:94, ]
  }
  if (!is.null(vars)) {
    if (any(!(vars %in% input$X2))) {
      missvar <- vars[!(vars %in% input$X2)]
      message(paste("Variables", paste(missvar, collapse=", "), "not present in microdata.\n"))
    }
    input %<>% subset(X2 %in% c(
      "Ano", "Trimestre", "UF", "UPA", "ID_DOMICILIO", "Estrato", 
      "V1008", "V1014", "V1027", "V1028", sprintf("V1028%03d", seq(1:200)), 
      "V1029", "V1030", "V1031", "V1032", sprintf("V1032%03d", seq(1:200)), 
      "V1033", "V1034", "V1035", "V1036", sprintf("V1036%03d", seq(1:200)), 
      "V1037", "V1038", "V1039", "V1040", sprintf("V1040%03d", seq(1:200)), 
      "V1041", "V1042", "posest", "posest_sxi", "V2003", 
      "S090000", "S12001A", vars
    ))
  }
  columns <- input %$% readr::fwf_positions(start, end, X2)
  data_pof <- suppressWarnings(readr::read_fwf(microdata, columns, col_types=paste0(input$type, collapse="")))
  return(data_pof)
}