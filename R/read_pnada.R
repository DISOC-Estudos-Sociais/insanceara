read_pnada <- function(microdata, input_txt, vars=NULL){
  X1 = X2 = X3 = start = end = NULL
  input <- suppressWarnings(
    suppressMessages({
      readr::read_table(input_txt, col_names=FALSE) %>% 
        dplyr::mutate(X1=iconv(X1, from="", to="UTF-8", sub=""))%>%
        subset(substr(X1, 1, 1) == "@") %>%
        dplyr::mutate(type=ifelse(substr(X3, 1, 1) == "$","c","d"),
                      start=as.numeric(gsub("@", "", X1)),
                      X3=as.integer(chartr("$", " ", X3)),
                      end=start+X3-1)
    })
  )
  if (input_txt == "data/raw_data/input DOM2009.txt") {
    input[51,"end"] <- 107
    input[53,"end"] <- 122
  }
  if (!is.null(vars)) {
    if (any(!(vars %in% input$X2))) {
      missvar <- vars[!(vars %in% input$X2)]
      message(paste("Variables", paste(missvar, collapse=", "), "not present in microdata.\n"))
    }
    input %<>% subset(X2 %in% c("Ano", "Trimestre", "UF", "UPA", "ID_DOMICILIO", "Estrato", "V1008", "V1014", "V1027", "V1028", sprintf("V1028%03d", seq(1:200)), "V1029", "V1030", "V1031", "V1032", sprintf("V1032%03d", seq(1:200)), "V1033", "V1034", "V1035", "V1036", sprintf("V1036%03d", seq(1:200)), "V1037", "V1038", "V1039", "V1040", sprintf("V1040%03d", seq(1:200)), "V1041", "V1042", "posest", "posest_sxi", "V2003", "S090000", "S12001A", vars))
  }
  columns <- input %$% readr::fwf_positions(start, end, X2)
  data_pnada <- suppressWarnings(readr::read_fwf(microdata, columns, col_types=paste0(input$type, collapse="")))
  return(data_pnada)
}