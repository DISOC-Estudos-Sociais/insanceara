pof_labeller <- function(data_pof, dictionary.file, sheet = "Condições de Vida") {
  library(magrittr)
  if(sum(class(data_pof)=="tbl_df")>0){
    dictionary <- readxl::read_excel(dictionary.file, sheet) |> 
      dplyr::rename(categorias = 6) |> 
      tidyr::separate(categorias, into = c("codigo", "nome"), sep = " – ", fill = "right", remove = T)
    X__3 = X__4 = X__6 = X__7 = NULL
    colnames(dictionary) <- paste0("X__",1:dim(dictionary)[2])
    dictionary %<>% subset(!is.na(X__6))
    codcurrent <- dictionary$X__4
    for (i in 1:dim(dictionary)[1]) {
      if (is.na(dictionary$X__4[i])) {
        dictionary$X__4[i] <- codcurrent
      }
      else {
        codcurrent <- dictionary$X__4[i]
      }
    }
    dictionary <- dictionary %>%
      dplyr::mutate(X__6 = stringr::str_remove(X__6, "^0+"))
    notlabel <- c("ESTRATO_POF", "COD_UPA", "NUM_DOM", "NUM_UC", "COD_INFORMANTE", "V6102",
                  "V6103", "PESO", "PESO_FINAL", "RENDA_TOTAL", "V0403")
    vars <- names(data_pof)
    varsf <- setdiff(vars, notlabel)
    data_pof <- data_pof |> 
      dplyr::mutate(dplyr::across(all_of(varsf), ~tidyr::replace_na(as.character(.), "Branco"))        )
    for (i in 1:length(varsf)) {
      if (i > 0 & varsf[i] %in% (dictionary$X__4)) {
        data_pof[varsf[i]] <- factor(suppressWarnings(unlist(data_pof[varsf[i]])), 
                                     levels=suppressWarnings(unlist(dictionary |> subset(X__4 == varsf[i]) |> dplyr::select(X__6))),
                                     labels=unlist(dictionary |> subset(X__4 == varsf[i]) |> dplyr::select(X__7)))
      }
    }
  }
  else{
    message("The microdata object is not of the tibble class or sample design was already defined for microdata, so labeling categorical variables is not possible.\n")
  }
  return(data_pof)
}