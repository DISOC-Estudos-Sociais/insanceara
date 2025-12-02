pnada_labeller <- function(data_pnada, dictionary.file){
  if(sum(class(data_pnada)=="tbl_df")>0){
    dictionary <- suppressMessages(readxl::read_excel(dictionary.file))
    X__3 = X__6 = X__7 = NULL
    colnames(dictionary) <- paste0("X__",1:dim(dictionary)[2])
    dictionary %<>% subset(!is.na(X__6))
    codcurrent <- dictionary$X__3
    for (i in 1:dim(dictionary)[1]) {
      if (is.na(dictionary$X__3[i])) {
        dictionary$X__3[i] <- codcurrent
      }
      else {
        codcurrent <- dictionary$X__3[i]
      }
    }
    dictionary <- dictionary |>
      dplyr::filter(X__6 != "TIPO A" & X__6 != "TIPO B" & X__6 != "TIPO C")
    notlabel <- c("V0101", "V0102", "V0103", "V0105", "V0106", "V02270",
                  "V02271", "V4600", "V4601", "V4602", "V4604", "V4606",
                  "V4608", "V4609", "UPA", "V4620", "V0301", "V3031",
                  "V3032", "V3033", "V0403", "V7060", "V7070", "V7090",
                  "V7100", "V9971", "V9972", "V9990", "V9991", "V9910",
                  "V9911", "V1141", "V1142", "V1151", "V1152", "V1161",
                  "V1162", "V1181", "V1182", "V1111", "V1112", "V4724",
                  "V9993", "V2102", "V0407", "V29010", "V29030", "V29040",
                  "V2940", "V29180")
    vars <- names(data_pnada)
    varsc <- vars[sapply(data_pnada, class) == "character"]
    varsf <- setdiff(varsc, notlabel)
    for (i in 1:length(varsf)) {
      if (i > 0 & varsf[i] %in% (dictionary$X__3)) {
        data_pnada[varsf[i]] <- factor(suppressWarnings(as.numeric(unlist(data_pnada[varsf[i]]))), 
                                       levels=suppressWarnings(as.numeric(unlist(dictionary %>% subset(X__3 == varsf[i]) %>% dplyr::select(X__6)))),
                                       labels=unlist(dictionary %>% subset(X__3 == varsf[i]) %>% dplyr::select(X__7)))
      }
    }
    return(data_pnada)
  }
  else{
    message("The microdata object is not of the tibble class or sample design was already defined for microdata, so labeling categorical variables is not possible.\n")
  }
}