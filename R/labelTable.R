#' Exchange label for name of variable according external file
#
#' Open a excel file and apply label to variables from table
#
#' @param data data.frame
#'
#' @param taulavariables excel file with description field
#'
#' @param camp_descripcio Field with label in excel file
#'
#' @return Data.frame or tibble with labeled variables
#'
#' @import dplyr
#'
#' @export
#' 

labelTable<-function(data=resumtotal,taulavariables="variables_R.xls",camp_descripcio="descripcio",camp="variable") {

  # data=T3.MI
  # taulavariables=conductor_variables
  # camp="variables_taula"
  # camp_descripcio="descripcio"

  # Llegir etiquetes i variables a analitzar ####
  variables <- readxl::read_excel(taulavariables) %>% tidyr::as_tibble()
  # variables[is.na(variables)]<- 0
  camp_sym<-rlang::sym(camp)
  variables<-variables %>% dplyr::filter(!is.na(!!camp_sym))

  # Canviar nom de camp de variables al de la data 
  colnames(variables)[colnames(variables)=="camp"] <- camp

  # Canviar arguments per ser evaluats
  camp_eval<-rlang::sym(camp)
  camp_descripcio_eval<-rlang::sym(camp_descripcio)
  # Canviar el format de la data 
  data<-data %>% 
    left_join(dplyr::select(variables,c(!!camp_eval,camp_descripcio)),by=quo_name(camp_eval)) %>% 
    mutate(!!camp_eval:=descripcio) %>% 
    dplyr::select(-descripcio)
  }
