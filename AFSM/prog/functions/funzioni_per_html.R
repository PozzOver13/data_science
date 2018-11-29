
# Funzione per mostrare i migliori giocatori per determinate stat

show_me_best <- function(df_in, ruolo_char, 
                         var_target, lenght_show = 1:10) {
  
  df_in %>%
    filter(str_detect(Pos, ruolo_char)) %>%
    arrange_(paste0("-",var_target)) %>%
    dplyr::slice(lenght_show) %>%
    as.data.frame()
  
  
}

show_me_best_score <- function(df_in, ruolo_char, 
                               var_target, lenght_show = 1:10) {
  
  if (ruolo_char == "P") {
    
    out <- df_in %>%
      filter(str_detect(role, ruolo_char)) %>%
      arrange_(paste0("-",var_target)) %>%
      dplyr::slice(lenght_show) %>%
      as.data.frame()
    
  }
  
  if (ruolo_char == "D") {
    
    out <- df_in %>%
      filter(str_detect(role, ruolo_char)) %>%
      arrange_(paste0("-",var_target)) %>%
      dplyr::slice(lenght_show) %>%
      as.data.frame()
    
  }
  
  if (ruolo_char == "C") {
    
    out <- df_in %>%
      filter(str_detect(role, ruolo_char)) %>%
      arrange_(paste0("-",var_target)) %>%
      dplyr::slice(lenght_show) %>%
      as.data.frame()
    
  }
  
  if (ruolo_char == "A") {
    
    out <- df_in %>%
      filter(str_detect(role, ruolo_char)) %>%
      arrange_(paste0("-",var_target)) %>%
      dplyr::slice(lenght_show) %>%
      as.data.frame()
    
  }
  
  out
  
}


# show_me_best("SF", "eFG_PCT", 1:20)
