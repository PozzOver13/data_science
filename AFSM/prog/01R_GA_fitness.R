

#### Funzione di fitness ####

## Funzione di fitness che riceve in input una stringa di 0 e 1 e calcola un numero di fintess da massimizzare

## Argomenti

# string = stringa 0 1
# vars_test = vettore con elenco campi e segni da dare in input
# db_input = database con perizie "pulite" per stima
# db_input_v = database con perizie "pulite" per validazione
# w_r2 = peso relativo del R quadro
# w_pvalues = peso relativo delle variabili con pvalues significativo
# w_rmse = peso relativo dell'errore quadratico medio su campione di validazione
# w_nvar = peso relativo del numero di variabili considerate dal modello 

# return = valore di fitness

### NOTA: a parte il peso del r2, tutti gli altri pesi devono sommare a 1 ###

## Pacchetti richiesti
# require(pkg)

avm_ga_fitness <- function(string = NULL, 
                           df_in = NULL, 
                           w_score_por_tot,
                           w_score_def_tot,
                           w_score_cc_tot,
                           w_score_att_tot,
                           w_costo_tot_nrm,
                           w_val_sq_ideale,
                           
                           formazione_ideale = c(4,3,3)) {
  
  #### versione_1.01 ####
  
  if (is.null(string) | !is.numeric(string)) {
    stop("Inserire stringa 0 1, o controlla tipo", call.=FALSE) 
  } 
  
  if (is.null(df_in) | !is.data.frame(df_in)) {
    stop("Inserire dataset di input per stima [no data.table]", call.=FALSE) 
  } 
  
  ## ESTRAZIONE INFO DA 'string' ##
  
  # seleziono variabili in base alle 
  # creo booleana
  selez_var <- string == 1
  
  # se la stringa non seleziona nessuna variabile
  if (sum(selez_var) == 0) { return(1/999) }
  
  # seleziono lista
  players <- df_in$nome[selez_var]
  
  df_app <- df_in %>% 
    filter(nome %in% players)
  
  n_p = sum(df_app$r == "P")
  n_d = sum(df_app$r == "D")
  n_c = sum(df_app$r == "C")
  n_a = sum(df_app$r == "A")
  
  # se la stringa non seleziona i ruoli correttamente taglio selezione
  if (n_p != 3) { return(1/999) }
  
  if (n_d != 8) { return(1/999) }
  
  if (n_c != 8) { return(1/999) }
  
  if (n_a != 6) { return(1/999) }
  
  # controllo che valore iniziale non superi il limite
  costo_tot = sum(df_app$qt_i)
  # normalizzo per fitness
  costo_tot_nrm = costo_tot / 250
  
  # se supera il threshold elimino
  if (costo_tot > 250) { return(1/999) }
  
  # estraggo score
  score_por_tot <- sum(df_app$score_port[df_app$r == "P"])
  score_def_tot <- sum(df_app$score_dif[df_app$r == "D"])
  score_cc_tot <- sum(df_app$score_cc[df_app$r == "C"])
  score_att_tot <- sum(df_app$score_att[df_app$r == "A"])
  
  score_por_tot_nrm <- score_por_tot / 3
  score_def_tot_nrm <- score_def_tot / 8
  score_cc_tot_nrm  <- score_cc_tot / 8
  score_att_tot_nrm <- score_att_tot / 6
  
  # seleziono 11 ideale
  df_app_ideale <- df_app %>%
    arrange(desc(r), desc(qt_i))
  
  frm_id <- c(1,
             (4):(formazione_ideale[1] + 3),
             (12):(formazione_ideale[2] + 11),
             (20):(formazione_ideale[3] + 19)
              )
  
  df_app_ideale_f <- df_app_ideale[frm_id,]
  
  sq_id_score_por_tot <- sum(df_app_ideale_f$score_port[df_app_ideale_f$r == "P"])
  sq_id_score_def_tot <- sum(df_app_ideale_f$score_dif[df_app_ideale_f$r == "D"])
  sq_id_score_cc_tot <- sum(df_app_ideale_f$score_cc[df_app_ideale_f$r == "C"])
  sq_id_score_att_tot <- sum(df_app_ideale_f$score_att[df_app_ideale_f$r == "A"])
  
  score_sq_ideale_tot <- sq_id_score_por_tot +
    sq_id_score_def_tot + sq_id_score_cc_tot + sq_id_score_att_tot
  
  score_sq_ideale_tot_nrm <- score_sq_ideale_tot / 11
  
  ## FUNZIONE DI FIT ##
  # variabili considerate:
  #                       - R2
  #                       - numero variabili con p value x
  #                       - rmse 
  #                       - n_coeff
  #                       - percentuale di copertura entro il 20 percento di errore
  
  fit_value <- 
    w_score_por_tot        *    score_por_tot_nrm + 
    w_score_def_tot        *    score_def_tot_nrm + 
    w_score_cc_tot         *    score_cc_tot_nrm +
    w_score_att_tot        *    score_att_tot_nrm +
    w_costo_tot_nrm        *    costo_tot_nrm +
    w_val_sq_ideale        *    score_sq_ideale_tot_nrm
  
  fit_value
}


