#### Lancio Algoritmo Genetico ####

## Lancio dell'algoritmo con funzione di fitness

## Argomenti

# path_datasets = percorso per i dataset di input
# segmento_modello = <character> inserire segmento da stimare


# return = lista con modello, variabili, beta, r2 e rmse 

## Pacchetti richiesti
require(dplyr)
require(stringr)
require(stringi)
require(GA)

amv_ga_lancia_algoritmo <- function(df_input = NULL,
                                    app_w_score_por_tot= 0.10, 
                                    app_w_score_def_tot= 0.20, 
                                    app_w_score_cc_tot = 0.2, 
                                    app_w_score_att_tot = 0.5,
                                    app_w_costo_tot_nrm = 1,
                                    app_w_val_sq_ideale = 1,
                                    app_formazione_ideale = c(4,3,3)) {
  
  #### versione_1.01 ###
  
  source("Documents/GitHub/data_science/AFSM/prog/01R_GA_fitness.R")
  
  # controllo lunghezza lista
  l_var_list <- length(df_input$nome)
  
  ## LANCIO GA
  
  # modifico la creazione della popolazione 
  ## [NOTE] : cambiare numero di variabili iniziali create in 'string' 
  #           piu' il valore e' alto, piu' difficolta' avra' l'algortimo a convergere
  #           di default il valore e' settato a nvar_iniz
  #           ratio del valore (nvar_iniz) == 
  #           i modelli creati non devono avere piu' di nvar_iniz variabili
  
  n_p = sum(df_input$r == "P")
  n_d = sum(df_input$r == "D")
  n_c = sum(df_input$r == "C")
  n_a = sum(df_input$r == "A")
  
  crea_pop_iniziale <- function(object, 
                                n_p_i = 3,
                                n_d_i = 8,
                                n_c_i = 8,
                                n_a_i = 6,
                                
                                n_p_d = n_p,
                                n_d_d = n_d,
                                n_c_d = n_c,
                                n_a_d = n_a) {
    ## generating a popsize x nbits matrix (population)
    init <- t(replicate(object@popSize, {
      
      c(sample(c(rep(1, n_p_i), rep(0, n_p_d - n_p_i))),
        sample(c(rep(1, n_d_i), rep(0, n_d_d - n_d_i))),
        sample(c(rep(1, n_c_i), rep(0, n_c_d - n_c_i))),
        sample(c(rep(1, n_a_i), rep(0, n_a_d - n_a_i)))
        )
      
    }))
    
    return(init)
  }
  
  # debug(avm_ga_fitness)
  
  # lancio funzione che calcola la fitness del GA
  modello_score_ga = ga(type = "binary", 
                        nBits = l_var_list, 
                        population = crea_pop_iniziale,
                        fitness = avm_ga_fitness, 
                        df_in = df_input, 
                        w_score_por_tot =  app_w_score_por_tot,
                        w_score_def_tot =  app_w_score_def_tot,
                        w_score_cc_tot  =  app_w_score_cc_tot,
                        w_score_att_tot =  app_w_score_att_tot,
                        w_costo_tot_nrm =  app_w_costo_tot_nrm,
                        w_val_sq_ideale =  app_w_val_sq_ideale,
                        
                        formazione_ideale = app_formazione_ideale,
                        
                        popSize = 100, 
                        maxiter = 100, 
                        pcrossover = 0.8, 
                        pmutation = 0.2, 
                        run = 13, 
                        seed = 13)
  
  # estrazione best solution
  var_modello <- modello_score_ga@solution[1,]
  
  # seleziono lista
  selez_var <- var_modello == 1
  
  # seleziono lista
  players <- df_input$nome[selez_var]
  
  df_out <- df_input %>% 
    filter(nome %in% players) %>%
    arrange(r, desc(pg))
  
  # costruisco output come lista
  output <- list(df_output_finale = df_out)
  
  output
}




