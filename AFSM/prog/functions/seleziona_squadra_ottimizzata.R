
# funzione di team selection
seleziona_squadra_opt <- function(df,
                                  num_gk = 1, 
                                  num_def = 6, 
                                  num_mid = 6, 
                                  num_fwd = 3, 
                                  max_cost = 180,
                                  opt_score = "pg") {
  
  
  # Create vectors to constrain by position
  df$Goalkeeper = ifelse(df$r == "P", 1, 0)
  df$Defender = ifelse(df$r == "D", 1, 0)
  df$Midfielder = ifelse(df$r == "C", 1, 0)
  df$Forward = ifelse(df$r == "A", 1, 0)
  # Create vector to constrain by max number of players allowed per team
  team_constraint = unlist(lapply(unique(df$squadra), function(x, df){
    ifelse(df$squadra==x, 1, 0)
  }, df=df))
  # next we need the constraint directions
  const_dir <- c("=", "=", "=", "=", rep("<=", 21))
  
  # The vector to optimize against
  objective = df[[opt_score]]
  
  # Put the complete matrix together
  const_mat = matrix(c(df$Goalkeeper, df$Defender, df$Midfielder, df$Forward,
                       df$qt_i, team_constraint),
                     nrow=(5 + length(unique(df$squadra))),
                     byrow=TRUE)
  
  const_rhs = c(num_gk, num_def, num_mid, num_fwd, max_cost, rep(2, 20))
  
  dim(const_mat)
  length(objective)
  
  
  # And solve the linear system
  x = lp ("max", objective, const_mat, const_dir, const_rhs, all.bin=TRUE, all.int=TRUE)
  
  
  # team resulting
  df[which(x$solution==1),] %>%
    arrange(desc(Goalkeeper), desc(Defender), 
            desc(Midfielder), desc(Forward), 
            desc(qt_i))
  
  
}