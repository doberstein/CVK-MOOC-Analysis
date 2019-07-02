### UTIL
# add conditions to functions
MCKaddConditions <- function(df) {
  if (nrow(df) > 0) {
    df$condition <- NA
    
    # Task1
    df$condition[df$group_id %in% c(371:373)] <- "M"
    df$condition[df$group_id %in% c(368:370)] <- "S"
    
    # Task2
    df$condition[df$group_id %in% c(417:418)] <- "M"
    df$condition[df$group_id %in% c(414:415)] <- "S"
    
    # Task3
    df$condition[df$group_id %in% c(433:434)] <- "M"
    df$condition[df$group_id %in% c(430:431)] <- "S"
    
    # Task4
    df$condition[df$group_id %in% c(439:440)] <- "M"
    df$condition[df$group_id %in% c(437:438)] <- "S"
    
  }
  
  df
}

MCKaddTask <- function(df) {
  if (nrow(df) > 0) {
    df$task <- NA
    
    # Task1
    df$task[df$group_id %in% c(368:373)] <- "task1"
    
    # Task2
    df$task[df$group_id %in% c(414:418)] <- "task2"
    
    # Task3
    df$task[df$group_id %in% c(430:434)] <- "task3"
    
    # Task4
    df$task[df$group_id %in% c(437:440)] <- "task4"
  }
  
  df
}
