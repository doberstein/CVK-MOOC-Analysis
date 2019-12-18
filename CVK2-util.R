### UTIL
# add conditions to functions
addConditionsCVK2 <- function(df) {
  if (nrow(df) > 0) {
    df$condition <- NA
    
    # Task1:
    # M_H:    493 - 498 (mit Heraushebung)
    # M_H_T:  500 - 505 (mit Heraushebung, transparenz)
    # O_H:    486 - 491 (ohne Heraushebung)
    
    # Task2:
    # M_H:    521 - 525 (mit Heraushebung)
    # M_H_T:  526 - 531 (mit Heraushebung, transparenz)
    # O_H:    514 - 519 (ohne Heraushebung)
    
    # Task3:
    # S:      536 - 543 (System)
    # S_R:    544 - 549 (System + Reflexion)
    
    # Task4:
    # M:      553 - 559 (Mensch)
    # SY:     560 - 565 (System)     
    
    
    # Task1
    df$condition[df$group_id %in% c(493:498)] <- "M_H"
    df$condition[df$group_id %in% c(500:505)] <- "M_H_T"
    df$condition[df$group_id %in% c(486:491)] <- "O_H"
    
    # Task2
    df$condition[df$group_id %in% c(521:525)] <- "M_H"
    df$condition[df$group_id %in% c(526:531)] <- "M_H_T"
    df$condition[df$group_id %in% c(514:519)] <- "O_H"
    
    # Task3
    df$condition[df$group_id %in% c(536:543)] <- "S"
    df$condition[df$group_id %in% c(544:549)] <- "S_R"
    
    # Task4
    df$condition[df$group_id %in% c(553:559)] <- "M"
    df$condition[df$group_id %in% c(560:565)] <- "SY"
    
  }
  
  df
}

addTaskCVK2 <- function(df) {
  if (nrow(df) > 0) {
    df$task <- NA
    
    # Task1
    df$task[df$group_id %in% c(486:505)] <- "task1"
    
    # Task2
    df$task[df$group_id %in% c(514:531)] <- "task2"
    
    # Task3
    df$task[df$group_id %in% c(536:549)] <- "task3"
    
    # Task4
    df$task[df$group_id %in% c(553:565)] <- "task4"
  }
  
  df
}
