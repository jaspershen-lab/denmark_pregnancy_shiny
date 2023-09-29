load("data/denmark_data")

library(plyr)

variable_info <-
  denmark_data %>% 
  dplyr::select(variable_id, SAM_score, class) %>% 
  dplyr::distinct(variable_id, .keep_all = TRUE) %>% 
  plyr::dlply(.variables = .(class)) %>% 
  purrr::map(function(x){
    x_pos <-
      x %>% 
      dplyr::filter(SAM_score > 0) %>% 
      dplyr::arrange(desc(SAM_score)) %>% 
      head(10)
    
    x_neg <-
      x %>% 
      dplyr::filter(SAM_score < 0) %>% 
      dplyr::arrange(desc(SAM_score)) %>% 
      tail(10)
    
    rbind(x_pos,
          x_neg)
  }) %>% 
  do.call(rbind, .) %>% 
  as.data.frame()
  
denmark_data2 <-
  denmark_data %>% 
  dplyr::filter(variable_id %in% variable_info$variable_id)

save(denmark_data2, file = "data/denmark_data2")
