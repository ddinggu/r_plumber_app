library(plumber)

source('controller/recommend_menu.R')
source('controller/dong_menu.R')
source('controller/default_graph.R')

MODEL_VERSION <- '0.0.1'

#* @get /healthcheck
healthcheck <- function() {
  health <- data.frame(
    status = 200,
    version = MODEL_VERSION
  )

  return(health)
}

#* @serializer unboxedJSON	
#* @get /recommend
recommend <- function(userInfo = '여|20대|강남구') {
  recommed_food <- get_recommend_menu(userInfo)
  default_graph_data <- default_graph_fn()
  
  combined_data <- list(
    result = recommed_food,
    table = default_graph_data
  )
  
  return(combined_data)
}

#* @serializer unboxedJSON	
#* @get /dong
dong <- function(dongInfo = '') {
   dong_menu <- dong.rcmd.fn(dongInfo)

   return(list(result = dong_menu))
}