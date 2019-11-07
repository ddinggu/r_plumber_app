set_graph_caption <- function(data, caption, yAxisname) {
  framed_data <- as.data.frame(data)

  framed_data$caption <- caption
  framed_data$yAxisname <- yAxisname

  return(framed_data)
}