# 세션 시작시 해당 working directory에서 시작
# .Rprofile.site가 상위에 위치하여 같이 시작된다.

# General options
options(encoding = 'UTF-8') 
options(continue="...")

setwd("/home/ddingu/plumber")

# 세션 시작시 실행되는 함수
.First <- function(){
  cat("\n Welcome!!! \n")
    if(interactive()){
      requiredPackages = c('aws.s3','plumber','ggtern', 'data.table', 'fst')

      for(package in requiredPackages){
        if(!require(package, character.only = TRUE)) install.packages(p)

        library(package, character.only = TRUE)
      }
  }
}

# 세션 종료시 시작되는 함수
.Last <- function(){ 
  # if(interactive()){
  #   hist_file <- Sys.getenv("R_HISTFILE")
  #   if(hist_file=="") hist_file <- "~/.RHistory"
  #   savehistory(hist_file)
  # }

 cat("\n Goodbye!!! \n")
}
