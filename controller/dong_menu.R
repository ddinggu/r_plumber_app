temp_file <- tempfile()
save_object('dong_data.fst', bucket = "mechureal-realdata", file = temp_file)

interest_dong_model <- read.fst(temp_file)

####
# @params 
# dong_info: string ('연희동')
####

dong.rcmd.fn <- function(dong_info){
  tmp.list.dong <- list()
  sp.dong <- strsplit(interest_dong_model$lhs,",")
  sp_log.dong <-lapply(1:nrow(interest_dong_model),function(x){grepl(dong_info,sp.dong[[x]])})
  for(i in 1:nrow(interest_dong_model)){
    tmp.list.dong[[i]] <- ifelse(sum(sp_log.dong[[i]])==length(strsplit(dong_info,"\\|")[[1]]),interest_dong_model$rhs[i],NA)
  }
  tmp.table.dong <- table(do.call(rbind,tmp.list.dong))
  
  if(length(tmp.table.dong)!=0){ #만약에 규칙이 있다면
    framed_table_for_name <- (as.data.frame(tmp.table.dong)) #업종 결과값을 프린트 해라
    recommend_menu <- rownames(framed_table_for_name)
  
    return(framed_table_for_name$Var1[1])
  } else { #만약 규칙이 없다면
    idx <- which(grepl(dong_info, interest_dong_model$lhs)) 
    table_df <- as.data.frame(table(interest_dong_model$rhs)) # 업종별 테이블을 만들자
    freq_menu <- table_df[which.max(table_df$Freq),1]
    return(freq_menu)
  }
}
