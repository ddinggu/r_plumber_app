interest_model_df  <- read.fst('data/arules_data.fst')

####
# @params 
# user_info: string ('20대|남자|강남구')
####

get_recommend_menu <- function(user_info){
  tmp.list <- list()
  sp <- strsplit(interest_model_df$lhs,",")
  sp_log <-lapply(1:69,function(x){grepl(user_info,sp[[x]])})
  for(i in 1:69){
    tmp.list[[i]] <- ifelse(sum(sp_log[[i]])==length(strsplit(user_info,"\\|")[[1]]),interest_model_df$rhs[i],NA)
  }
  tmp.table <- table(do.call(rbind,tmp.list)) #모든 입력값을 만족시키는 업종별 테이블
  
  if(length(tmp.table)!=0){ #만약에 규칙이 있다면
    index <- which.max(tmp.table)
    framed_table_for_name <- as.data.frame(tmp.table[index])
    recommend_menu <- rownames(framed_table_for_name)
    
    return(recommend_menu)
    # return(tmp.table) #업종 결과값을 프린트 해라
  } else { #만약 규칙이 없다면
    idx <- which(grepl(user_info,interest_model_df$lhs)) #입력값 별로 |를 적용시켜서
    table_df <- as.data.frame(table(interest_model_df$rhs[idx])) # 업종별 테이블을 만들자

    recommend_menu <- table_df[which.max(table_df$Freq),1]
    return(recommend_menu)
  } # 가장 빈도수가 많은 업종을 보여줘라(규칙이 없을때)
}
