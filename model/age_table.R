###
# 1. 연령대변수 기준으로 업종 확인하기
###

source("./arules_data.R", encoding = "UTF-8")

#연령대별로 뽑기
adj_rule_age <- subset(ttRule.pruned, (lhs %in% c("연령대=10대","연령대=20대","연령대=30대",
                                                  "연령대=40대","연령대=50대","연령대=60대이상")))

age.df <- as(adj_rule_age,"data.frame")
age.df$rules <- as.character(age.df$rules)
age.df$lhs <- do.call(rbind,strsplit(age.df$rules,"=>"))[,1]
age.df$rhs <- do.call(rbind,strsplit(age.df$rules,"=>"))[,2]

age.rule <- list()
for(i in 1:length(unique(food$연령대))){
  age.rule[[as.character(unique(food$연령대)[i])]] <- grep(unique(food$연령대)[i],age.df$lhs)
}

age.idx <- lapply(1:length(unique(food$연령대)),function(x){age.df[age.rule[[x]],]})
sa.age <- lapply(1:length(unique(food$연령대)),function(x){as.data.frame(table(age.idx[[x]]$rhs))})

age.table <- matrix(,nrow =length(unique(food$연령대)),ncol=4,
                    dimnames=list(unique(food$연령대),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:6){
  age.table[i,"중국집"] <- ifelse(length(sa.age[[i]]$Freq)==0,0,sa.age[[i]][grep("중국집",sa.age[[i]]$Var1),"Freq"])
  age.table[i,"패밀리레스토랑"] <- ifelse(length(sa.age[[i]]$Freq)==0,0,sa.age[[i]][grep("패밀리레스토랑",sa.age[[i]]$Var1),"Freq"])
  age.table[i,"치킨"] <- ifelse(length(sa.age[[i]]$Freq)==0,0,sa.age[[i]][grep("치킨",sa.age[[i]]$Var1),"Freq"])
  age.table[i,"피자"] <- ifelse(length(sa.age[[i]]$Freq)==0,0,sa.age[[i]][grep("피자",sa.age[[i]]$Var1),"Freq"])
}

age.table[which(is.na(age.table),arr.ind=T)] <- 0

#연령대별 업종 table
# age.table

#연령대별 - 업종 그래프 그리기 위한 작업 
# age.graph <- list()

# for(i in 1:6){
#   for(j in 1:4){
#     age.graph[[paste(row.names(age.table)[i],colnames(age.table)[j])]] <- 
#       apply(data.frame(연령대=row.names(age.table)[i],업종=colnames(age.table)[j]),2,
#            function(x){rep(x,age.table[i,j])})
#   }
# }

# age.graph <- data.frame(do.call(rbind, age.graph))

#연령대별 업종 그래프
# library(ggplot2)
# ggplot(age.graph,aes(x=연령대,group=업종,fill=업종))+geom_bar(position = "dodge")+
#   geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9))+theme_classic()+
#   ggtitle("연령대별 주문한 음식")+theme(plot.title = element_text(size=20, face="bold",hjust = 0.5))

