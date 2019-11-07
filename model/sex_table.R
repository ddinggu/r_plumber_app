###
#2. 성별변수 기준으로 업종 확인하기
###

source("./arules_data.R", encoding = "UTF-8")

adj_rule_sex <- subset(ttRule.pruned, (lhs %in% c("성별=남","성별=여")))

sex.df <- as(adj_rule_sex,"data.frame")
sex.df$rules <- as.character(sex.df$rules)
sex.df$lhs <- do.call(rbind,strsplit(sex.df$rules,"=>"))[,1]
sex.df$rhs <- do.call(rbind,strsplit(sex.df$rules,"=>"))[,2]

sex.rule <- list()
for(i in 1:length(unique(food$성별))){
  sex.rule[[as.character(unique(food$성별)[i])]] <- grep(unique(food$성별)[i],sex.df$lhs)
}

sex.idx <- lapply(1:length(unique(food$성별)),function(x){sex.df[sex.rule[[x]],]})
sa.sex <- lapply(1:length(unique(food$성별)),function(x){as.data.frame(table(sex.idx[[x]]$rhs))})

sex.table <- matrix(,nrow =length(unique(food$성별)),ncol=4,
                    dimnames=list(unique(food$성별),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:2){
  sex.table[i,"중국집"] <- ifelse(length(sa.sex[[i]]$Freq)==0,0,sa.sex[[i]][grep("중국집",sa.sex[[i]]$Var1),"Freq"])
  sex.table[i,"패밀리레스토랑"] <- ifelse(length(sa.sex[[i]]$Freq)==0,0,sa.sex[[i]][grep("패밀리레스토랑",sa.sex[[i]]$Var1),"Freq"])
  sex.table[i,"치킨"] <- ifelse(length(sa.sex[[i]]$Freq)==0,0,sa.sex[[i]][grep("치킨",sa.sex[[i]]$Var1),"Freq"])
  sex.table[i,"피자"] <- ifelse(length(sa.sex[[i]]$Freq)==0,0,sa.sex[[i]][grep("피자",sa.sex[[i]]$Var1),"Freq"])
}

sex.table[which(is.na(sex.table),arr.ind=T)] <- 0

## 성별-업종 table
# sex.table

## 성별 - 업종 그래프 그리기 위한 작업 
# sex.graph <- list()
# for(i in 1:2){
#  for(j in 1:4){
#     sex.graph[[paste(row.names(sex.table)[i],colnames(sex.table)[j])]] <- 
#      apply(data.frame(성별=row.names(sex.table)[i],업종=colnames(sex.table)[j]),2,
#            function(x){rep(x,sex.table[i,j])})
#   }
# }

# sex.graph <- data.frame(Reduce(rbind, sex.graph))

## 성별 업종 그래프
# ggplot(sex.graph,aes(x=성별,group=업종,fill=업종))+geom_bar(position = "dodge")+
#   geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9))+theme_classic()+
#   ggtitle("성별 주문한 음식")+theme(plot.title = element_text(size=20, face="bold",hjust = 0.5))
