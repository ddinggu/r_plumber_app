getwd()
food <- read.csv("../foodequal.csv", fileEncoding = "euc-kr")

place_split <- split(food,food$시군구)
place_spl <- lapply(1:25,function(x){
  place_split[[x]][,-4]
})
names(place_spl) <- names(place_split)

##구별 규칙생성 함수
rule.fn <- function(x,a,b,c){
  apriori(x,control=list(verbose=F),
          parameter = list(minlen=a,supp=b,conf=c),
          appearance = list(rhs=c("업종=중국집","업종=치킨","업종=피자","업종=패밀리레스토랑"),
                            default="lhs"))
}

#구별 규칙만들기----------------
#1. 노원구(29)
noone_rule <- rule.fn(place_spl$노원구,3,0.00005,0.6)
ttRule.subset <- is.subset(noone_rule, noone_rule, sparse=FALSE)  

ttRule.subset[lower.tri(ttRule.subset, diag=T)] <- NA #상삼각행렬
redundant <- colSums(ttRule.subset, na.rm=T) >= 1
noone.pruned <- noone_rule[!redundant]# 불필요한(중복되는) 규칙을 제거

noone.df <- as(noone.pruned,"data.frame")
noone.df$rules <- as.character(noone.df$rules)
noone.df$lhs <- do.call(rbind,strsplit(noone.df$rules,"=>"))[,1]
noone.df$rhs <- do.call(rbind,strsplit(noone.df$rules,"=>"))[,2]

noone.rule <- list()
for(i in 1:length(unique(place_spl$노원구$읍면동))){
  noone.rule[[as.character(unique(place_spl$노원구$읍면동)[i])]] <- 
    grep(unique(place_spl$노원구$읍면동)[i],noone.df$lhs)
}

noone.idx <- lapply(1:length(unique(place_spl$노원구$읍면동)),function(x){noone.df[noone.rule[[x]],]})
sa.noone <- lapply(1:length(unique(place_spl$노원구$읍면동)),function(x){as.data.frame(table(noone.idx[[x]]$rhs))})

noone.table <- matrix(,nrow =length(unique(place_spl$노원구$읍면동)),ncol=4,
                      dimnames=list(unique(place_spl$노원구$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:nrow(noone.table)){
  noone.table[i,"중국집"] <- ifelse(length(sa.noone[[i]]$Freq)==0,0,sa.noone[[i]][grep("중국집",sa.noone[[i]]$Var1),"Freq"])
  noone.table[i,"패밀리레스토랑"] <- ifelse(length(sa.noone[[i]]$Freq)==0,0,sa.noone[[i]][grep("패밀리레스토랑",sa.noone[[i]]$Var1),"Freq"])
  noone.table[i,"치킨"] <- ifelse(length(sa.noone[[i]]$Freq)==0,0,sa.noone[[i]][grep("치킨",sa.noone[[i]]$Var1),"Freq"])
  noone.table[i,"피자"] <- ifelse(length(sa.noone[[i]]$Freq)==0,0,sa.noone[[i]][grep("피자",sa.noone[[i]]$Var1),"Freq"])
}
noone.table[which(is.na(noone.table),arr.ind=T)] <- 0

#노원구 동별 업종
noone.table

#---------------------
#2.도봉구(39)
dobong_rule <- rule.fn(place_spl$도봉구,3,0.0005,0.6)

#중복규칙 제거
ttRule.subset <- is.subset(dobong_rule, dobong_rule, sparse=FALSE)  
ttRule.subset[lower.tri(ttRule.subset, diag=T)] <- NA
redundant <- colSums(ttRule.subset, na.rm=T) >= 1
dobong.pruned <- dobong_rule[!redundant]

dobong.df <- as(dobong.pruned,"data.frame")
dobong.df$rules <- as.character(dobong.df$rules)
dobong.df$lhs <- do.call(rbind,strsplit(dobong.df$rules,"=>"))[,1]
dobong.df$rhs <- do.call(rbind,strsplit(dobong.df$rules,"=>"))[,2]

dobong.rule <- list()
for(i in 1:length(unique(place_spl$도봉구$읍면동))){
  dobong.rule[[as.character(unique(place_spl$도봉구$읍면동)[i])]] <- 
    grep(unique(place_spl$도봉구$읍면동)[i],dobong.df$lhs)
}

dobong.idx <- lapply(1:length(unique(place_spl$도봉구$읍면동)),function(x){dobong.df[dobong.rule[[x]],]})
sa.dobong <- lapply(1:length(unique(place_spl$도봉구$읍면동)),function(x){as.data.frame(table(dobong.idx[[x]]$rhs))})

dobong.table <- matrix(,nrow =length(unique(place_spl$도봉구$읍면동)),ncol=4,
                       dimnames=list(unique(place_spl$도봉구$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:nrow(dobong.table)){
  dobong.table[i,"중국집"] <- ifelse(length(sa.dobong[[i]]$Freq)==0,0,sa.dobong[[i]][grep("중국집",sa.dobong[[i]]$Var1),"Freq"])
  dobong.table[i,"패밀리레스토랑"] <- ifelse(length(sa.dobong[[i]]$Freq)==0,0,sa.dobong[[i]][grep("패밀리레스토랑",sa.dobong[[i]]$Var1),"Freq"])
  dobong.table[i,"치킨"] <- ifelse(length(sa.dobong[[i]]$Freq)==0,0,sa.dobong[[i]][grep("치킨",sa.dobong[[i]]$Var1),"Freq"])
  dobong.table[i,"피자"] <- ifelse(length(sa.dobong[[i]]$Freq)==0,0,sa.dobong[[i]][grep("피자",sa.dobong[[i]]$Var1),"Freq"])
}
dobong.table[which(is.na(dobong.table),arr.ind=T)] <- 0

#도봉구 동별 업종
dobong.table

#----------------
#3. 동대문구(33)
dongdaemun_rule <- rule.fn(place_spl$동대문구,3,0.0007,0.6)

#중복규칙 제거
ttRule.subset <- is.subset(dongdaemun_rule, dongdaemun_rule, sparse=FALSE)  
ttRule.subset[lower.tri(ttRule.subset, diag=T)] <- NA
redundant <- colSums(ttRule.subset, na.rm=T) >= 1
dongdaemun.pruned <- dongdaemun_rule[!redundant]

dongdaemun.df <- as(dongdaemun.pruned,"data.frame")
dongdaemun.df$rules <- as.character(dongdaemun.df$rules)
dongdaemun.df$lhs <- do.call(rbind,strsplit(dongdaemun.df$rules,"=>"))[,1]
dongdaemun.df$rhs <- do.call(rbind,strsplit(dongdaemun.df$rules,"=>"))[,2]

dongdaemun.rule <- list()
for(i in 1:length(unique(place_spl$동대문구$읍면동))){
  dongdaemun.rule[[as.character(unique(place_spl$동대문구$읍면동)[i])]] <- 
    grep(paste0("읍면동=",unique(place_spl$동대문구$읍면동)[i]),dongdaemun.df$lhs)
}

dongdaemun.idx <- lapply(1:length(unique(place_spl$동대문구$읍면동)),function(x){dongdaemun.df[dongdaemun.rule[[x]],]})
sa.dongdaemun <- lapply(1:length(unique(place_spl$동대문구$읍면동)),function(x){as.data.frame(table(dongdaemun.idx[[x]]$rhs))})

dongdaemun.table <- matrix(,nrow =length(unique(place_spl$동대문구$읍면동)),ncol=4,
                           dimnames=list(unique(place_spl$동대문구$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:nrow(dongdaemun.table)){
  dongdaemun.table[i,"중국집"] <- ifelse(length(sa.dongdaemun[[i]]$Freq)==0,0,sa.dongdaemun[[i]][grep("중국집",sa.dongdaemun[[i]]$Var1),"Freq"])
  dongdaemun.table[i,"패밀리레스토랑"] <- ifelse(length(sa.dongdaemun[[i]]$Freq)==0,0,sa.dongdaemun[[i]][grep("패밀리레스토랑",sa.dongdaemun[[i]]$Var1),"Freq"])
  dongdaemun.table[i,"치킨"] <- ifelse(length(sa.dongdaemun[[i]]$Freq)==0,0,sa.dongdaemun[[i]][grep("치킨",sa.dongdaemun[[i]]$Var1),"Freq"])
  dongdaemun.table[i,"피자"] <- ifelse(length(sa.dongdaemun[[i]]$Freq)==0,0,sa.dongdaemun[[i]][grep("피자",sa.dongdaemun[[i]]$Var1),"Freq"])
}
dongdaemun.table[which(is.na(dongdaemun.table),arr.ind=T)] <- 0

#동대문구 동별 업종
dongdaemun.table

#----------------
#4. 동작구(28)
dongjak_rule <- rule.fn(place_spl$동작구,3,0.0002,0.6)

#중복규칙 제거
ttRule.subset <- is.subset(dongjak_rule, dongjak_rule, sparse=FALSE)  
ttRule.subset[lower.tri(ttRule.subset, diag=T)] <- NA
redundant <- colSums(ttRule.subset, na.rm=T) >= 1
dongjak.pruned <- dongjak_rule[!redundant]

dongjak.df <- as(dongjak.pruned,"data.frame")
dongjak.df$rules <- as.character(dongjak.df$rules)
dongjak.df$lhs <- do.call(rbind,strsplit(dongjak.df$rules,"=>"))[,1]
dongjak.df$rhs <- do.call(rbind,strsplit(dongjak.df$rules,"=>"))[,2]

dongjak.rule <- list()
for(i in 1:length(unique(place_spl$동작구$읍면동))){
  dongjak.rule[[as.character(unique(place_spl$동작구$읍면동)[i])]] <- 
    grep(paste0("읍면동=",unique(place_spl$동작구$읍면동)[i]),dongjak.df$lhs)
}

dongjak.idx <- lapply(1:length(unique(place_spl$동작구$읍면동)),function(x){dongjak.df[dongjak.rule[[x]],]})
sa.dongjak <- lapply(1:length(unique(place_spl$동작구$읍면동)),function(x){as.data.frame(table(dongjak.idx[[x]]$rhs))})

dongjak.table <- matrix(,nrow =length(unique(place_spl$동작구$읍면동)),ncol=4,
                        dimnames=list(unique(place_spl$동작구$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:nrow(dongjak.table)){
  dongjak.table[i,"중국집"] <- ifelse(length(sa.dongjak[[i]]$Freq)==0,0,sa.dongjak[[i]][grep("중국집",sa.dongjak[[i]]$Var1),"Freq"])
  dongjak.table[i,"패밀리레스토랑"] <- ifelse(length(sa.dongjak[[i]]$Freq)==0,0,sa.dongjak[[i]][grep("패밀리레스토랑",sa.dongjak[[i]]$Var1),"Freq"])
  dongjak.table[i,"치킨"] <- ifelse(length(sa.dongjak[[i]]$Freq)==0,0,sa.dongjak[[i]][grep("치킨",sa.dongjak[[i]]$Var1),"Freq"])
  dongjak.table[i,"피자"] <- ifelse(length(sa.dongjak[[i]]$Freq)==0,0,sa.dongjak[[i]][grep("피자",sa.dongjak[[i]]$Var1),"Freq"])
}
dongjak.table[which(is.na(dongjak.table),arr.ind=T)] <- 0

#동작구 동별 업종
dongjak.table

#----------------
#5. 마포구(50)_패밀리 밖에 안남음..
mapo_rule <- rule.fn(place_spl$마포구,3,0.005,0.6)

#중복규칙 제거
ttRule.subset <- is.subset(mapo_rule, mapo_rule, sparse=FALSE)  
ttRule.subset[lower.tri(ttRule.subset, diag=T)] <- NA
redundant <- colSums(ttRule.subset, na.rm=T) >= 1
mapo.pruned <- mapo_rule[!redundant]

mapo.df <- as(mapo.pruned,"data.frame")
mapo.df$rules <- as.character(mapo.df$rules)
mapo.df$lhs <- do.call(rbind,strsplit(mapo.df$rules,"=>"))[,1]
mapo.df$rhs <- do.call(rbind,strsplit(mapo.df$rules,"=>"))[,2]

mapo.rule <- list()
for(i in 1:length(unique(place_spl$마포구$읍면동))){
  mapo.rule[[as.character(unique(place_spl$마포구$읍면동)[i])]] <- 
    grep(paste0("읍면동=",unique(place_spl$마포구$읍면동)[i]),mapo.df$lhs)
}

mapo.idx <- lapply(1:length(unique(place_spl$마포구$읍면동)),function(x){mapo.df[mapo.rule[[x]],]})
sa.mapo <- lapply(1:length(unique(place_spl$마포구$읍면동)),function(x){as.data.frame(table(mapo.idx[[x]]$rhs))})

mapo.table <- matrix(,nrow =length(unique(place_spl$마포구$읍면동)),ncol=4,
                     dimnames=list(unique(place_spl$마포구$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:nrow(mapo.table)){
  mapo.table[i,"중국집"] <- ifelse(length(sa.mapo[[i]]$Freq)==0,0,sa.mapo[[i]][grep("중국집",sa.mapo[[i]]$Var1),"Freq"])
  mapo.table[i,"패밀리레스토랑"] <- ifelse(length(sa.mapo[[i]]$Freq)==0,0,sa.mapo[[i]][grep("패밀리레스토랑",sa.mapo[[i]]$Var1),"Freq"])
  mapo.table[i,"치킨"] <- ifelse(length(sa.mapo[[i]]$Freq)==0,0,sa.mapo[[i]][grep("치킨",sa.mapo[[i]]$Var1),"Freq"])
  mapo.table[i,"피자"] <- ifelse(length(sa.mapo[[i]]$Freq)==0,0,sa.mapo[[i]][grep("피자",sa.mapo[[i]]$Var1),"Freq"])
}
mapo.table[which(is.na(mapo.table),arr.ind=T)] <- 0

#마포구 동별 업종
mapo.table

#----------------
#6. 서대문구(56)
sedaemun_rule <- rule.fn(place_spl$서대문구,3,0.0004,0.6)

#중복규칙 제거
ttRule.subset <- is.subset(sedaemun_rule, sedaemun_rule, sparse=FALSE)  
ttRule.subset[lower.tri(ttRule.subset, diag=T)] <- NA
redundant <- colSums(ttRule.subset, na.rm=T) >= 1
sedaemun.pruned <- sedaemun_rule[!redundant]

sedaemun.df <- as(sedaemun.pruned,"data.frame")
sedaemun.df$rules <- as.character(sedaemun.df$rules)
sedaemun.df$lhs <- do.call(rbind,strsplit(sedaemun.df$rules,"=>"))[,1]
sedaemun.df$rhs <- do.call(rbind,strsplit(sedaemun.df$rules,"=>"))[,2]

sedaemun.rule <- list()
for(i in 1:length(unique(place_spl$서대문구$읍면동))){
  sedaemun.rule[[as.character(unique(place_spl$서대문구$읍면동)[i])]] <- 
    grep(paste0("읍면동=",unique(place_spl$서대문구$읍면동)[i]),sedaemun.df$lhs)
}

sedaemun.idx <- lapply(1:length(unique(place_spl$서대문구$읍면동)),function(x){sedaemun.df[sedaemun.rule[[x]],]})
sa.sedaemun <- lapply(1:length(unique(place_spl$서대문구$읍면동)),function(x){as.data.frame(table(sedaemun.idx[[x]]$rhs))})

sedaemun.table <- matrix(,nrow =length(unique(place_spl$서대문구$읍면동)),ncol=4,
                         dimnames=list(unique(place_spl$서대문구$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:nrow(sedaemun.table)){
  sedaemun.table[i,"중국집"] <- ifelse(length(sa.sedaemun[[i]]$Freq)==0,0,sa.sedaemun[[i]][grep("중국집",sa.sedaemun[[i]]$Var1),"Freq"])
  sedaemun.table[i,"패밀리레스토랑"] <- ifelse(length(sa.sedaemun[[i]]$Freq)==0,0,sa.sedaemun[[i]][grep("패밀리레스토랑",sa.sedaemun[[i]]$Var1),"Freq"])
  sedaemun.table[i,"치킨"] <- ifelse(length(sa.sedaemun[[i]]$Freq)==0,0,sa.sedaemun[[i]][grep("치킨",sa.sedaemun[[i]]$Var1),"Freq"])
  sedaemun.table[i,"피자"] <- ifelse(length(sa.sedaemun[[i]]$Freq)==0,0,sa.sedaemun[[i]][grep("피자",sa.sedaemun[[i]]$Var1),"Freq"])
}
sedaemun.table[which(is.na(sedaemun.table),arr.ind=T)] <- 0

#서대문구 동별 업종
sedaemun.table

#----------------
#7. 서초구(37)_그대로
seucho_rule <- rule.fn(place_spl$서초구,3,0.0004,0.6)

#중복규칙 제거
ttRule.subset <- is.subset(seucho_rule, seucho_rule, sparse=FALSE)  
ttRule.subset[lower.tri(ttRule.subset, diag=T)] <- NA
redundant <- colSums(ttRule.subset, na.rm=T) >= 1
seucho.pruned <- seucho_rule[!redundant]

seucho.df <- as(seucho.pruned,"data.frame")
seucho.df$rules <- as.character(seucho.df$rules)
seucho.df$lhs <- do.call(rbind,strsplit(seucho.df$rules,"=>"))[,1]
seucho.df$rhs <- do.call(rbind,strsplit(seucho.df$rules,"=>"))[,2]

seucho.rule <- list()
for(i in 1:length(unique(place_spl$서초구$읍면동))){
  seucho.rule[[as.character(unique(place_spl$서초구$읍면동)[i])]] <- 
    grep(paste0("읍면동=",unique(place_spl$서초구$읍면동)[i]),seucho.df$lhs)
}

seucho.idx <- lapply(1:length(unique(place_spl$서초구$읍면동)),function(x){seucho.df[seucho.rule[[x]],]})
sa.seucho <- lapply(1:length(unique(place_spl$서초구$읍면동)),function(x){as.data.frame(table(seucho.idx[[x]]$rhs))})

seucho.table <- matrix(,nrow =length(unique(place_spl$서초구$읍면동)),ncol=4,
                       dimnames=list(unique(place_spl$서초구$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:nrow(seucho.table)){
  seucho.table[i,"중국집"] <- ifelse(length(sa.seucho[[i]]$Freq)==0,0,sa.seucho[[i]][grep("중국집",sa.seucho[[i]]$Var1),"Freq"])
  seucho.table[i,"패밀리레스토랑"] <- ifelse(length(sa.seucho[[i]]$Freq)==0,0,sa.seucho[[i]][grep("패밀리레스토랑",sa.seucho[[i]]$Var1),"Freq"])
  seucho.table[i,"치킨"] <- ifelse(length(sa.seucho[[i]]$Freq)==0,0,sa.seucho[[i]][grep("치킨",sa.seucho[[i]]$Var1),"Freq"])
  seucho.table[i,"피자"] <- ifelse(length(sa.seucho[[i]]$Freq)==0,0,sa.seucho[[i]][grep("피자",sa.seucho[[i]]$Var1),"Freq"])
}
seucho.table[which(is.na(seucho.table),arr.ind=T)] <- 0

#서초구 동별 업종
seucho.table

#----------------
#8. 성동구(35)
seongdong <- place_spl$성동구
seongdong$읍면동 <- ifelse(grepl("금호",as.character(seongdong$읍면동))==T,"금호동",as.character(seongdong$읍면동))
seongdong$읍면동 <- as.factor(seongdong$읍면동)
seongdong_rule <- rule.fn(seongdong,3,0.0004,0.6)

#중복규칙 제거
ttRule.subset <- is.subset(seongdong_rule, seongdong_rule, sparse=FALSE)  
ttRule.subset[lower.tri(ttRule.subset, diag=T)] <- NA
redundant <- colSums(ttRule.subset, na.rm=T) >= 1
seongdong.pruned <- seongdong_rule[!redundant]

seongdong.df <- as(seongdong.pruned,"data.frame")
seongdong.df$rules <- as.character(seongdong.df$rules)
seongdong.df$lhs <- do.call(rbind,strsplit(seongdong.df$rules,"=>"))[,1]
seongdong.df$rhs <- do.call(rbind,strsplit(seongdong.df$rules,"=>"))[,2]

seongdong.rule <- list()
for(i in 1:length(unique(seongdong$읍면동))){
  seongdong.rule[[as.character(unique(seongdong$읍면동)[i])]] <- 
    grep(paste0("읍면동=",unique(seongdong$읍면동)[i]),seongdong.df$lhs)
}

seongdong.idx <- lapply(1:length(unique(seongdong$읍면동)),function(x){seongdong.df[seongdong.rule[[x]],]})
sa.seongdong <- lapply(1:length(unique(seongdong$읍면동)),function(x){as.data.frame(table(seongdong.idx[[x]]$rhs))})

seongdong.table <- matrix(,nrow =length(unique(seongdong$읍면동)),ncol=4,
                          dimnames=list(unique(seongdong$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:nrow(seongdong.table)){
  seongdong.table[i,"중국집"] <- ifelse(length(sa.seongdong[[i]]$Freq)==0,0,sa.seongdong[[i]][grep("중국집",sa.seongdong[[i]]$Var1),"Freq"])
  seongdong.table[i,"패밀리레스토랑"] <- ifelse(length(sa.seongdong[[i]]$Freq)==0,0,sa.seongdong[[i]][grep("패밀리레스토랑",sa.seongdong[[i]]$Var1),"Freq"])
  seongdong.table[i,"치킨"] <- ifelse(length(sa.seongdong[[i]]$Freq)==0,0,sa.seongdong[[i]][grep("치킨",sa.seongdong[[i]]$Var1),"Freq"])
  seongdong.table[i,"피자"] <- ifelse(length(sa.seongdong[[i]]$Freq)==0,0,sa.seongdong[[i]][grep("피자",sa.seongdong[[i]]$Var1),"Freq"])
}
seongdong.table[which(is.na(seongdong.table),arr.ind=T)] <- 0

#성동구 동별 업종
seongdong.table

#---------------------
#9. 강동구(37개)
gd <- place_spl$강동구
gd.all <- rule.fn(gd,3,0.0001,0.6)

# 불필요한(중복되는) 규칙을 제거
remove.gd <- is.subset(gd.all, gd.all, sparse=FALSE)
remove.gd[lower.tri(remove.gd, diag=T)] <- NA
redundant.gd <- colSums(remove.gd, na.rm=T) >= 1
gd.pruned <- gd.all[!redundant.gd]

#강동구 : 동별로 추출 
gd.df <- as(gd.pruned,"data.frame")
gd.df$rules <- as.character(gd.df$rules)
gd.df$lhs <- do.call(rbind,strsplit(gd.df$rules,"=>"))[,1]
gd.df$rhs <- do.call(rbind,strsplit(gd.df$rules,"=>"))[,2]

gd.rules <- list()
for(i in 1:length(unique(gd$읍면동))){
  gd.rules[[as.character(unique(gd$읍면동)[i])]] <- grep(unique(gd$읍면동)[i],gd.df$lhs)
}

gd.idx <- lapply(1:length(unique(gd$읍면동)),function(x){gd.df[gd.rules[[x]],]})
sa.gd <- lapply(1:length(unique(gd$읍면동)),function(x){as.data.frame(table(gd.idx[[x]]$rhs))})

gangdong.table <- matrix(,nrow =length(unique(gd$읍면동)),ncol=4,
                         dimnames=list(unique(gd$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:9){
  gangdong.table[i,"중국집"] <- ifelse(length(sa.gd[[i]]$Freq)==0,0,sa.gd[[i]][grep("중국집",sa.gd[[i]]$Var1),"Freq"])
  gangdong.table[i,"패밀리레스토랑"] <- ifelse(length(sa.gd[[i]]$Freq)==0,0,sa.gd[[i]][grep("패밀리레스토랑",sa.gd[[i]]$Var1),"Freq"])
  gangdong.table[i,"치킨"] <- ifelse(length(sa.gd[[i]]$Freq)==0,0,sa.gd[[i]][grep("치킨",sa.gd[[i]]$Var1),"Freq"])
  gangdong.table[i,"피자"] <- ifelse(length(sa.gd[[i]]$Freq)==0,0,sa.gd[[i]][grep("피자",sa.gd[[i]]$Var1),"Freq"])
}
gangdong.table[which(is.na(gangdong.table),arr.ind=T)] <- 0

#강동구 테이블
gangdong.table

#---------------------
#10. 강남구(41개)
gn <- place_spl$강남구
gn.all <- rule.fn(gn,3,0.0001,0.6)

# 불필요한(중복되는) 규칙을 제거
remove.gn <- is.subset(gn.all, gn.all, sparse=FALSE)  
remove.gn[lower.tri(remove.gn, diag=T)] <- NA
redundant.gn <- colSums(remove.gn, na.rm=T) >= 1
gn.pruned <- gn.all[!redundant.gn]

#강남구 : 동별로 추출 
gn.df <- as(gn.pruned,"data.frame")
gn.df$rules <- as.character(gn.df$rules)
gn.df$lhs <- do.call(rbind,strsplit(gn.df$rules,"=>"))[,1]
gn.df$rhs <- do.call(rbind,strsplit(gn.df$rules,"=>"))[,2]

gn.rules <- list()
for(i in 1:length(unique(gn$읍면동))){
  gn.rules[[as.character(unique(gn$읍면동)[i])]] <- grep(unique(gn$읍면동)[i],gn.df$lhs)
}

gn.idx <- lapply(1:length(unique(gn$읍면동)),function(x){gn.df[gn.rules[[x]],]})
sa.gn <- lapply(1:length(unique(gn$읍면동)),function(x){as.data.frame(table(gn.idx[[x]]$rhs))})

gangnam.table <- matrix(,nrow =length(unique(gn$읍면동)),ncol=4,
                        dimnames=list(unique(gn$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:length(unique(gn$읍면동))){
  gangnam.table[i,"중국집"] <- ifelse(length(sa.gn[[i]]$Freq)==0,0,sa.gn[[i]][grep("중국집",sa.gn[[i]]$Var1),"Freq"])
  gangnam.table[i,"패밀리레스토랑"] <- ifelse(length(sa.gn[[i]]$Freq)==0,0,sa.gn[[i]][grep("패밀리레스토랑",sa.gn[[i]]$Var1),"Freq"])
  gangnam.table[i,"치킨"] <- ifelse(length(sa.gn[[i]]$Freq)==0,0,sa.gn[[i]][grep("치킨",sa.gn[[i]]$Var1),"Freq"])
  gangnam.table[i,"피자"] <- ifelse(length(sa.gn[[i]]$Freq)==0,0,sa.gn[[i]][grep("피자",sa.gn[[i]]$Var1),"Freq"])
}
gangnam.table[which(is.na(gangnam.table),arr.ind=T)] <- 0

#강남구 테이블
gangnam.table

#-----------------
#11. 강북구(34)
gsth <- place_spl$강북구
gsth.all <- rule.fn(gsth, 3, 0.00005, 0.6)

#중복규칙 제거
remove.gsth <- is.subset(gsth.all, gsth.all, sparse=FALSE)  
remove.gsth[lower.tri(remove.gsth, diag=T)] <- NA
redundant.gsth <- colSums(remove.gsth, na.rm=T) >= 1
gsth.pruned <- gsth.all[!redundant.gsth] 

#강북구 : 동별로 추출 
gsth.df <- as(gsth.pruned,"data.frame")
gsth.df$rules <- as.character(gsth.df$rules)
gsth.df$lhs <- do.call(rbind,strsplit(gsth.df$rules,"=>"))[,1]
gsth.df$rhs <- do.call(rbind,strsplit(gsth.df$rules,"=>"))[,2]

gsth.rules <- list()
for(i in 1:length(unique(gsth$읍면동))){
  gsth.rules[[as.character(unique(gsth$읍면동)[i])]] <- grep(unique(gsth$읍면동)[i],gsth.df$lhs)
}

gsth.idx <- lapply(1:length(unique(gsth$읍면동)),function(x){gsth.df[gsth.rules[[x]],]})
sa.gsth <- lapply(1:length(unique(gsth$읍면동)),function(x){as.data.frame(table(gsth.idx[[x]]$rhs))})

gangbuk.table <- matrix(,nrow =length(unique(gsth$읍면동)),ncol=4,
                        dimnames=list(unique(gsth$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:length(unique(gsth$읍면동))){
  gangbuk.table[i,"중국집"] <- ifelse(length(sa.gsth[[i]]$Freq)==0,0,sa.gsth[[i]][grep("중국집",sa.gsth[[i]]$Var1),"Freq"])
  gangbuk.table[i,"패밀리레스토랑"] <- ifelse(length(sa.gsth[[i]]$Freq)==0,0,sa.gsth[[i]][grep("패밀리레스토랑",sa.gsth[[i]]$Var1),"Freq"])
  gangbuk.table[i,"치킨"] <- ifelse(length(sa.gsth[[i]]$Freq)==0,0,sa.gsth[[i]][grep("치킨",sa.gsth[[i]]$Var1),"Freq"])
  gangbuk.table[i,"피자"] <- ifelse(length(sa.gsth[[i]]$Freq)==0,0,sa.gsth[[i]][grep("피자",sa.gsth[[i]]$Var1),"Freq"])
}
gangbuk.table[which(is.na(gangbuk.table),arr.ind=T)] <- 0

#강북구 테이블
gangbuk.table

#----------------------
#12. 강서구(41)
gs <- place_spl$강서구
gs.all <- rule.fn(gs, 3, 0.00007, 0.6)

# 불필요한(중복되는) 규칙을 제거
remove.gs <- is.subset(gs.all, gs.all, sparse=FALSE)  
remove.gs[lower.tri(remove.gs, diag=T)] <- NA
redundant.gs <- colSums(remove.gs, na.rm=T) >= 1
gs.pruned <- gs.all[!redundant.gs]

#강서구 : 동별로 추출 
gs.df <- as(gs.pruned,"data.frame")
gs.df$rules <- as.character(gs.df$rules)
gs.df$lhs <- do.call(rbind,strsplit(gs.df$rules,"=>"))[,1]
gs.df$rhs <- do.call(rbind,strsplit(gs.df$rules,"=>"))[,2]

gs.rules <- list()
for(i in 1:length(unique(gs$읍면동))){
  gs.rules[[as.character(unique(gs$읍면동)[i])]] <- grep(unique(gs$읍면동)[i],gs.df$lhs)
}

gs.idx <- lapply(1:length(unique(gs$읍면동)),function(x){gs.df[gs.rules[[x]],]})
sa.gs <- lapply(1:length(unique(gs$읍면동)),function(x){as.data.frame(table(gs.idx[[x]]$rhs))})

gangseo.table <- matrix(,nrow =length(unique(gs$읍면동)),ncol=4,
                        dimnames=list(unique(gs$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:length(unique(gs$읍면동))){
  gangseo.table[i,"중국집"] <- ifelse(length(sa.gs[[i]]$Freq)==0,0,sa.gs[[i]][grep("중국집",sa.gs[[i]]$Var1),"Freq"])
  gangseo.table[i,"패밀리레스토랑"] <- ifelse(length(sa.gs[[i]]$Freq)==0,0,sa.gs[[i]][grep("패밀리레스토랑",sa.gs[[i]]$Var1),"Freq"])
  gangseo.table[i,"치킨"] <- ifelse(length(sa.gs[[i]]$Freq)==0,0,sa.gs[[i]][grep("치킨",sa.gs[[i]]$Var1),"Freq"])
  gangseo.table[i,"피자"] <- ifelse(length(sa.gs[[i]]$Freq)==0,0,sa.gs[[i]][grep("피자",sa.gs[[i]]$Var1),"Freq"])
}
gangseo.table[which(is.na(gangseo.table),arr.ind=T)] <- 0

#강서구 테이블
gangseo.table

#-----------------------
#13.관악구(31)
gw <- place_spl$관악구
gw.all <- rule.fn(gw, 3, 0.0001, 0.6)

# 불필요한(중복되는) 규칙을 제거
remove.gw <- is.subset(gw.all, gw.all, sparse=FALSE)  
remove.gw[lower.tri(remove.gw, diag=T)] <- NA
redundant.gw <- colSums(remove.gw, na.rm=T) >= 1
gw.pruned <- gw.all[!redundant.gw] 

#관악구 : 동별로 추출 
gw.df <- as(gw.pruned,"data.frame")
gw.df$rules <- as.character(gw.df$rules)
gw.df$lhs <- do.call(rbind,strsplit(gw.df$rules,"=>"))[,1]
gw.df$rhs <- do.call(rbind,strsplit(gw.df$rules,"=>"))[,2]

gw.rules <- list()
for(i in 1:length(unique(gw$읍면동))){
  gw.rules[[as.character(unique(gw$읍면동)[i])]] <- grep(unique(gw$읍면동)[i],gw.df$lhs)
}

gw.idx <- lapply(1:length(unique(gw$읍면동)),function(x){gw.df[gw.rules[[x]],]})
sa.gw <- lapply(1:length(unique(gw$읍면동)),function(x){as.data.frame(table(gw.idx[[x]]$rhs))})

gwanak.table <- matrix(,nrow =length(unique(gw$읍면동)),ncol=4,
                       dimnames=list(unique(gw$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:length(unique(gw$읍면동))){
  gwanak.table[i,"중국집"] <- ifelse(length(sa.gw[[i]]$Freq)==0,0,sa.gw[[i]][grep("중국집",sa.gw[[i]]$Var1),"Freq"])
  gwanak.table[i,"패밀리레스토랑"] <- ifelse(length(sa.gw[[i]]$Freq)==0,0,sa.gw[[i]][grep("패밀리레스토랑",sa.gw[[i]]$Var1),"Freq"])
  gwanak.table[i,"치킨"] <- ifelse(length(sa.gw[[i]]$Freq)==0,0,sa.gw[[i]][grep("치킨",sa.gw[[i]]$Var1),"Freq"])
  gwanak.table[i,"피자"] <- ifelse(length(sa.gw[[i]]$Freq)==0,0,sa.gw[[i]][grep("피자",sa.gw[[i]]$Var1),"Freq"])
}
gwanak.table[which(is.na(gwanak.table),arr.ind=T)] <- 0

#관악구 테이블
gwanak.table

#------------------
#14.광진구(61)
gj <- place_spl$광진구
gj.all <- rule.fn(gj, 3, 0.001, 0.63)

# 불필요한(중복되는) 규칙을 제거
remove.gj <- is.subset(gj.all, gj.all, sparse=FALSE)  
remove.gj[lower.tri(remove.gj, diag=T)] <- NA
redundant.gj <- colSums(remove.gj, na.rm=T) >= 1
gj.pruned <- gj.all[!redundant.gj] 

#광진구 : 동별로 추출 
gj.df <- as(gj.pruned,"data.frame")
gj.df$rules <- as.character(gj.df$rules)
gj.df$lhs <- do.call(rbind,strsplit(gj.df$rules,"=>"))[,1]
gj.df$rhs <- do.call(rbind,strsplit(gj.df$rules,"=>"))[,2]

gj.rules <- list()
for(i in 1:length(unique(gj$읍면동))){
  gj.rules[[as.character(unique(gj$읍면동)[i])]] <- grep(unique(gj$읍면동)[i],gj.df$lhs)
}

gj.idx <- lapply(1:length(unique(gj$읍면동)),function(x){gj.df[gj.rules[[x]],]})
sa.gj <- lapply(1:length(unique(gj$읍면동)),function(x){as.data.frame(table(gj.idx[[x]]$rhs))})

gwangjin.table <- matrix(,nrow =length(unique(gj$읍면동)),ncol=4,
                         dimnames=list(unique(gj$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:length(unique(gj$읍면동))){
  gwangjin.table[i,"중국집"] <- ifelse(length(sa.gj[[i]]$Freq)==0,0,sa.gj[[i]][grep("중국집",sa.gj[[i]]$Var1),"Freq"])
  gwangjin.table[i,"패밀리레스토랑"] <- ifelse(length(sa.gj[[i]]$Freq)==0,0,sa.gj[[i]][grep("패밀리레스토랑",sa.gj[[i]]$Var1),"Freq"])
  gwangjin.table[i,"치킨"] <- ifelse(length(sa.gj[[i]]$Freq)==0,0,sa.gj[[i]][grep("치킨",sa.gj[[i]]$Var1),"Freq"])
  gwangjin.table[i,"피자"] <- ifelse(length(sa.gj[[i]]$Freq)==0,0,sa.gj[[i]][grep("피자",sa.gj[[i]]$Var1),"Freq"])
}
gwangjin.table[which(is.na(gwangjin.table),arr.ind=T)] <- 0

#광진구 테이블
gwangjin.table

#----------------
#15.구로구
gu <- place_spl$구로구
gu.all <- rule.fn(gu, 3, 0.00005, 0.6)

# 불필요한(중복되는) 규칙을 제거
remove.gu <- is.subset(gu.all, gu.all, sparse=FALSE)  
remove.gu[lower.tri(remove.gu, diag=T)] <- NA
redundant.gu <- colSums(remove.gu, na.rm=T) >= 1
gu.pruned <- gu.all[!redundant.gu] 

#구로구 : 동별로 추출 
gu.df <- as(gu.pruned,"data.frame")
gu.df$rules <- as.character(gu.df$rules)
gu.df$lhs <- do.call(rbind,strsplit(gu.df$rules,"=>"))[,1]
gu.df$rhs <- do.call(rbind,strsplit(gu.df$rules,"=>"))[,2]

gu.rules <- list()
for(i in 1:length(unique(gu$읍면동))){
  gu.rules[[as.character(unique(gu$읍면동)[i])]] <- grep(unique(gu$읍면동)[i],gu.df$lhs)
}

gu.idx <- lapply(1:length(unique(gu$읍면동)),function(x){gu.df[gu.rules[[x]],]})
sa.gu <- lapply(1:length(unique(gu$읍면동)),function(x){as.data.frame(table(gu.idx[[x]]$rhs))})

guro.table <- matrix(,nrow =length(unique(gu$읍면동)),ncol=4,
                     dimnames=list(unique(gu$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:length(unique(gu$읍면동))){
  guro.table[i,"중국집"] <- ifelse(length(sa.gu[[i]]$Freq)==0,0,sa.gu[[i]][grep("중국집",sa.gu[[i]]$Var1),"Freq"])
  guro.table[i,"패밀리레스토랑"] <- ifelse(length(sa.gu[[i]]$Freq)==0,0,sa.gu[[i]][grep("패밀리레스토랑",sa.gu[[i]]$Var1),"Freq"])
  guro.table[i,"치킨"] <- ifelse(length(sa.gu[[i]]$Freq)==0,0,sa.gu[[i]][grep("치킨",sa.gu[[i]]$Var1),"Freq"])
  guro.table[i,"피자"] <- ifelse(length(sa.gu[[i]]$Freq)==0,0,sa.gu[[i]][grep("피자",sa.gu[[i]]$Var1),"Freq"])
}
guro.table[which(is.na(guro.table),arr.ind=T)] <- 0

#구로구 테이블
guro.table

#------------------
#16.금천구(16)
gc <- place_spl$금천구
gc.all <- rule.fn(gc, 3, 0.0001, 0.6)

# 불필요한(중복되는) 규칙을 제거
remove.gc <- is.subset(gc.all, gc.all, sparse=FALSE)  
remove.gc[lower.tri(remove.gc, diag=T)] <- NA
redundant.gc <- colSums(remove.gc, na.rm=T) >= 1
gc.pruned <- gc.all[!redundant.gc] 

#금천구 : 동별로 추출 
gc.df <- as(gc.pruned,"data.frame")
gc.df$rules <- as.character(gc.df$rules)
gc.df$lhs <- do.call(rbind,strsplit(gc.df$rules,"=>"))[,1]
gc.df$rhs <- do.call(rbind,strsplit(gc.df$rules,"=>"))[,2]

gc.rules <- list()
for(i in 1:length(unique(gc$읍면동))){
  gc.rules[[as.character(unique(gc$읍면동)[i])]] <- grep(unique(gc$읍면동)[i],gc.df$lhs)
}

gc.idx <- lapply(1:length(unique(gc$읍면동)),function(x){gc.df[gc.rules[[x]],]})
sa.gc <- lapply(1:length(unique(gc$읍면동)),function(x){as.data.frame(table(gc.idx[[x]]$rhs))})

geumcheon.table <- matrix(,nrow =length(unique(gc$읍면동)),ncol=4,
                          dimnames=list(unique(gc$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:length(unique(gc$읍면동))){
  geumcheon.table[i,"중국집"] <- ifelse(length(sa.gc[[i]]$Freq)==0,0,sa.gc[[i]][grep("중국집",sa.gc[[i]]$Var1),"Freq"])
  geumcheon.table[i,"패밀리레스토랑"] <- ifelse(length(sa.gc[[i]]$Freq)==0,0,sa.gc[[i]][grep("패밀리레스토랑",sa.gc[[i]]$Var1),"Freq"])
  geumcheon.table[i,"치킨"] <- ifelse(length(sa.gc[[i]]$Freq)==0,0,sa.gc[[i]][grep("치킨",sa.gc[[i]]$Var1),"Freq"])
  geumcheon.table[i,"피자"] <- ifelse(length(sa.gc[[i]]$Freq)==0,0,sa.gc[[i]][grep("피자",sa.gc[[i]]$Var1),"Freq"])
}
geumcheon.table[which(is.na(geumcheon.table),arr.ind=T)] <- 0

#금천구 테이블
geumcheon.table

#--------------------------------
#17.은평구 테이블(91)
ep <- place_spl$은평구
ep.all <- rule.fn(ep, 3, 0.00007, 0.65)

# 불필요한(중복되는) 규칙을 제거
remove.ep <- is.subset(ep.all, ep.all, sparse=FALSE)  
remove.ep[lower.tri(remove.ep, diag=T)] <- NA
redundant.ep <- colSums(remove.ep, na.rm=T) >= 1
ep.pruned <- ep.all[!redundant.ep] 

#은평구 : 동별로 추출 
ep.df <- as(ep.pruned,"data.frame")
ep.df$rules <- as.character(ep.df$rules)
ep.df$lhs <- do.call(rbind,strsplit(ep.df$rules,"=>"))[,1]
ep.df$rhs <- do.call(rbind,strsplit(ep.df$rules,"=>"))[,2]

ep.rules <- list()
for(i in 1:length(unique(ep$읍면동))){
  ep.rules[[as.character(unique(ep$읍면동)[i])]] <- grep(unique(ep$읍면동)[i],ep.df$lhs)
}

ep.idx <- lapply(1:length(unique(ep$읍면동)),function(x){ep.df[ep.rules[[x]],]})
sa.ep <- lapply(1:length(unique(ep$읍면동)),function(x){as.data.frame(table(ep.idx[[x]]$rhs))})

ep.table <- matrix(,nrow =length(unique(ep$읍면동)),ncol=4,
                   dimnames=list(unique(ep$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:length(unique(ep$읍면동))){
  ep.table[i,"중국집"] <- ifelse(length(sa.ep[[i]]$Freq)==0,0,sa.ep[[i]][grep("중국집",sa.ep[[i]]$Var1),"Freq"])
  ep.table[i,"패밀리레스토랑"] <- ifelse(length(sa.ep[[i]]$Freq)==0,0,sa.ep[[i]][grep("패밀리레스토랑",sa.ep[[i]]$Var1),"Freq"])
  ep.table[i,"치킨"] <- ifelse(length(sa.ep[[i]]$Freq)==0,0,sa.ep[[i]][grep("치킨",sa.ep[[i]]$Var1),"Freq"])
  ep.table[i,"피자"] <- ifelse(length(sa.ep[[i]]$Freq)==0,0,sa.ep[[i]][grep("피자",sa.ep[[i]]$Var1),"Freq"])
}
ep.table[which(is.na(ep.table),arr.ind=T)] <- 0

#은평구 테이블
ep.table

#------------------------------
#18. 종로구 테이블(151)
jn <- place_spl$종로구
jn.all <- rule.fn(jn, 3, 0.0005, 0.62)
                  
# 불필요한(중복되는) 규칙을 제거
remove.jn <- is.subset(jn.all, jn.all, sparse=FALSE)  
remove.jn[lower.tri(remove.jn, diag=T)] <- NA
redundant.jn <- colSums(remove.jn, na.rm=T) >= 1
jn.pruned <- jn.all[!redundant.jn]

jn.df <- as(jn.pruned,"data.frame")
jn.df$rules <- as.character(jn.df$rules)
jn.df$lhs <- do.call(rbind,strsplit(jn.df$rules,"=>"))[,1]
jn.df$rhs <- do.call(rbind,strsplit(jn.df$rules,"=>"))[,2]

jn.rules <- list()
for(i in 1:length(unique(jn$읍면동))){
  jn.rules[[as.character(unique(jn$읍면동)[i])]] <- grep(unique(jn$읍면동)[i],jn.df$lhs)
}

jn.idx <- lapply(1:length(unique(jn$읍면동)),function(x){jn.df[jn.rules[[x]],]})
sa.jn <- lapply(1:length(unique(jn$읍면동)),function(x){as.data.frame(table(jn.idx[[x]]$rhs))})

jn.table <- matrix(,nrow =length(unique(jn$읍면동)),ncol=4,
                   dimnames=list(unique(jn$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:length(unique(jn$읍면동))){
  jn.table[i,"중국집"] <- ifelse(length(sa.jn[[i]]$Freq)==0,0,sa.jn[[i]][grep("중국집",sa.jn[[i]]$Var1),"Freq"])
  jn.table[i,"패밀리레스토랑"] <- ifelse(length(sa.jn[[i]]$Freq)==0,0,sa.jn[[i]][grep("패밀리레스토랑",sa.jn[[i]]$Var1),"Freq"])
  jn.table[i,"치킨"] <- ifelse(length(sa.jn[[i]]$Freq)==0,0,sa.jn[[i]][grep("치킨",sa.jn[[i]]$Var1),"Freq"])
  jn.table[i,"피자"] <- ifelse(length(sa.jn[[i]]$Freq)==0,0,sa.jn[[i]][grep("피자",sa.jn[[i]]$Var1),"Freq"])
}
jn.table[which(is.na(jn.table),arr.ind=T)] <- 0

jn.table

#----------------------------------
#19.중구 테이블(383)
jg <- place_spl$중구
jg.all <- rule.fn(jg, 3, 0.00005, 0.65)

# 불필요한(중복되는) 규칙을 제거
remove.jg <- is.subset(jg.all, jg.all, sparse=FALSE)  
remove.jg[lower.tri(remove.jg, diag=T)] <- NA
redundant.jg <- colSums(remove.jg, na.rm=T) >= 1
jg.pruned <- jg.all[!redundant.jg] 

#중구 : 동별로 추출 
jg.df <- as(jg.pruned,"data.frame")
jg.df$rules <- as.character(jg.df$rules)
jg.df$lhs <- do.call(rbind,strsplit(jg.df$rules,"=>"))[,1]
jg.df$rhs <- do.call(rbind,strsplit(jg.df$rules,"=>"))[,2]

jg.rules <- list()
for(i in 1:length(unique(jg$읍면동))){
  jg.rules[[as.character(unique(jg$읍면동)[i])]] <- grep(unique(jg$읍면동)[i],jg.df$lhs)
}

jg.idx <- lapply(1:length(unique(jg$읍면동)),function(x){jg.df[jg.rules[[x]],]})
sa.jg <- lapply(1:length(unique(jg$읍면동)),function(x){as.data.frame(table(jg.idx[[x]]$rhs))})

jg.table <- matrix(,nrow =length(unique(jg$읍면동)),ncol=4,
                   dimnames=list(unique(jg$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:length(unique(jg$읍면동))){
  jg.table[i,"중국집"] <- ifelse(length(sa.jg[[i]]$Freq)==0,0,sa.jg[[i]][grep("중국집",sa.jg[[i]]$Var1),"Freq"])
  jg.table[i,"패밀리레스토랑"] <- ifelse(length(sa.jg[[i]]$Freq)==0,0,sa.jg[[i]][grep("패밀리레스토랑",sa.jg[[i]]$Var1),"Freq"])
  jg.table[i,"치킨"] <- ifelse(length(sa.jg[[i]]$Freq)==0,0,sa.jg[[i]][grep("치킨",sa.jg[[i]]$Var1),"Freq"])
  jg.table[i,"피자"] <- ifelse(length(sa.jg[[i]]$Freq)==0,0,sa.jg[[i]][grep("피자",sa.jg[[i]]$Var1),"Freq"])
}
jg.table[which(is.na(jg.table),arr.ind=T)] <- 0

#테이블
jg.table

#-----------------------------
#20.중랑구 테이블
jnang <- place_spl$중랑구
jnang.all <- rule.fn(jnang, 3, 0.00007, 0.6)

# 불필요한(중복되는) 규칙을 제거
remove.jnang <- is.subset(jnang.all, jnang.all, sparse=FALSE)  
remove.jnang[lower.tri(remove.jnang, diag=T)] <- NA
redundant.jnang <- colSums(remove.jnang, na.rm=T) >= 1
jnang.pruned <- jnang.all[!redundant.jnang] 

#중랑구 : 동별로 추출 
jnang.df <- as(jnang.pruned,"data.frame")
jnang.df$rules <- as.character(jnang.df$rules)
jnang.df$lhs <- do.call(rbind,strsplit(jnang.df$rules,"=>"))[,1]
jnang.df$rhs <- do.call(rbind,strsplit(jnang.df$rules,"=>"))[,2]

jnang.rules <- list()
for(i in 1:length(unique(jnang$읍면동))){
  jnang.rules[[as.character(unique(jnang$읍면동)[i])]] <- grep(unique(jnang$읍면동)[i],jnang.df$lhs)
}

jnang.idx <- lapply(1:length(unique(jnang$읍면동)),function(x){jnang.df[jnang.rules[[x]],]})
sa.jnang <- lapply(1:length(unique(jnang$읍면동)),function(x){as.data.frame(table(jnang.idx[[x]]$rhs))})

jnang.table <- matrix(,nrow =length(unique(jnang$읍면동)),ncol=4,
                      dimnames=list(unique(jnang$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:length(unique(jnang$읍면동))){
  jnang.table[i,"중국집"] <- ifelse(length(sa.jnang[[i]]$Freq)==0,0,sa.jnang[[i]][grep("중국집",sa.jnang[[i]]$Var1),"Freq"])
  jnang.table[i,"패밀리레스토랑"] <- ifelse(length(sa.jnang[[i]]$Freq)==0,0,sa.jnang[[i]][grep("패밀리레스토랑",sa.jnang[[i]]$Var1),"Freq"])
  jnang.table[i,"치킨"] <- ifelse(length(sa.jnang[[i]]$Freq)==0,0,sa.jnang[[i]][grep("치킨",sa.jnang[[i]]$Var1),"Freq"])
  jnang.table[i,"피자"] <- ifelse(length(sa.jnang[[i]]$Freq)==0,0,sa.jnang[[i]][grep("피자",sa.jnang[[i]]$Var1),"Freq"])
}
jnang.table[which(is.na(jnang.table),arr.ind=T)] <- 0

#중랑구 테이블
jnang.table

#---------------------------
#21.성북구 테이블
sb <- place_spl$성북구
sb.all <- rule.fn(sb, 3, 0.0001, 0.65)
                  
# 불필요한(중복되는) 규칙을 제거
remove.sb <- is.subset(sb.all, sb.all, sparse=FALSE)  
remove.sb[lower.tri(remove.sb, diag=T)] <- NA
redundant.sb <- colSums(remove.sb, na.rm=T) >= 1
sb.pruned <- sb.all[!redundant.sb] 

sb.df <- as(sb.pruned,"data.frame")
sb.df$rules <- as.character(sb.df$rules)
sb.df$lhs <- do.call(rbind,strsplit(sb.df$rules,"=>"))[,1]
sb.df$rhs <- do.call(rbind,strsplit(sb.df$rules,"=>"))[,2]

sb.rules <- list()
for(i in 1:length(unique(sb$읍면동))){
  sb.rules[[as.character(unique(sb$읍면동)[i])]] <- grep(unique(sb$읍면동)[i],sb.df$lhs)
}

sb.idx <- lapply(1:length(unique(sb$읍면동)),function(x){sb.df[sb.rules[[x]],]})
sa.sb <- lapply(1:length(unique(sb$읍면동)),function(x){as.data.frame(table(sb.idx[[x]]$rhs))})

sb.table <- matrix(,nrow =length(unique(sb$읍면동)),ncol=4,
                   dimnames=list(unique(sb$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:length(unique(sb$읍면동))){
  sb.table[i,"중국집"] <- ifelse(length(sa.sb[[i]]$Freq)==0,0,sa.sb[[i]][grep("중국집",sa.sb[[i]]$Var1),"Freq"])
  sb.table[i,"패밀리레스토랑"] <- ifelse(length(sa.sb[[i]]$Freq)==0,0,sa.sb[[i]][grep("패밀리레스토랑",sa.sb[[i]]$Var1),"Freq"])
  sb.table[i,"치킨"] <- ifelse(length(sa.sb[[i]]$Freq)==0,0,sa.sb[[i]][grep("치킨",sa.sb[[i]]$Var1),"Freq"])
  sb.table[i,"피자"] <- ifelse(length(sa.sb[[i]]$Freq)==0,0,sa.sb[[i]][grep("피자",sa.sb[[i]]$Var1),"Freq"])
}
sb.table[which(is.na(sb.table),arr.ind=T)] <- 0


sb.table

#--------------------------------
#22.송파구 테이블(126)
sp <- place_spl$송파구
sp.all <- rule.fn(sp, 3, 0.00005, 0.63)

# 불필요한(중복되는) 규칙을 제거
remove.sp <- is.subset(sp.all, sp.all, sparse=FALSE)  
remove.sp[lower.tri(remove.sp, diag=T)] <- NA
redundant.sp <- colSums(remove.sp, na.rm=T) >= 1
sp.pruned <- sp.all[!redundant.sp] 

sp.df <- as(sp.pruned,"data.frame")
sp.df$rules <- as.character(sp.df$rules)
sp.df$lhs <- do.call(rbind,strsplit(sp.df$rules,"=>"))[,1]
sp.df$rhs <- do.call(rbind,strsplit(sp.df$rules,"=>"))[,2]

sp.rules <- list()
for(i in 1:length(unique(sp$읍면동))){
  sp.rules[[as.character(unique(sp$읍면동)[i])]] <- grep(unique(sp$읍면동)[i],sp.df$lhs)
}

sp.idx <- lapply(1:length(unique(sp$읍면동)),function(x){sp.df[sp.rules[[x]],]})
sa.sp <- lapply(1:length(unique(sp$읍면동)),function(x){as.data.frame(table(sp.idx[[x]]$rhs))})

sp.table <- matrix(,nrow =length(unique(sp$읍면동)),ncol=4,
                   dimnames=list(unique(sp$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:length(unique(sp$읍면동))){
  sp.table[i,"중국집"] <- ifelse(length(sa.sp[[i]]$Freq)==0,0,sa.sp[[i]][grep("중국집",sa.sp[[i]]$Var1),"Freq"])
  sp.table[i,"패밀리레스토랑"] <- ifelse(length(sa.sp[[i]]$Freq)==0,0,sa.sp[[i]][grep("패밀리레스토랑",sa.sp[[i]]$Var1),"Freq"])
  sp.table[i,"치킨"] <- ifelse(length(sa.sp[[i]]$Freq)==0,0,sa.sp[[i]][grep("치킨",sa.sp[[i]]$Var1),"Freq"])
  sp.table[i,"피자"] <- ifelse(length(sa.sp[[i]]$Freq)==0,0,sa.sp[[i]][grep("피자",sa.sp[[i]]$Var1),"Freq"])
}
sp.table[which(is.na(sp.table),arr.ind=T)] <- 0

sp.table

#---------------------------
#24.양천구 테이블(26)
yc <- place_spl$양천구
yc.all <- rule.fn(yc, 3, 0.0001, 0.6)

# 불필요한(중복되는) 규칙을 제거
remove.yc <- is.subset(yc.all, yc.all, sparse=FALSE)  
remove.yc[lower.tri(remove.yc, diag=T)] <- NA
redundant.yc <- colSums(remove.yc, na.rm=T) >= 1
yc.pruned <- yc.all[!redundant.yc] 

yc.df <- as(yc.pruned,"data.frame")
yc.df$rules <- as.character(yc.df$rules)
yc.df$lhs <- do.call(rbind,strsplit(yc.df$rules,"=>"))[,1]
yc.df$rhs <- do.call(rbind,strsplit(yc.df$rules,"=>"))[,2]

yc.rules <- list()
for(i in 1:length(unique(yc$읍면동))){
  yc.rules[[as.character(unique(yc$읍면동)[i])]] <- grep(unique(yc$읍면동)[i],yc.df$lhs)
}

yc.idx <- lapply(1:length(unique(yc$읍면동)),function(x){yc.df[yc.rules[[x]],]})
sa.yc <- lapply(1:length(unique(yc$읍면동)),function(x){as.data.frame(table(yc.idx[[x]]$rhs))})

yc.table <- matrix(,nrow =length(unique(yc$읍면동)),ncol=4,
                   dimnames=list(unique(yc$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:length(unique(yc$읍면동))){
  yc.table[i,"중국집"] <- ifelse(length(sa.yc[[i]]$Freq)==0,0,sa.yc[[i]][grep("중국집",sa.yc[[i]]$Var1),"Freq"])
  yc.table[i,"패밀리레스토랑"] <- ifelse(length(sa.yc[[i]]$Freq)==0,0,sa.yc[[i]][grep("패밀리레스토랑",sa.yc[[i]]$Var1),"Freq"])
  yc.table[i,"치킨"] <- ifelse(length(sa.yc[[i]]$Freq)==0,0,sa.yc[[i]][grep("치킨",sa.yc[[i]]$Var1),"Freq"])
  yc.table[i,"피자"] <- ifelse(length(sa.yc[[i]]$Freq)==0,0,sa.yc[[i]][grep("피자",sa.yc[[i]]$Var1),"Freq"])
}
yc.table[which(is.na(yc.table),arr.ind=T)] <- 0

yc.table

#---------------------------------
#25.영등포 테이블(151)
ydp <- place_spl$영등포구
ydp.all <- rule.fn(ydp, 3, 0.0001, 0.6)

# 불필요한(중복되는) 규칙을 제거
remove.ydp <- is.subset(ydp.all, ydp.all, sparse=FALSE)  
remove.ydp[lower.tri(remove.ydp, diag=T)] <- NA
redundant.ydp <- colSums(remove.ydp, na.rm=T) >= 1
ydp.pruned <- ydp.all[!redundant.ydp] 

ydp.df <- as(ydp.pruned,"data.frame")
ydp.df$rules <- as.character(ydp.df$rules)
ydp.df$lhs <- do.call(rbind,strsplit(ydp.df$rules,"=>"))[,1]
ydp.df$rhs <- do.call(rbind,strsplit(ydp.df$rules,"=>"))[,2]

ydp.rules <- list()
for(i in 1:length(unique(ydp$읍면동))){
  ydp.rules[[as.character(unique(ydp$읍면동)[i])]] <- grep(unique(ydp$읍면동)[i],ydp.df$lhs)
}

ydp.idx <- lapply(1:length(unique(ydp$읍면동)),function(x){ydp.df[ydp.rules[[x]],]})
sa.ydp <- lapply(1:length(unique(ydp$읍면동)),function(x){as.data.frame(table(ydp.idx[[x]]$rhs))})

ydp.table <- matrix(,nrow =length(unique(ydp$읍면동)),ncol=4,
                    dimnames=list(unique(ydp$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:length(unique(ydp$읍면동))){
  ydp.table[i,"중국집"] <- ifelse(length(sa.ydp[[i]]$Freq)==0,0,sa.ydp[[i]][grep("중국집",sa.ydp[[i]]$Var1),"Freq"])
  ydp.table[i,"패밀리레스토랑"] <- ifelse(length(sa.ydp[[i]]$Freq)==0,0,sa.ydp[[i]][grep("패밀리레스토랑",sa.ydp[[i]]$Var1),"Freq"])
  ydp.table[i,"치킨"] <- ifelse(length(sa.ydp[[i]]$Freq)==0,0,sa.ydp[[i]][grep("치킨",sa.ydp[[i]]$Var1),"Freq"])
  ydp.table[i,"피자"] <- ifelse(length(sa.ydp[[i]]$Freq)==0,0,sa.ydp[[i]][grep("피자",sa.ydp[[i]]$Var1),"Freq"])
}
ydp.table[which(is.na(ydp.table),arr.ind=T)] <- 0

ydp.table

#-------------------------
#26.용산구 테이블 
ys <- place_spl$용산구
ys.all <- rule.fn(ys, 3, 0.0003, 0.6)

# 불필요한(중복되는) 규칙을 제거
remove.ys <- is.subset(ys.all, ys.all, sparse=FALSE)  
remove.ys[lower.tri(remove.ys, diag=T)] <- NA
redundant.ys <- colSums(remove.ys, na.rm=T) >= 1
ys.pruned <- ys.all[!redundant.ys] 

ys.df <- as(ys.pruned,"data.frame")
ys.df$rules <- as.character(ys.df$rules)
ys.df$lhs <- do.call(rbind,strsplit(ys.df$rules,"=>"))[,1]
ys.df$rhs <- do.call(rbind,strsplit(ys.df$rules,"=>"))[,2]

ys.rules <- list()
for(i in 1:length(unique(ys$읍면동))){
  ys.rules[[as.character(unique(ys$읍면동)[i])]] <- grep(unique(ys$읍면동)[i],ys.df$lhs)
}

ys.idx <- lapply(1:length(unique(ys$읍면동)),function(x){ys.df[ys.rules[[x]],]})
sa.ys <- lapply(1:length(unique(ys$읍면동)),function(x){as.data.frame(table(ys.idx[[x]]$rhs))})

ys.table <- matrix(,nrow =length(unique(ys$읍면동)),ncol=4,
                   dimnames=list(unique(ys$읍면동),c("중국집","패밀리레스토랑","치킨","피자")))

for(i in 1:length(unique(ys$읍면동))){
  ys.table[i,"중국집"] <- ifelse(length(sa.ys[[i]]$Freq)==0,0,sa.ys[[i]][grep("중국집",sa.ys[[i]]$Var1),"Freq"])
  ys.table[i,"패밀리레스토랑"] <- ifelse(length(sa.ys[[i]]$Freq)==0,0,sa.ys[[i]][grep("패밀리레스토랑",sa.ys[[i]]$Var1),"Freq"])
  ys.table[i,"치킨"] <- ifelse(length(sa.ys[[i]]$Freq)==0,0,sa.ys[[i]][grep("치킨",sa.ys[[i]]$Var1),"Freq"])
  ys.table[i,"피자"] <- ifelse(length(sa.ys[[i]]$Freq)==0,0,sa.ys[[i]][grep("피자",sa.ys[[i]]$Var1),"Freq"])
}
ys.table[which(is.na(ys.table),arr.ind=T)] <- 0

ys.table

#------------------------------------
#동입력 했을 때 규칙 값 뽑는 함수만들기

#names <- ls.str(mode="S4",pattern = ".pruned$")[-23]
names <- c("dobong.pruned", "dongdaemun.pruned" ,"dongjak.pruned", "ep.pruned", "gc.pruned" ,   
           "gd.pruned", "gj.pruned" , "gn.pruned","gs.pruned","gsth.pruned","gu.pruned","gw.pruned",
           "jg.pruned","jn.pruned","jnang.pruned","mapo.pruned","noone.pruned","sb.pruned",
           "sedaemun.pruned","seongdong.pruned","seucho.pruned","sp.pruned","yc.pruned","ydp.pruned","ys.pruned")
gu.names <- c("도봉구","동대문구","동작구","은평구","금천구","강동구","광진구","강남구","강서구","강북구",
              "구로구","관악구","중구","종로구","중랑구","마포구","노원구","성북구","서대문구","성동구",
              "서초구","송파구","양천구","영등포구","용산구")

all.list <- list()
for (i in 1:length(names)){
  all.list[[i]] <- as(get(names[i]),'data.frame')
  all.list[[i]]$rules <- as.character(all.list[[i]]$rules)
  all.list[[i]]$lhs <- do.call(rbind,strsplit(all.list[[i]]$rules,"=>"))[,1]
  all.list[[i]]$rhs <- do.call(rbind,strsplit(all.list[[i]]$rules,"=>"))[,2]
  all.list[[i]]$rules <- NULL
  all.list[[i]]$rhs <- gsub("\\{업종=|\\}","",all.list[[i]]$rhs)
  all.list[[i]]$lhs <- gsub("\\{|\\}","",all.list[[i]]$lhs)
  all.list[[i]]$lhs <- paste0(all.list[[i]]$lhs,",구=",gu.names[i])
}

dong.rule <- rbindlist(all.list)
