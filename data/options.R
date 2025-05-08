rm(list=ls())
setwd("homepage/study_25spring/data/")
getwd()
library(tidyverse)

# 원본데이터 준비
options_raw <- read_csv("options_data.csv") %>% tibble()
idx_raw <- read_csv("idx_data.csv") %>% tibble()
mmf_raw <- read_csv("mmf.csv") %>% tibble()

# 옵션 데이터 전처리
options <- options_raw %>% 
  # K200, WK200 이외의 옵션 제외
  filter(substr(PROD_NM,1,3)=="코스피") %>%
  # 종가가 없는 경우 익일 기준가격(이론가격) 사용
  mutate(PRICE = as.double(if_else(TDD_CLSPRC=="-",NXTDD_BAS_PRC,TDD_CLSPRC))) %>% 
  drop_na(PRICE) %>% 
  select(BAS_DD,ISU_NM,PRICE,ACC_TRDVOL) %>% 
  separate(ISU_NM, into=c("PROD","RGHT","EXP","EXER_PRC"), sep=' ') %>%
  mutate(EXER_PRC=as.double(EXER_PRC),
         EXPMM=if_else(PROD=="코스피200",substr(EXP,3,6),substr(EXP,1,4)),
         EXPWW=if_else(PROD=="코스피200",2,as.integer(substr(EXP,6,6)))) %>% 
  filter(EXER_PRC>min(idx_raw$KOSPI200)*0.85,
         EXER_PRC<max(idx_raw$KOSPI200)*1.15,
         PROD!="코스피위클리M") %>% # 월요일 만기 위클리옵션 제외
  mutate(PRIOR=as.integer(EXPMM)*10+EXPWW) # 만기가 짧은 순으로 우선순위 부여

# 옵션 만기일(매주) 생성
target_date <- options %>% 
  distinct(.,BAS_DD, PRIOR) %>% 
  arrange(PRIOR, desc(BAS_DD)) %>% 
  group_by(PRIOR) %>% 
  slice(1) %>% 
  ungroup() %>% 
  distinct(BAS_DD) %>% 
  arrange(desc(BAS_DD)) %>% 
  filter(BAS_DD<20241231, BAS_DD>20200101)

# K200 및 V-K200지수 전처리
idx_data <- idx_raw %>%
  mutate(LAG_K200=lag(KOSPI200),
         LAG_VK200=lag(VKOSPI200))

# 포트폴리오 기본 정보 생성
target_info <- target_date %>% 
  left_join(options, by="BAS_DD") %>% 
  distinct(BAS_DD,PRIOR) %>% 
  arrange(desc(BAS_DD),PRIOR) %>% 
  group_by(BAS_DD) %>% 
  # 만기가 도래하는 옵션을 제외하고 우선순위가 높은 옵션 선택
  slice(2) %>% 
  ungroup() %>% 
  arrange(desc(BAS_DD)) %>% 
  left_join(idx_data, by="BAS_DD") %>% 
  left_join(mmf_raw, by="BAS_DD") %>% 
  # 옵션 보유기간 및 단기금리(MMF) 계산
  mutate(DIFF_DAY=as.integer(ymd(lag(BAS_DD))-ymd(BAS_DD)),
         MMF=MMF*0.01) %>% 
  # 옵션 보유기간에 해당하는 시장기대변동성 계산(V-K200활용)
  mutate(VOL=LAG_VK200*sqrt(DIFF_DAY)/sqrt(365)/100) %>% 
  # 변동성에 따른 옵션 행사가격 상단(2.5단위 올림) 및 하단(2.5단위 내림) 계산
  mutate(TARGET_C_K=ceiling(KOSPI200*(1+VOL)*0.4)/0.4,
         TARGET_P_K=floor(KOSPI200*(1-VOL)*0.4)/0.4) %>% 
  drop_na(DIFF_DAY) %>% 
  select(BAS_DD,VOL,DIFF_DAY,MMF,KOSPI200,LAG_VK200,PRIOR,TARGET_C_K,TARGET_P_K)

# 거래대상 옵션 정보 생성
target_options <- target_info %>% 
  select(BAS_DD, PRIOR, TARGET_C_K, TARGET_P_K) %>% 
  pivot_longer(cols=c("TARGET_C_K","TARGET_P_K"),names_to = "GRP", values_to = "EXER_PRC") %>% 
  mutate(RGHT=substr(GRP,8,8)) %>% 
  left_join(options,by=c("BAS_DD","PRIOR","RGHT","EXER_PRC")) %>%
  select(BAS_DD,GRP,PRICE,ACC_TRDVOL) %>% 
  pivot_wider(names_from = "GRP", values_from = c("PRICE","ACC_TRDVOL"))

# 원금 100억원 가정
cash = 10000*10000*100

# 변동성 조정 위클리 양매도 포트폴리오
portfolio <- target_info %>% 
  left_join(target_options,by="BAS_DD") %>% 
  arrange(BAS_DD) %>% 
  # 원금에 따른 옵션 매도수량 계산
  mutate(SELL_AMT=cash/250000/KOSPI200) %>% 
  # 옵션 프리미엄(매도수익)) 계산
  mutate(PREMIUM=SELL_AMT*(PRICE_TARGET_C_K+PRICE_TARGET_P_K)*250000) %>% 
  # 원금 및 프리미엄의 MMF 이자수익 및 권리행사손실 계산
  mutate(INTEREST=(cash+replace_na(PREMIUM,0))*MMF*DIFF_DAY/365,
         EXERCISE=(if_else(lead(KOSPI200)-TARGET_C_K>0,lead(KOSPI200)-TARGET_C_K,0)+
                     if_else(TARGET_P_K-lead(KOSPI200)>0,TARGET_P_K-lead(KOSPI200),0))*250000*SELL_AMT) %>% 
  # 주단위 포트폴리오 손익 계산(원금 100억 가정, 당일 투자시 다음주 실현손익을 의미)
  mutate(REVENUE=PREMIUM+INTEREST-EXERCISE) %>% 
  # 거래대상 옵션이 상장되어있지 않은 경우, MMF수익만 고려
  mutate(REVENUE=if_else(is.na(REVENUE),INTEREST,REVENUE)) %>% 
  mutate(RATE=REVENUE/cash) # 주단위 포트폴리오 수익률
portfolio

