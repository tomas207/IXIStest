
pacman::p_load(tidyverse,lubridate,openxlsx)
setwd("/Users/tomasgeorgsson/Documents/IXIS.test")
addsToCart <- read.csv("DataAnalyst_Ecom_data_addsToCart.csv")
sessionCounts <- read.csv("DataAnalyst_Ecom_data_sessionCounts.csv")

session1<-sessionCounts
adds<-addsToCart
session1$dim_date
session1$date <- lubridate::mdy(session1$dim_date)
session1$dim_month<-month(session1$date)
skimr::skim(session1)
session2<-session1%>%group_by(dim_month,dim_deviceCategory)%>%
  summarise(session_sum=sum(sessions), transaction_sum=sum(transactions), QTY_sum=sum(QTY), ECR=sum(transactions)/sum(sessions))%>%
  mutate(across(where(is.numeric), round, 4))
session2


session3<-session1%>%group_by(dim_year=year(date),dim_month)%>%
  summarise(session_sum=sum(sessions), transaction_sum=sum(transactions), QTY_sum=sum(QTY))%>%
  inner_join(adds)
session3

#session5 <- as.data.frame(t(session4))%>%mutate(absolute=abs(V2-V1),relative=V2-V1)
#skimr::skim(session5)

session4<-tail(session3,2)
session4 <- as.data.frame(t(session3))
session4<-session4%>%mutate()

abs.val <- function(value) {
  abs.diff<-abs(lag(value)-value)
  return(abs.diff)
}
rel.val <- function(value) {
  rel.diff<-lag(value)-value
  return(rel.diff)
}

session6<-as.data.frame(session3)%>%
  mutate(session_diff_abs = abs.val(session_sum),
         session_diff_rel = rel.val(session_sum),
         transaciton_diff_abs = abs.val(transaction_sum),
         transaciton_diff_rel = rel.val(transaction_sum),
         QTY_diff_abs = abs.val(QTY_sum),
         QTY_diff_rel = rel.val(QTY_sum),
         addsToCart_diff_abs = abs.val(addsToCart),
         addsToCart_diff_rel = rel.val(addsToCart),)
session7<-as.data.frame(t(session6))



workbook<-createWorkbook()
addWorksheet(workbook,sheetName = "Month*Device aggregation")
addWorksheet(workbook,sheetName = "Month over Month comparison")
writeData(workbook,"Month*Device aggregation",session2)
writeData(workbook,"Month over Month comparison",session6)
saveWorkbook(workbook,file="IXIStestxlxs",overwrite = TRUE)


