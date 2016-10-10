

unloadNamespace("UNOS")
unloadNamespace("dplyr")
unloadNamespace("DBI")
options(java.parameters = "-Xmx10g")
library(sqldf)
library(dplyr)
library(ggplot2)

conn <- dbConnect(RSQLServer::SQLServer(), 'modbsas/SASDataSet')

tx <- dbGetQuery(conn, "
                 select c.*,  IIF(c.TX_DATE <= '10-30-2014', 'Pre', 'Post') as Era
                 from SASDataSet.SASV.TX as c
                 where  c.TX_DATE >='05-01-2013' and c.TX_DATE <= '04-30-2016' 
                 ")

tx2 <- dbGetQuery(conn, "
                        select * 
                         from sasdataset.sasv.tx
                        where  TX_DATE >='05-01-2013' and TX_DATE <= '04-30-2016' and
                        organ != 'KI' and multiorg = 'Y'
                          ")



fuldon <- dbGetQuery(conn, "
                     
                     select  IIF(a.DON_DATE <= '10-30-2014', 'Pre', 'Post') as Era, a.*
                     from SASDataSet.sasv.FULLDONR as a 
                     where a.DISP_ALL in ('1' , '3') and a.DON_TY = 'C' and 
                     a.DON_DATE >='05-01-2013' and a.DON_DATE <= '04-30-2016' 
                     ")




don2 <- tx %>% 
        filter(ORGAN == "KI" & MULTIORG =='Y') %>% 
        arrange(DON_ID,TRR_ID) %>% 
        group_by(DON_ID) %>% 
        filter((duplicated(DON_ID)|duplicated(DON_ID, fromLast = TRUE)))


            
#if they want to know what other organs they went too

dontx <- sqldf("Select a.trr_id, a.organ as aorgan, b.organ as borgan, a.don_id, b.don_id as donidb
               From don2 as a
               Left join tx2 as b 
               on a.PT_CODE = b.pt_code and a.don_id=b.don_id
               order by a.don_id")


#if they want to know what happened to the pa

#unique don_id values
don2_nodups <- don2 %>% 
  select(DON_ID) %>% 
  arrange(DON_ID) %>% 
  group_by(DON_ID) %>% 
  filter(row_number() == 1) 


padon <- sqldf("select a.num_pa_tx, a.num_pa_recov, a.num_pa_recov, a.don_id
               from fuldon as a 
               inner join don2_nodups as b 
               on a.don_id = b.don_id")

table(padon$NUM_PA_TX) #20% txed


table(fuldon$NUM_PA_TX) #11% txed






