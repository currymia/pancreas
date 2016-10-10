


unloadNamespace("UNOS")
unloadNamespace("dplyr")
unloadNamespace("DBI")
options(java.parameters = "-Xmx10g")
library(DBI)
library(dplyr)
library(ggplot2)

conn <- dbConnect(RSQLServer::SQLServer(), 'modbsas/SASDataSet')

tx <- dbGetQuery(conn, "
                     select c.*,  IIF(c.TX_DATE <= '10-30-2014', 'Pre', 'Post') as Era
                     from SASDataSet.SASV.KIDPAN as c
                  where  c.TX_DATE >='05-01-2013' and c.TX_DATE <= '04-30-2016' and c.TX_TYPE != 'PWK'
                     ")


pak <- tx %>% 
       group_by(TX_TYPE, Era) %>% 
       tally %>% 
       tidyr::spread(Era,n)
       
chisq.test(table(tx$Era,tx$TX_TYPE))







