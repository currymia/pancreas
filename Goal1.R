


unloadNamespace("UNOS")
unloadNamespace("dplyr")
unloadNamespace("DBI")
options(java.parameters = "-Xmx10g")
library(DBI)
library(ggplot2)
library(rms)
conn <- dbConnect(RSQLServer::SQLServer(), 'modbsas/SASDataSet')

fuldon <- dbGetQuery(conn, "

select  IIF(a.DON_DATE <= '10-30-2014', 'Pre', 'Post') as Era, a.*, x.ORGAN, x.TX_TYPE
                     from SASDataSet.sasv.FULLDONR as a 
                     left join(select c.*
                     from SASDataSet.SASV.KIDPAN as c
                     where c.organ in ('PA', 'KP') and c.TX_DATE >= '10-30-2013') as x
                     on a.DON_ID = x.DON_ID 
                     where a.DISP_ALL in ('1' , '3') and a.DON_TY = 'C' and 
                     a.DON_DATE >='05-01-2013' and a.DON_DATE <= '04-30-2016' 
                     ")



fuldon2 <- fuldon %>% 
          mutate(PDRI = ifelse((is.na(AGE_DON) | is.na(HGT_CM_DON)| ETHCAT_DON == 998 | is.na(COD_CAD_DON)
                                | is.na(BMI_DON)| is.na(CREAT_DON) | is.na(GENDER_DON)| 
                                  is.na(NON_HRT_DON)), NA , 
  exp(- 0.13792*(GENDER_DON == "F") - 0.034455*(AGE_DON< 20)*(AGE_DON-20) + 0.026149*(AGE_DON-28) 
  + 0.19490*(CREAT_DON > 2.5) + 0.23951*(ETHCAT_DON == 2) + 0.15711*(ETHCAT_DON == 5)
  - 0.000986347*(BMI_DON-24) + 0.033274*(BMI_DON>25)*(BMI_DON-25) 
  - 0.006073879*(HGT_CM_DON-173)+ 0.21018*(COD_CAD_DON == 2) 
  + 0.014678*(-4) + 0.33172*(NON_HRT_DON =="Y"))))


pdrisummary <- fuldon2 %>% 
            group_by(REG_OPO) %>% 
            summarise(medpdri = median(PDRI, na.rm = TRUE), meanpdri = mean(PDRI, na.rm=TRUE),
                      q25 = quantile(PDRI, .25, na.rm = TRUE), q75 = quantile(PDRI, .75, na.rm= TRUE))

decsummary <-  quantile(fuldon2$PDRI, prob = seq(0, 1, length = 11), type = 5, na.rm = TRUE)


natpdrimean <- mean(fuldon2$PDRI, na.rm = TRUE)

natpdrimedian <- median(fuldon2$PDRI, na.rm = TRUE)
  
  
fuldon3 <- fuldon2 %>% 
           left_join(pdrisummary, by= "REG_OPO") %>% 
            mutate(ideal = ifelse(PDRI < q25 & AGE_DON < 35 & BMI_DON < 30
                          & DIABETES_DON != 'Y' & CREAT_DON < 1.5
                          & CDC_RISK_HIV_DON != 'Y', 'Yes', 'No'),
                   decrank = cut(PDRI, decsummary, labels = 1:10))
                   

#utilization by transplant kp v pa
txtab1 <- fuldon3 %>% 
          filter(NUM_PA_TX == 1 & !is.na(ORGAN)) %>% 
          group_by(Era, ORGAN) %>% 
          tally %>%
          tidyr::spread(ORGAN,n) %>% 
          ungroup(Era,ORGAN)
        # mutate(pct = paste0(round((100*n/sum(n)), digits= 1),"%"))
          

g <- table(fuldon3$Era,fuldon3$ORGAN)
chisq.test(g)

#discareded pancreas by decile 

pdrirank <- fuldon3 %>% 
            filter(NUM_PA_RECOV > 0) %>% 
            group_by(decrank, Era, NUM_PA_DISC) %>% 
            tally %>% 
            mutate(pct = round((100*n/sum(n)), digits= 1)) %>% 
            ungroup(desranl,Era, NUM_PA_DISC) %>% 
            filter(NUM_PA_DISC ==1)

prdrrankplot <- ggplot(data=pdrirank)+
                geom_line(size=3, mapping = (aes(x= decrank, y=pct, group= Era, colour = Era)))+
                scale_y_continuous("Percent Discarded", limits = c(0,100))+
                scale_x_discrete("PDRI Rank")+ 
                scale_colour_manual(values=c('#0F99D6','#80C342'), guide="legend")+ 
                theme(legend.background=element_rect(fill="white"),
                panel.grid.major=element_line(colour="grey"),
                panel.background=element_rect(fill="white"))
#logistic goes here 

dismod <- rms::lrm(NUM_PA_DISC ~ decrank + Era + decrank*Era, data=fuldon3)


#recovery rate by pdri decile 
recov <- fuldon3 %>% 
  filter(!is.na(decrank)) %>% 
  group_by(decrank, Era, NUM_PA_RECOV) %>% 
  tally %>% 
  mutate(pct = round((100*n/sum(n)), digits= 1)) %>% 
  ungroup(desranl,Era, NUM_PA_RECOV) %>% 
  filter(NUM_PA_RECOV ==1)

recovplot <- ggplot(data=recov)+
  geom_line(size=3, mapping = (aes(x= decrank, y=pct, group= Era, colour = Era)))+
  scale_y_continuous("Percent Recovered", limits = c(0,100))+
  scale_x_discrete("PDRI Rank")+ 
  scale_colour_manual(values=c('#0F99D6','#80C342'), guide="legend")+ 
  theme(legend.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey"),
        panel.background=element_rect(fill="white"))

recovmod <- rms::lrm(NUM_PA_RECOV ~ decrank + Era + decrank*Era, data=fuldon3)



#CDC INCREASED RISK RECOVERY 
cdcreco <- fuldon3 %>% 
            arrange(DON_DATE) %>% 
             filter(CDC_RISK_HIV_DON != "U") %>% 
             mutate(yrmo = paste(lubridate::month(DON_DATE),lubridate::year(DON_DATE),sep = "/"),
                    mo = lubridate::month(DON_DATE), yr= lubridate::year(DON_DATE)) %>% 
             group_by(yrmo,mo,yr, CDC_RISK_HIV_DON, NUM_PA_RECOV) %>% 
             tally %>% 
             mutate(pct = round((100*n/sum(n)), digits= 1)) %>% 
             ungroup(yrmo, CDC_RISK_HIV_DON, NUM_PA_RECOV) %>% 
             filter(NUM_PA_RECOV ==1) %>% 
              arrange(yr,mo)

cdcreco$yrmo <- factor(cdcreco$yrmo, levels = cdcreco$yrmo[order(c(cdcreco$yr,cdcreco$mo))])

cdc_r_plot <- ggplot(data=cdcreco)+
  geom_line(size=2, mapping = (aes(x= yrmo, y=pct, group= CDC_RISK_HIV_DON, colour = CDC_RISK_HIV_DON)))+
  scale_y_continuous("Percent Recovered", limits = c(0,100))+
  scale_x_discrete("Date")+ 
  geom_vline(size=2,mapping = aes(xintercept=19))+
  scale_colour_manual(values=c('#0F99D6','#80C342'), guide="legend")+ 
  theme(legend.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey"),
        panel.background=element_rect(fill="white"),
        axis.text.x = element_text(angle = 45, hjust = 1, size=14))
            
cdcr_lm <- rms::lrm(NUM_PA_RECOV ~ CDC_RISK_HIV_DON + Era, data= fuldon3)
# more likely to recover in pre era, #more likely to not recover highrisk 


#CDC DISCARD RATES 

cdcdisc <- fuldon3 %>% 
  arrange(DON_DATE) %>% 
  filter(CDC_RISK_HIV_DON != "U", NUM_PA_RECOV > 0) %>% 
  mutate(yrmo = paste(lubridate::month(DON_DATE),lubridate::year(DON_DATE),sep = "/"),
         mo = lubridate::month(DON_DATE), yr= lubridate::year(DON_DATE)) %>% 
  group_by(yrmo,mo,yr, CDC_RISK_HIV_DON, NUM_PA_DISC) %>% 
  tally %>% 
  mutate(pct = round((100*n/sum(n)), digits= 1)) %>% 
  ungroup(yrmo, CDC_RISK_HIV_DON, NUM_PA_DISC) %>% 
  filter(NUM_PA_DISC ==1) %>% 
  arrange(yr,mo)

cdcdisc$yrmo <- factor(cdcdisc$yrmo, levels = cdcdisc$yrmo[order(c(cdcdisc$yr,cdcdisc$mo))])

cdc_d_plot <- ggplot(data=cdcdisc)+
  geom_line(size=2, mapping = (aes(x= yrmo, y=pct, group= CDC_RISK_HIV_DON, colour = CDC_RISK_HIV_DON)))+
  scale_y_continuous("Percent Recovered", limits = c(0,100))+
  scale_x_discrete("Date")+ 
  geom_vline(size=2,mapping = aes(xintercept=19))+
  scale_colour_manual(values=c('#0F99D6','#80C342'), guide="legend")+ 
  theme(legend.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey"),
        panel.background=element_rect(fill="white"),
        axis.text.x = element_text(angle = 45, hjust = 1, size=14))

cdcdis_lm <- rms::lrm(NUM_PA_DISC ~ CDC_RISK_HIV_DON + Era, data= fuldon3)
#more likely to not recover highrisk 




####ideal panc donor recovery

idlreco <- fuldon3 %>% 
  arrange(DON_DATE) %>% 
  mutate(yrmo = paste(lubridate::month(DON_DATE),lubridate::year(DON_DATE),sep = "/"),
         mo = lubridate::month(DON_DATE), yr= lubridate::year(DON_DATE)) %>% 
  group_by(yrmo,mo,yr, ideal, NUM_PA_RECOV) %>% 
  tally %>% 
  mutate(pct = round((100*n/sum(n)), digits= 1)) %>% 
  ungroup(yrmo, ideal, NUM_PA_RECOV) %>% 
  filter(NUM_PA_RECOV ==1) %>% 
  arrange(yr,mo)

idlreco$yrmo <- factor(idlreco$yrmo, levels = idlreco$yrmo[order(c(idlreco$yr,idlreco$mo))])

idl_r_plot <- ggplot(data=idlreco)+
  geom_line(size=2, mapping = (aes(x= yrmo, y=pct, group= ideal, colour = ideal)))+
  scale_y_continuous("Percent Recovered", limits = c(0,100))+
  scale_x_discrete("Date")+ 
  geom_vline(size=2,mapping = aes(xintercept=19))+
  scale_colour_manual(values=c('#0F99D6','#80C342'), guide="legend")+ 
  theme(legend.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey"),
        panel.background=element_rect(fill="white"),
        axis.text.x = element_text(angle = 45, hjust = 1, size=14))

idlr_lm <- rms::lrm(NUM_PA_RECOV ~ ideal + Era, data= fuldon3)
# more likely to recover in pre era, #more likely to not recover highrisk 


#ideal pancreas donor discards 

idldisc <- fuldon3 %>% 
  arrange(DON_DATE) %>% 
  filter( NUM_PA_RECOV > 0) %>% 
  mutate(yrmo = paste(lubridate::month(DON_DATE),lubridate::year(DON_DATE),sep = "/"),
         mo = lubridate::month(DON_DATE), yr= lubridate::year(DON_DATE)) %>% 
  group_by(yrmo,mo,yr, ideal, NUM_PA_DISC) %>% 
  tally %>% 
  mutate(pct = round((100*n/sum(n)), digits= 1)) %>% 
  ungroup(yrmo, ideal, NUM_PA_DISC) %>% 
  filter(NUM_PA_DISC ==1) %>% 
  arrange(yr,mo)

idldisc$yrmo <- factor(idldisc$yrmo, levels = idldisc$yrmo[order(c(idldisc$yr,idldisc$mo))])

idl_d_plot <- ggplot(data=idldisc)+
  geom_line(size=2, mapping = (aes(x= yrmo, y=pct, group= ideal, colour = ideal)))+
  scale_y_continuous("Percent Recovered", limits = c(0,100))+
  scale_x_discrete("Date")+ 
  geom_vline(size=2,mapping = aes(xintercept=19))+
  scale_colour_manual(values=c('#0F99D6','#80C342'), guide="legend")+ 
  theme(legend.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey"),
        panel.background=element_rect(fill="white"),
        axis.text.x = element_text(angle = 45, hjust = 1, size=14))

idldis_lm <- rms::lrm(NUM_PA_DISC ~ ideal + Era, data= fuldon3)



###############
###discard rate by DSA and Region
###############

dsadis <- fuldon3 %>% 
          filter(NUM_PA_RECOV == 1) %>% 
          group_by(OPO_CTR,Era,NUM_PA_DISC) %>% 
          tally %>% 
          mutate(pct = 100*n/sum(n)) %>% 
          filter(NUM_PA_DISC ==1) %>% 
          select(OPO_CTR, Era,pct) %>% 
          tidyr::spread(Era,pct) %>%
          mutate(Pre = ifelse(is.na(Pre), 0 , Pre),
                 Post = ifelse(is.na(Post), 0, Post))
          

cor.test(dsadis$Pre,dsadis$Post)
dsacor <- round(cor(dsadis$Pre,dsadis$Post), digits = 2)

dsadisplot <- ggplot(data=dsadis)+  
              geom_point(mapping = aes(dsadis$Post,dsadis$Pre))+
              scale_x_continuous("Post Era Discards", limits = c(0,100))+
              scale_y_continuous("Pre Era Discards", limits = c(0,100))+
              geom_text(aes(x=70,y=80, label =  paste("r = ",  dsacor, sep = " ")))+
              geom_abline(intercept = 0, slope = 1)


####################################
##Region


regdis <- fuldon3 %>% 
  filter(NUM_PA_RECOV == 1) %>% 
  group_by(REG_OPO,Era,NUM_PA_DISC) %>% 
  tally %>% 
  mutate(pct = 100*n/sum(n)) %>% 
  filter(NUM_PA_DISC ==1) %>% 
  select(REG_OPO, Era,pct) %>% 
  tidyr::spread(Era,pct) %>%
  mutate(Pre = ifelse(is.na(Pre), 0 , Pre),
         Post = ifelse(is.na(Post), 0, Post))


cor.test(regdis$Pre,regdis$Post)
regcor <- round(cor(regdis$Pre,regdis$Post), digits = 2)

regplot <- ggplot(data=regdis)+  
  geom_point(mapping = aes(regdis$Post,regdis$Pre))+
  scale_x_continuous("Post Era Discards", limits = c(0,100))+
  scale_y_continuous("Pre Era Discards", limits = c(0,100))+
  geom_text(aes(x=70,y=80, label =  paste("r = ",  dsacor, sep = " ")))+
  geom_abline(intercept = 0, slope = 1)










