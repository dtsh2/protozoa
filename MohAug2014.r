#========================================= Ministry of Health Report - Aug 2014 =========================================

require(XLConnect); library(sqldf); library(ggplot2)
wb = loadWorkbook("C:/Users/dtshayma/Documents/Massey_2014/MoH_Crypto/2014_report/MohReportAug2014/CatchmentData.xlsx", create = TRUE)
catchdata = readWorksheet(wb, sheet = "LabData")
sitedata  = readWorksheet(wb, sheet = "Sites")


# Create dummy variables
# 1. Season dummy variable
NzSeasons <- function(DATES) {
  Sm <- as.Date("2012-12-01", format = "%Y-%m-%d") # NZ Summer starts
  At <- as.Date("2012-03-01", format = "%Y-%m-%d") # NZ Autumn starts
  Wn <- as.Date("2012-06-01", format = "%Y-%m-%d") # NZ Winter starts
  Sp <- as.Date("2012-09-01", format = "%Y-%m-%d") # NZ Spring starts
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))  
  ifelse (d >= Sm | d < At, "Summer",
          ifelse (d >= At & d < Wn, "Autumn",
                  ifelse (d >= Wn & d < Sp, "Winter", "Spring")))
}
catchdata$nzseason <- NzSeasons(catchdata$Dates)

catchdata$Dates  <- as.Date(catchdata$Dates)
catchdata$mon    <- format(catchdata$Dates, "%b")
catchdata$moyr   <- format(catchdata$Dates, "%b-%y")
catchdata$sqdate <- as.character(catchdata$Dates)


##========================================= Descriptives =============================================== 
# For text: Number of positive samples for each pathogen in the last four rounds of sampling (Jun 2013 - May 2014)
sqldf("SELECT SUM(Crypto >=1) Crypto_pos,SUM(Giardia >=1) Giardia_pos, SUM(Ecoli >=1) Ecoli_pos, SUM(Campy =1) Campy_pos FROM catchdata WHERE sqdate > '2013-06-30' ")
sqldf("SELECT COUNT(*) Samplecount FROM catchdata WHERE sqdate > '2013-06-30' ") ## Number of samples collected in the last four rounds
sqldf("SELECT Site, SUM(Crypto >=1) Crypto_pos,SUM(Giardia >=1) Giardia_pos, SUM(Ecoli >=1) Ecoli_pos, SUM(Campy =1) Campy_pos FROM catchdata JOIN sitedata USING(Scode) WHERE sqdate > '2013-06-30' GROUP BY Site")

## Table 1. Summary of positive and negative results from all rounds of sampling 
table1<-sqldf("SELECT Site,  COALESCE(SUM(Crypto  < 1), 0) cr_neg,
      COALESCE(SUM(Crypto >= 1), 0) cr_pos,
      ROUND(SUM(Crypto >= 1)*100.0/COUNT(*),1) cr_pct,
      COALESCE(SUM(Giardia < 1), 0) gd_neg,
      COALESCE(SUM(Giardia >=1), 0) gd_pos,  
      ROUND(SUM(Giardia >= 1)*100.0/COUNT(*),1) gd_pct,
      COALESCE(SUM(Ecoli  < 100), 0) ec_neg,
      COALESCE(SUM(Ecoli >= 100), 0) ec_pos,  
      ROUND(SUM(Ecoli >= 250)*100.0/COUNT(*),1) ec_pct,
      COALESCE(SUM(Campy   < 1), 0) cb_neg,
      COALESCE(SUM(Campy >=  1), 0) cb_pos,  
      ROUND(SUM(Campy >= 1)*100.0/COUNT(*),1) cb_pct
      FROM catchdata a JOIN sitedata b USING(Scode) GROUP BY Site ")

## Figure 2. Percentage of samples taken in each month that were positive for each pathogen
## Fig.2 Data
modat <- sqldf("SELECT mon, (SUM(Crypto  >= 1) *100.0)/ COUNT(*) AS pos_perc, 'Cryptosporidium'  AS cate FROM catchdata GROUP BY mon 
               UNION ALL
               SELECT mon, (SUM(Giardia >= 1) *100.0)/ COUNT(*) AS pos_perc, 'Giardia' AS cate FROM catchdata GROUP BY mon
               UNION ALL
               SELECT mon, (SUM(Ecoli   >= 500)*100.0)/ COUNT(*) AS pos_perc, 'E.coli' AS cate FROM catchdata GROUP BY mon
               UNION ALL
               SELECT mon, (SUM(Campy   >= 1) *100.0)/ COUNT(*) AS pos_perc, 'Campylobacter' AS cate FROM catchdata GROUP BY mon")

## Fig.2 Plot
moplot <- ggplot(modat, aes(x=reorder(mon, match(mon, month.abb)), y=pos_perc, fill=cate)) +
                 geom_bar(position="dodge", stat="identity", width=0.4, alpha=0.75) + 
                 theme(axis.text  = element_text(size=13),
                        axis.title = element_text(size=15)) + 
                 xlab("Month") + ylab("Percentage of positive samples") +
                 guides(fill = guide_legend(title = "Organism", title.theme=element_text(size=12, angle=0, face="bold"),
                                            label.theme=element_text(size=11, angle=0, face="italic")))
ggsave(file="/home/bphiri/Documents/Bernard_PhD/Reports/MOH/Aug_2014/Fig2_MonthlyPct.png", width=30.0, height=15.0, units='cm')

## Figure 3. Percent of samples taken in each month that were positive for protozoa
## Fig.3 Data
sitdat <- sqldf("SELECT Site, (SUM(Crypto  >= 1) *100)/ COUNT(*) AS pos_perc, 'Cryptosporidium' AS cate FROM catchdata
                JOIN sitedata USING(Scode) GROUP BY Site HAVING pos_perc > 0
                UNION ALL
                SELECT Site, (SUM(Giardia >= 1) *100)/ COUNT(*) AS pos_perc, 'Giardia' AS cate FROM catchdata
                JOIN sitedata USING(Scode) GROUP BY Site HAVING pos_perc > 0")

## Fig.3 Plot
sitplot <- ggplot(sitdat, aes(x=reorder(Site, pos_perc), y=pos_perc, fill=cate)) +
                  geom_bar(position="dodge", stat="identity", width=0.4, alpha=1) + coord_flip() + 
                  theme(axis.text  = element_text(size=13),  
                          axis.title = element_text(size=15)) + 
                  xlab("Site") + ylab("Percentage of positive samples") +
                  guides(fill=guide_legend(title="Organism", title.theme=element_text(size=12, angle=0, face="bold"),
                                          label.theme=element_text(size=11, angle=0, face="italic")))
ggsave(file="/home/bphiri/Documents/Bernard_PhD/Reports/MOH/Aug_2014/Fig3_ProtozoaBySite.png", width=30.0, height=15.0, units='cm')

## Figure 4. Locations and dates of Cryptosporidium-positive samples
## Fig.4 Data: Select rows with a count of one or more protozoa oocysts
logcrypt <- sqldf("SELECT Scode, Site, Crypto, mon, moyr, nzseason,
                  (CASE WHEN Crypto >= 8 THEN 'Log_cr4' ELSE 'Log_cr3' END) AS cryptlog 
                  FROM catchdata a JOIN sitedata b USING(Scode) WHERE Crypto >=1")
logcrypt$moyr_sort <- as.numeric(as.Date(paste('01-', logcrypt$moyr, sep=''), '%d-%b-%y'))

## Fig.4 Plot
cp <- ggplot(logcrypt, aes(x=reorder(moyr, moyr_sort), y=Crypto, fill=cryptlog)) + 
             geom_bar(position="dodge", stat="identity", width=0.4, alpha=1) + facet_wrap(~ Site, nrow=3) +
             theme(axis.text.x = element_text(angle = 90, size=13),
                    axis.text.y = element_text(angle = 0,  size=13),
                    axis.title  = element_text(size=15),
                    strip.text  = element_text(size=12)) +
             labs(x="Month", y=expression(paste("Count of ", italic('Cryptosporidium'), " oocysts/100L"))) +
             scale_fill_manual(name="Log Credit", values = c("blue","red"), labels=c("Log 3", "Log 4")) +
             guides(fill=guide_legend(title="Crypto log", title.theme=element_text(size=12, angle=0, face="bold"),
                                label.theme=element_text(size=11, angle=0)))
ggsave(file="/home/bphiri/Documents/Bernard_PhD/Reports/MOH/Aug_2014/Fig4_CryptoBySite.png", width=30.0, height=15.0, units='cm')


## Figure 5. Locations and dates of Giardia-positive samples
## Fig.5 Data: Select rows with a count of one or more protozoa oocysts
loggiad <- sqldf("SELECT Scode, Site, Giardia, mon, moyr, nzseason FROM catchdata a JOIN sitedata b USING(Scode) WHERE Giardia >=1")
loggiad$moyr_sort <- as.numeric(as.Date(paste('01-', loggiad$moyr, sep=''), '%d-%b-%y'))

## Fig.5 Plot
gd <- ggplot(loggiad, aes(x=reorder(moyr, moyr_sort), y=Giardia)) + 
             geom_bar(position="dodge", stat="identity", width=0.4, alpha=1, fill='blue') + facet_wrap(~ Site, nrow=3) +
             theme(axis.text.x = element_text(angle = 90, size=13),
                      axis.text.y = element_text(angle = 0,  size=13),
                      axis.title  = element_text(size=15),
                      strip.text  = element_text(size=12)) +
            labs(x="Month", y=expression(paste("Count of ", italic('Giardia'), " cysts/100L"))) 

ggsave(file="/home/bphiri/Documents/Bernard_PhD/Reports/MOH/Aug_2014/Fig5_GiardiaBySite.png", width=30.0, height=15.0, units='cm')

## Figure 6. Locations and dates of E.coli-positive samples
## Fig.6 Data
logecoli <- sqldf("SELECT Scode, Site, (CASE WHEN Ecoli >= 1000 THEN 1000 ELSE Ecoli END) Ecoli, mon, moyr, nzseason,
                  (CASE WHEN Ecoli >= 550 THEN '>=550' ELSE '<  550' END) AS ecolilog 
                  FROM catchdata a JOIN sitedata b USING(Scode) WHERE Ecoli >= 50" )
logecoli$moyr_sort <- as.numeric(as.Date(paste('01-', logecoli$moyr, sep=''), '%d-%b-%y'))


## Fig.6 Plot
ep <- ggplot(logecoli, aes(x=reorder(moyr, moyr_sort), y=Ecoli, fill=ecolilog)) + 
             geom_bar(position="dodge", stat="identity", width=0.4, alpha=1) + facet_wrap(~ Site, ncol=3) +
             theme(axis.text.x = element_text(angle = 90, size=11),
                 axis.text.y = element_text(angle = 0,  size=11),
                 axis.title  = element_text(size=15),
                 strip.text  = element_text(size=12)) +
             labs(x="Month", y=expression(paste('Count of ',italic('E. coli'),'/100mL'))) +
             scale_fill_manual(name="Count Levels", values = c("blue","red"), labels=c("<  550", ">=550")) + 
             guides(fill=guide_legend(title.theme=element_text(size=12, angle=0, face="bold"),
                                      label.theme=element_text(size=11, angle=0)))
ggsave(file="/home/bphiri/Documents/Bernard_PhD/Reports/MOH/Aug_2014/Fig6_EcoliBySite.png", width=30.0, height=30.0, units='cm')

## Figure 7. Count/Percent of Campylobacter-positive samples taken at each site
## Fig.7 Percentages
pctcampy <- sqldf("SELECT Scode, Site, Campy, SUM(Campy)*100/COUNT(*) pos_perc
                  FROM catchdata a JOIN sitedata b USING(Scode) GROUP BY Site")

cp <- ggplot(pctcampy, aes(x=reorder(Site, pos_perc), y=pos_perc)) +
             geom_point(position="dodge", stat="identity", color='blue', size=4.5) + coord_flip() + 
             labs(x='Site', y='Percentage of positive samples') + 
             theme(axis.text.x = element_text(size=13),
                   axis.text.y = element_text(size=13),
                   axis.title  = element_text(size=15)) +
            scale_y_continuous(minor_breaks = seq(0 , 90, 5), breaks = seq(0, 90, 10))
ggsave(file="/home/bphiri/Documents/Bernard_PhD/Reports/MOH/Aug_2014/Fig7_CampyBySitePerc.png", width=30.0, height=15.0, units='cm')

