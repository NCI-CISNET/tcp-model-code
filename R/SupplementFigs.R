## 
### loop through all states to make snapshot pdfs for supplement 
setwd("/Users/jt936/Dropbox/state_tcp_tool/LC_code/TCP_model/")
library(ggpubr)
library(stringr)
library(readr)
library(openxlsx)
library(reshape2)
library(dplyr)
library(grid)
library(gridBase)
library(gridExtra)
library(ggplot2)
library(cdlTools)
ages=18.99
minage=18
maxage=99
labellist=c('A.', 'B.', 'C.', 'D.', 'E.', 'F.')
statefips=c('01','02','04','05','06','08','09','10','11','12','13','15','16',
            '17','18','19','20','21','22','23','24','25','26','27','28','29',
            '30','31','32','33','34','35','36','37','38','39','40','41','42',
            '44','45','46','47','48','49','50','51','53','54','55','56')

statefips=c('26')
statetotals <- read.xlsx('data-raw/Census State Population Total 2003-2023.xlsx',sheet='statetotals')
statetotals$fips <- fips(statetotals$statename,to="FIPS")
statetotals<-statetotals[-1,]
#---------------------- Load data sets --------------------------------------------------------
load("data/T21policycoverage2005.2025.Rda") # T21 policy coverage data
load('data/harmonized_tuscps_prevs.Rda') # TUSCPS survey data for model verification

# Figure Functions --------------------------------------------------------
# 6 figures 
# TUSCPS prev
TUSCPS_supp<-function(fipscode, labelval){
  thisstate <- subset(prevstateTUS,state==fipscode & mla.effect=='baseline' & policy.scen=='baseline' & age=='18.99')
  fig1 <- ggplot() + geom_line(data=thisstate, aes(x=year, y=prev*100, color=gender, linetype=policy.scen),show.legend = FALSE) + 
    geom_pointrange(data=subset(tuscps_prevs,st_fips==fipscode & age==ages & smkstatus=='currentsmk2'),aes(x=year,y=est*100,ymin= ci_low*100, ymax=ci_high*100, color=gender))+ 
    scale_y_continuous(name="Smoking prevalence (%)",limits=c(0,50),breaks=seq(0,50,5)) +
    # scale_x_continuous(name="Year",limits=c(1990,2025),breaks=seq(1990,2025, 5)) +
    scale_x_continuous(name="Year",limits=c(1990,2100),breaks=c(1990,seq(2000,2100,20) )) +
    theme_light()+ 
    labs(title= paste0(labelval, ' ', "Model vs. TUS-CPS prevalence, ages 18-99"))+
    theme(legend.text=element_text(size=9),
          legend.position = c(.95, .95),legend.justification = c("right", "top"), legend.box.just = "right",legend.title=element_blank(),
          text = element_text(size=9),axis.text.x=element_text(angle=60, hjust=1),
          legend.background = element_rect(fill=alpha('white', 0.4)))
  return(fig1)
}

# prev reduction 
prevs_policycoverage <- function(fipscode, labelval){
  thisstate <- subset(prevdiff, state == fipscode)
  thisstate$policy.scen<-factor(thisstate$policy.scen, levels = c("fedstatelocal", "statelocal", "local"))
  # re-order policy scenarios
  fig <- ggplot(data = thisstate) +
    geom_line(aes(x = year, y = (diff) * 100, color = policy.scen), lwd = 0.8) +
    scale_color_manual(name = "policy",
                       labels = c('fedstatelocal' = "Combined federal T21", 'statelocal' = 'State&local', 'local' = "Local"),
                       values = c('fedstatelocal' = "#E69F00", 'statelocal' = "#0072B2", 'local' = "#009E73")) +
    scale_y_continuous(name = "Prevalence reduction (pp)", limits = c(0, 2.2), breaks = seq(0, 2.2, 0.2)) +
    scale_x_continuous(name = "Year", limits = c(2010, 2100), breaks = seq(2010, 2100, 10)) +
    theme_light() + 
    theme(legend.text = element_text(size = 9),
          text = element_text(size = 9),
          axis.text.x = element_text(angle = 60, hjust = 1),
          legend.background = element_rect(fill = alpha('white', 0.4)),
          legend.position = c(.05, .95),
          legend.justification = c("left", "top"),
          legend.box.just = "left") +
    labs(title = paste0(labelval, ' ', "Smoking prevalence reduction, ages ", minage, "-", maxage))
  
  return(fig)
}

# T21 policy coverage, 2014-2100

plotstate<- function(s,labelval){
  thisstate <- subset(df_t21data2005.2025, statefips0==s)
  stateT21date <- subset(thisstate,t21statepolicy==1)$date
  stateT21 <- subset(thisstate,date=="2025-01-01")$statelocal
  localT21 <- subset(thisstate,date=="2025-01-01")$local
  
  plot<- ggplot(data=thisstate)+
    geom_line(aes(x=date, y =fedstatelocal*100))+
    geom_ribbon(aes(x=date, ymin=statelocal*100, ymax =fedstatelocal*100),alpha=0.3, fill="#E69F00")+
    geom_ribbon(aes(x=date, ymin=local*100,ymax =statelocal*100),alpha=0.3, fill="#0072B2")+
    geom_area(aes(x=date, y =local*100),alpha=0.3, fill="#009E73")+
    geom_vline(xintercept = as.numeric(as.Date("2019-12-20")), linetype="dashed", color="#E69F00", size=0.8)+
    geom_vline(xintercept = as.numeric(as.Date(stateT21date)), linetype="dotted", color="#0072B2", size=0.8)+
    annotate(geom="text",x=as.Date("2014-01-05"), y=90,label="Federal T21",color="#E69F00",hjust=0, size=2.5)+
    annotate(geom="text",x=as.Date("2014-01-05"), y=55,label="Combined T21",color="black",hjust=0, size=2.5)+
    {if(stateT21==1) # if state-wide policy, then annotate
      annotate(geom="text",x=as.Date("2014-01-05"), y=80,label="State T21",color="#0072B2",hjust=0, size=2.5)}+
    {if(localT21>0) # if local coverage, then annotate
      annotate(geom="text",x=as.Date("2014-01-05"), y=70,label="Local T21",color="#009E73",hjust=0, size=2.5)}+
    ylab("Percent of population covered (%)")+
    xlab("Year")+
    labs(title= paste0(labelval, ' ',' Tobacco 21 policy coverage'))+
    scale_y_continuous(limits=c(0,100))+
    scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                 limit=c(as.Date("2014-01-01"),as.Date("2024-12-31")))+
    theme_light()+theme(text = element_text(size=9),legend.position="none", axis.text.x=element_text(angle=60, hjust=1),panel.grid.minor = element_blank())
  return(plot) 
}

# SADs
SADfig_supp<-function(fipscode, labelval){
  thisstate <- subset(df_mort.policy, state==fipscode & gender=="Both")
  thisstate$policy.scenario <- factor(thisstate$policy,levels=c("fedstatelocal","statelocal","local")) # re-order policy scenarios
  fig <- ggplot(data=thisstate) + 
    scale_color_manual(name="policy",labels=c("Combined federal T21", 'State&local', "Local"),values=c("#E69F00","#0072B2","#009E73"))+
    geom_line(aes(x=year, y=SADsAvertedcum, color=policy.scenario,linetype=mla.effects), lwd=0.6) +
    scale_linetype_manual(name="T21 effects estimate",labels=c("Lower", "Main", 'Upper'),values=c("dotted","solid","twodash"))+
    scale_y_continuous(name="SADs averted",labels=scales::comma,breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(name="Year",limits=c(2008,2100),breaks=seq(2010,2100, 10)) +
    {if(fipscode=='00') # if entire US
      labs(title= paste0("T21 deaths averted for for US (50 states + DC)"))}+
    {if(fipscode!='00') # if state
      labs(title= paste0(labelval, ' ', 'Cumulative SADs averted'))}+
    theme_light()+ 
    guides(color = guide_legend(nrow = 3))+
    guides(linetype = guide_legend(nrow = 3))+
    theme(legend.title = element_text(size=9),text = element_text(size=9), axis.text.x=element_text(angle=60, hjust=1),
          legend.background = element_rect(fill=alpha('white', 0.4)),
          legend.position = c(.05, .95),legend.justification = c("left", "top"), legend.box.just = "left" )
  return(fig)
}

# LYG
LYG_manuscript<-function(fipscode, labelval){
  thisstate <- subset(df_mort.policy,state==fipscode & gender=="Both")
  thisstate$policy.scenario <- factor(thisstate$policy.scenario,levels=c("fedstatelocal","statelocal","local")) # re-order policy scenarios
  fig <- ggplot(data=thisstate) + 
    scale_color_manual(name="policy",labels=c('fedstatelocal'="Combined federal T21", 'statelocal'='State&local', 'local'="Local"),values=c('fedstatelocal'="#E69F00",'statelocal'="#0072B2",'local'="#009E73"))+
    geom_line(aes(x=year, y=LYGcum, color=policy.scenario,linetype=mla.effects), lwd=0.6) +
    scale_linetype_manual(name="T21 effects estimate",labels=c("Lower", "Main", 'Upper'),values=c("dotted","solid","twodash"))+
    #scale_fill_discrete(breaks=c("fedstatelocal","statelocal","local"))+
    scale_y_continuous(name="Life years gained",labels=scales::comma,breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(name="Year",limits=c(2008,2100),breaks=seq(2010,2100, 10)) +
    {if(fipscode=='00') # if entire US
      labs(title= paste0("T21 deaths averted for for US (50 states + DC)"))}+
    {if(fipscode!='00') # if state
      labs(title= paste0(labelval, ' ', 'Cumulative life years gained'))}+
    theme_light()+ 
    guides(linetype = guide_legend(nrow = 3))+
    theme(legend.title = element_text(size=9),text = element_text(size=9),axis.text.x=element_text(angle=60, hjust=1),
          legend.background = element_rect(fill=alpha('white', 0.4)),
          legend.position = c(.05, .95),legend.justification = c("left", "top"), legend.box.just = "left" )
  return(fig)
}

# table

LYGSAD2100<-function(fipscode, sex){
  state2100=subset(vals2100, state==fipscode & gender==sex)
  upper=subset(state2100, mla.effects=='upper')
  main=subset(state2100, mla.effects=='main')
  lower=subset(state2100, mla.effects=='lower')
  for (mla.effects in c('lower', 'main', 'upper')){
    #subset
    df=get(mla.effects)
    fed.LYG=df[df$policy.scenario=='fedstatelocal', 'LYGcum']-df[df$policy.scenario=='statelocal', 'LYGcum']
    fed.SAD=df[df$policy.scenario=='fedstatelocal', 'SADsAvertedcum']-df[df$policy.scenario=='statelocal', 'SADsAvertedcum']
    state.LYG=df[df$policy.scenario=='statelocal', 'LYGcum']-df[df$policy.scenario=='local', 'LYGcum']
    state.SAD=df[df$policy.scenario=='statelocal', 'SADsAvertedcum']-df[df$policy.scenario=='local', 'SADsAvertedcum']
    loc.SAD=df[df$policy.scenario=='local', 'SADsAvertedcum']
    loc.LYG=df[df$policy.scenario=='local', 'LYGcum']
    fedeffort=cbind(fedeffort, fed.SAD, fed.LYG)
    state.effort=cbind(state.effort, state.SAD, state.LYG)
    loc.effort=cbind(loc.effort, loc.SAD, loc.LYG)
  }
  effortdf=cbind(rbind(effortdf, loc.effort, state.effort, fedeffort),c('local', 'state', 'federal'),sex)
  return(effortdf)
}  

tableplot<-function(fipscode, labelval){
  
  edf=NULL  
  for (sex in c('Female', 'Male')){
    efforttemp=LYGSAD2100(fipscode, sex)
    edf=rbind(edf, efforttemp)
  }
  colnames(edf)=c('lowerSAD','lowerLYG', 'mainSAD', 'mainLYG','upperSAD',  'upperLYG', 'policyeffort', 'gender') 
  edf=as.data.frame(edf)
  edf <- edf %>%
    mutate(across(c(lowerSAD, mainSAD, upperSAD, lowerLYG, mainLYG, upperLYG), as.numeric)) %>%
    mutate(across(c(lowerSAD, mainSAD, upperSAD, lowerLYG, mainLYG, upperLYG), round))%>%
    mutate(across(c(lowerSAD, mainSAD, upperSAD, lowerLYG, mainLYG, upperLYG), ~ signif(., digits = 2)))
  
  M.locSAD=subset(edf, gender=='Male' & policyeffort=='local')
  F.locSAD=subset(edf, gender=='Female' & policyeffort=='local')
  M.staSAD=subset(edf, gender=='Male' & policyeffort=='state')
  F.staSAD=subset(edf, gender=='Female' & policyeffort=='state')
  M.fedSAD=subset(edf, gender=='Male' & policyeffort=='federal')
  F.fedSAD=subset(edf, gender=='Female' & policyeffort=='federal')
  totals=sum(M.locSAD$mainSAD+M.staSAD$mainSAD+M.fedSAD$mainSAD)
  locpercent=round(M.locSAD$mainSAD/totals*100,digits = 2)
  statepercent=round(M.staSAD$mainSAD/totals*100,digits = 2)
  fedpercent=round(M.fedSAD$mainSAD/totals*100,digits = 2)
  
  numeric_cols <- c('lowerSAD', 'mainSAD', 'upperSAD', 'lowerLYG', 'mainLYG', 'upperLYG')
  M.locSAD[, numeric_cols] <- format(as.data.frame(M.locSAD[, numeric_cols]), big.mark = ",", scientific = FALSE)
  F.locSAD[, numeric_cols] <- format(as.data.frame(F.locSAD[, numeric_cols]), big.mark = ",", scientific = FALSE)
  M.staSAD[, numeric_cols] <- format(as.data.frame(M.staSAD[, numeric_cols]), big.mark = ",", scientific = FALSE)
  F.staSAD[, numeric_cols] <- format(as.data.frame(F.staSAD[, numeric_cols]), big.mark = ",", scientific = FALSE)
  M.fedSAD[, numeric_cols] <- format(as.data.frame(M.fedSAD[, numeric_cols]), big.mark = ",", scientific = FALSE)
  F.fedSAD[, numeric_cols] <- format(as.data.frame(F.fedSAD[, numeric_cols]), big.mark = ",", scientific = FALSE)
  
  df = rbind(c(paste0("Local\n"," (",locpercent,"%)"),paste0(M.locSAD$mainSAD,"\n (",M.locSAD$lowerSAD,"-",M.locSAD$upperSAD,")"),paste0(M.locSAD$mainLYG, "\n (",M.locSAD$lowerLYG,"-",M.locSAD$upperLYG,")"),paste0(F.locSAD$mainSAD,"\n (",F.locSAD$lowerSAD,"-",F.locSAD$upperSAD,")"),paste0(F.locSAD$mainLYG, "\n (",F.locSAD$lowerLYG,"-",F.locSAD$upperLYG,")")),
             c(paste0("State\n"," (",statepercent,"%)"),paste0(M.staSAD$mainSAD,"\n (",M.staSAD$lowerSAD,"-",M.staSAD$upperSAD,")"),paste0(M.staSAD$mainLYG, "\n (",M.staSAD$lowerLYG,"-",M.staSAD$upperLYG,")"),paste0(F.staSAD$mainSAD,"\n (",F.staSAD$lowerSAD,"-",F.staSAD$upperSAD,")"),paste0(F.staSAD$mainLYG, "\n (",F.staSAD$lowerLYG,"-",F.staSAD$upperLYG,")")),
             c(paste0("Federal\n"," (",fedpercent,"%)"),paste0(M.fedSAD$mainSAD,"\n (",M.fedSAD$lowerSAD,"-",M.fedSAD$upperSAD,")"),paste0(M.fedSAD$mainLYG, "\n (",M.fedSAD$lowerLYG,"-",M.fedSAD$upperLYG,")"),paste0(F.fedSAD$mainSAD,"\n (",F.fedSAD$lowerSAD,"-",F.fedSAD$upperSAD,")"),paste0(F.fedSAD$mainLYG, "\n (",F.fedSAD$lowerLYG,"-",F.fedSAD$upperLYG,")")))
  
  colnames(df) <-   c("Policy tier\n (% contribution)","Men:\n SADs averted","\n LYG","Women:\n SADs averted","\n LYG")
  
  # get size of the state population in 2023 based on Census Bureau data
  popsize = format(statetotals[statetotals$fips==as.numeric(fipscode),"2023"],big.mark=",",scientific=FALSE)
  
  g= ggtexttable(t(df), theme= ttheme("light",base_size=7))
  g=g %>%
    tab_add_footnote(text = paste0("Notes: T21 = Tobacco 21; LYG = life-years gained; \nSADs = premature smoking-attributable deaths.\n 
                            Parentheses indicate lower and upper-bound estimates \nusing 95% confidence interval policy effects sizes.\n
                            2023 Census population estimate: ",popsize), 
                     size = 7)
  
  fig1 <- ggplot() + 
    scale_y_continuous(name="",limits=c(0,50),breaks=seq(0,50,5)) +
    scale_x_continuous(name="",limits=c(1990,2100),breaks=c(1990,seq(2000,2100,20) )) +
    theme_light()+ 
    labs(title= paste0(labelval, ' ', "Mortality reductions by T21 policy tier"))+
    theme(text = element_text(size=9),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank())+
    annotation_custom(ggplotGrob(plot(g)),xmin = 1990,xmax=2100,ymin=0,ymax=50)
  return(fig1)
}


# Generate PDFs ------------------------------------------------------------

# Main figures
load('output/model_output_MI_11.25.24.RData') # load main results
prevstateTUS=subset(df_prev.by.state, gender=='Women' | gender=='Men')
prevstate=subset(df_prev.by.state, gender=='Both' & age=='18.99')
baseprev=subset(prevstate, policy.scen=='baseline' & mla.effect=='baseline')
localprev=subset(prevstate, policy.scen=='local' & mla.effect=='main')
localprev$diff=baseprev$prev-localprev$prev
stateprev=subset(prevstate, policy.scen=='statelocal' & mla.effect=='main')
stateprev$diff=baseprev$prev-stateprev$prev
fedprev=subset(prevstate, policy.scen=='fedstatelocal' & mla.effect=='main')
fedprev$diff=baseprev$prev-fedprev$prev
prevdiff=rbind(localprev,stateprev, fedprev)
vals2100=subset(df_mort.policy, year==2100) 
fedeffort=state.effort=loc.effort=effortdf=NULL

fignum = 24 # starting figure number

for (s in statefips) {
  plot <- ggarrange(
    tableplot(s,labellist[1]),
    TUSCPS_supp(s, labellist[2]), 
    plotstate(s, labellist[3]), 
    prevs_policycoverage(s, labellist[4]),
    SADfig_supp(s, labellist[5]), 
    LYG_manuscript(s, labellist[6]),
    ncol = 3, nrow = 2
  )
  
  # Add margins and title using theme() and text_grob()
  plot <- annotate_figure(plot, 
                          top = text_grob(paste0("eFigure ",fignum,". ",fips(as.numeric(s), to = "Name")," T21 model outcomes"), face = "bold", size = 12)
  ) 
  jpeg(filename = paste0("figs/",sprintf("%03d", fignum), ".jpg"),width=11, height=8.5, units ="in", res=1000)
  print(plot) # Print plot to PDF
  dev.off()
  fignum = fignum+1
}

# Exponential decay figures
load('output/model_output_MI_exp_11.25.24.RData') # exponential decay scenario

prevstateTUS=subset(df_prev.by.state, gender=='Women' | gender=='Men')
prevstate=subset(df_prev.by.state, gender=='Both' & age=='18.99')
baseprev=subset(prevstate, policy.scen=='baseline' & mla.effect=='baseline')
localprev=subset(prevstate, policy.scen=='local' & mla.effect=='main')
localprev$diff=baseprev$prev-localprev$prev
stateprev=subset(prevstate, policy.scen=='statelocal' & mla.effect=='main')
stateprev$diff=baseprev$prev-stateprev$prev
fedprev=subset(prevstate, policy.scen=='fedstatelocal' & mla.effect=='main')
fedprev$diff=baseprev$prev-fedprev$prev
prevdiff=rbind(localprev,stateprev, fedprev)
vals2100=subset(df_mort.policy, year==2100) 
fedeffort=state.effort=loc.effort=effortdf=NULL
fignum=75
for (s in statefips) {
  plot <- ggarrange(
    tableplot(s,labellist[1]),
    TUSCPS_supp(s, labellist[2]), 
    plotstate(s, labellist[3]), 
    prevs_policycoverage(s, labellist[4]),
    SADfig_supp(s, labellist[5]), 
    LYG_manuscript(s, labellist[6]),
    ncol = 3, nrow = 2
  )
  
  # Add margins and title using theme() and text_grob()
  plot <- annotate_figure(plot, 
                          top = text_grob(paste0("eFigure ",fignum,". ",fips(as.numeric(s), to = "Name")," T21 model outcomes with policy decay"), face = "bold", size = 12)
  ) 
  jpeg(filename = paste0("figs/",sprintf("%03d", fignum), ".jpg"),width=11, height=8.5, units ="in", res=1000)
  print(plot) # Print plot to PDF
  dev.off()
  fignum = fignum+1
}


# 
# fignum = 2
# for (s in statefips) {
#   print(paste0("eFigure ",fignum,". ",fips(as.numeric(s), to = "Name")," T21 model outcomes"))
#   fignum = fignum+1
# }
# 
# 
# for (s in statefips) {
#   print(paste0("eFigure ",fignum,". ",fips(as.numeric(s), to = "Name")," T21 model outcomes with policy decay"))
#   fignum = fignum+1
# }