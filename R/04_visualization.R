## Functions to generate the figures used in the manuscript 
## 'US Tobacco 21 policies and potential mortality reductions by state'
library(ggplot2)
library(grid)  # For textGrob, if needed
labellist=c('A.', 'B.', 'C.', 'D.')
paperfips=c('06','21','25','55')
#load('output/model_output_10.09.24.RData')  #load only needed if running file separately

##------------------- Smoking Attributable Deaths Averted ---------------------
SADs_averted_fig <- function(fipscode, labelval) {
  thisstate <- subset(df_mort.policy, state == fipscode & gender == "Both")
  thisstate$policy.scenario <- factor(thisstate$policy.scenario,levels=c("fedstatelocal","statelocal","local")) # re-order policy scenarios
  fig <- ggplot(data = thisstate) + 
    scale_color_manual(name = "policy",
                       labels = c("Combined federal T21", 'State&local',"Local"),
                       values = c("#E69F00",  "#0072B2","#009E73")) +
    geom_line(aes(x = year, y = SADsAvertedcum, color = policy.scenario, linetype = mla.effects), lwd = 0.6) +
    scale_linetype_manual(name = "T21 effects estimate", 
                          labels = c("Lower", "Main", "Upper"), 
                          values = c("dotted", "solid", "twodash")) +
    scale_y_continuous(name = "SADs averted", 
                       labels = scales::comma, 
                       limits = c(0, 43000), 
                       breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(name = "Year", 
                       limits = c(2025, 2100), 
                       breaks = seq(2030, 2100, 10)) +
    labs(title = if (fipscode == '00') {
      paste0("T21 deaths averted for for US (50 states + DC)")
    } else {
      paste0(labelval, ' ', fips(fipscode, to = 'Name'))
    }) +
    theme_light() + 
    guides(color = guide_legend(nrow = 3)) +
    guides(linetype = guide_legend(nrow = 3)) +
    theme(legend.title = element_text(size = 9), 
          text = element_text(size = 10), 
          axis.text.x = element_text(angle = 60, hjust = 1))
  
  return(fig)
}

#jpeg(file=paste0('figs/SADs_averted_manuscript_', date_variable, '.jpg'), width=6, height=6, units="in", res=1800)
pdf(file=paste0('figs/SADs_averted_manuscript_', date_variable, '.pdf'), width=6, height=6)
ggarrange(SADs_averted_fig(paperfips[1],labellist[1]),SADs_averted_fig(paperfips[2], labellist[2]),
          SADs_averted_fig(paperfips[3], labellist[3]), SADs_averted_fig(paperfips[4], labellist[4]),
          ncol=2,nrow=2, common.legend = TRUE, legend='top')

dev.off()

#-------------Life Years Gained -------------------------------------------------
LYG_manuscript<-function(fipscode, labelval){
  thisstate <- subset(df_mort.policy,state==fipscode&gender=="Both")
  thisstate$policy.scenario <- factor(thisstate$policy.scenario,levels=c("fedstatelocal","statelocal","local")) # re-order policy scenarios
  fig <- ggplot(data=thisstate) + 
    scale_color_manual(name="policy",labels=c("Combined federal T21", 'State&local',"Local"),values=c("#E69F00","#0072B2","#009E73"))+
    geom_line(aes(x=year, y=LYGcum, color=policy.scenario,linetype=mla.effects), lwd=0.6) +
    scale_linetype_manual(name="T21 effects estimate",labels=c("Lower", "Main", 'Upper'),values=c("dotted","solid","twodash"))+
    scale_y_continuous(name="Life Years Gained",labels=scales::comma,limits=c(0, 1305000),breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(name="Year",limits=c(2025,2100),breaks=seq(2030,2100, 10)) +
    {if(fipscode=='00') # if entire US
      labs(title= paste0("T21 deaths averted for for US (50 states + DC)"))}+
    {if(fipscode!='00') # if state
      labs(title= paste0(labelval, ' ', fips(fipscode,to='Name')))}+
    theme_light()+ 
    guides(color = guide_legend(nrow = 3))+
    guides(linetype = guide_legend(nrow = 3))+
    theme(legend.title = element_text(size=9),text = element_text(size=10),
          axis.text.x=element_text(angle=60, hjust=1))
  return(fig)
}
#jpeg(file=paste0('figs/LYG_manuscript_', date_variable, '.jpg'), width=6, height=6, units="in", res=1800)
pdf(file=paste0('figs/LYG_manuscript_', date_variable, '.pdf'), width=6, height=6)
ggarrange(LYG_manuscript(paperfips[1],labellist[1]),LYG_manuscript(paperfips[2], labellist[2]),
          LYG_manuscript(paperfips[3], labellist[3]), LYG_manuscript(paperfips[4], labellist[4]),
          ncol=2,nrow=2, common.legend = TRUE)

dev.off()

#---------------- Policy Coverage Timeline -------------------------------------
df_t21data2003.2024[is.na(df_t21data2003.2024$local), 'local']<-0
df_t21data2003.2024[is.na(df_t21data2003.2024$statelocal), 'statelocal']<-0
df_t21data2003.2024[is.na(df_t21data2003.2024$fedstatelocal), 'fedstatelocal']<-0


plot_policy_cover<- function(s,labelval){
  thisstate <- subset(df_t21data2003.2024, statefips0==s)
  stateT21date <- subset(thisstate,t21statepolicy==1)$date
  stateT21 <- subset(thisstate,date=="2024-01-01")$statelocal
  localT21 <- subset(thisstate,date=="2024-01-01")$local
  
  plot<- ggplot(data=thisstate)+
    geom_line(aes(x=date, y =fedstatelocal*100))+
    geom_ribbon(aes(x=date, ymin=statelocal*100, ymax =fedstatelocal*100),alpha=0.3, fill="#E69F00")+
    geom_ribbon(aes(x=date, ymin=local*100,ymax =statelocal*100),alpha=0.3, fill="#0072B2")+
    geom_area(aes(x=date, y =local*100),alpha=0.3, fill="#009E73")+
    geom_vline(xintercept = as.numeric(as.Date("2019-12-20")), linetype="dashed", color="#E69F00", size=0.8)+
    geom_vline(xintercept = as.numeric(as.Date(stateT21date)), linetype="dotted", color="#0072B2", size=0.8)+
    annotate(geom="text",x=as.Date("2014-01-05"), y=90,label="Federal T21",color="#E69F00",hjust=0, size=3)+
    annotate(geom="text",x=as.Date("2014-01-05"), y=55,label="Combined T21",color="black",hjust=0, size=3)+
    {if(stateT21==1) # if state-wide policy, then annotate
      annotate(geom="text",x=as.Date("2014-01-05"), y=80,label="State T21",color="#0072B2",hjust=0, size=3)}+
    {if(localT21>0) # if local coverage, then annotate
      annotate(geom="text",x=as.Date("2014-01-05"), y=70,label="Local T21",color="#009E73",hjust=0, size=3)}+
    ylab("Percent of population covered by T21 (%)")+
    xlab("Year")+
    labs(title= paste0(labelval, ' ', fips(s,to='Name')))+
    scale_y_continuous(limits=c(0,100))+
    scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                 limit=c(as.Date("2014-01-01"),as.Date("2024-12-31")))+
    theme_light()  +theme(text = element_text(size=7),legend.position="none", 
                          axis.text.x=element_text(angle=60, hjust=1),panel.grid.minor = element_blank())
  return(plot) 
}

#jpeg(file=paste0('figs/state_cover_manuscript', date_variable, '.jpg'), width=6.5,height = 6.5, units ="in", res=1800)
pdf(file=paste0('figs/state_cover_manuscript', date_variable, '.pdf'), width=6.5,height = 6.5)
ggarrange(plot_policy_cover(paperfips[1],labellist[1]),plot_policy_cover(paperfips[2], labellist[2]),
          plot_policy_cover(paperfips[3], labellist[3]), plot_policy_cover(paperfips[4], labellist[4]),
          ncol=2,nrow=2, common.legend = TRUE)
dev.off()

#-------- stacked mortality reduction bar plots all states--------------------------
#true SADs averted values
genderval='Both'
barplot_allstates_SADs <-ggplot(subset(df_tiered_mort, gender==genderval & year==2100), aes(x=reorder(abbr, policyeffort), y=policyeffort, fill=factor(policy_tier, levels=c('federal', 'state','local')))) +
  geom_bar(stat='identity')+ coord_flip()+
  scale_fill_manual(values = c('red3', 'blue4', 'aquamarine3'), 
                    labels = c('Federal', 'State', 'Local')) + 
  labs(x="State",y="Smoking-attributable deaths averted")+
  scale_y_continuous(labels = scales::comma)+
  guides(fill = guide_legend(title = "T21 policy tier"))+labs(title= paste0('A. Absolute mortality reductions by state'))+theme_light()

#relative reduction in SADs
barplot_allstates_RR <-ggplot(subset(df_tiered_mort, gender==genderval & year==2100), aes(x=reorder(abbr, SADreduc_tier), y=SADreduc_tier, fill=factor(policy_tier, levels=c('federal', 'state','local')))) +
  geom_bar(stat='identity')+ coord_flip()+
  scale_fill_manual(values = c('red3', 'blue4', 'aquamarine3'), 
                    labels = c('Federal', 'State', 'Local')) + 
  labs(x="State",y="% reduction in smoking-attributable deaths")+
  scale_y_continuous(limits=c(0, 8),n.breaks = 10)+
  guides(fill = guide_legend(title = "T21 policy tier"))+labs(title= paste0('B. Relative mortality reductions by state'))+theme_light()

########### Both
#jpeg(filename = paste0('figs/SADs_allstates_manuscript_', date_variable, '.jpg'),width=8, height=6, units ="in", res=1800)
pdf(file = paste0('figs/SADs_allstates_manuscript_', date_variable, '.pdf'),width=8, height=6)
genderval='Both'
ggarrange(barplot_allstates_SADs, barplot_allstates_RR, nrow=1, common.legend = TRUE)
dev.off()

#------------------- US T21 policy coverage----------------------------------------
load("data/us_t21coverage.RData")
#add in state data
T21_state <- read_excel("data-raw/T21policycoverage.xlsx",sheet="Tobacco21.org")
T21_state$Date <- as.Date(with(T21_state,paste(year,month,day,sep="-")),"%Y-%m-%d")
#statelabels_orig = merge(us_t21coverage[,c("year","month","statelocal")],T21_state[,c("year","month","abbrev","Date")], by=c("month","year"),all.x=TRUE)
statelabels = merge(T21_state[,c("year","month","abbrev","Date")],us_t21coverage[,c("year","month","statelocal")], by=c("month","year"),all.x=TRUE)

#manuscript version
plotUS <- ggplot(data=us_t21coverage) +
  geom_line(aes(x=Date, y=fedstatelocal*100)) +
  geom_ribbon(aes(x=Date, ymin=statelocal*100, ymax=fedstatelocal*100), alpha=0.3, fill="#E69F00") +
  geom_ribbon(aes(x=Date, ymin=local*100, ymax=statelocal*100), alpha=0.3, fill="#0072B2") +
  geom_area(aes(x=Date, y=local*100), alpha=0.3, fill="#009E73") +
  geom_vline(xintercept = as.numeric(as.Date("2019-12-20")), linetype="dashed", color="#E69F00", size=0.6) +
  annotate(geom="text", x=as.Date("2014-01-05"), y=35, label="Combined T21", color="black", hjust=0, size=5) +
  annotate(geom="text", x=as.Date("2014-01-05"), y=90, label="Federal T21", color="#E69F00", hjust=0, size=5) +
  annotate(geom="text", x=as.Date("2014-01-05"), y=80, label="State T21", color="#0072B2", hjust=0, size=5) +
  annotate(geom="text", x=as.Date("2014-01-05"), y=70, label="Local T21", color="#009E73", hjust=0, size=5) +
  geom_text_repel(data=statelabels, aes(x=Date, y=statelocal*100, label=abbrev), color="#0072B2",
                  bg.color="white", bg.r=0.15, force=1.5, nudge_y=0.1, min.segment.length=0, segment.alpha=0.5,
                  max.overlaps=Inf, size=4, segment.size=0.3) +
  ylab("Percent of population covered by T21 (%)") +
  xlab("Year") +
  scale_y_continuous(limits=c(0, 100)) +
  scale_x_date(date_labels="%Y", date_breaks="1 year",
               limits=c(as.Date("2014-01-01"), as.Date("2024-12-31"))) +
  theme_light() +
  theme(
    text = element_text(size=10),  # Increase overall text size
    axis.title = element_text(size=12),  # Axis titles
    axis.text = element_text(size=10),  # Axis text
    legend.text = element_text(size=10),  # Legend text
    legend.title = element_text(size=12),  # Legend title
    axis.text.x = element_text(angle=60, hjust=1),  # Adjust x-axis text angle
    panel.grid.minor = element_blank()
  )


pdf(file="figs/US_T21policycoverage_2003.2024.pdf", width=8,height = 6,onefile=TRUE)
print(plotUS)
dev.off()



