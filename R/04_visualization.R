## Functions to generate the figures used the in the manuscript 
## 'US Tobacco 21 policies and potential mortality reductions by state'

labellist=c('A.', 'B.', 'C.', 'D.')
paperfips=c('06','21','25','55')

##------------------- Smoking Attributable Deaths Averted ---------------------
SADsAvertedFig <- function(fipscode, labelval) {
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

jpeg(file=paste0('../figs/SADs_averted_manuscript_', date_variable, '.jpg'), width=6, height=6, units="in", res=1800)
ggarrange(SADsAvertedFig(paperfips[1],labellist[1]),SADsAvertedFig(paperfips[2], labellist[2]),
          SADsAvertedFig(paperfips[3], labellist[3]), SADsAvertedFig(paperfips[4], labellist[4]),
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
jpeg(file=paste0('../figs/LYG_manuscript_', date_variable, '.jpg'), width=6, height=6, units="in", res=1800)
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
    annotate(geom="text",x=as.Date("2014-01-05"), y=90,label="Federal T21",color="#E69F00",hjust=0, size=2)+
    annotate(geom="text",x=as.Date("2014-01-05"), y=55,label="Combined T21",color="black",hjust=0, size=2)+
    {if(stateT21==100) # if state-wide policy, then annotate
      annotate(geom="text",x=as.Date("2014-01-05"), y=80,label="State T21",color="#0072B2",hjust=0, size=2)}+
    {if(localT21>0) # if local coverage, then annotate
      annotate(geom="text",x=as.Date("2014-01-05"), y=70,label="Local T21",color="#009E73",hjust=0, size=2)}+
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

jpeg(file=paste0('../figs/state_cover_manuscript', date_variable, '.jpg'), width=6.5,height = 6.5, units ="in", res=1800)
ggarrange(plot_policy_cover(paperfips[1],labellist[1]),plot_policy_cover(paperfips[2], labellist[2]),
          plot_policy_cover(paperfips[3], labellist[3]), plot_policy_cover(paperfips[4], labellist[4]),
          ncol=2,nrow=2, common.legend = TRUE)
dev.off()

#-------- stacked mortality reduction bar plots all states--------------------------
#true SADs averted values
genderval='Both'
Barplot_allstates_SADs <-ggplot(subset(df_tiered_mort, gender==genderval & year==2100), aes(x=reorder(abbr, policyeffort), y=policyeffort, fill=factor(policy_tier, levels=c('federal', 'state','local')))) +
  geom_bar(stat='identity')+ coord_flip()+
  scale_fill_manual(values = c('red3', 'blue4', 'aquamarine3'), 
                    labels = c('Federal', 'State', 'Local')) + 
  labs(x="State",y="Smoking-attributable deaths averted")+
  scale_y_continuous(labels = scales::comma)+
  guides(fill = guide_legend(title = "T21 policy tier"))+labs(title= paste0('A. Absolute mortality reductions by state'))+theme_light()

#relative reduction in SADs
Barplot_allstates_RR <-ggplot(subset(df_tiered_mort, gender==genderval & year==2100), aes(x=reorder(abbr, SADreduc_tier), y=SADreduc_tier, fill=factor(policy_tier, levels=c('federal', 'state','local')))) +
  geom_bar(stat='identity')+ coord_flip()+
  scale_fill_manual(values = c('red3', 'blue4', 'aquamarine3'), 
                    labels = c('Federal', 'State', 'Local')) + 
  labs(x="State",y="% reduction in smoking-attributable deaths")+
  scale_y_continuous(limits=c(0, 8),n.breaks = 10)+
  guides(fill = guide_legend(title = "T21 policy tier"))+labs(title= paste0('B. Relative mortality reductions by state'))+theme_light()

########### Both
jpeg(filename = paste0('../figs/SADs_allstates_manuscript_', date_variable, '.jpg'),width=8, height=6, units ="in", res=1800)
genderval='Both'
ggarrange(Barplot_allstates_SADs, Barplot_allstates_RR, nrow=1, common.legend = TRUE)
dev.off()

