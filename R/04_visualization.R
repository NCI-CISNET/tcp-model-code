## Functions to generate the figures used in the manuscript 
## 'Tax effects on smoking prevalence and mortality by state'
library(ggplot2)
lapply(c("grid", "ggpubr", "scales", "cdlTools"), library, character.only = TRUE)
labellist=c('A.', 'B.', 'C.', 'D.')
paperfips=c('06','21','25','55')
#load('output/model_output_10.09.24.RData')  #load only needed if running file separately

##------------------- Smoking Attributable Deaths Averted ---------------------
SADs_averted_fig <- function(fipscode, labelval) {
  thisstate <- subset(df_mort.policy, state == fipscode & gender == "Both")
  thisstate$policy.scenario <- factor(thisstate$policy.scenario, levels = c("baseline", "tax"))
  fig <- ggplot(data = thisstate) + 
    scale_color_manual(name = "policy",
                       labels = c("Baseline", "Tax policy"),
                       values = c("#E69F00",  "#0072B2")) +
    geom_line(aes(x = year, y = SADsAvertedcum, color = policy.scenario, linetype = tax.effects), lwd = 0.6) +
    scale_y_continuous(name = "SADs averted", 
                       labels = scales::comma, 
                       limits = c(0, 43000), 
                       breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(name = "Year", 
                       limits = c(2025, 2100), 
                       breaks = seq(2030, 2100, 10)) +
    labs(title = if (fipscode == '00') {
      paste0("Deaths averted under tax policy: US (50 states + DC)")
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

jpeg(file=paste0('figs/SADs_averted_manuscript_', date_variable, '.jpg'), width=6, height=6, units="in", res=1800)
pdf(file=paste0('figs/SADs_averted_manuscript_', date_variable, '.pdf'), width=6, height=6)
ggarrange(SADs_averted_fig(paperfips[1],labellist[1]),SADs_averted_fig(paperfips[2], labellist[2]),
          SADs_averted_fig(paperfips[3], labellist[3]), SADs_averted_fig(paperfips[4], labellist[4]),
          ncol=2,nrow=2, common.legend = TRUE, legend='top')

dev.off()




#-------------Life Years Gained -------------------------------------------------
LYG_manuscript<-function(fipscode, labelval){
  thisstate <- subset(df_mort.policy,state==fipscode&gender=="Both")
  thisstate$policy.scenario <- factor(thisstate$policy.scenario, levels = c("baseline", "tax"))
  fig <- ggplot(data=thisstate) + 
    scale_color_manual(name="policy",labels=c("Baseline", "Tax policy"),values=c("#E69F00","#0072B2"))+
    geom_line(aes(x=year, y=LYGcum, color=policy.scenario,linetype=mla.effects), lwd=0.6) +

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

jpeg(file=paste0('figs/LYG_manuscript_', date_variable, '.jpg'), width=6, height=6, units="in", res=1800)
pdf(file=paste0('figs/LYG_manuscript_', date_variable, '.pdf'), width=6, height=6)
ggarrange(LYG_manuscript(paperfips[1],labellist[1]),LYG_manuscript(paperfips[2], labellist[2]),
          LYG_manuscript(paperfips[3], labellist[3]), LYG_manuscript(paperfips[4], labellist[4]),
          ncol=2,nrow=2, common.legend = TRUE)

dev.off()



#------------------- Tax Effects by Age Group ---------------------
plot_tax_effects_by_age <- function(fipscode, labelval) {
  thisstate <- subset(df_mort.policy, state == fipscode & gender == "Both")
  thisstate$policy.scen <- factor(thisstate$policy.scen, levels=c("baseline", "tax"))
  fig <- ggplot(data = thisstate) + 
    geom_line(aes(x = year, y = prev*100, color = policy.scen, linetype = age), lwd = 0.6) +
    scale_color_manual(name = "Scenario",
                      labels = c("Baseline", "With Tax"),
                      values = c("#0072B2", "#E69F00")) +
    scale_y_continuous(name = "Smoking Prevalence (%)", 
                      limits = c(0, 30), 
                      breaks = seq(0, 30, 5)) +
    scale_x_continuous(name = "Year", 
                      limits = c(2025, 2100), 
                      breaks = seq(2030, 2100, 10)) +
    labs(title = paste0(labelval, ' ', fips(fipscode, to = 'Name'))) +
    theme_light() + 
    theme(legend.title = element_text(size=9),
          text = element_text(size=10),
          axis.text.x = element_text(angle=60, hjust=1))
  return(fig)
}

