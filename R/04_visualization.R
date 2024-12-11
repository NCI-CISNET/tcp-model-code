#load('output/model_output_10.09.24.RData')  #load only needed if running file separately

## Prevalence figure
prev_projections<-function(agegrp, whichgender){
  fig1 <- ggplot() + 
    geom_line(data=subset(df_prev.out, age==agegrp & gender==whichgender), aes(x=year, y=prev*100, color=mla.effect), lwd = 0.6) + 
    geom_pointrange(data=subset(df_survey_data, age==agegrp & smkstatus=='currentsmk2' & gender==whichgender),
                    aes(x=year,y=est*100,ymin= ci_low*100, ymax=ci_high*100, shape=gender), show.legend = FALSE)+ 
    scale_y_continuous(name="Prevalence (%)",limits=c(0,50),breaks=seq(0,50,5)) +
    scale_x_continuous(name="Year",limits=c(1990,2100),breaks=c(1990,seq(2000,2100,20) )) +
    theme_light()+ 
    labs(title= paste0("Smoking prevalence, ",whichgender," ages 18-99"))+
    theme(legend.text=element_text(size=9),
          legend.position = c(.95, .95),legend.justification = c("right", "top"), legend.box.just = "right",legend.title=element_blank(),
          text = element_text(size=9),axis.text.x=element_text(angle=60, hjust=1),
          legend.background = element_rect(fill=alpha('white', 0.4)))
  return(fig1)
}

## Smoking Attributable Deaths Averted figure
SADs_averted_fig <- function(whichgender) {
  model <- subset(df_mortality.out, gender == whichgender)
  fig <- ggplot(data = model) + 
    geom_line(aes(x = year, y = cSADsAverted, color = mla.effect), lwd = 0.6) +
    scale_y_continuous(name = "SADs averted", 
                       labels = scales::comma, 
                       limits = c(0, 43000), 
                       breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(name = "Year", 
                       limits = c(2025, 2100), 
                       breaks = seq(2025, 2100, 5)) +
    labs(title = paste0("Cumulative premature SADs averted, ", whichgender))+
    theme_light() + 
    guides(color = guide_legend(nrow = 3)) +
    guides(linetype = guide_legend(nrow = 3)) +
    theme(legend.title = element_text(size = 9), 
          text = element_text(size = 10), 
          axis.text.x = element_text(angle = 60, hjust = 1))
  
  return(fig)
}

## Life Years Gained
LYG_fig <- function(whichgender) {
  model <- subset(df_mortality.out, gender == whichgender)
  fig <- ggplot(data = model) + 
    geom_line(aes(x = year, y = cLYG, color = mla.effect), lwd = 0.6) +
    scale_y_continuous(name="Life Years Gained",labels=scales::comma,limits=c(0, 1305000),breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(name = "Year", 
                       limits = c(2025, 2100), 
                       breaks = seq(2025, 2100, 5)) +
    labs(title = paste0("Cumulative life-years gained, ", whichgender))+
    theme_light() + 
    guides(color = guide_legend(nrow = 3)) +
    guides(linetype = guide_legend(nrow = 3)) +
    theme(legend.title = element_text(size = 9), 
          text = element_text(size = 10), 
          axis.text.x = element_text(angle = 60, hjust = 1))
  
  return(fig)
}


pdf(file=paste0('output/T21_results_', date_variable, '.pdf'), width=8.5, height=11)
ggarrange(prev_projections(18.99,"Men"),prev_projections(18.99,"Women"),
          SADs_averted_fig("Men"), SADs_averted_fig("Women"),
          LYG_fig("Men"),LYG_fig("Women"),
          ncol=2,nrow=3, common.legend = TRUE, legend='top')
dev.off()
