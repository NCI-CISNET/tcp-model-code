#[[[[The entire script is currently structured around T21 policy and will require a full rewrite.]]]


### loop through all states to make policy coverage plots for the TCP tool  

setwd("/Users/wangmengyao/Documents/GitHub/tcp-model-code/")
library(ggplot2)
library(cdlTools)
install.packages('svglite')
statefips=c('01','02','04','05','06','08','09','10','11','12','13','15','16',
            '17','18','19','20','21','22','23','24','25','26','27','28','29',
            '30','31','32','33','34','35','36','37','38','39','40','41','42',
            '44','45','46','47','48','49','50','51','53','54','55','56')

#---------------------- Load data sets --------------------------------------------------------
load("data/T21policycoverage2005.2025.Rda") # T21 policy coverage data
load("data/us_t21coverage.RData")
#add in state data
T21_state <- read_excel("data-raw/T21policycoverage.xlsx",sheet="Tobacco21.org")
T21_state$Date <- as.Date(with(T21_state,paste(year,month,day,sep="-")),"%Y-%m-%d")
statelabels = merge(T21_state[,c("year","month","abbrev","Date")],df_us_t21coverage[,c("year","month","statelocal")], by=c("month","year"),all.x=TRUE)

###############T21 policy coverage, 2014-2100

plotstate<- function(s,labelval){
  thisstate <- subset(df_t21data2005.2025, statefips0==s)
  stateT21date <- subset(thisstate,t21statepolicy==1)$date
  stateT21 <- subset(thisstate,date=="2025-01-01")$statelocal
  localT21 <- subset(thisstate,date=="2025-01-01")$local
  
  plot<- ggplot(data=thisstate)+
    geom_line(aes(x=date, y =fedstatelocal*100), size=0.4)+
    geom_ribbon(aes(x=date, ymin=statelocal*100, ymax =fedstatelocal*100),alpha=0.3, fill="#E69F00")+
    geom_ribbon(aes(x=date, ymin=local*100,ymax =statelocal*100),alpha=0.3, fill="#0072B2")+
    geom_area(aes(x=date, y =local*100),alpha=0.3, fill="#009E73")+
    geom_vline(xintercept = as.numeric(as.Date("2019-12-20")), linetype="dashed", color="#E69F00", size=0.6)+
    geom_vline(xintercept = as.numeric(as.Date(stateT21date)), linetype="dotted", color="#0072B2", size=0.6)+
    annotate(geom="text",x=as.Date("2014-01-05"), y=90,label="Federal T21",color="#E69F00",hjust=0, size=1.5)+
    annotate(geom="text",x=as.Date("2014-01-05"), y=55,label="Combined T21",color="black",hjust=0, size=1.5)+
    {if(stateT21==1) # if state-wide policy, then annotate
      annotate(geom="text",x=as.Date("2014-01-05"), y=80,label="State T21",color="#0072B2",hjust=0, size=1.5)}+
    {if(localT21>0) # if local coverage, then annotate
      annotate(geom="text",x=as.Date("2014-01-05"), y=70,label="Local T21",color="#009E73",hjust=0, size=1.5)}+
    ylab("Percent of population covered (%)")+
    xlab("Year")+
    labs(title= paste0(fips(s, to = 'Name'), ' ','T21 policy coverage'))+
    scale_y_continuous(limits=c(0,100))+
    scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                 limit=c(as.Date("2014-01-01"),as.Date("2024-12-31")))+
    theme_light()+theme(text = element_text(size=4),legend.position="none", axis.text.x=element_text(angle=60, hjust=1),panel.grid.minor = element_blank())
  return(plot) 
}

for (s in statefips){
  p <- plotstate(s,'')
  ggsave(filename = paste0("figs/state_coverage/", fips(s,to="Abbreviation"), ".svg"), plot = p, width=630, height= 500, units = "px")
}

#--------------- Whole US plot ----------------------------------------------------
# Extract the year from the Date column
statelabels$Year <- format(statelabels$Date, "%Y")

# Set a base y position for the labels to start at y = 0
base_y_position <- -12

# Calculate vertical offsets by year for consistent spacing
statelabels <- statelabels %>%
  group_by(Year) %>%
  mutate(
    offset = (row_number() - 1) * -3  # Adjust -2 for desired spacing
  ) %>%
  ungroup()

# Create the plot
plotUS <- ggplot(data=df_us_t21coverage) +
  geom_line(aes(x=Date, y=fedstatelocal*100)) +
  geom_ribbon(aes(x=Date, ymin=statelocal*100, ymax=fedstatelocal*100), alpha=0.3, fill="#E69F00") +
  geom_ribbon(aes(x=Date, ymin=local*100, ymax=statelocal*100), alpha=0.3, fill="#0072B2") +
  geom_area(aes(x=Date, y=local*100), alpha=0.3, fill="#009E73") +
  geom_vline(xintercept=as.numeric(as.Date("2019-12-20")), linetype="dashed", color="#E69F00", size=0.6) +
  annotate(geom="text",x=as.Date("2014-01-05"), y=35,label="Combined T21",color="black",hjust=0, size=3)+
  annotate(geom="text",x=as.Date("2014-01-05"), y=90,label="Federal T21",color="#E69F00",hjust=0, size=3)+
  annotate(geom="text",x=as.Date("2014-01-05"), y=80,label="State T21",color="#0072B2",hjust=0, size=3)+
  annotate(geom="text",x=as.Date("2014-01-05"), y=70,label="Local T21",color="#009E73",hjust=0, size=3)+
  ylab("Percent of population covered by T21 (%)") +
  xlab("") +  # Remove the default x-axis label
  scale_y_continuous(limits=c(0, 100)) +
  scale_x_date(date_labels="%Y", date_breaks="1 year",
               limits=c(as.Date("2014-01-01"), as.Date("2024-12-31"))) +
  theme_light() +
  theme(
    text = element_text(size=10),
    legend.position = "none",
    axis.text.x = element_text(angle=0, hjust=0.5),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),  # Hide the default x-axis title
    plot.margin = margin(b = 100)  # Increase bottom margin for space
  )

# Add custom x-axis label to the left side of the plot
plotUS <- plotUS +
  annotation_custom(
    grob = textGrob(
      "Year",  # The label text
      hjust = -1,  # Align left
      vjust = 26,  # Adjust vertical position
      gp = gpar(fontsize = 10)  # Set font size here
    ),
    xmin = as.Date("2014-01-01") - 500,  # Position slightly left of the plot
    xmax = as.Date("2014-01-01") - 500,  # Same as xmin to keep text vertical
    ymin = -Inf,  # Extend text to the bottom of the plot area
    ymax = Inf  # Extend text to the top of the plot area
  )

# Add annotations below the x-axis with consistent vertical spacing
annotation_font_size <- 7  # Adjust this value to change text size

for (i in 1:nrow(statelabels)) {
  plotUS <- plotUS +
    annotation_custom(
      grob = textGrob(
        statelabels$abbrev[i],
        vjust = 1, hjust = 0.5,
        gp = gpar(fontsize = annotation_font_size)  # Set font size here
      ),
      xmin = as.Date(paste0(statelabels$Year[i], "-01-01")),
      xmax = as.Date(paste0(statelabels$Year[i], "-01-01")),
      ymin = base_y_position + statelabels$offset[i],
      ymax = base_y_position + statelabels$offset[i]
    )
}

# Adjust the plot to avoid clipping of annotations
p <- plotUS + coord_cartesian(clip = 'off')

ggsave("figs/plotUS.svg", plot = plotUS + coord_cartesian(clip = 'off'), width = 6, height = 6, units = "in", dpi = 300)
#ggsave(filename = paste0("UScoverage8.11.svg"), plot = p, width=630, height= 500, units = "px")
dev.off()



