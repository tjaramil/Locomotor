# Load Libraries

library(dplyr)
library(readr)

# Set the working directory to the file that contains your data; Session > Set Working Directory > Choose Directory

# Load Data

DS1 <- read.csv("session1_Int.csv", stringsAsFactors = F)  # Put file name between " "
DS2 <- read.csv("session2_Int.csv", stringsAsFactors = F)
DS3 <- read.csv("session3_Int.csv", stringsAsFactors = F)
DS4 <- read.csv("session4_Int.csv", stringsAsFactors = F)
DS5 <- read.csv("session5_Int.csv", stringsAsFactors = F)
# Repeat if you have more files

# Combine the data into one dataset 

DST <- rbind(DS1, DS2, DS3, DS4, DS5)

# Load Cohort Id sheet with Geno, Sex and SubjectID

Cohort_ID <- read.csv("cohort_ID.csv")

# Join the two

DST2 <- left_join(DST, Cohort_ID, by = c("SubjectId"="Subject")) # Put your column heads between ""=""

# Take a peek at the data
str(DST)


# Summarize XFTot, XATot, XTTot without Sex

DST_Summary_wo_sex <- DST2 %>%
        group_by(IntervalNum, Genotype) %>% 
        summarise(Count = n(),
                  XF_Tot = mean(XFTot),
                  XF_Tot_SD = sd(XFTot),  #Another way : sqrt(sum((XFTot-mean(XFTot))^2/(length(XFTot)-1)))
                  XF_Tot_SEM = XF_Tot_SD/sqrt(Count),
                  XA_Tot = mean(XATot),
                  XA_Tot_SD = sd(XATot),
                  XA_Tot_SEM = XA_Tot_SD/sqrt(Count),
                  XT_Tot = mean(XTTot),
                  XT_Tot_SD = sd(XTTot),
                  XT_SEM = XT_Tot_SD/sqrt(Count),
                  Total_BeamBreaks = sum(XF_Tot,XA_Tot,XT_Tot),
                  Total_BeamBreaks_SD = sd(Total_BeamBreaks),
                  Total_BeamBreaks_SEM = Total_BeamBreaks_SD/sqrt(Count))

write.csv(DST_Summary_wo_sex, "DST_Summary_wo_sex.csv")  # If you want to take the data an plot on Prism

# Summarize XFTot, XATot, XTTot without Sex

DST_Summary_w_sex <- DST2 %>%
        group_by(IntervalNum, Genotype, Sex) %>% 
        summarise(Count = n(),
                  XF_Tot = mean(XFTot),
                  XF_Tot_SD = sd(XFTot),  #Another way : sqrt(sum((XFTot-mean(XFTot))^2/(length(XFTot)-1)))
                  XF_Tot_SEM = XF_Tot_SD/sqrt(Count),
                  XA_Tot = mean(XATot),
                  XA_Tot_SD = sd(XATot),
                  XA_Tot_SEM = XA_Tot_SD/sqrt(Count),
                  XT_Tot = mean(XTTot),
                  XT_Tot_SD = sd(XTTot),
                  XT_SEM = XT_Tot_SD/sqrt(Count),
                  Total_BeamBreaks = sum(XF_Tot, XA_Tot, XT_Tot),
                  Total_BeamBreaks_SD = sd(Total_BeamBreaks),
                  Total_BeamBreaks_SEM = Total_BeamBreaks_SD/sqrt(Count))

write.csv(DST_Summary_w_sex, "DST_Summary_w_sex.csv")  # If you want to take the data an plot on Prism

# Plot without Sex


library(ggplot2)

Fine1 <- ggplot(DST_Summary_wo_sex, aes(x = IntervalNum, y = XF_Tot))+
        geom_errorbar(aes(ymin = XF_Tot-XF_Tot_SEM, ymax = XF_Tot + XF_Tot_SEM),  width=.2)+
        geom_line(aes(linetype = Genotype, color = Genotype))+
        geom_point(aes(color = Genotype))+
        scale_y_continuous("Beam Breaks", limits = c(10,60), expand = c(0,0))+
        scale_x_continuous("Bins", limits = c(0,25), expand = c(0,0))+
        theme(axis.line = element_line(colour = "black"), legend.title = element_blank(), 
              legend.key = element_rect(colour = "white", fill = NA), panel.background = element_blank(),
              legend.position = c(0.30,0.90)) +
        labs(title = "Fine Movement")

Ambulatory1 <- ggplot(DST_Summary_wo_sex, aes(x = IntervalNum, y = XA_Tot))+
        geom_errorbar(aes(ymin = XA_Tot-XA_Tot_SEM, ymax = XA_Tot + XA_Tot_SEM),  width=.2)+
        geom_line(aes(linetype = Genotype, color = Genotype))+
        geom_point(aes(color = Genotype))+
        scale_y_continuous("Beam Breaks", limits = c(40, 220), expand = c(0,0))+
        scale_x_continuous("Bins", limits = c(0,25), expand = c(0,0))+
        theme(axis.line = element_line(colour = "black"), legend.title = element_blank(), 
              legend.key = element_rect(colour = "white", fill = NA), panel.background = element_blank(),
              legend.position = c(0.25,0.85)) +
        labs(title = "Ambulatory Movement")

Total1 <- ggplot(DST_Summary_wo_sex, aes(x = IntervalNum, y = XT_Tot))+
        geom_errorbar(aes(ymin = XT_Tot-XT_SEM, ymax = XT_Tot + XT_SEM),  width=.2)+
        geom_line(aes(linetype = Genotype, color = Genotype))+
        geom_point(aes(color = Genotype))+
        scale_y_continuous("Beam Breaks", limits = c(50,250), expand = c(0,0))+
        scale_x_continuous("Bins", limits = c(0,25), expand = c(0,0))+
        theme(axis.line = element_line(colour = "black"), legend.title = element_blank(), 
              legend.key = element_rect(colour = "white", fill = NA), panel.background = element_blank(),
              legend.position = c(0.25,0.85)) +
        labs(title = "Total Movement")


# Run multiplot function

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        library(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}


# Print all plots to one sheet

multiplot(Fine1, Ambulatory1, Total1, cols=2)

################################################################################################################
# Plot with Sex
# Male

DST_male <- DST_Summary_w_sex %>%
        filter(Sex =="M")

Fine2 <- ggplot(DST_male, aes(x = DST_male$IntervalNum, y = DST_male$XF_Tot))+
        geom_errorbar(aes(ymin = DST_male$XF_Tot - DST_male$XF_Tot_SEM, ymax = DST_male$XF_Tot + DST_male$XF_Tot_SEM),  width=.2)+
        geom_line(aes(linetype = DST_male$Genotype, color = DST_male$Genotype))+
        geom_point(aes(color = DST_male$Genotype))+
        scale_y_continuous("Beam Breaks", limits = c(10,60), expand = c(0,0))+
        scale_x_continuous("Bins", limits = c(0,25), expand = c(0,0))+
        theme(axis.line = element_line(colour = "black"), legend.title = element_blank(), 
              legend.key = element_rect(colour = "white", fill = NA), panel.background = element_blank(),
              legend.position = c(0.30,0.90)) +
        labs(title = "Fine Movement: Male")

Ambulatory2 <- ggplot(DST_male, aes(x = DST_male$IntervalNum, y = DST_male$XA_Tot))+
        geom_errorbar(aes(ymin = DST_male$XA_Tot-DST_male$XA_Tot_SEM, ymax = DST_male$XA_Tot + DST_male$XA_Tot_SEM),  width=.2)+
        geom_line(aes(linetype = DST_male$Genotype, color = DST_male$Genotype))+
        geom_point(aes(color = DST_male$Genotype))+
        scale_y_continuous("Beam Breaks", limits = c(40, 220), expand = c(0,0))+
        scale_x_continuous("Bins", limits = c(0,25), expand = c(0,0))+
        theme(axis.line = element_line(colour = "black"), legend.title = element_blank(), 
              legend.key = element_rect(colour = "white", fill = NA), panel.background = element_blank(),
              legend.position = c(0.25,0.85)) +
        labs(title = "Ambulatory Movement: Male")

Total2 <- ggplot(DST_male, aes(x = DST_male$IntervalNum, y = DST_male$XT_Tot))+
        geom_errorbar(aes(ymin = DST_male$XT_Tot-DST_male$XT_SEM, ymax = DST_male$XT_Tot + DST_male$XT_SEM),  width=.2)+
        geom_line(aes(linetype = DST_male$Genotype, color = DST_male$Genotype))+
        geom_point(aes(color = DST_male$Genotype))+
        scale_y_continuous("Beam Breaks", limits = c(50,250), expand = c(0,0))+
        scale_x_continuous("Bins", limits = c(0,25), expand = c(0,0))+
        theme(axis.line = element_line(colour = "black"), legend.title = element_blank(), 
              legend.key = element_rect(colour = "white", fill = NA), panel.background = element_blank(),
              legend.position = c(0.25,0.85)) +
        labs(title = "Total Movement: Male")

multiplot(Fine2, Ambulatory2, Total2, cols=2)

################################################################################################################
# Female

DST_female <- DST_Summary_w_sex %>%
        filter(Sex =="F")

Fine3 <- ggplot(DST_female, aes(x = DST_female$IntervalNum, y = DST_female$XF_Tot))+
        geom_errorbar(aes(ymin = DST_female$XF_Tot - DST_female$XF_Tot_SEM, ymax = DST_female$XF_Tot + DST_female$XF_Tot_SEM),  width=.2)+
        geom_line(aes(linetype = DST_female$Genotype, color = DST_female$Genotype))+
        geom_point(aes(color = DST_female$Genotype))+
        scale_y_continuous("Beam Breaks", limits = c(10,60), expand = c(0,0))+
        scale_x_continuous("Bins", limits = c(0,25), expand = c(0,0))+
        theme(axis.line = element_line(colour = "black"), legend.title = element_blank(), 
              legend.key = element_rect(colour = "white", fill = NA), panel.background = element_blank(),
              legend.position = c(0.30,0.90)) +
        labs(title = "Fine Movement: Female")

Ambulatory3 <- ggplot(DST_female, aes(x = DST_female$IntervalNum, y = DST_female$XA_Tot))+
        geom_errorbar(aes(ymin = DST_female$XA_Tot-DST_female$XA_Tot_SEM, ymax = DST_female$XA_Tot + DST_female$XA_Tot_SEM),  width=.2)+
        geom_line(aes(linetype = DST_female$Genotype, color = DST_female$Genotype))+
        geom_point(aes(color = DST_female$Genotype))+
        scale_y_continuous("Beam Breaks", limits = c(40, 220), expand = c(0,0))+
        scale_x_continuous("Bins", limits = c(0,25), expand = c(0,0))+
        theme(axis.line = element_line(colour = "black"), legend.title = element_blank(), 
              legend.key = element_rect(colour = "white", fill = NA), panel.background = element_blank(),
              legend.position = c(0.25,0.85)) +
        labs(title = "Ambulatory Movement: Female")

Total3 <- ggplot(DST_female, aes(x = DST_female$IntervalNum, y = DST_female$XT_Tot))+
        geom_errorbar(aes(ymin = DST_female$XT_Tot-DST_female$XT_SEM, ymax = DST_female$XT_Tot + DST_female$XT_SEM),  width=.2)+
        geom_line(aes(linetype = DST_female$Genotype, color = DST_female$Genotype))+
        geom_point(aes(color = DST_female$Genotype))+
        scale_y_continuous("Beam Breaks", limits = c(50,250), expand = c(0,0))+
        scale_x_continuous("Bins", limits = c(0,25), expand = c(0,0))+
        theme(axis.line = element_line(colour = "black"), legend.title = element_blank(), 
              legend.key = element_rect(colour = "white", fill = NA), panel.background = element_blank(),
              legend.position = c(0.25,0.85)) +
        labs(title = "Total Movement: Famle")

multiplot(Fine3, Ambulatory3, Total3, cols=2)

# Stats

## ANOVA
# Here we have 3 indep variables (Genotype, Sex, and Bin) so we conduct a 3-way ANOVA
# Dependant variable is beam breaks

Anova.1 <- aov(XF_Tot ~ Genotype * Sex * IntervalNum, data = DST_Summary_w_sex)

summary(Anova.1)

# Post hoc test
library(agricolae)

tukey <- TukeyHSD(Anova.1, trt='group')
tukey

scheffe.Geno <- scheffe.test(Anova.1, 'Genotype', group = F, console = F, alpha = 0.05)
scheffe.Geno

scheffe.Sex <- scheffe.test(Anova.1, c("Sex", "Genotype"), group = F, console = F, alpha = 0.05)
scheffe.Sex

LSD.Geno <- LSD.test(Anova.1, "Genotype", p.adj = "hochberg", group = F, console = F) 
LSD.Geno

LSD.Sex <- LSD.test(Anova.1, "Sex", p.adj = "hochberg", group = F, console = F)
LSD.Sex

