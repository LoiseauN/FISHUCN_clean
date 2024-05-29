
library(ggplot2)
library(magrittr)


# --- Stat tests: --------------------------------------------------------------
# 1. Test if there is a difference between before and after within each category
# The data are not normally distributed so I used a Wilcoxon test. And in fact
# the data aren't paired because the number of samples change between before and 
# after (it's what we want to test). A paired test would be comparing measures
# that would change between before and after...

# 2. Test if there is a difference between categories TH/NS/NT within each 
# before/after group. I used a Kruskal Wallis test because there are more than 2 
# groups.
MPA_stat <- function(){ 
load(here::here("MPA_Protect.RData"))
BEFORE <- cbind.data.frame(perc_cover=MPA_Protect$perc_cover, Target_achievement_I_IV=MPA_Protect$Target_achievement_I_IV,IUCN=MPA_Protect$IUCN_cat,What="BEFORE")
AFTER <- cbind.data.frame(perc_cover=MPA_Protect$perc_cover, Target_achievement_I_IV=MPA_Protect$Target_achievement_I_IV,IUCN=MPA_Protect$IUCN_final,What="AFTER")
MPA_FINAL <- rbind(BEFORE,AFTER)

MPA_FINAL$What <- factor(MPA_FINAL$What, levels = c("AFTER","BEFORE"))
MPA_FINAL$IUCN <- factor(MPA_FINAL$IUCN, levels = c("Threatened","Non Threatened","No Status"))
MPA_FINAL$category <- as.factor(paste(MPA_FINAL$IUCN, MPA_FINAL$What, sep="_"))
MPA_FINAL$category <- factor(MPA_FINAL$category,levels=c("No Status_BEFORE","No Status_AFTER","Non Threatened_BEFORE","Non Threatened_AFTER","Threatened_BEFORE","Threatened_AFTER")) 



options(digits = 3, scipen = 5)

## TARGET ACHIEVEMENT

# Before - after 
NS <- wilcox.test(MPA_FINAL$Target_achievement_I_IV[MPA_FINAL$category == "No Status_BEFORE"],
                  MPA_FINAL$Target_achievement_I_IV[MPA_FINAL$category == "No Status_AFTER"])
NS
Threat <- wilcox.test(MPA_FINAL$Target_achievement_I_IV[MPA_FINAL$category == "Threatened_BEFORE"],
                      MPA_FINAL$Target_achievement_I_IV[MPA_FINAL$category == "Threatened_AFTER"])
Threat
NT <- wilcox.test(MPA_FINAL$Target_achievement_I_IV[MPA_FINAL$category == "Non Threatened_BEFORE"],
                  MPA_FINAL$Target_achievement_I_IV[MPA_FINAL$category == "Non Threatened_AFTER"])
NT

# Intra group:
m_before <- kruskal.test(BEFORE$Target_achievement_I_IV ~ BEFORE$IUCN)
dt_before <- FSA::dunnTest(BEFORE$Target_achievement_I_IV~BEFORE$IUCN)
dt_before$res


m_after <- kruskal.test(AFTER$Target_achievement_I_IV ~ AFTER$IUCN)
dt_after <- FSA::dunnTest(AFTER$Target_achievement_I_IV~AFTER$IUCN)
dt_after$res

## PERCENT COVER

# Before - after 
NS <- wilcox.test(MPA_FINAL$perc_cover[MPA_FINAL$category == "No Status_BEFORE"],
                  MPA_FINAL$perc_cover[MPA_FINAL$category == "No Status_AFTER"])
NS
Threat <- wilcox.test(MPA_FINAL$perc_cover[MPA_FINAL$category == "Threatened_BEFORE"],
                      MPA_FINAL$perc_cover[MPA_FINAL$category == "Threatened_AFTER"])
Threat
NT <- wilcox.test(MPA_FINAL$perc_cover[MPA_FINAL$category == "Non Threatened_BEFORE"],
                  MPA_FINAL$perc_cover[MPA_FINAL$category == "Non Threatened_AFTER"])
NT

# Intra group:
m_before <- kruskal.test(BEFORE$perc_cover ~ BEFORE$IUCN)
dt_before <- FSA::dunnTest(BEFORE$perc_cover~BEFORE$IUCN)
dt_before$res


m_after <- kruskal.test(AFTER$perc_cover ~ AFTER$IUCN)
dt_after <- FSA::dunnTest(AFTER$perc_cover~AFTER$IUCN)
dt_after$res


# sample sizes
table(MPA_FINAL$category)

# --- Stat tests end -----------------------------------------------------------
}



