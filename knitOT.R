start <- Sys.time()

library(tidyverse)
library(rmarkdown)
#library(checkpoint); checkpoint(snapshotDate = "2017-08-15")
# https://www.r-bloggers.com/reproducibility-a-cautionary-tale-from-data-journalism/


# Function for first-level dashboard template

firstlevelrender <- function(CCName, path, startingdate) {
    # CCName is the CostCenterName
    # Path is where to save the rendering, including backslash
    CCNameprint <- gsub(pattern = "[[:punct:]]", replacement = "", x = CCName)
    rmarkdown::render("Z:/Projects/dashboard/RPDOvertimeDashboard/OT_Repo/RPD_OTDashboards/FirstLevel_OTDashboard.RMD",
                      params = list(CostCenterName = CCName,
                                    StartDate = startingdate),
                      output_file = paste0(path, CCNameprint, "_OTDashboard.html"))
    }

secondlevelrender <- function(leveltwoname, path, startingdate) {
    # leveltwoname is the grouping (Patrol, CIS, etc.)
    # Path is where to save the html file, including backslash
    CCNameprint <- gsub(pattern = "[[:punct:]]", replacement = "", x = leveltwoname)
    rmarkdown::render("Z:/Projects/dashboard/RPDOvertimeDashboard/OT_Repo/RPD_OTDashboards/SecondLevel_OTDashboard.RMD",
                      params = list(LevelTwoName = leveltwoname,
                                    StartDate = startingdate),
                      output_file = paste0(path, CCNameprint, "_OTDashboard.html"))
    }

thirdlevelrender <- function(levelthreename, path, startingdate) {
    # levelthreename is OPS, Admin or Grants
    # Path is where to save the html file, including backslash
    CCNameprint <- gsub(pattern = "[[:punct:]]", replacement = "", x = levelthreename)
    rmarkdown::render("Z:/Projects/dashboard/RPDOvertimeDashboard/OT_Repo/RPD_OTDashboards/ThirdLevel_OTDashboard.RMD",
                      params = list(LevelThreeName = levelthreename,
                                    StartDate = startingdate),
                      output_file = paste0(path, CCNameprint, "_OTDashboard.html"))
    }

# Function for third-level dashboard template

ditchTheseCostCenters <- c("Total", "Budget", "Difference", "NSC1", "NSC3", "NSC5", "NSC7", "NSC9")

hierarchy <- read_csv("Z:/Projects/dashboard/RPDOvertimeDashboard/FY1718_Budgets.csv") %>%
    select(CostCenter = `Overtime Allotments`,
           Allotment = `FY18 Budget`,
           LevelTwo = `2nd Level`,
           LevelThree = `3rd Level`,
           LevelFour = `4th Level`) %>%
    filter(! is.na(CostCenter) | 
               ! CostCenter %in% ditchTheseCostCenters) %>%
    mutate(Allotment = gsub(Allotment, pattern = ",", replacement = "") %>%
               as.numeric()) %>%
    filter(! is.na(Allotment))

LevelTwos <- hierarchy %>%
    select(LevelTwo) %>%
    filter(!is.na(LevelTwo)) %>%
    distinct() %>%
    .$LevelTwo

LevelThrees <- hierarchy %>%
    select(LevelThree) %>%
    filter(!is.na(LevelThree)) %>%
    distinct() %>%
    .$LevelThree

rmarkdown::render("Z:/Projects/dashboard/RPDOvertimeDashboard/OT_repo/RPD_OTDashboards/LongtermOTDashboard.RMD",
                  params = list(StartDate = as.Date("2017-07-01")),
                  output_file = "//cor.local/RPD/Shared/OBI-Analytics/OT Dashboards/Citywide/LongtermOTDashboard.html")

rmarkdown::render("Z:/Projects/dashboard/RPDOvertimeDashboard/OT_repo/RPD_OTDashboards/Citywide_OTdashboard.RMD",
                  params = list(StartDate = as.Date("2017-07-01")),
                  output_file = "//cor.local/RPD/Shared/OBI-Analytics/OT Dashboards/Citywide/Citywide_OTdashboard.html")

for (i in seq(LevelThrees)) { # Putting Level3 before Level2; so GRANTS & CHIEF get overwritten by Level2 GRANTS & CHIEF
    thirdlevelrender(levelthreename = LevelThrees[i],
                     startingdate = as.Date("2017-07-01"),
                     path = "//cor.local/RPD/Shared/OBI-Analytics/OT Dashboards/ThirdLevels/")
}

for (i in seq(LevelTwos)) {
    secondlevelrender(leveltwoname = LevelTwos[i],
                      startingdate = as.Date("2017-07-01"),
                      path = if(LevelTwos[i] %in% c("CHIEF", "GRANTS")) {
                          "//cor.local/RPD/Shared/OBI-Analytics/OT Dashboards/ThirdLevels/"
                      } else {
                          "//cor.local/RPD/Shared/OBI-Analytics/OT Dashboards/SecondLevels/"
                      }
    )
}

for (i in seq(hierarchy$CostCenter)) {
    firstlevelrender(CCName = hierarchy$CostCenter[i], 
                     startingdate = as.Date("2017-07-01"),
                     path = "//cor.local/RPD/Shared/OBI-Analytics/OT Dashboards/FirstLevels/")
}

end <- Sys.time()
took <- difftime(end, start)
print(took)