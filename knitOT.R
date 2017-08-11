start <- Sys.time()

library(tidyverse)
library(rmarkdown)

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
    filter(! is.na(CostCenter) & ! CostCenter %in% ditchTheseCostCenters) %>%
    mutate(Allotment = gsub(Allotment, pattern = ",", replacement = "") %>%
               as.numeric())

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

for (i in seq(hierarchy$CostCenter)) {
    firstlevelrender(CCName = hierarchy$CostCenter[i], 
                     startingdate = as.Date("2017-07-01"),
                     path = "Z:/Projects/dashboard/RPDOvertimeDashboard/Dashboards/")
}

for (i in seq(LevelTwos)) {
    secondlevelrender(leveltwoname = LevelTwos[i],
                      startingdate = as.Date("2017-07-01"),
                      path = "Z:/Projects/dashboard/RPDOvertimeDashboard/Dashboards/SecondLevels/")
}

for (i in seq(LevelThrees)) {
    thirdlevelrender(levelthreename = LevelThrees[i],
                     startingdate = as.Date("2017-07-01"),
                     path = "Z:/Projects/dashboard/RPDOvertimeDashboard/Dashboards/ThirdLevels/")
}

rmarkdown::render("Z:/Projects/dashboard/RPDOvertimeDashboard/OT_repo/RPD_OTDashboards/FourthLevel_OTdashboard.RMD",
                  params = list(StartDate = as.Date("2017-07-01")), 
                  output_file = "Z:/Projects/dashboard/RPDOvertimeDashboard/Dashboards/FourthLevel/FourthLevel_OTdashboard.html")

end <- Sys.time()
took <- difftime(end, start)
print(took)