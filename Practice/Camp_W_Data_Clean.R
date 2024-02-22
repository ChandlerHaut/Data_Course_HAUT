library(tidyverse)
library(tidyverse)
library(gapminder)
library(ggimage)
library(gganimate)
library(patchwork)
library(readxl)



dat <- read_xlsx("./Data/messy_bp.xlsx")
view(dat)

view(hr)

hr <- 
dat %>% 
  select(-starts_with("BP"))

n.vistis <- 
  hr %>% 
  select(starts_with("HR")) %>% 
  length()

which(grepl("^HR", names(hr)))

names(hr)[which(grepl("^HR", names(hr)))] <- paste0("visit",1:n.vistis)


hr.clean <- 
hr %>% 
  pivot_longer(starts_with("visit"),
               values_to = "hr",
               names_to = "visit",
               names_prefix = "visit",
               names_transform = as.numeric)



bp <- 
  dat %>% 
  select(-starts_with("HR"))

b.vistis <- 
  bp %>% 
  select(starts_with("BP")) %>% 
  length()

which(grepl("^BP", names(bp)))

names(bp)[which(grepl("^BP", names(bp)))] <- paste0("visit",1:b.vistis)


bp.clean <- 
  bp %>% 
  pivot_longer(starts_with("visit"),
               values_to = "bp",
               names_to = "visit",
               names_prefix = "visit",
               names_transform = as.numeric) %>% 
  separate(bp, into = c("systolic","diastolic"))
view(bp.clean)


library(measurements)
library(janitor)

x <- c(12,31,44)
measurements::conv_unit(x, from="inch", to = "ft")

path <- "./Practice/CW_CameraData_2019.xlsx"

sites <- c("South Oak Spring Site 2","North Oak Spring Site 1","Oak_Spring",
           "North Tickville Site 1", "South Tickville Site 3",
           "Tickville", "Redwood Road Underpass", "Water Fork Rose Canyon Spring")

sites[1] %>% str_replace_all(" ","_")

trap_days <- read_xlsx(path,sheet = sites[1], range = "B17:I17", col_names = FALSE) 
trap_days[1,] %>% as.numeric


South_Oak_Spring_Site_2 <- 
read_xlsx(path,sheet = sites[1], range = "A2:I12") %>%
  clean_names() %>% 
  pivot_longer(-species,
               names_to = "month",
                values_to= "obs_count") %>% 
  mutate(site = sites[1],
         month =str_to_sentence(month),
         species = str_to_sentence(species))

South_Oak_Spring_Site_2
data.frame(month = South_Oak_Spring_Site_2$month %>% unique,
           trap_days = trap_days[1,] %>% as.numeric)
South_Oak_Spring_Site_2 <- 
South_Oak_Spring_Site_2 %>% 
  full_join(data.frame(month = South_Oak_Spring_Site_2$month %>% unique,
                       trap_days = trap_days[1,] %>% as.numeric))




read_trap_data <- 
function(path,sheet,range1,range2){
  trap_days <- read_xlsx(path,sheet = sheet, range = range1, col_names = FALSE)
  
  x <- 
    read_xlsx(path,sheet = sites[1], range = range2) %>%
    clean_names() %>% 
    mutate(across(-species,as.numeric)) %>% 
    pivot_longer(-species,
                 names_to = "month",
                 values_to= "obs_count") %>% 
    mutate(site = sheet,
           month =str_to_sentence(month),
           species = str_to_sentence(species)) %>% 
    mutate(month = case_when(str_detect(month,"[J,j]an")~"January",
                             str_detect(month,"[F,f]eb")~"February",
                             str_detect(month,"[M,m]ar")~"March",
                             str_detect(month,"[A,a]pr")~"April",
                             str_detect(month,"[M,m]ay")~"May",
                             str_detect(month,"[J,j]un")~"June",
                             str_detect(month,"[J,j]ul")~"July",
                             str_detect(month,"[A,a]ug")~"August",
                             str_detect(month,"[S,s]ep")~"September",
                             str_detect(month,"[O,o]ct")~"October",
                             str_detect(month,"[N,n]ov")~"November",
                             str_detect(month,"[D,d]ec")~"December",
                             TRUE ~ month))
  
  x <- 
    x %>% 
    full_join(data.frame(month = x$month %>% unique,
                         trap_days = trap_days[1,] %>% as.numeric))
  return(x)
}

South_Oak_Spring_Site_2 <- 
read_trap_data(path = path,
               sheet = sites[1],
               range1 = "B17:I17", 
               range2 = "A2:I12")


North_Oak_Spring_Site_1 <- 
read_trap_data(path = path,
               sheet = sites[2],
               range1 = "B15:I15", 
               range2 = "A2:I12")

Oak_Spring <- 
  read_trap_data(path = path,
                 sheet = sites[3],
                 range1 = "B18:I18", 
                 range2 = "A2:I15")

North_Tickville_Site_1 <- 
  read_trap_data(path = path,
                 sheet = sites[4],
                 range1 = "B14:I14", 
                 range2 = "A2:I11")

South_Tickvell_Site_3 <- 
  read_trap_data(path = path,
                 sheet = sites[5],
                 range1 = "B13:I13", 
                 range2 = "A2:I10")

Tickville <- 
  read_trap_data(path = path,
                 sheet = sites[6],
                 range1 = "B14:I14", 
                 range2 = "A2:I11")

Redwood_Road_Underpass <- 
  read_trap_data(path = path,
                 sheet = sites[7],
                 range1 = "B15:F15", 
                 range2 = "A2:F12")

Water_Fork_Rose_Canyon_Spring <- 
  read_trap_data(path = path,
                 sheet = sites[8],
                 range1 = "B21:J21", 
                 range2 = "A2:J16")
mylist <- list()
for (i in sites %>% str_replace_all(" ","_")) {
  list[[i]] <- get(i)
}

sites %>% str_replace_all(" ","_")

CW_2019 <- 
South_Oak_Spring_Site_2 %>% 
  full_join(South_Tickvell_Site_3) %>% 
  full_join(North_Oak_Spring_Site_1) %>% 
  full_join(Oak_Spring) %>% 
  full_join(Redwood_Road_Underpass) %>% 
  full_join(Tickville) %>% 
  full_join(North_Tickville_Site_1) %>% 
  full_join(Water_Fork_Rose_Canyon_Spring)
view(CW_2019)



