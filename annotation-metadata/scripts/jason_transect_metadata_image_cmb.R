#align cruise dive data with transect data

#wd####
setwd("/Users/virginiabiede/Desktop/R/cruise_data/jason")

#libraries####
library(tidyverse)
library(plyr)
library(dplyr)

#data####
##jason sealog data####
#set to folder with the files
setwd("/Users/virginiabiede/Desktop/R/cruise_data/jason/Jason_Sealog_All")
#combine all files with .csv file type from a folder
#rbind fill allows for different numbers of columns
files <- list.files(pattern = '\\.csv$', full.names = TRUE)
all_data <- do.call(plyr::rbind.fill, lapply(files, function(x) 
  transform(read.csv(x), File = basename(x))))

##reset wd####
setwd("/Users/virginiabiede/Desktop/R/cruise_data/jason")

##transect meta data####
transects <- read.csv("/Users/virginiabiede/Desktop/R/cruise_data/jason/Jason Transects Summary_112524_VB.csv")

##jason imagery data####
#biigle downloads must be type "Image annotation report" report variant "Csv"
#don't highlight spearate users, this will generate multiple reports within folders
#while R can probably get into multiple layers of folders that is a trial for another day
#the final version should not have multiple labels per coral
#here will just filter to only one person as a trial

#remove files without annotations first, these are only 195 bytes in size

setwd("/Users/virginiabiede/Desktop/R/cruise_data/jason/biigle-reports/3039_csv_image_annotation_report_all_annotators")

ann_files <- list.files(pattern = '\\.csv$', full.names = TRUE)
all_annotations <- do.call(plyr::rbind.fill, lapply(ann_files, function(x) 
  transform(read.csv(x), File = basename(x))))

##reset wd####
setwd("/Users/virginiabiede/Desktop/R/cruise_data/jason")

#combining metadata####

#transects dive # Dive.. is J2-1442
names(all_data)
#dive held in File
head(all_data)
#dive number J2-1442_sealogExport.csv
#should be able to extract all J2-all numbers

#this is how I check the string extraction will work
all_data$File |>
  str_extract("J2-\\d*") |> #"J2-digit *zero or more (* is "greedy")
  unique()

#extract just the pertinent metadata from the sealog combined files
#goal: feature, dive, date, time, d.degX, d.degY, depth, temp
names(all_data)

#will need to extract dive number, time, date separately
#keep event_value so we can get the ASNAP image time stamps which are about every 30 seconds

jason_dive_meta <- all_data |>
  mutate(Dive = str_extract(File, "J2-\\d*"))|>
  mutate(Time = hms::as_hms(str_remove(str_extract(ts, "T[:digit:]*[:punct:]*[:digit:]*[:punct:]*[:digit:]*"), "T") ))|> #extract T hour:min:sec
  mutate(Date = as.POSIXct(ts)) |>
  dplyr::select(Dive, Date, Time, vehicleReNavData.longitude_value, 
                vehicleReNavData.latitude_value, vehicleReNavData.depth_value, 
                vehicleTemperatureProbe.probe1_temperature_value, event_value) |>
  dplyr::rename("dd.long" = 4, "dd.lat" = 5, "depth" = 6, "temp" = 7) #renames the columns better

#combine metadata with seamount
head(transects) #what would be better is actually the dive data summary, for now filter

dive_smnt_side <- transects |>
  dplyr::select(Seamount, Dive..)|>
  unique()

#most also have the seamount side...
#get it so there are no spaces in the name then extract or remove all word characters preceded by space
dive_smnt_side$Seamount <- dive.smnt.side$Seamount |>
  str_replace("SE Hancock", "SE.Hancock")|>
  str_replace("Yuryaku Southeast", "Yuryaku SE")|>
  str_replace("Bank 9", "Bank.9")
  
dive_smnt_side_nice <- dive_smnt_side |>
  mutate(Side = str_extract(Seamount, "(?<=\\s)\\w*")) |>
  mutate(Seamount = str_remove(Seamount, "(?<=\\s)\\w*")|>
           str_remove("\\s")) |> #this removes side from Seamount
  dplyr::rename("Dive" = "Dive..")

unique(dive_smnt_side_nice$Seamount)

#combine!####

jason_all_meta_w_smnt <- left_join(jason_dive_meta, dive_smnt_side_nice, by = "Dive")

#write csv####
write.csv(jason_all_meta_w_smnt, "Jason_SeaLog_dive_datetime_pos_z_c_smnt_side.csv")

#filter to just the ASNAP 30 seconds####
jason_30_meta <- jason_all_meta_w_smnt |>
  filter(event_value == "ASNAP")

write.csv(jason_30_meta, "Jason_SeaLog_30s_meta.csv")

#jason imagery annotations wrangling####
head(all_annotations)
names(all_annotations)
all_annotations |>
  dplyr::select(label_hierarchy, label_name) |>
  unique()

jason_biigle_cmb <- all_annotations |>
  dplyr::select(File, filename, user_id, firstname, lastname, image_id, label_name, created_at, annotation_id)

jason_biigle_cmb$site <- str_extract(jason_biigle_cmb$File, "(?<=-)\\w*(?=-)")

write.csv(jason_biigle_cmb, "Combined_All_Jason_Annotations_071025.csv")

#currently filtering to just Jules and Sierra annotations
#the progress sheet does not show any overlap between Jules and Sierra annotations
#then combine with the transects meta data by filename date and frame time
unique(jason_biigle_cmb$lastname)

jason_biigle_cmb_JMSL <- jason_biigle_cmb |>
  dplyr::filter(lastname == "Landreth" | lastname == "McHenry")

#this extracts the letters between the first and second -
jason_biigle_cmb_JMSL$date_time <- str_extract(jason_biigle_cmb_JMSL$filename, "(?<=_)\\d*")

#str_extract retrieves the first 8 digits
#POSIXct creates a date-time formatted class
#as.Date just selects the date, removes the time zone

jason_biigle_cmb_JMSL$Date <- as.Date(as.POSIXct(str_extract(jason_biigle_cmb_JMSL$date_time, "\\d{8}"), format = "%Y%m%d"))
jason_biigle_cmb_JMSL$time <- hms::as_hms(as.POSIXct(str_extract(jason_biigle_cmb_JMSL$date_time, "\\d*"), format = "%Y%m%d%H%M%S"))
jason_biigle_cmb_JMSL$frame.time <- (as.numeric(str_extract(jason_biigle_cmb_JMSL$filename, "(?<=frame)\\d*")))/30
#okay this is doing the very difficult thing of adding time, 
#time is not just straightup math to computers 
#because we've made arbitrary delineations for hours, minutes, seconds
#therefore, time had to be converted back 
#to a time class, 
#the seconds had to be added as seconds, 
#and then reconverted into just the hms
jason_biigle_cmb_JMSL$image.time <- hms::as_hms((as.POSIXct(jason_biigle_cmb_JMSL$time)) + seconds(jason_biigle_cmb_JMSL$frame.time))

write.csv(jason_biigle_cmb_JMSL, "JM_SL_annotations_datetime071025.csv")

#combine JM and SL annotations and with dive metadata by date_time####

head(jason_30_meta)
head(jason_biigle_cmb_JMSL)

#trial on just yuryaku for a minute
yury_ann <- jason_biigle_cmb_JMSL |>
  filter(site == "yuryaku") |>
  dplyr::rename("Date"="date")|>
  mutate(hm = paste0(lubridate::hour(image.time),":", lubridate::minute(image.time)))
yury_30_meta <- jason_30_meta |>
  filter(Seamount == "Yuryaku")|>
  mutate(hm = paste0(lubridate::hour(Time),":", lubridate::minute(Time)))

head(yury_ann)
head(yury_30_meta)

yury_cmb_ann_meta <- yury_30_meta |>
  #many to many pairs every possible hour minute match together
  left_join(yury_ann, by = c("Date", "hm"), relationship = "many-to-many") |> 
  #filter(!is.na(time))|> #remove where no overlaps
  #time_diff creates a way to track which hour minute combo are the closest! i.e. most accurate 
  mutate(time_diff = abs(image.time-Time))|> 
  #group by the annotation identification (ensure no duplicate annotations or loss of annotations)
  group_by(annotation_id) |> 
  arrange(time_diff) |> #this arranges by smallest to largest
  #this takes the first of every group, i.e. the smallest difference in time for each annotation and the metadata
  slice_head()|> 
  ungroup() #removes grouping

head(as.data.frame(yury_cmb_ann_meta))
length(unique(yury_ann$annotation_id))
length(unique(yury_cmb_ann_meta$annotation_id)) #fuck I've lost one


#try with all, is it still one or are there bigger issues with this method

jason_30_meta$hm <- paste0(lubridate::hour(jason_30_meta$Time),":", lubridate::minute(jason_30_meta$Time))
jason_biigle_cmb_JMSL$hm <- paste0(lubridate::hour(jason_biigle_cmb_JMSL$time), ":", lubridate::minute(jason_biigle_cmb_JMSL$time))

cmb_ann_meta <- jason_30_meta |>
  #many to many pairs every possible hour minute match together
  left_join(jason_biigle_cmb_JMSL, by = c("Date", "hm"), relationship = "many-to-many") |> 
  #filter(!is.na(time))|> #remove where no overlaps
  #time_diff creates a way to track which hour minute combo are the closest! i.e. most accurate 
  mutate(time_diff = abs(image.time-Time))|> 
  #group by the annotation identification (ensure no duplicate annotations or loss of annotations)
  group_by(annotation_id) |> 
  arrange(time_diff) |> #this arranges by smallest to largest
  #this takes the first of every group, i.e. the smallest difference in time for each annotation and the metadata
  slice_head()|> 
  ungroup() #removes grouping

#method from: https://stackoverflow.com/questions/68598809/match-the-closest-time-stamp-within-a-group-in-r

length(unique(jason_biigle_cmb_JMSL$annotation_id))
length(unique(cmb_ann_meta$annotation_id)) #how did we get one extra? <- NA

write.csv(cmb_ann_meta, "Combined_Jason_Biigle_Annotations_JMSL_w_30s_meta_071025.csv")

cmb_ann_meta |>
  ggplot() +
  geom_boxplot(aes(x = Seamount, y = -(depth))) +
  facet_wrap("label_name")
