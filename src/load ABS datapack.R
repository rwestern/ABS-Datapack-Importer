library(data.table)
library(foreach)
library(readxl)

load_ABS_datapacks <- function()
{
  ABS_datapacks <-
    list(UCL = parse_ABS_datapack(folder_name = 'C:/local datasets/ABS DataPacks/2016_GCP_UCL_for_Vic_short-header'),
       LGA = parse_ABS_datapack(folder_name = 'C:/local datasets/ABS DataPacks/2016_GCP_LGA_for_Vic_short-header'),
       SA1 = parse_ABS_datapack(folder_name = 'C:/local datasets/ABS DataPacks/2016_GCP_SA1_for_Vic_short-header'),
       SA2 = parse_ABS_datapack(folder_name = 'C:/local datasets/ABS DataPacks/2016_GCP_SA2_for_Vic_short-header'),
       SA3 = parse_ABS_datapack(folder_name = 'C:/local datasets/ABS DataPacks/2016_GCP_SA3_for_Vic_short-header'),
       SA4 = parse_ABS_datapack(folder_name = 'C:/local datasets/ABS DataPacks/2016_GCP_SA4_for_Vic_short-header')
  )
  return(ABS_datapacks)
}

parse_ABS_datapack <- function(folder_name)
{
  cat(paste('Loading', folder_name, '\n'))
  csv_filenames <- Sys.glob(file.path(folder_name, '*/*.csv'))
  metadata_filenames <- Sys.glob(file.path(folder_name, 'Metadata/*Metadata*.xlsx'))
  
  table_metadata <- read_excel(metadata_filenames[1], 
                               sheet = 'Table number, name, population', 
                               skip = 9)
  column_metadata <- read_excel(metadata_filenames[1], 
                                sheet = 'Cell descriptors information', 
                                skip = 10)
  csv_data <-
    foreach(csv_filename = csv_filenames) %do%
    data.table(read.csv(csv_filename))
  names(csv_data) <- gsub('.*_(G[0-9]{2}[A-Z]?)_.*', '\\1', csv_filenames)
  
  renamed_list <-
    llply(unique(column_metadata$`DataPack file`),
          function(datapack_file)
          {
            df <- csv_data[[datapack_file]]
            colnames(df) <- coalesce(column_metadata[match(colnames(df),
                                                           column_metadata[column_metadata$`DataPack file` == datapack_file,]$Short),]$Long,
                                     colnames(df))
            return(df)
          })
  names(renamed_list) <- names(csv_data)
  datapack_data <- 
    foreach(item = unique(substr(names(csv_data), 1, 3))) %do%
    {
      dfs <- renamed_list[item == substr(names(csv_data), 1, 3)]
      if(length(dfs) == 1)
        return(dfs[[1]])
      foreach(df = dfs[2:length(dfs)], .combine = cbind, .init = dfs[[1]]) %do% df[,2:ncol(df)]
    }
  names(datapack_data) <- table_metadata$`Table name`[match(table_metadata$`Table number`, unique(substr(names(csv_data), 1, 3)))]
  stopifnot(length(datapack_data) > 1)
  stopifnot(nrow(datapack_data[[1]]) > 1)
  stopifnot(sd(unlist(lapply(datapack_data, nrow))) == 0)
  return(datapack_data)
}

parse_ABS_datapack2 <- function(folder_name)
{
  cat(paste('Loading', folder_name, '\n'))
  csv_filenames <- Sys.glob(file.path(folder_name, '*/*.csv'))
  metadata_filenames <- Sys.glob(file.path(folder_name, 'Metadata/*Metadata*.xlsx'))
  
  table_metadata <- read_excel(metadata_filenames[1], 
                               sheet = 'Table number, name, population', 
                               skip = 9)
  column_metadata <- read_excel(metadata_filenames[1], 
                                sheet = 'Cell descriptors information', 
                                skip = 10)
  csv_data <-
    ldply(csv_filenames,
          function(csv_filename)
          {
            raw_data <- read.csv(csv_filename)
            data.table(table = factor(gsub('.*_(G[0-9]{2})[A-Z]?_.*', '\\1', csv_filename)),
                       file = factor(gsub('.*_(G[0-9]{2}[A-Z]?)_.*', '\\1', csv_filename)),
                       melt(raw_data, id.vars = 1))
          })
  csv_data <-
    merge(table_metadata[,1:2],
          csv_data,
          by.y = 'table', by.x = 'Table number')
  
  column_metadata$`Profile table` <- toupper(column_metadata$`Profile table`)
  datapack_data <-
    merge(column_metadata[,c(2, 3, 4)],
          csv_data,
          by.y = c('file', 'variable'), by.x = c('DataPack file', 'Short'))
  datapack_data <- as.data.table(rename(select(datapack_data, -c('DataPack file', 'Short')),
                                        Variable = 'Long'))
  datapack_data[,numeric := as.numeric(value)]
  return(datapack_data)
}


