load_dataset_info <- function () {
  read_csv('data/dataset_info.csv')
} 

refeather <- function(query, connection, feather_path){
  print(query)
  
  fresh_data <- sqlQuery(connection, query) 
  
  fresh_data %>%
    write_feather(feather_path)
}



loadDataset <- function(dataset_name, configs = load_dataset_info(),
                        refresh_feather = FALSE) {
  config <- configs %>% 
    filter(Name == dataset_name) 
  
  if (refresh_feather == TRUE | 
      file.exists(config$FeatherFilePath) == FALSE) { 
    
    connector <- odbcDriverConnect(connection="Driver={SQL Server};
                            server=cno-sqlreport01;database=Buyspeed;
                            trusted_connection=yes;")
    
    refeather(config$Query, connector, config$FeatherFilePath)
    
    close(connector)
  } 
  
  read_feather(config$FeatherFilePath) 
}
