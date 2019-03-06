library(readxl)
library(data.table)

load_VIFSA_ERP <- function(filename = "J:/LGA performance stats/VIF2016_LGAs_VIFSAs_ERP_5yr_age_sex_2011_2031.xlsx")
{
  VIF2016_LGAs <-
    as.data.table(read_excel(filename, 
                             sheet = "ERP_Ages_LGAs", col_types = c("text", 
                                                                    "text", "numeric", "numeric", "numeric", 
                                                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                    "numeric", "numeric", "numeric", "numeric", "skip", 
                                                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                    "numeric", "numeric", "numeric", "skip", 
                                                                    "numeric", "numeric", "numeric"),
                             n_max = 83,
                             skip = 100))
  
  VIF2016_LGAs <- VIF2016_LGAs[`Local Government Area` != 'Victoria']
  rownames(VIF2016_LGAs) <- VIF2016_LGAs$`Local Government Area`
  colnames(VIF2016_LGAs) <- gsub('[ \t\r\n]+', ' ', colnames(VIF2016_LGAs))
  VIF2016_LGAs[, Clean_LGA := gsub(' \\([A-Z]{1,2}\\)$', '', `Local Government Area`)]
}