project_DDM_multiple <- function(file, year, number_of_sheets){

  usePackage <- function(p = c(dplyr, magrittr, tidyr)) {
    if (!is.element(p, installed.packages()[,1]))
      install.packages(p, dep = TRUE)
    require(p, character.only = TRUE)
  }

  for(k in 1:number_of_sheets){

    sheets <- readxl::excel_sheets(file)

    data <- read_excel(file, sheet = sheets[k])

    growth_rate <- ((sum(data[,3])/sum(data[,2]))^0.1)-1

    number_of_years_to_project <- 24

    mid <- vector()
    for(i in 0:number_of_years_to_project){
      mid[i] <- (print((4/12)+i))
    }


    diff_data <- data %>%
      mutate(diff = data[,3]-data[,2],
             diff_prop = diff/sum(diff))

    Total_pop <- (sum(data[,3])*(1+growth_rate)^mid)

    growth <- round(Total_pop - sum(data[,3]),0)

    name <- colnames(diff_data)


    projected <- data.frame()

    for(i in 1:number_of_years_to_project){
      projected[c(1:16),i] <- round((diff_data$diff_prop*growth[i]) + diff_data[,3],0)
    }


    colnames(projected) <- paste0("year_",as.character((year+1):(year + 24)))

    projected_data <- data.frame(diff_data[,c(1,2,3)], projected)

    projected_population <- projected_data


    write.xlsx(projected_population, "projected_population.xlsx", sheetName = sheets[k], append = TRUE)

  }
}



