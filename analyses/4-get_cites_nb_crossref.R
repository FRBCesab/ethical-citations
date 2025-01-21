#Check the number of cited works found for each articles with open Alex and Crossref
#over the 337 journals scraped for 2023

library(httr)
library(jsonlite)
library(ggplot2)

#Collect all the crossrefs (run only once !)
all_files <- list.files(here::here("outputs","original_papers"))

for (j in 1:length(all_files)) {
  #j=8
  df <- qs::qread(here::here("outputs","original_papers",all_files[j]))
  
  if (nrow(df)>0){
    df_new <- do.call(rbind,lapply(1:nrow(df), function(i){
      
      if (i %% 200 == 0) {
        cat(".   Sleeping 10s ...\n")
        Sys.sleep(10)
      }
      cat("i=",i, "over ",nrow(df)," - file",j,"over ",length(all_files),"files","\n")
      url <- paste0("https://api.crossref.org/works/", df$oa_work_doi[i])
      response <- GET(url)
      if (status_code(response) == 200) {
        data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
        # Extract reference count
        reference_count <- data$message$`reference-count`
        reference_counts <- ifelse(is.null(reference_count), 0, reference_count)
      } else {
        reference_counts <- NA  # Handle failed requests
      }
      cbind.data.frame(journal_oa_id=gsub(".qs","",all_files[j]),
                       doi=df$oa_work_doi[i],
                       oa=length(strsplit(df[i,"oa_referenced_works"]," \\| ")[[1]]),
                       cr=reference_counts)
      
    }))
    
    cat(".  Sleeping 30s ...\n")
    Sys.sleep(30)
    
    qs::qsave(df_new,(here::here("outputs","cr_papers_check",all_files[j])))
    
  }
 
}

  #=>Only 314 journals contained articles over the 337

#Assemble all, compute % and plot 

  all_files_cr <- list.files(here::here("outputs","cr_papers_check"))
  
  all_cr <- do.call(rbind,pbmcapply::pbmclapply(all_files_cr, function(id){
    #id <- all_files_cr[1]
    df <- qs::qread(here::here("outputs","cr_papers_check",id))
    df
  },mc.cores=12))

  df_new <- all_cr[all_cr$cr != 0, ]
  df_new <- df_new[df_new$oa != 0, ]
  
  #=> 6547 article had no cited works
  
  range <- range(c(df_new$oa, df_new$cr), na.rm = TRUE)
  
  df_new$percentage <- (df_new$oa / df_new$cr) * 100
  average_percentage <- mean(df_new$percentage, na.rm = TRUE)

  all_cr_plot <- ggplot(df_new, aes(x = oa, y = cr)) +
    geom_point(color = "blue", alpha = 0.7) +  # Scatter plot points
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +  # 1:1 line
    labs(
      title = paste("OA vs CR % mean = ",round(average_percentage,1)),
      x = "Open Alex",
      y = "Crossref"
    ) +
    scale_x_log10()+ 
    scale_y_log10()+
    #scale_x_continuous(limits = range) +  
    #scale_y_continuous(limits = range) +
    theme_bw()

  ##residuals
  df_new$residual <- abs(df_new$oa - df_new$cr)
  
  ## Calculate Z-scores for residuals
  df_new$z_score <- scale(df_new$residual)
  
  ## Identify outliers based on Z-score threshold
  outliers <- df_new[abs(df_new$z_score) > 6, ]

#Compute % per journal and density plot 
  
  all_cr_per <- do.call(rbind,lapply(all_files_cr, function(id){
    #id <- all_files_cr[5]
    df <- qs::qread(here::here("outputs","cr_papers_check",id))
    df_new_per <- df[df$cr != 0, ]
    if (nrow(df_new_per)>0){
      df_new_per$percentage <- (df_new_per$oa / df_new_per$cr) * 100
    average_percentage <- mean(df_new_per$percentage, na.rm = TRUE)
  } else {average_percentage <- NA}
    cbind.data.frame(journal=gsub(".qs","",id),av_per=average_percentage,ncite=nrow(df_new_per))
  }))
  
  library(ggExtra)
  
  p1 <- ggplot(all_cr_per, aes(y=av_per, x=all_cr_per$ncite)) +
    geom_point(color = "blue", alpha = 0.7) +
    geom_hline(yintercept = average_percentage, linetype = "dashed", size = 1) +
    scale_x_log10()+ 
    labs(
      x = "Number of article per journal",
      y = "OA vs CR %"
    )+ 
    theme_bw()
  
  
#Exemple of journals with low % 
  
  dafnee_raw <- read.csv(here::here("data","derived-data","DAFNEE_db_with_issn.csv"))
  
  dafnee_raw$oa_source <- gsub("https://openalex.org/","",dafnee_raw$oa_source_id)

  all_cr_per$jounal_name <- NA
  for (i in 1:nrow(all_cr_per)){
    #i=1
    if (sum(dafnee_raw$oa_source%in%all_cr_per$journal[i])==1){
      all_cr_per$jounal_name[i]=dafnee_raw$journal_clean[dafnee_raw$oa_source%in%all_cr_per$journal[i]]
    } else {
      all_cr_per$jounal_name[i]=NA
    }
  }
  
  all_cr_per <- all_cr_per[order(all_cr_per$av_per),]
  
  sum(is.na(all_cr_per$av_per)) #=> 29 journals do not have % (oa was zero)
  
  ##Ex lowest 
  all_cr[all_cr$journal_oa_id%in%"S4210204291",]
  
  ##What are the journals with highest per ? 
  
  tail(all_cr_per[!is.na(all_cr_per$av_per),])
  
#Save the figure   
 
  all_jr_plot <- ggExtra::ggMarginal(p1, type = "histogram", fill = "lightblue")

  library(gridExtra)
  fig_cr_oa <- arrangeGrob(all_cr_plot,all_jr_plot,ncol=2) #generates g
  ggsave(file=here::here("figures","fig_cr_oa.tiff"), fig_cr_oa,width = 25, height = 12, dpi = 200, units = "cm", device='tiff') 

#Solution ? 
  
    #=> either remove journals with low % or rather refs with low % 
    #use crossref rather than oa ? 
  
  