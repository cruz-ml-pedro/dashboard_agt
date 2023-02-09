library(here)
library(glue)
library(shiny)
library(plotly)
library(janitor)
library(openxlsx)
library(gridExtra)
library(flextable)
library(reactable)
library(paletteer)
library(tidyverse)
library(lubridate)
library(nationalparkcolors)
# Get dates ---------------------------------------------------------------

this_month <- lubridate::today() %>% lubridate::month() 

this_year <-  lubridate::today() %>% lubridate::year()

#--------------------------------------------------------------------------

path <- "dados/ANDAMENTO_PRODUTOS.xlsx"
# getting data from sheets
sheets <- openxlsx::getSheetNames(path)
#
data_frame_list <- lapply(sheets,
                          openxlsx::read.xlsx,
                          startRow = 2,
                          xlsxFile=path)
# assigning names to data frame
names(data_frame_list) <- sheets

#---------------------------------------------------------------

produtos_df <- as_tibble()#tibble para guardar os df's

for (i in 1:length(unique(data_frame_list[[1]]$X1))) {
  
  for (j in 1:length(data_frame_list)) {
    
df   <-  data_frame_list[[j]] %>% 
            filter(X1 %in% unique(data_frame_list[[1]]$X1)[i]) %>%
            mutate(mes = sheets[j])

produtos_df <- rbind(produtos_df, df)

  }
  
}

#------------------------------------------------------
names(produtos_df) <- make_clean_names(names(produtos_df))

#

produtos_df  <- produtos_df %>% 
      mutate(
        percent_aprovado = percent_aprovado*100,
        percent_executado = percent_executado*100
        )
#----------------------------------------------------------------------------------
df_totais <- produtos_df %>% 
      group_by(x1) %>% 
      summarise(
        total_aprovado = sum(aprovado),
        total_em_analise = sum(em_analise),
        total_reprovado = sum(reprovado),
        total_horas = sum(total_horas),
        percent_total_aprovado = sum(percent_aprovado),
        percent_total_executado = sum(percent_executado),
        valor_aprovado = sum(aprovado_r),
        total_projetos = sum(total_r)
        
      ) %>% 
      ungroup()
#
df_totais <- df_totais %>% 
    rename(
      "Produtos" = x1,
      "Total aprovado" = total_aprovado,
      "Total em vistoria" = total_em_analise,
      "Total reprovado" = total_reprovado,
      "Total horas" = total_horas,
      "% Total aprovada" = percent_total_aprovado,
      "% Total executada" = percent_total_executado,
      "Valor aprovado R$" = valor_aprovado,
      "Valor total R$" = total_projetos
    )


produtos_df <- produtos_df %>% 
    rename(
      "Produtos" = x1,
      "Horas previstas" = horas_previstas,
      "Aprovado" = aprovado,
      "Em vistoria" = em_analise,
      "Reprovado" = reprovado,
      "Total horas" = total_horas,
      "% Aprovada" = percent_aprovado,
      "% Executada" = percent_executado,
      "Valor hora (HH) R$" = unitario_hh,
      "Valores aprovados R$" = aprovado_r,
      "Valor total R$" = total_r,
      "Data" = mes
    )

#--------------------------------------------
#ggplo2 theme
my_theme <- function() {
    theme_bw() +
      theme(panel.background = element_blank()) +
      theme(plot.background = element_rect(fill = "#E3EEF4FF")) +
      theme(panel.border = element_blank()) +                     
      theme(strip.background = element_blank()) +                 
      theme(plot.margin = unit(c(.5, .5, .5, .5), "cm")) +
      theme(panel.spacing = unit(3, "lines")) +
      theme(panel.grid.major = element_blank()) +
      theme(panel.grid.minor = element_blank()) +
      theme(legend.background = element_blank()) +
      theme(legend.key = element_blank()) +
      theme(legend.title = element_blank())+
      theme(axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=0.5),
             legend.position = "bottom",
             axis.title.x = element_blank(),
             plot.title = element_text(size = rel(1.2))
            )

}

#--------------------------------------------------------
rm(i,j, path, sheets, df, data_frame_list)
