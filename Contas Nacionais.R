
  #####################################################
  ################                     ################
  ################  Contas Nacionais   ################
  ################      01/10/2023     ################
  ################                     ################
  #####################################################
  
  if (!require("pacman")) {install.packages("pacman")} else {library(pacman)}
  p_load(rbcb, sidrar,
         tidyverse, magrittr, janitor, rlang, gt, gtExtras, glue, 
         fontawesome, bstfun, pBrackets, ggtext, shades, remotes) 
  p_loaded()  
  #remotes::install_github("ddsjoberg/bstfun")
  #remotes::install_github("cararthompson/ophelia", auth_token = "the-secret-authorisation-token")
  
  
  ## Importando dados ---- 

   # Oferta
   sub_industria = c("Indústrias extrativas", "Indústrias de transformação", "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos", "Construção")
   sub_servicos  = c("Comércio", "Transporte, armazenagem e correio", "Informação e comunicação", "Atividades financeiras, de seguros e serviços relacionados", "Atividades imobiliárias", "Outras atividades de serviços", "Administração, saúde e educação públicas e seguridade social")
   
   # Demanda
   sub_demanda = c("Consumo das Famílias", "Consumo do Governo", "FBCF", "Exportação", "Importação (-)")
  
   pib_tri = list( get_sidra(api = "/t/1621/n1/all/v/all/p/all/c11255/all/d/v584%202") %>% 
                   clean_names() %>% 
                   mutate(mes = case_when(substr(trimestre_codigo, 5, 6) == "01" ~ as.Date(paste0(substr(trimestre_codigo, 1, 4), "-03-01")),
                                          substr(trimestre_codigo, 5, 6) == "02" ~ as.Date(paste0(substr(trimestre_codigo, 1, 4), "-06-01")),
                                          substr(trimestre_codigo, 5, 6) == "03" ~ as.Date(paste0(substr(trimestre_codigo, 1, 4), "-09-01")),
                                          substr(trimestre_codigo, 5, 6) == "04" ~ as.Date(paste0(substr(trimestre_codigo, 1, 4), "-12-01"))),
                          trimestre = case_when(substr(trimestre_codigo, 5, 6) == "01" ~ paste0(substr(trimestre_codigo, 1, 4), ".I"),
                                                substr(trimestre_codigo, 5, 6) == "02" ~ paste0(substr(trimestre_codigo, 1, 4), ".II"),
                                                substr(trimestre_codigo, 5, 6) == "03" ~ paste0(substr(trimestre_codigo, 1, 4), ".III"),
                                                substr(trimestre_codigo, 5, 6) == "04" ~ paste0(substr(trimestre_codigo, 1, 4), ".IV"))) %>% 
                   select(mes, trimestre, setores_e_subsetores, `Número-índice` = valor) %>% 
                   group_by(setores_e_subsetores) %>% 
                   mutate(QoQ = ((`Número-índice` / lag(`Número-índice`, 1)) - 1) * 100) %>% 
                   pivot_longer(cols = 4:5, names_to = "variavel", values_to = "valor") %>% 
                   select(mes, trimestre, variavel, setores_e_subsetores, valor),
   
                   get_sidra(api = "/t/5932/n1/all/v/6561,6562,6563/p/all/c11255/all/d/v6561%201,v6562%201,v6563%201") %>%
                   clean_names() %>%
                   mutate(variavel = case_when(grepl("Taxa trimestral", variavel)                     ~ "YoY",
                                               grepl("Taxa acumulada em quatro trimestres", variavel) ~ "12m",
                                               grepl("Taxa acumulada ao longo do ano", variavel)      ~ "YTD",
                                               T ~ as.character(variavel)),
                          mes = case_when(substr(trimestre_codigo, 5, 6) == "01" ~ as.Date(paste0(substr(trimestre_codigo, 1, 4), "-03-01")),
                                          substr(trimestre_codigo, 5, 6) == "02" ~ as.Date(paste0(substr(trimestre_codigo, 1, 4), "-06-01")),
                                          substr(trimestre_codigo, 5, 6) == "03" ~ as.Date(paste0(substr(trimestre_codigo, 1, 4), "-09-01")),
                                          substr(trimestre_codigo, 5, 6) == "04" ~ as.Date(paste0(substr(trimestre_codigo, 1, 4), "-12-01"))),
                          trimestre = case_when(substr(trimestre_codigo, 5, 6) == "01" ~ paste0(substr(trimestre_codigo, 1, 4), ".I"),
                                                substr(trimestre_codigo, 5, 6) == "02" ~ paste0(substr(trimestre_codigo, 1, 4), ".II"),
                                                substr(trimestre_codigo, 5, 6) == "03" ~ paste0(substr(trimestre_codigo, 1, 4), ".III"),
                                                substr(trimestre_codigo, 5, 6) == "04" ~ paste0(substr(trimestre_codigo, 1, 4), ".IV"))) %>% 
                   select(mes, trimestre, variavel, setores_e_subsetores, valor)
                 ) %>% 
             reduce(bind_rows) %>% 
             mutate(setores_e_subsetores = case_when(setores_e_subsetores == "Agropecuária - total" ~ "Agropecuária",                                              
                                                     setores_e_subsetores == "Indústria - total" ~ "Indústria",                                                 
                                                     setores_e_subsetores == "Serviços - total" ~ "Serviços",                                                  
                                                     setores_e_subsetores == "Valor adicionado a preços básicos" ~ "Valor Adicionado (pb)",                                 
                                                     setores_e_subsetores == "PIB a preços de mercado" ~ "PIB (pm)",                                           
                                                     setores_e_subsetores == "Despesa de consumo das famílias" ~ "Consumo das Famílias",                                   
                                                     setores_e_subsetores == "Despesa de consumo da administração pública" ~ "Consumo do Governo",                       
                                                     setores_e_subsetores == "Formação bruta de capital fixo" ~ "FBCF",                                    
                                                     setores_e_subsetores == "Exportação de bens e serviços" ~ "Exportação",                                     
                                                     setores_e_subsetores == "Importação de bens e serviços (-)" ~ "Importação (-)",                                 
                                                     setores_e_subsetores == "Impostos líquidos sobre produtos" ~ "Impostos (líq. s/ prod.)",
                                                     T ~ as.character(setores_e_subsetores)),
                    setores_e_subsetores = factor(setores_e_subsetores, levels = c("Agropecuária", "Indústria", sub_industria, "Serviços", sub_servicos,
                                                                                   sub_demanda,
                                                                                   "Valor Adicionado (pb)", "Impostos (líq. s/ prod.)", "PIB (pm)"))) %>% 
                    pivot_wider(names_from = variavel, values_from = valor) %>% 
                    mutate(`QoQ Anualizado` = (((1 + (QoQ / 100)) ^ 4) - 1) * 100, .after = QoQ) %>% 
                    pivot_longer(cols = 4:9, names_to = "variavel", values_to = "valor")
   
   
  ## Tabelas ----
  
   pwalk(tibble(filtro = list(c("Agropecuária", "Indústria", "Serviços", "PIB (pm)", sub_demanda, "Valor Adicionado (pb)", "Impostos (líq. s/ prod.)"), c("Indústria", sub_industria), c("Serviços", sub_servicos)),
                
                titulo = c("**Tabela 1. Variação do PIB (%)**", "**Tabela 2. Variação do PIB na Indústria (%)**", "**Tabela 3. Variação do PIB nos Serviços (%)**"),
               
                borda = list(c(4, 9, 11), c(1, 2), c(1, 2)),
                texto = list(c(9, 10, 11), 1, 1),
                
                save = c("PIB", "PIB_Industria", "PIB_Serviços")
               ),
         
        function(filtro, titulo, borda, texto, save) {
          
          pib_tri %>% 
            filter(setores_e_subsetores %in% filtro,
                   mes == max(mes),
                   variavel != "Número-índice") %>% 
            pivot_wider(names_from = "variavel", values_from = "valor") %>% 
            arrange(setores_e_subsetores) %>% 
            
            gt(rowname_col = "setores_e_subsetores", groupname_col = NA) %>%
            cols_hide(c(mes, trimestre)) %>% 
            tab_header(title = md(titulo),
                       subtitle = md(unique(.$`_data`$trimestre))) %>% 
            tab_source_note(source_note = md("Fonte: IBGE. Elaboração própria.")) %>%
            tab_spanner(label = "QoQ (SA)",
                        columns = c(QoQ, `QoQ Anualizado`)) %>% 
            
            tab_style(style = cell_text(weight = "bold"),
                      locations = cells_body(columns = 5)) %>%
            
            tab_style(style = cell_text(color = "black"),
                      locations = cells_body()) %>%
            
            tab_style(style = cell_text(color = "darkgrey", font = google_font("Source Sans Pro")), 
                      locations = cells_column_labels(everything())) %>% 
            
            tab_style(style = cell_borders(sides = c("top"), color = "black", weight = px(2.5), style = "solid"),
                      locations = list(cells_body(rows = borda), cells_stub(rows = borda))) %>% 
          
            tab_style(style = cell_text(weight = "bold"),
                      locations = list(cells_body(rows = texto), cells_stub(rows = texto))) %>% 
            
            
            # text_transform(locations = cells_body(),
            # fn = function(x) {
            #   paste(x, case_when(x > 0 ~ "<i class='fa fa-arrow-up' '></i>",
            #                      x <= 0 ~ "<i class='fa fa-arrow-down '></i>"))
            # }) %>% 
            
            opt_table_font(font = list(google_font("Lato"), default_fonts())) %>% 
            
            tab_options(heading.title.font.size = 25,
                        heading.subtitle.font.size = 22,
                        heading.align = "left",
                        
                        table.border.top.style = "hidden",
                        heading.border.bottom.style = "hidden",
                        
                        table.font.size = 20,
                        
                        source_notes.border.bottom.style = "hidden",
                        source_notes.padding = px(10),
                        
                        data_row.padding = px(10)) %>%
            fmt_number(decimals = 2) %>% 
            cols_align("center") %>%
            cols_width(stub() ~ px(300),
                       everything() ~ px(130)) %>% 
            
            gtsave(paste0(save, ".png")) 
          
        })
  
  
  ## Gráficos - Componentes da Demanda e da Oferta ----
  
  pwalk(tibble(titulo = c("**Gráfico 1. PIB pela Ótica da Demanda**", "**Gráfico 3. PIB pela Ótica da Oferta**"),
               filtro = list(sub_demanda, c("Agropecuária", "Indústria", "Serviços")),
               save = c("Demanda", "Oferta")
               ),
        
        function(titulo, filtro, save) {
          
          pib_tri %>% 
            filter(setores_e_subsetores %in% filtro & variavel == "QoQ Anualizado") %>% 
            mutate(trimestre = factor(trimestre),
                   trimestre = fct_reorder(trimestre, mes)) %>%
            group_by(setores_e_subsetores) %>%
            top_n(4, mes) %>% 
            
            {ggplot(., aes(x = trimestre, y = valor)) +
                geom_col(aes(fill = trimestre), width = 0.7) +
                facet_wrap(~ setores_e_subsetores, nrow = 1, strip.position = "bottom",  labeller = label_wrap_gen(10)) +
                geom_hline(yintercept = 0) +
                
                labs(title = titulo,
                     subtitle = "Var. % QoQ anualizada, com aj. sazonal",
                     x = "", y = "", fill = "Trimestre") +
                lightness(scale_fill_brewer(palette = "Blues"), scalefac(0.8)) +
                geom_text(aes(y = valor, label = scales::number(valor, accuracy = 0.1), vjust = ifelse(valor > 0, -0.5, 1.5)), size = 5) +
                
                theme(plot.title = element_markdown(size = 23),
                      plot.subtitle = element_markdown(size = 15, margin = margin(0,0,30,0)),
                      plot.background = element_blank(),
                      
                      panel.background = element_blank(),
                      panel.grid.major.y = element_line(linetype = "dotdash", colour = "grey", linewidth = 1),
                      
                      strip.text = element_text(size = 15),
                      strip.background = element_rect(fill = "white"),
                      strip.background.y = element_rect(fill = "black"),
                      panel.spacing = unit(2, "lines"),
                      
                      axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      axis.text.y = element_markdown(size = 15, colour = "black"),
                      
                      legend.position = c(0.8, 1.1),
                      legend.direction = "horizontal",
                      legend.title = element_blank(),
                      legend.text = element_markdown(size = 13))
            } %>% 
            
            ggsave(paste0(save, ".png"), ., width = 12, height = 7, units = "in", dpi = 300)
        
         }) 
            
     
   ## Gráficos - Desagregação Indústria e Serviços ----
    
   pwalk(tibble(titulo = c("**Gráfico 2. PIB, Consumo das Famílias, Governo e Investimento**", "**Gráfico 4. PIB da Indústria**", "**Gráfico 5. PIB dos Serviços**"),
                filtro = list(c("PIB (pm)", sub_demanda[!grepl(c("Exportação|Importação"), sub_demanda)]), sub_industria, sub_servicos),
                facet = c(T, F, F), 
                escala = list(rep("#2171B5", 4), c("#ffb90f", "#cc940c", "#996f09", "#664a06"), c("#ffd56f", "#ffb90f", "#cc940c", "#996f09", "#664a06", "#332503", "#000000")),
                save = c("Sub_Demanda", "Sub_Industria", "Sub_Serviços")
               ),
   
         function(titulo, filtro, facet, escala, save) {
           
           pib_tri %>% 
             filter(setores_e_subsetores %in% filtro & variavel == "Número-índice",
                    mes >= "2014-03-01") %>% 
             group_by(setores_e_subsetores) %>%
             mutate(valor = case_when(variavel == "Número-índice" ~ 100 * valor / valor[variavel == "Número-índice"][mes == "2019-12-01"],
                                      T ~ valor)) %>%  
      
             {ggplot(., aes(x = mes, y = valor, colour = setores_e_subsetores, group = setores_e_subsetores)) +
                 geom_line(size = 1) +
                 geom_point(aes(x = as.Date("2019-12-01"), y = 100), size = 4, colour = "black") +
                 guides(colour = guide_legend(ncol = 2)) +
                 
                 labs(title = titulo,
                      subtitle = "2019.IV = 100, com aj. sazonal",
                      x = "", y = "", fill = "Trimestre") +
                 scale_x_date(breaks = "6 month", labels = scales::label_date_short()) + 
                 scale_colour_manual(values = escala) +
                 
                 theme(plot.title = element_markdown(size = 23),
                       plot.subtitle = element_markdown(size = 15, margin = margin(0,0,30,0)),
                       plot.background = element_blank(),
                       
                       panel.background = element_blank(),
                       panel.grid.major.y = element_line(linetype = "dotdash", colour = "grey", linewidth = 1),
                       
                       axis.ticks.x = element_blank(),
                       axis.text = element_markdown(size = 15, colour = "black"),
                       
                       legend.position = c(0.6, 1),
                       legend.direction = "vertical",
                       legend.title = element_blank(),
                       legend.key = element_blank(),
                       legend.background = element_blank(),
                       legend.text = element_markdown(size = 13)) +
               
               {if(facet == T) {
                   list(facet_wrap(~ setores_e_subsetores, scales = "free_y"),
                        geom_hline(yintercept = 100, size = 0.6, colour = "black"),
                        scale_x_date(breaks = "12 month", labels = scales::label_date_short()),
                        theme(axis.text = element_markdown(size = 12, colour = "black"),
                              strip.text = element_text(size = 15),
                              strip.background = element_rect(fill = "white"),
                              strip.background.y = element_rect(fill = "black"),
                              legend.position = "none"))}} 
             } %>%
       
          ggsave(paste0(save, ".png"), ., width = 12, height = 7, units = "in", dpi = 300)
     
   }) 
    