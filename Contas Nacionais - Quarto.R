  
  #####################################################
  ################                     ################
  ################  Contas Nacionais   ################
  ################      03/01/2024     ################
  ################                     ################
  #####################################################
  
  # Script auxiliar criado para ser importado dentro do documento Quarto
  
  if (!require("pacman")) {install.packages("pacman")} else {library(pacman)}
  p_load(rbcb, sidrar,
         tidyverse, magrittr, janitor, rlang, gt, gtExtras, glue, 
         fontawesome, bstfun, pBrackets, ggtext, shades, remotes) 
  p_loaded()  
  # remotes::install_github("ddsjoberg/bstfun")
  # remotes::install_github("cararthompson/ophelia", auth_token = "the-secret-authorisation-token")
  
  
  ## Vetores Auxiliares ----
  
  ordinais_masc = c("primeiro", "segundo", "terceiro", "quarto", "quinto", "sexto", "sétimo", "oitavo", "nono", "décimo",
                    "décimo primeiro", "décimo segundo", "décimo terceiro", "décimo quarto", "décimo quinto", "décimo sexto", "décimo sétimo", "décimo oitavo", "décimo nono", "vigésimo",
                    "vigésimo primeiro", "vigésimo segundo", "vigésimo terceiro", "vigésimo quarto", "vigésimo quinto", "vigésimo sexto", "vigésimo sétimo", "vigésimo oitavo", "vigésimo nono", "trigésimo")
  
  ordinais_femi = c("primeira", "segunda", "terceira", "quarta", "quinta", "sexta", "sétima", "oitava", "nona", "décima",
                    "décima primeira", "décima segunda", "décima terceira", "décima quarta", "décima quinta", "décima sexta", "décima sétima", "décima oitava", "décima nona", "vigésima",
                    "vigésima primeira", "vigésima segunda", "vigésima terceira", "vigésima quarta", "vigésima quinta", "vigésima sexta", "vigésima sétima", "vigésima oitava", "vigésima nona", "trigésima")
  
  
  
  
  ## Funções Auxiliares ----
  
  # [valor] Buscar taxa de uma variável em determinada métrica
  valor <- function(setores_e_subsetores = "PIB (pm)", variavel = "QoQ") {
              geral[geral$setores_e_subsetores == setores_e_subsetores, variavel] %>% round(2)
            }
 
  
  # [texto] Buscar texto de crescimento/contração
  texto_crescim <- function(valor, num) {
                      case_when(num == 1 ~ ifelse(valor > 0, "cresceu", "contraiu"),
                                num == 2 ~ ifelse(valor > 0, "crescimento", "decrescimento"),
                                num == 3 ~ ifelse(valor > 0, "expansão", "contração"))
                    }   
  
  
  # [texto] Buscar texto de posição de crescimento/contração em sequência
  texto_posicao <- function(setores_e_subsetores = "PIB (pm)", variavel = "QoQ", genero = c("M", "F")) {
                    posicao = pos[pos$setores_e_subsetores == setores_e_subsetores, variavel]
                    num = str_extract(posicao, "([0-9]+)") %>% as.numeric
                    adj = str_extract(posicao, "[a-z]+")
                    
                    df  = tibble( M  = ordinais_masc,
                                 `F` = ordinais_femi)
                    
                    pt1 = ifelse(genero == "M", df[num, "M"], paste(df[num, "F"], "taxa"))
                    
                    pt2 = case_when(adj == 'pos' & genero == "M" ~ "crescimento",
                                    adj == 'pos' & genero == "F" ~ "positiva")
                    
                    paste(pt1, pt2)
                  }
  

  # [texto] Buscar texto sobre estar acima ou abaixo das expectativas do Focus
  focus <- function(tri, num) {
              case_when(
                num == 1 ~ ifelse(expec_focus > valor("PIB (pm)", "YoY"), 
                                  "negativamente", "positivamente"),
                num == 2 ~ ifelse(expec_focus > valor("PIB (pm)", "YoY"), 
                                  "abaixo", "acima")
              )
           }
  
  
  
  
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
    mutate(trimestre = case_when(str_extract(trimestre, '(?<=\\.).*') == "I"   ~ str_c("1T/", str_extract(trimestre, '.*(?=\\.)')),
                                 str_extract(trimestre, '(?<=\\.).*') == "II"  ~ str_c("2T/", str_extract(trimestre, '.*(?=\\.)')),
                                 str_extract(trimestre, '(?<=\\.).*') == "III" ~ str_c("3T/", str_extract(trimestre, '.*(?=\\.)')),
                                 str_extract(trimestre, '(?<=\\.).*') == "IV"  ~ str_c("4T/", str_extract(trimestre, '.*(?=\\.)'))),
           `QoQ Anualizado` = (((1 + (QoQ / 100)) ^ 4) - 1) * 100, .after = QoQ) %>% 
    pivot_longer(cols = 4:9, names_to = "variavel", values_to = "valor")
  
  
  
  
  
  ## Trimestre Atual e Anterior 
  
  tri_atual     = pib_tri %>% filter(mes == max(mes)) %>% pull(trimestre) %>% unique
  tri_atual_num = substr(tri_atual, 1, 1)
  ano_atual     = substr(tri_atual, 4, 8)
  
  tri_anter     = pib_tri %>% filter(mes == nth(unique(mes), 2, order_by = desc(unique(mes)))) %>% pull(trimestre) %>% unique
  tri_anter_num = substr(tri_anter, 1, 1)
  ano_anter     = (as.numeric(ano_atual) - 1) %>% as.character
  
  
  
  
  ## Expectativa Focus PIB YoY (crescimento interanual)  
  
  expec_focus = get_quarterly_market_expectations(indic = "PIB Total", 
                                                  start_date = "2023-09-01", 
                                                  end_date = Sys.Date()) %>% 
                filter(str_detect(reference_date, paste0(tri_atual_num, "/")) & str_detect(reference_date, "2023")) %>% 
                filter(date == max(date) & base == 1) %$% 
                median
  
  
  
  
  ## Tabela Geral ----
  
  geral = pib_tri %>% 
          filter(setores_e_subsetores %in% c("Agropecuária", "Indústria", "Serviços", "PIB (pm)", sub_demanda, "Valor Adicionado (pb)", "Impostos (líq. s/ prod.)"),
                 mes == max(mes),
                 variavel != "Número-índice") %>% 
          pivot_wider(names_from = "variavel", values_from = "valor") %>%
          arrange(setores_e_subsetores) 
  
  
  
  
  ## Crescimento/Contração Consecutivos ----
  
  pos = pib_tri %>% 
        filter(variavel != "Número-índice", !is.na(valor)) %>% 
        group_by(setores_e_subsetores, variavel) %>% 
        mutate(position = coalesce(accumulate(ifelse(sign(valor) - lag(sign(valor)) != 0 | row_number() == 1, 1, NA),
                                              ~ ifelse(is.na(.y), .x + 1, .y))),
               position = ifelse(sign(valor) > 0, paste(position, "pos"), 
                                 ifelse(sign(valor) < 0, paste(position, "neg"), 'zero'))) %>% 
        filter(mes == max(mes)) %>% 
        pivot_wider(id_cols = 1:3, names_from = variavel, values_from = position)
