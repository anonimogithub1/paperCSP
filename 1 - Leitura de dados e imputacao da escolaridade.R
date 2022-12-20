
# Pacotes ----------------------------------------------------------------

library(tidyverse)
library(dplyr)


# Funções auxiliares ------------------------------------------------------

# Função que faz a imputação usando o método ABB

set.seed(35627)

abb_imputation <- function(x) {
  
  n_missing <- sum(is.na(x))
  
  not_na <- x[!is.na(x)]
  not_na <- sample(not_na, replace = TRUE)
  
  new_values <- sample(not_na, size = n_missing, replace = TRUE)
  
  x[is.na(x)] <- new_values
  x
}


# Execução ----------------------------------------------------------------

m <- 25 #Número de imputações para imputação múltipla

df <- readRDS("ano2010_adultos.rds")
uf_regiao <- readxl::read_excel("arquivos_auxiliares/uf-regiao.xlsx")
df <- df %>% left_join(uf_regiao, by = "uf")

# filtrar (aqui foram excluídos os missing das variáveis sexo e idade)
df <- df %>%
  filter(SEXO != "(missing)", gretarioQ != "(missing)") %>% 
  group_by(regiao, SEXO, gretarioQ, capitulo_CID2)

for(i in 1:m) {
  
  nome_var <- sprintf("imp%02d", i)
  
  df <- df %>%
    mutate(
      !!nome_var := ifelse(esc2 == "(missing)", NA, as.character(esc2)) %>% 
        abb_imputation()
    )

}

df <- df %>%
  ungroup()


saveRDS(df, "ano2010_adultos_imputado.rds")
  


