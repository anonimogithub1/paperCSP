# Pacotes ----------------------------------------------------------------
library(tidyverse)
library(tidyr)

# Carregar dados ----------------------------------------------------------

df <- readRDS("ano2010_adultos_imputado.rds")

# Criar tabelas -----------------------------------------------------------

# Objeto auxiliar com todas as frequências
aux <- df %>%
  select(regiao, sexo = SEXO, gretarioq = gretarioQ, starts_with("imp")) %>%
  mutate_at(vars(regiao , sexo, gretarioq), funs(fct_drop)) %>%
  gather(inputacao, esc_inputada, starts_with("imp")) %>%
  count(regiao, sexo, esc_inputada, gretarioq, inputacao) %>%
  tidyr::complete(regiao, sexo, esc_inputada, gretarioq, inputacao, fill = list(n = 0))

# função que calcula a variância pela Imputação múltipla
var2 <- function(x) {
  (1 + 1/length(x))*var(x)
}

# Função p/ calcular o intervalo de confiança. 
# Retorna NA quando a variância = 0
ic <- function(x, type) {
  
  if(var(x) == 0) 
    return(NA)
  
  ta <- abs(qt(0.05/2, df = length(x) - 1))
  
  if (type == "LI")
    mean(x) - sqrt(var2(x))*ta
  else if (type == "LS")
    mean(x) + sqrt(var2(x))*ta
}

# Cálculo das medidas resumo das tabelas
medidas <- aux %>%
  group_by(regiao, sexo, esc_inputada, gretarioq) %>%
  summarise(`Média` = mean(n), `Variância` = var2(n), LI = ic(n, "LI"), LS = ic(n, "LS"))

# Função que faz a transposição e guarda nome das linhas.
tn <- function(df){
  a <- df %>%
    select(starts_with("imp")) %>%
    t()
  colnames(a) <- df$gretarioq
  a
}

cov2 <- function (x) {
  mat <- matrix(nrow = ncol(x), ncol = ncol(x))
  colnames(mat) <- colnames(x)
  for(row in 1:nrow(mat)) {
    for(col in 1:ncol(mat)) {
      B <- 1/(nrow(x) - 1)*sum((x[,row] - mean(x[,row]))*(x[,col] - mean(x[,col])))
      mat[row, col] <- 0 + (1 + 1/nrow(x)) * B
    }
  }
  
  mat
}

# gera a tabela
tabelas <- aux %>%
  spread(inputacao, n) %>%
  left_join(medidas) %>%
  nest(-regiao, -sexo, -esc_inputada) %>%
  mutate(
    cov_mat = map(data, ~ .x %>% tn() %>% cov2())
  )

# Salva a tabela
saveRDS(tabelas, "tabelas.rds")

