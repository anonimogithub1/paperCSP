# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(xlsx)
library(ggthemes)

theme_set(theme_bw()) # Tema para gráficos ggplot - Preto e Branco

# Base --------------------------------------------------------------------

rm(list=ls())

df <- read_rds("tabelas.rds") # Base dos óbitos já imputados
df2 <- readxl::read_xlsx("exposicao por grupo etario quinquenal com o Brasil.xlsx") #Base das medidas de exposição

# Organizando a base de óbitos já imputada ------------------------------------

df <- df %>%
  select(-cov_mat) %>%
  unnest(data)

df <- df %>%
  select(-Variância, -starts_with("imp"))
  
df = as.data.frame(df)

# Aplicando os fatores de correção de sub-registro baseado em Queiroz et al. (2017) --------------

fc = readxl::read_xlsx("arquivos_auxiliares/fatores_de_correcao_por_regiao_2010.xlsx")

df = cbind(df,fc[,5])

df = df %>%
  mutate(Média = Média * FC, LI = LI * FC, LS = LS * FC) %>%
  select(-FC)


# Acrescentando os óbitos do Brasil (soma das regiões) -------------------------

obitos_brasil = df %>%
  group_by(sexo, esc_inputada, gretarioq) %>%
  summarise_at(c("Média", "LI", "LS"), sum)
obitos_brasil = as.data.frame(obitos_brasil)


obitos_brasil = obitos_brasil %>%
  mutate(regiao = 'Brasil' ) %>%
  select(regiao, everything())

df = bind_rows(df, obitos_brasil) %>%
  arrange(regiao, sexo, esc_inputada, gretarioq)


# Organizando a base das medidas de exposição -----------------------------------------------

df3 = df2 %>% 
  select(tipo, Sexo, Região, Escolaridade, gretarioQ, Contagem) %>%
  mutate(Sexo = factor(Sexo, levels = c("Masculino", "Feminino"))) %>%
  group_by(tipo, Sexo, Região, Escolaridade, gretarioQ) %>%
  summarise(sum_cont = sum(Contagem)) %>%
  arrange(Região, Sexo, Escolaridade, gretarioQ) %>%
  filter(gretarioQ == "25 a 29 anos" | gretarioQ == "30 a 34 anos" | gretarioQ == "35 a 39 anos" | gretarioQ == "40 a 44 anos" | gretarioQ == "45 a 49 anos" | gretarioQ == "50 a 54 anos" | gretarioQ == "55 a 59 anos")

# Juntando as bases de óbito e medida de exposição ------------------------------------------

df4 = cbind(as.data.frame(df3),df[,5:7])
names(df4)[5:6] = c("Grupo etário", "Pop. exposta")
df4$Escolaridade = factor(df4$Escolaridade, levels = c('Baixa', 'Média', 'Alta'))

############################ Calculando nmx
df4 = df4 %>%
  mutate(mx_média = Média/`Pop. exposta`, mx_LI = LI/`Pop. exposta`, mx_LS = LS/`Pop. exposta`,
         log_mx_média = log(Média/`Pop. exposta`, 10), log_mx_LI = log(LI/`Pop. exposta`, 10), log_mx_LS = log(LS/`Pop. exposta`, 10))

############################ Alterando as categorias para o inglês

df5 <- df4 %>%
  mutate(Sex = factor(Sexo, levels = c("Masculino", "Feminino"), labels = c("Male", "Female")),
         Region = factor(Região, levels = c("Brasil", "Centro-oeste", "Nordeste", "Norte", "Sudeste", "Sul"), labels = c("Brazil", "Midwest", "Northeast", "North", "Southeast", "South")),
         `Level of education` = factor(Escolaridade, levels = c("Baixa", "Média", "Alta"), labels = c("Low", "Medium", "High")))
         

#############################Gráficos de nmx (Figura 1 do repositório)

# OBS.: Mudei os níveis do eixo x dos gráficos: Grupo etário ---> 1,2,3,4,5,6,7 para os gráficos ficarem mais limpos -----------------------------

######## Brasil e grandes regiões

figura1 = df5 %>%
  ggplot(aes(x = factor(df5$`Grupo etário`, levels = c("25 a 29 anos", "30 a 34 anos", "35 a 39 anos",
                                                       "40 a 44 anos", "45 a 49 anos", "50 a 54 anos",
                                                       "55 a 59 anos" ),
                        labels = c("25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59")),
             y = `log_mx_média`, color = `Level of education`)) + 
  geom_line(aes(group = `Level of education`, linetype = "Point estimate")) +
  geom_line(aes(y = log_mx_LI, group = `Level of education`, linetype = "Lower limit and Upper limit")) +
  geom_line(aes(y = log_mx_LS, group = `Level of education`, linetype = "Lower limit and Upper limit")) +
  facet_grid(Sex ~ Region) +
  theme(legend.position="bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y = "Log(mx)", x = "Age group ", color = "Level of education", linetype = "") +
  guides(linetype = guide_legend(order = 2), color = guide_legend(order = 1))

  ggsave(plot = figura1, "figuras/Figura 1.eps", width=7, height=4, dpi=300)

############# Tabelas de sobrevivência (apenas nqx e lx) ###################

# Função que calcula nqx e lx ---------------------------------

calcula_nqx_lx = function(nmx, n=5, a=2.5) {
 nqx = (n*nmx)/(1+(n-a)*nmx)
 npx = 1-nqx
 l0 = 1e+05
 lx = cumprod(c(l0, npx))
 lx = lx[-length(lx)]
 return(cbind(nqx,lx)
 )
}

# Automatizando a construção das funções de sobrevivência em cada recorte -------------------------

Regiao = unique(df4$Região)
Sexo = unique(df4$Sexo)
Escolaridade = unique(df4$Escolaridade)
df4$lx_LS = df4$lx_LI = df4$lx_média = df4$nqx_LS = df4$nqx_LI= df4$nqx_média = NA


for (regiao in Regiao) {
  for (sexo in Sexo) {
    for (escolaridade in Escolaridade) {
      for(coluna in 10:12) {
        df4[df4$Região == regiao & df4$Sexo == sexo & df4$Escolaridade == escolaridade, c(coluna+6,coluna+9)] =
          calcula_nqx_lx (nmx = df4[df4$Região == regiao & df4$Sexo == sexo & df4$Escolaridade == escolaridade, coluna])
      }
    }
  }
}

write.xlsx2(df4, 'tabelas/tabela de sobrevivência (com correção sub-registro).xlsx', sheetName="tab_sobrevivencia",
            col.names=TRUE, row.names = FALSE)


# Calculando a razão de risco (RR) em relação aos nmx ----------------------------------

RR = df4 %>%
  select(Sexo, Região, Escolaridade, `Grupo etário`, mx_média, mx_LI, mx_LS) %>%
  filter(Escolaridade != "Média")

RRbaixa = RR %>%
  filter(Escolaridade == "Baixa") %>%
  rename(escbaixa_mx_média = mx_média) %>%
  rename(escbaixa_mx_LI = mx_LI) %>%
  rename(escbaixa_mx_LS = mx_LS)

RRalta = RR %>%
  filter(Escolaridade == "Alta") %>%
  rename(escalta_mx_média = mx_média) %>%
  rename(escalta_mx_LI = mx_LI) %>%
  rename(escalta_mx_LS = mx_LS)

aux = cbind(RRbaixa[,-3], RRalta[,5:7])

RRfinal = aux %>%
  group_by(Sexo, Região, `Grupo etário`) %>%
  mutate(
    RR_média = escbaixa_mx_média /escalta_mx_média,
    RR_LI = escbaixa_mx_LI /escalta_mx_LI,
    RR_LS = escbaixa_mx_LS / escalta_mx_LS) %>%
  select(Sexo, Região, `Grupo etário`, RR_média, RR_LI, RR_LS)

write.xlsx2(as.data.frame(RRfinal), 'tabelas/tabela das RR_COM_CORR.xlsx', sheetName="RR",
            col.names=TRUE, row.names = FALSE)
            
# Gráficos RR com nmx -----------------------------------------------

############################ Alterando as categorias para o inglês

RRfinal <- RRfinal %>%
  mutate(Sex = factor(Sexo, levels = c("Masculino", "Feminino"), labels = c("Male", "Female")),
         Region = factor(Região, levels = c("Brasil", "Centro-oeste", "Nordeste", "Norte", "Sudeste", "Sul"), labels = c("Brazil", "Midwest", "Northeast", "North", "Southeast", "South")),
         )


# Automatizando o cálculo de 35q25 para cada recorte ----------------------------------

mort = df4[df4$`Grupo etário` == "25 a 29 anos",2:4]
mort$LS_35q25 = mort$LI_35q25 = mort$média_35q25 = NA

for (regiao in Regiao) {
  for (sexo in Sexo) {
    for (escolaridade in Escolaridade) {
      for(coluna in 19:21) {
        mort[mort$Região == regiao & mort$Sexo == sexo & mort$Escolaridade == escolaridade, coluna-15] =
          1-(df4[df4$Região == regiao & df4$Sexo == sexo & df4$Escolaridade == escolaridade & df4$`Grupo etário` == "55 a 59 anos", coluna]/
               df4[df4$Região == regiao & df4$Sexo == sexo & df4$Escolaridade == escolaridade & df4$`Grupo etário` == "25 a 29 anos", coluna])
      }
    }
  }
}

write.xlsx2(mort, '35q25_COM_CORR.xlsx', sheetName="35q25",
            col.names=TRUE, row.names = FALSE)

#######Gráficos 35q25 (Figura 2 do repositório)

############################ Alterando as categorias para o inglês

mort <- mort %>%
  mutate(Sex = factor(Sexo, levels = c("Masculino", "Feminino"), labels = c("Male", "Female")),
         Region = factor(Região, levels = c("Brasil", "Centro-oeste", "Nordeste", "Norte", "Sudeste", "Sul"), labels = c("Brazil", "Midwest", "Northeast", "North", "Southeast", "South")),
         `Level of education` = factor(Escolaridade, levels = c("Baixa", "Média", "Alta"), labels = c("Low", "Medium", "High")))



figura2 = ggplot(mort, aes(Sex, média_35q25, fill = `Level of education`)) +
  geom_col(position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = LI_35q25, ymax = LS_35q25), position = position_dodge(width=0.9), width = 0.25) +
  facet_grid(~ Region) +
  labs(y = "35q25") +
  # ggtitle("A1: Probabilidade de morte dos 25 aos 59 anos (35q25) por sexo, segundo as regiões, 2010. ") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="bottom") 

ggsave(plot = figura2, "figuras/Figura2.eps", width=6, height=4, dpi=300)

# Por IDADE (Figura 3 do repositório)

figura3 = ggplot(RRfinal, aes(x = factor(RRfinal$`Grupo etário`, levels = c("25 a 29 anos", "30 a 34 anos", "35 a 39 anos",
                                                              "40 a 44 anos", "45 a 49 anos", "50 a 54 anos",
                                                              "55 a 59 anos" ),
                                         labels = c("25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59")), y = RR_média)) +
  geom_col(position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = RR_LI, ymax = RR_LS), position = position_dodge(width=0.9), width = 0.25) +
  facet_grid(Sex ~ Region) +
  labs(x = "Age group", y = "Risc Ratio (RR)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  ggsave(plot = figura3, "figuras/Figura 3.eps", width=6, height=4, dpi=300)

# Por SEXO (Figura 4 do repositório)

figura4 = ggplot(RRfinal, aes(x = factor(RRfinal$`Grupo etário`, levels = c("25 a 29 anos", "30 a 34 anos", "35 a 39 anos",
                                                              "40 a 44 anos", "45 a 49 anos", "50 a 54 anos",
                                                              "55 a 59 anos" ),
                                         labels = c("25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59")), y = RR_média, fill = Sex)) +
  geom_col(position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = RR_LI, ymax = RR_LS), position = position_dodge(width=0.9), width = 0.25) +
  facet_grid(~ Region) +
  labs(x = "Age group", y = "Risk Ratio (RR)") +
  theme(legend.position="bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values = c("#3399FF", "#FF6666"))

  ggsave(plot = figura4, "figuras/Figura 4.eps", width=6, height=4, dpi=300)

# Por Região (Figura 4 do repositório)

figura5 = ggplot(RRfinal, aes(x = factor(RRfinal$`Grupo etário`, levels = c("25 a 29 anos", "30 a 34 anos", "35 a 39 anos",
                                                                  "40 a 44 anos", "45 a 49 anos", "50 a 54 anos",
                                                                  "55 a 59 anos" ),
                                         labels = c("25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59")), y = RR_média, fill = Region)) +
  geom_col(position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = RR_LI, ymax = RR_LS), position = position_dodge(width=0.9), width = 0.25) +
  facet_grid(~ Sex) +
  labs(x = "Age group", y = "Risc Ratio (RR)") +
  theme(legend.position="bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_brewer(palette="Paired")

  ggsave(plot = figura5, "figuras/Figura 5.eps", width=6, height=4, dpi=300)



# Calculando a razão de probabilidade (RP = 35q25(baixa) / 35q25(alta) ----------------------------------

RP = mort %>%
  select(Sexo, Região, Escolaridade, média_35q25, LI_35q25, LS_35q25) %>%
  filter(Escolaridade != "Média")

RPbaixa = RP %>%
  filter(Escolaridade == "Baixa") %>%
  rename(escbaixa_35q25_média = média_35q25) %>%
  rename(escbaixa_35q25_LI = LI_35q25) %>%
  rename(escbaixa_35q25_LS = LS_35q25)

RPalta = RP %>%
  filter(Escolaridade == "Alta") %>%
  rename(escalta_35q25_média = média_35q25) %>%
  rename(escalta_35q25_LI = LI_35q25) %>%
  rename(escalta_35q25_LS = LS_35q25)

aux = cbind(RPbaixa[,-3], RPalta[,4:6])

RPfinal = aux %>%
  group_by(Sexo, Região) %>%
  mutate(
    RP_média = escbaixa_35q25_média / escalta_35q25_média,
    RP_LI = escbaixa_35q25_LI /escalta_35q25_LI,
    RP_LS = escbaixa_35q25_LS / escalta_35q25_LS) %>%
  select(Sexo, Região, RP_média, RP_LI, RP_LS)

# Gráficos RP com 35q25 -----------------------------------------------

RPfinal <- RPfinal %>%
  mutate(Sex = factor(Sexo, levels = c("Masculino", "Feminino"), labels = c("Male", "Female")),
         Region = factor(Região, levels = c("Brasil", "Centro-oeste", "Nordeste", "Norte", "Sudeste", "Sul"), labels = c("Brazil", "Midwest", "Northeast", "North", "Southeast", "South"))
  )

# Por SEXO (Figura A1 do repositório)

A1 = ggplot(RPfinal, aes(Sex, RP_média)) +
  geom_col(position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = RP_LI, ymax = RP_LS), position = position_dodge(width=0.9), width = 0.25) +
  facet_grid(~ Region) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y = "Probability ratio (PR)")
  # ggtitle("A2: Razão de probabilidade (RP) por sexo, segundo as regiões, 2010.")

ggsave(plot = A1, "figuras/Figura A1.eps", width=6, height=4, dpi=300)

# Por região (Figura A3 do repositório)
A2 = ggplot(RPfinal, aes(Sex, RP_média, fill = Region)) +
  geom_col(position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = RP_LI, ymax = RP_LS), position = position_dodge(width=0.9), width = 0.25) +
  labs(y = "Probability ratio (PR)") +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette="Paired") 
  # ggtitle("A2: Razão de probabilidade (RP) por região, segundo o sexo, 2010.")

ggsave(plot = A2, "figuras/Figura A2.eps", width=6, height=4, dpi=300)


# # Figura 5 do repositório - Unindo A1, A2 e A3
# 
# library(patchwork)
# A =  A1 / A2 / A3
# ggsave(plot = A, "figuras/Figura 5.pdf", width=10, height=8, dpi=300)
# 
# 
# # Figuras do manuscrito - Sexo masculino e Brasil --------------------
# 
# #############################Gráficos de nmx (Figura 1 do manuscrito)
# 
# # OBS.: Mudei os níveis do eixo x dos gráficos: Grupo etário ---> 1,2,3,4,5,6,7 para os gráficos ficarem mais limpos -----------------------------
# 
# 
# df_figura1_manuscrito = df4 %>%
#   filter(Região == "Brasil", Sexo == "Masculino")
# 
#   figura1_manuscrito = df_figura1_manuscrito %>% 
#     ggplot(aes(x = factor(`Grupo etário`, levels = c("25 a 29 anos", "30 a 34 anos", "35 a 39 anos",
#                                                        "40 a 44 anos", "45 a 49 anos", "50 a 54 anos",
#                                                        "55 a 59 anos" ),
#                         labels = c("1", "2", "3", "4", "5", "6", "7")),
#              y = `log_mx_média`, color = Escolaridade)) + 
#   geom_line(aes(group = Escolaridade, linetype = "Estimativa pontual")) +
#   geom_line(aes(y = log_mx_LI, group = Escolaridade, linetype = "LI e LS")) +
#   geom_line(aes(y = log_mx_LS, group = Escolaridade, linetype = "LI e LS")) +
#   theme(legend.position="bottom") +
#   labs(y = "Log(TEM)", x = "Grupo etário", fill = "IC Escolaridade", color = "Escolaridade",linetype = "") +
#   guides(linetype = guide_legend(order = 2), color = guide_legend(order = 1))
# 
# ggsave(plot = figura1_manuscrito, "figuras/figura1_manuscrito.pdf", width=6, height=4, dpi=300)
# ggsave(plot = figura1_manuscrito, "figuras/figura1_manuscrito.png", width=6, height=4, dpi=300)
# 
# 
# #######Gráficos 35q25 (Figura 2 do manuscrito)
# 
# df_figura2_manuscrito = mort %>%
#   filter(Região == "Brasil")
# 
# figura2_manuscrito = ggplot(df_figura2_manuscrito, aes(Sexo, média_35q25, fill = Escolaridade)) +
#   geom_col(position = position_dodge(width=0.9)) +
#   geom_errorbar(aes(ymin = LI_35q25, ymax = LS_35q25), position = position_dodge(width=0.9), width = 0.25) +
#   labs(y = "35q25") +
#   theme(legend.position="bottom") 
# 
# ggsave(plot = figura2_manuscrito, "figuras/figura2_manuscrito.pdf", width=6, height=4, dpi=300)
# ggsave(plot = figura2_manuscrito, "figuras/figura2_manuscrito.png", width=6, height=4, dpi=300)
# 
# # Gráficos RR com nmx -----------------------------------------------
# 
# # Por IDADE (Figura 3 do manuscrito)
# 
# df_figura3_manuscrito = RRfinal %>%
#   filter(Região == "Brasil", Sexo == "Masculino")
# 
# figura3_manuscrito = ggplot(df_figura3_manuscrito, aes(x = factor(`Grupo etário`, levels = c("25 a 29 anos", "30 a 34 anos", "35 a 39 anos",
#                                                                             "40 a 44 anos", "45 a 49 anos", "50 a 54 anos",
#                                                                             "55 a 59 anos" ),
#                                          labels = c("1", "2", "3", "4", "5", "6", "7")), y = RR_média)) +
#   geom_col(position = position_dodge(width=0.9)) +
#   geom_errorbar(aes(ymin = RR_LI, ymax = RR_LS), position = position_dodge(width=0.9), width = 0.25) +
#   labs(x = "Grupo etário", y = "Razão de risco (RR)")
# 
# ggsave(plot = figura3_manuscrito, "figuras/figura3_manuscrito.pdf", width=6, height=4, dpi=300)
# ggsave(plot = figura3_manuscrito, "figuras/figura3_manuscrito.png", width=6, height=4, dpi=300)
# 
# # Por SEXO (Figura 4 do manuscrito)
# 
# df_figura4_manuscrito = RRfinal %>%
#   filter(Região == "Brasil")
# 
# figura4_manuscrito = ggplot(df_figura4_manuscrito, aes(x = factor(`Grupo etário`, levels = c("25 a 29 anos", "30 a 34 anos", "35 a 39 anos",
#                                                                             "40 a 44 anos", "45 a 49 anos", "50 a 54 anos",
#                                                                             "55 a 59 anos" ),
#                                          labels = c("1", "2", "3", "4", "5", "6", "7")), y = RR_média, fill = Sexo)) +
#   geom_col(position = position_dodge(width=0.9)) +
#   geom_errorbar(aes(ymin = RR_LI, ymax = RR_LS), position = position_dodge(width=0.9), width = 0.25) +
#   labs(x = "Grupo etário", y = "Razão de risco (RR)") +
#   theme(legend.position="bottom")
# 
# ggsave(plot = figura4_manuscrito, "figuras/figura4_manuscrito.pdf", width=6, height=4, dpi=300)
# ggsave(plot = figura4_manuscrito, "figuras/figura4_manuscrito.png", width=6, height=4, dpi=300)
# 
# 
# 
# # Por Região (Figura 5 do manuscrito)
# 
# df_figura5_manuscrito = RRfinal %>%
#   filter(Sexo == "Masculino")
# 
# figura5_manuscrito = ggplot(df_figura5_manuscrito, aes(x = factor(`Grupo etário`, levels = c("25 a 29 anos", "30 a 34 anos", "35 a 39 anos",
#                                                                             "40 a 44 anos", "45 a 49 anos", "50 a 54 anos",
#                                                                             "55 a 59 anos" ),
#                                          labels = c("1", "2", "3", "4", "5", "6", "7")), y = RR_média, fill = Região)) +
#   geom_col(position = position_dodge(width=0.9)) +
#   geom_errorbar(aes(ymin = RR_LI, ymax = RR_LS), position = position_dodge(width=0.9), width = 0.25) +
#   labs(x = "Grupo etário", y = "Razão de risco (RR)") +
#   theme(legend.position="bottom") +
#   scale_fill_brewer(palette="Paired")
# 
# ggsave(plot = figura5_manuscrito, "figuras/figura5_manuscrito.pdf", width=6, height=4, dpi=300)
# ggsave(plot = figura5_manuscrito, "figuras/figura5_manuscrito.png", width=6, height=4, dpi=300)



