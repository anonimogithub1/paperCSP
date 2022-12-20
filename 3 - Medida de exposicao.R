
rm(list=ls())
library(dplyr)
library(xlsx)
library(ggplot2)

#graphics.off()
#if (.Platform$OS.type == 'windows') windows(record=TRUE)

## construct survival functions l(x) for males and females, based on HMD standard
## the calculated multipliers won't be very sensitive to the standard chosen
## the key point is to account for non-survival at high ages: a 97.58-year old
## observed in the Census (t=2010.58) represents *more* than one expected person-year 
#  of exposure at age 96 over [2009.0, 2010.0), for example, because we only see the survivors.

mort = read.csv('arquivos_auxiliares/HMDstd.csv') %>%
  mutate( sf = exp(-exp(f)), sm = exp(-exp(m)),
          lf = c(1, head(cumprod(sf),-1)),
          lm = c(1, head(cumprod(sm),-1))
  )

lx.male   = approxfun( x=mort$age, y=mort$lm, yleft=1, yright=0)
lx.female = approxfun( x=mort$age, y=mort$lf, yleft=1, yright=0)

## calculate the person-years of exposure at ages [A,A+h) x times [0,bigT]
##   per exactly x-year-old at census date bigC 
## if 2009.0 is t=0, 1 Aug 2010 Census was at t=1.58

PY = function(x, A , h=1, bigC=1.58, bigT=3, this.sex='m', dt=.05) {
  tA = bigC - x + A   # time of A-th birthday
  tgrid = seq(dt/2, bigT-dt/2, dt) # grid of times at which exposure is possible
  
  this.lx <- lx.male
  if (this.sex == 'f') this.lx <- lx.female
  
  # per x year old at time bigC, how many p-y of exposure in ages [A,A+n) x times [0,T)
  sum( (tgrid >= tA) * (tgrid < (tA+h)) * this.lx(x-bigC+tgrid) * dt) / this.lx(x)
}


## build a (long) data frame with all combinations of exact census ages (in 1/5ths of a year)
##  and one-year age groups. The expos variables will contain the expected person-years
##  lived at ages [A,A+1) over years [0,3], per x-year-old at the Census date of 1.58 

census.age  = seq(-1.9, 99.9,.20)   # possible ages at time C 
age.group   = 0:99

df = expand.grid( x=census.age, A=age.group)

for (i in 1:nrow(df)) {
  df[i,'m.expos'] = PY(x=df$x[i], A=df$A[i], this.sex='m')
  df[i,'f.expos'] = PY(x=df$x[i], A=df$A[i], this.sex='f')
}  

## construct a multiplier matrix to convert Census populations by single years of age
## to period exposure.  Aggregate exact census ages into integer age groups and then\
## calculate the average exposure. For example
##   Em [100x100] %*% male.census.pop [100x1] = male period exposure [100x1]

tmp = df %>% 
  mutate(intx = floor(x)) %>% 
  group_by(intx,A) %>% 
  summarize(f.expos=mean(f.expos), m.expos=mean(m.expos))

Em  = matrix(round(tmp$m.expos,2), nrow=100, 
             dimnames=list(paste0('A',0:99),paste0('x',-2:99)))

Ef  = matrix(round(tmp$f.expos,2), nrow=100, 
             dimnames=list(paste0('A',0:99),paste0('x',-2:99) ))

## collapse the first 3 columns (floor(x) = -2, -1, 0) by summing. This is equivalent to assuming
##  that the cohorts born over the two years after the Census will be identical in size to current 0-yr-olds

W = rbind( cbind(1, matrix(0, 2, 99)) , 
           diag(100))

Em = Em %*% W
Ef = Ef %*% W




####################################################################################################################
############ Automatizando o cálculo da medida de exposição por idade segundo escolaridade, sexo e região ##########
####################################################################################################################

#df é a base de dados original das contagens populacionais do Censo 2010 por escolaridade, região, sexo e idade simples (o metodo exige idade simples);
#df2 é a base de dados resultante do cálculo da medida de exposição por escolaridade, região, sexo e idade simples;
#df_grupo_etario é a base de dados resultante do cálculo da medida de exposição por grupo quinquenal segundo escolaridade, região e sexo;
#df3 é o emplilhamento de df e df2 usado para fazer os gráficos de comparação da população x medida de exposição;
#df4 é um filtro das idade de 25 a 59 anos em df3 (se quiser os gráficos apenas para os adutos);

#######################Leitura da base extraída dos microdados do censo 2010###########################
df <- readxl::read_excel("arquivos_auxiliares/pop_escolaridade_censo2010.xlsX")
df <- as.data.frame(df)

##################### Acrescentando o Brasil na base (soma das regiões) ################################

brasil = df %>%
  group_by(Sexo, Escolaridade, Idade) %>%
  summarise(Contagem=sum(Contagem))
  
brasil = as.data.frame(brasil)

brasil = brasil %>%
  mutate(Região = 'Brasil' ) %>%
  select(Sexo, Região, everything())

df = bind_rows(df, brasil)

######################## Automatizando o cálculo da medida de exposição #########################

esc = unique(df$Escolaridade)
regiao = unique(df$Região)
sex = unique(df$Sexo)
df2 = df
df2[,5] = 0



for (Esc in esc) {
  for (Sex in sex) {
    for (Regiao in regiao) {
      if (Sex == 'Masculino') {
        df2[df2$Sexo == Sex & df2$Região == Regiao & df2$Escolaridade == Esc, 'Contagem'] = (Em %*% df[df$Sexo == Sex & df$Região == Regiao & df$Escolaridade == Esc, 'Contagem'])/3
      } else {
        df2[df2$Sexo == Sex & df2$Região == Regiao & df2$Escolaridade == Esc, 'Contagem'] = (Ef %*% df[df$Sexo == Sex & df$Região == Regiao & df$Escolaridade == Esc, 'Contagem'])/3
      }
    }
  }
}

df = df %>%
  mutate(tipo = 'original' ) %>%
  select(tipo,everything())

df2 = df2 %>%
  mutate(tipo = 'exposicao' ) %>%
  select(tipo,everything()) %>%
  arrange(Região, Escolaridade)

####### Convertendo em idade quinquenal

criar_gretarioQ <- function(x) {
  # gretarioQ: Idade em grupos etários quinquenais, exceto nas primeiras idades que fica de 0 a 1
  # (exclusive) e 1 a 5 (exclusive) a partir da variável “idade”.
  case_when(
    x < 1 ~ "0 a 1 ano",
    x >= 1 & x < 5 ~ "1 a 4 anos",
    x >= 5 & x < 10 ~ "5 a 9 anos",
    x >= 10 & x < 15 ~ "10 a 14 anos",
    x >= 15 & x < 20 ~ "15 a 19 anos",
    x >= 20 & x < 25 ~ "20 a 24 anos",
    x >= 25 & x < 30 ~ "25 a 29 anos",
    x >= 30 & x < 35 ~ "30 a 34 anos",
    x >= 35 & x < 40 ~ "35 a 39 anos",
    x >= 40 & x < 45 ~ "40 a 44 anos",
    x >= 45 & x < 50 ~ "45 a 49 anos",
    x >= 50 & x < 55 ~ "50 a 54 anos",
    x >= 55 & x < 60 ~ "55 a 59 anos",
    x >= 60 & x < 65 ~ "60 a 64 anos",
    x >= 65 & x < 70 ~ "65 a 69 anos",
    x >= 70 & x < 75 ~ "70 a 74 anos",
    x >= 75 & x < 80 ~ "75 a 79 anos",
    x >= 80  ~ "80 a 99 anos", ## Essa função veio até 99 anos porque o método de cálculo da medida de exposição exige.
    TRUE ~ NA_character_
  )
}

df_grupo_etario = df2 %>%
  mutate(gretarioQ = criar_gretarioQ(Idade)) %>%
  select(tipo, Região, Sexo, Escolaridade, Idade, gretarioQ, Contagem) %>%
  arrange(Região, Escolaridade)

write.xlsx2(df_grupo_etario, 'exposicao por grupo etario quinquenal com o Brasil.xlsx', sheetName="Exposicao",
            col.names=TRUE, row.names = FALSE)

# Filtrando os grupos quinquenais adultos

# Na população original
df2_grupo_etario = df %>%
  mutate(gretarioQ = criar_gretarioQ(Idade)) %>%
  select(tipo, Região, Sexo, Escolaridade, Idade, gretarioQ, Contagem) %>%
  arrange(Região, Escolaridade)

gr_et_adulto_pop = df2_grupo_etario %>%
  filter(Idade>=25 & Idade<=59) %>%
  group_by(tipo, Região, Sexo, Escolaridade, gretarioQ) %>%
  summarise(sum_cont = sum(Contagem)) %>%
  arrange(tipo, Região, Sexo, Escolaridade, gretarioQ)
gr_et_adulto_pop = as.data.frame(gr_et_adulto_pop)

#Na medida de exposição
gr_et_adulto_expo = df_grupo_etario %>%
  filter(Idade>=25 & Idade<=59) %>%
  group_by(tipo, Região, Sexo, Escolaridade, gretarioQ) %>%
  summarise(sum_cont = sum(Contagem)) %>%
  arrange(tipo, Região, Sexo, Escolaridade, gretarioQ)
gr_et_adulto_expo = as.data.frame(gr_et_adulto_expo)

################# Juntando população e exposição por grupo etário quinquenal adulto --------

gr_et_adulto_final = rbind(gr_et_adulto_pop, gr_et_adulto_expo)


