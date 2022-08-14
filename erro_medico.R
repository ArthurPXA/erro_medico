library (tjsp)
library (tidyverse)

baixar_cjsg(
  livre = "",
  aspas = FALSE,
  classe = "198",
  assunto = "10434, 10440, 9995",
  orgao_julgador = "",
  inicio = "",
  fim = "",
  inicio_pb = "",
  fim_pb = "",
  tipo = "A",
  n = NULL,
  diretorio = "data-raw/cjsg"
)

autenticar()

cjsg <- tjsp_ler_cjsg(diretorio = "data-raw/cjsg")

tjsp_baixar_cposg(cjsg$processo,diretorio = "data-raw/cposg")

arquivos <- list.files("data-raw/cposg",full.names = T)

cposg <- ler_dados_cposg(arquivos)

partes <- tjsp_ler_partes(arquivos)                      

movimentacao <- ler_movimentacao_cposg(arquivos)

dispositivo <- tjsp_ler_dispositivo(arquivos)

cposg <- cposg |>
  distinct(processo, assunto, classe, .keep_all = T)

cjsg <- cjsg |>
  distinct(processo, .keep_all = T)

dispositivo$decisao <- tjsp_classificar_recurso(dispositivo$dispositivo)

dispositivo|>
  count(decisao)

apelante <- partes|>
  filter(tipo_parte == "Apelante:")

apelado <- partes|>
  filter(tipo_parte == "Apelado:")

apelante <- apelante|>
  filter(classe_parte != "pessoa fisica")

apelado <- apelado|>
  filter(classe_parte != "pessoa fisica")

apelado <- apelado|>
  mutate(classe_parte = case_when(
    str_detect(parte, "(?i)beneficente") ~ "associação",
    str_detect(parte, "(?i)sant. casa") ~ "santa casa",
    str_detect(parte, "(?i)sanrta casa") ~ "santa casa",   
    str_detect(parte, "(?i)hosp") ~ "hospital",
    str_detect(parte, "(?i)funda..o") ~ "associação",
    str_detect(parte, "(?i)servi.o.social") ~ "associação",
    str_detect(parte, "(?i)estado") ~ "estado",
    str_detect(parte, "(?i)prefeit") ~ "estado",
    str_detect(parte, "(?i)munic.p") ~ "estado",
    str_detect(parte, "(?i)associa") ~ "associação",
    str_detect(parte, "(?i)unive") ~ "universidade",
    str_detect(parte, "(?i)especializad") ~ "hospital",
    str_detect(parte, "(?i)irmandade") ~ "associação",
    str_detect(parte, "(?i)segur") ~ "seguradora",
    str_detect(parte, "(?i)m.dic.") ~ "hospital",
    str_detect(parte, "(?i)s[áa][úu]de") ~ "hospital",
    str_detect(parte, "(?i)sociedade") ~ "associação",
    str_detect(parte, "(?i)pronto.socorro") ~ "hospital",
    str_detect(parte, "(?i)laborat.ori") ~ "hospital",
    str_detect(parte, "(?i)unidade") ~ "hospital",
    str_detect(parte, "(?i)ju.z") ~ "juiz",
    str_detect(parte, "(?i)organiza..o") ~ "hospital",
    str_detect(parte, "(?i)companhia") ~ "hospital",
    str_detect(parte, "(?i)droga") ~ "farmácia",
    str_detect(parte, "(?i)farm.c") ~ "farmácia",
    str_detect(parte, "(?i)maternidade") ~ "hospital",
    str_detect(parte, "(?i)distribuidora") ~ "hospital",
    str_detect(parte, "(?i)diagn") ~ "hospital",
    str_detect(parte, "(?i)tratamento") ~ "hospital",
    str_detect(parte, "(?i)governo") ~ "estado",
    str_detect(parte, "(?i)estudo") ~ "universidade",
    str_detect(parte, "(?i)pesquisa") ~ "universidade",
    str_detect(parte, "(?i)laborat.rio") ~ "hospital",
    str_detect(parte, "(?i)cl.n.ca") ~ "hospital",
    str_detect(parte, "(?i)institu") ~ "associação",
    str_detect(parte, "(?i)servi[çco]") ~ "hospital",
    str_detect(parte, "(?i)centro") ~ "hospital",
    str_detect(parte, "(?i)odonto") ~ "hospital",
    str_detect(parte, "(?i)ind.stria") ~ "hospital",
    str_detect(parte, "(?i)anatomia") ~ "hospital",
    str_detect(parte, "(?i)unimed") ~ "hospital",
    str_detect(parte, "(?i)federal") ~ "federal",
    str_detect(parte, "(?i)ubs") ~ "hospital",
    str_detect(parte, "(?i)câmara") ~ "juiz",
    str_detect(parte, "(?i)cruz.azul") ~ "hospital",
    str_detect(parte, "(?i)rede") ~ "hospital",
    str_detect(parte, "(?i)aneste") ~ "hospital",
    str_detect(parte, "(?i)união") ~ "associação",
    str_detect(parte, "(?i)repouso") ~ "hospital",
    str_detect(parte, "(?i)ltda") ~ "hospital",
    str_detect(parte, "(?i)participa..es") ~ "hospital",
    str_detect(parte, "(?i)grupo") ~ "associação",
    str_detect(parte, "(?i)olhos") ~ "hospital",   
    str_detect(parte, "(?i)banco") ~ "seguradora",
    str_detect(parte, "(?i)exclusivos") ~ "hospital",
    str_detect(parte, "(?i)Ministério Público") ~ "MP",  
    str_detect(parte, "(?i)asilo") ~ "asilo",
    str_detect(parte, "(?i)Beneficência") ~ "associação",  
    str_detect(parte, "(?i)massa falida") ~ "hospital",
    str_detect(parte, "(?i)sindicato") ~ "sindicato",
    str_detect(parte, "(?i)maternal") ~ "hospital", 
    str_detect(parte, "(?i)dental care") ~ "hospital", 
    str_detect(parte, "(?i)ação social") ~ "associação",
    str_detect(parte, "(?i)pl.stica") ~ "hospital", 
    str_detect(parte, "(?i)fleury") ~ "farmácia",
    TRUE ~ "pessoa fisica"
  ))


apelante <- apelante|>
  mutate(classe_parte = case_when(
    str_detect(parte, "(?i)beneficente") ~ "associação",
    str_detect(parte, "(?i)sant. casa") ~ "santa casa",
    str_detect(parte, "(?i)sanrta casa") ~ "santa casa",   
    str_detect(parte, "(?i)hosp") ~ "hospital",
    str_detect(parte, "(?i)funda..o") ~ "associação",
    str_detect(parte, "(?i)servi.o.social") ~ "associação",
    str_detect(parte, "(?i)estado") ~ "estado",
    str_detect(parte, "(?i)prefeit") ~ "estado",
    str_detect(parte, "(?i)munic.p") ~ "estado",
    str_detect(parte, "(?i)associa") ~ "associação",
    str_detect(parte, "(?i)unive") ~ "universidade",
    str_detect(parte, "(?i)especializad") ~ "hospital",
    str_detect(parte, "(?i)irmandade") ~ "associação",
    str_detect(parte, "(?i)segur") ~ "seguradora",
    str_detect(parte, "(?i)m.dic.") ~ "hospital",
    str_detect(parte, "(?i)s[áa][úu]de") ~ "hospital",
    str_detect(parte, "(?i)sociedade") ~ "associação",
    str_detect(parte, "(?i)pronto.socorro") ~ "hospital",
    str_detect(parte, "(?i)laborat.ori") ~ "hospital",
    str_detect(parte, "(?i)unidade") ~ "hospital",
    str_detect(parte, "(?i)ju.z") ~ "juiz",
    str_detect(parte, "(?i)organiza..o") ~ "hospital",
    str_detect(parte, "(?i)companhia") ~ "hospital",
    str_detect(parte, "(?i)droga") ~ "farmácia",
    str_detect(parte, "(?i)farm.c") ~ "farmácia",
    str_detect(parte, "(?i)maternidade") ~ "hospital",
    str_detect(parte, "(?i)distribuidora") ~ "hospital",
    str_detect(parte, "(?i)diagn") ~ "hospital",
    str_detect(parte, "(?i)tratamento") ~ "hospital",
    str_detect(parte, "(?i)governo") ~ "estado",
    str_detect(parte, "(?i)estudo") ~ "universidade",
    str_detect(parte, "(?i)pesquisa") ~ "universidade",
    str_detect(parte, "(?i)laborat.rio") ~ "hospital",
    str_detect(parte, "(?i)cl.n.ca") ~ "hospital",
    str_detect(parte, "(?i)institu") ~ "associação",
    str_detect(parte, "(?i)servi[çco]") ~ "hospital",
    str_detect(parte, "(?i)centro") ~ "hospital",
    str_detect(parte, "(?i)odonto") ~ "hospital",
    str_detect(parte, "(?i)ind.stria") ~ "hospital",
    str_detect(parte, "(?i)anatomia") ~ "hospital",
    str_detect(parte, "(?i)unimed") ~ "hospital",
    str_detect(parte, "(?i)federal") ~ "federal",
    str_detect(parte, "(?i)ubs") ~ "hospital",
    str_detect(parte, "(?i)câmara") ~ "juiz",
    str_detect(parte, "(?i)cruz.azul") ~ "hospital",
    str_detect(parte, "(?i)rede") ~ "hospital",
    str_detect(parte, "(?i)aneste") ~ "hospital",
    str_detect(parte, "(?i)união") ~ "associação",
    str_detect(parte, "(?i)repouso") ~ "hospital",
    str_detect(parte, "(?i)ltda") ~ "hospital",
    str_detect(parte, "(?i)participa..es") ~ "hospital",
    str_detect(parte, "(?i)grupo") ~ "associação",
    str_detect(parte, "(?i)olhos") ~ "hospital",   
    str_detect(parte, "(?i)banco") ~ "seguradora",
    str_detect(parte, "(?i)exclusivos") ~ "hospital",
    str_detect(parte, "(?i)Ministério Público") ~ "MP",  
    str_detect(parte, "(?i)asilo") ~ "asilo",
    str_detect(parte, "(?i)Beneficência") ~ "associação",  
    str_detect(parte, "(?i)massa falida") ~ "hospital",
    str_detect(parte, "(?i)sindicato") ~ "sindicato",
    str_detect(parte, "(?i)maternal") ~ "hospital", 
    str_detect(parte, "(?i)dental care") ~ "hospital", 
    str_detect(parte, "(?i)ação social") ~ "associação",
    str_detect(parte, "(?i)pl.stica") ~ "hospital", 
    str_detect(parte, "(?i)fleury") ~ "farmácia",
    TRUE ~ "pessoa fisica"
  ))

analise <- merge(cposg , dispositivo,
                 by = "processo",
                 all.x = TRUE)

analise$cd_processo = NULL
analise$v1 = NULL
analise$area = NULL
analise$situacao = NULL
analise$digital = NULL
analise$assunto = NULL
analise$classe = NULL
analise$distribuicao = NULL
analise$origem = NULL
analise$outros_numeros = NULL
analise$revisor = NULL
analise$ultima_carga = NULL
analise$volume_apenso = NULL

analise <- analise|>
  filter(dispositivo != is.na(TRUE))

analise <- analise|>
  distinct(processo, .keep_all = T)  

apelante <- apelante|>
  distinct(processo, .keep_all = T)  

apelado <- apelado|>
  distinct(processo, .keep_all = T) 


analise <- analise|>
  filter(decisao == "improvido"|decisao == "provido"|decisao == "parcial"|decisao == "não conhecido")


analise <- analise|>
  mutate(favoravel = ifelse(decisao == "parcial"|decisao == "provido", "sim", "nao"))

analise_apelante <- merge(analise, apelante,
                          by = "processo",
                          all.x = TRUE)

analise_apelante <- analise_apelante|>
  filter(tipo_parte != is.na(TRUE))


analise_apelado <- merge(analise, apelado,
                          by = "processo",
                          all.x = TRUE)

analise_apelado <- analise_apelado|>
  filter(tipo_parte != is.na(TRUE))




