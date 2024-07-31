#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#@ Author: GABRIEL CARVALHO CASEIRO (gabrielcaseiro99@gmail.com) @#
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#@@@@@ This code extracts political affiliates list from TSE @@@@@#
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#

library(tidyverse)
library(httr)

# 1. Get API ids ----

## 1.1. Municipalities ----

list_mun<-lapply(1:28, function(i){
  
  print(i)
  
  url<-paste0('https://filia2-consulta.tse.jus.br/filia-consulta/rest/v1/localidade/',i,'/municipios')
  
  resp<-GET(url)
    cont<-content(resp,as='parse')
    cont<-lapply(cont, function(x) lapply(rapply(x, enquote, how="unlist"), eval))
    cont<-do.call(bind_rows,cont)
    
  return(cont)  
        
})

list_mun<-do.call(bind_rows,list_mun)

saveRDS(list_mun,'output/list_mun.rds')

## 1.2. Electoral Zones ----

list_zon<-lapply(1:length(list_mun$codObjeto), function(i){
  
  print(i)
  
  url<-paste0('https://filia2-consulta.tse.jus.br/filia-consulta/rest/v1/zona/municipio/',list_mun$codObjeto[i],'/zonasEleitorais')
  
  resp<-GET(url)
  cont<-content(resp,as='parse')
  cont<-lapply(cont, function(x) lapply(rapply(x, enquote, how="unlist"), eval))
  cont<-do.call(bind_rows,cont)
  cont$codObjetoMun<-list_mun$codObjeto[i]
  
  return(cont)  
  
})

list_zon<-do.call(bind_rows,list_zon)
  
  # Create UF list:

  uf<-list_mun%>%
    transmute(codObjetoUf=as.character(uf.codUf),uf.sglUf)%>%
    distinct()%>%
    arrange(uf.sglUf)

list_zon<-list_zon%>%left_join(uf)

saveRDS(list_zon,'output/list_zon.rds')

## 1.3. Parties ----

url<-'https://filia2-consulta.tse.jus.br/filia-consulta/rest/v1/partidos'

  list_part<-GET(url)
  list_part<-content(list_part,as='parse')
  list_part<-do.call(bind_rows,list_part)
  
saveRDS(list_part,'output/list_part.rds')
  
# 2. Extract filiados data (by UF) ----
  

for (i in uf$uf.sglUf) {
  
  # Select all municipalities and electoral zones in a UF and cross join with all possible parties:
  
  list<-list_zon%>%
    filter(uf.sglUf==i)%>%
    cross_join(list_part%>%select(id))
  
  m<-nrow(list) # Number of mun-zone/party pairs 

fili<-lapply(1:m,function(s){  
  
print(paste0(i,' ',s,' of ',m))  
  
# Base API url:  
  
url_base<-paste0('https://filia2-consulta.tse.jus.br/filia-consulta/rest/v1/relacao-filiados?',
            'sgUe=',list$uf.sglUf[s],
            '&cdMunicipio=',list$codObjetoMun[s],
            '&cdZona=',list$codObjeto[s],
            '&sqPartido=',list$id[s])

inter<-0 # Object for pagination (fisrt page is 0)

# Adjust API url for pagination:

url<-paste0(url_base,
            '&currentPage=',inter,
            '&pageSize=',10000)

# Get response and content:

resp<-GET(url)
cont<-content(resp,as='parse')

# Get the total number of filiados in the mun-zone/party pair:

n<-cont$totalElements  
n<-ceiling(n/10000)-1

# Extract filiados list:

cont<-cont$entitys

# If the list is non-empty, ajust the parsed list as a data frame: 

  if(length(cont)>0){
    
  # Adjust date columns:  
  
  adj_list<-lapply(cont, function(x) lapply(x, is.list))
  cont<-lapply(1:length(cont),function(x){
    c=cont[[x]]
    c[unlist(adj_list[[x]])]=lapply(c[unlist(adj_list[[x]])],function(y) paste(unlist(y),collapse = ';'))
    c
  })  
  
  }

cont<-do.call(bind_rows,cont)

dt<-cont

# If the number of filiados exceed pagination limit, get and adjust the other pages:
  
  while (inter<n) {
    
    inter<-inter+1
    
    url<-paste0(url_base,
                '&currentPage=',inter,
                '&pageSize=',10000)
    
    
    resp<-GET(url)
    cont<-content(resp,as='parse')
    
    cont<-cont$entitys
    adj_list<-lapply(cont, function(x) lapply(x, is.list))
    cont<-lapply(1:length(cont),function(x){
        c=cont[[x]]
        c[unlist(adj_list[[x]])]=lapply(c[unlist(adj_list[[x]])],function(y) paste(unlist(y),collapse = ';'))
        c
    }) 
    
    cont<-do.call(bind_rows,cont)
    
    dt<-bind_rows(dt,cont)
    
  }

return(dt)
  
})

fili<-do.call(bind_rows,fili) # Consolidade single data.frame for UF

saveRDS(fili,paste0('output/list_filiados_',i,'.rds')) # Save data.frame

rm(list)
rm(fili)

}

rm(list = ls())
