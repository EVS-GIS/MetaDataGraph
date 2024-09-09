fdate=function(chain){
  result=rep(NA,length(chain))
  for (i in 1:length(chain)){
    result[i]=strsplit(as.vector(chain[i]),"T")[[1]][1]
  }
  return(result)
}

NAise=function(x){
  if(length(x)==0)return(NA) else return(x)
}

get_xml=function(xmlpath){
  file=paste0(xmlpath,
              "/metadata/metadata.xml")
  myxml=read_xml(file,encoding="iso-8889-1")
  return(myxml)
}

get_info=function(myxml){
  ID_fiche=xml_text(xml_find_first(myxml,".//gco:CharacterString"))
  
  debut=xml_text(xml_find_first(myxml,
                                ".//gml:beginPosition")) %>% 
    fdate() %>% lubridate::ymd()
  fin=xml_text(xml_find_first(myxml,
                              ".//gml:endPosition")) %>% 
    fdate() %>% lubridate::ymd()
  xmin=xml_text(xml_find_first(myxml,
                               ".//gmd:westBoundLongitude")) %>% 
    as.numeric()
  
  xmax=xml_text(xml_find_first(myxml,
                               ".//gmd:eastBoundLongitude")) %>% 
    as.numeric()
  
  ymin=xml_text(xml_find_first(myxml,
                               ".//gmd:southBoundLatitude")) %>% 
    as.numeric()
  
  ymax=xml_text(xml_find_first(myxml,
                               ".//gmd:northBoundLatitude")) %>% 
    as.numeric()
  nOSR=xml_text(xml_find_first(myxml,
                              ".//gmd:supplementalInformation")) %>% 
    stringr::str_extract_all("OSR.") %>% 
    unlist() %>% 
    length()
  links=xml_find_all(myxml,
                                  ".//gmd:CI_OnlineResource") %>% 
    xml_find_all(".//gmd:URL") %>% 
    xml_text() %>% 
    unlist()
  nlinks=length(links)
  ntables=links %>% 
    stringr::str_detect("(//.xls)|(//.txt)|(//.csv)$") %>%
    which() %>% 
    length()
  info=data.frame(ID_fiche,debut,fin,xmin,xmax,ymin,ymax,nOSR,nlinks,ntables)
  return(info)
}
get_auteurs=function(myxml){
  
  ID_fiche=xml_text(xml_find_first(myxml,".//gco:CharacterString"))
  Ressource=xml_find_all(myxml, ".//gmd:pointOfContact")
  R_personnes=xml_text(xml_find_all(Ressource,".//gmd:individualName")) %>% NAise()
  R_orgs=xml_text(xml_find_all(Ressource,".//gmd:organisationName")) %>% NAise()
  auteurs=data.frame(ID_fiche,R_personnes,R_orgs)
  return(auteurs)
}

get_keywords=function(myxml){
  
  ID_fiche=xml_text(xml_find_first(myxml,".//gco:CharacterString"))
  all_keywords=xml_find_all(myxml,".//gmd:MD_Keywords")
  keywords=c()
  types=c()
  for (k in 1:length(all_keywords)){
    tkeywords=xml_text(xml_find_all(all_keywords[k],".//gmd:keyword"))
    ttypes=rep("FREE",length(tkeywords))
    if(length(xml_find_all(all_keywords[k],".//gmd:thesaurusName"))>0){
      ttypes=rep("INSPIRE",length(tkeywords))
    }
    
    keywords=c(keywords,tkeywords)
    types=c(types,ttypes)
  }
  iso_keywords=xml_text(xml_find_all(myxml,".//gmd:MD_TopicCategoryCode"))
  keywords=c(keywords,iso_keywords)
  types=c(types,rep("ISO",length(iso_keywords)))
  #####
  dat_keywords=data.frame(ID_fiche,keywords,types)
  return(dat_keywords)
}

get_metadata=function(metadata_dir){
    fiches=list.files(metadata_dir)
    fiches=paste0(metadata_dir,"/",fiches)
    all_metadata=c()
    for (i in 1:length(fiches)){
      myxml=get_xml(fiches[i])
      info=get_info(myxml)
      auteurs=get_auteurs(myxml)
      keywords=get_keywords(myxml)

      dat_tmp=merge(info,auteurs,by="ID_fiche",all=T)
      dat_tmp=merge(dat_tmp,keywords,by="ID_fiche",all=T)
      all_metadata=rbind(all_metadata,dat_tmp)
    }
     return(all_metadata)
}

get_categories=function(metadata_dir){
  files=list.files(metadata_dir)
  dat_categories=c()
  for(i in 1:length(files)){
    file=paste0(metadata_dir,"/",files[i],"/",
                "info.xml")
    myxml=read_xml(file,encoding="iso-8889-1")
    categories=xml_find_all(myxml,".//category") %>% 
                  xml_attr("name")
    result=data.frame(ID_fiche=rep(files[i],length(categories)),
                      categories=categories)
    dat_categories=bind_rows(dat_categories,result)
  }
  return(dat_categories)
}
