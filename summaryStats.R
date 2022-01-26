out<-lapply(tsaBnds, function(x){ 
  st_drop_geometry(fetaPolySf[fetaTSA[fetaTSA$tsa %in% x,]$fid,]) %>% 
  summarise(Total =n(), abund=sum(abund), n_fish= sum(n_fish), thlb = sum(thlb), ogma= sum(ogma), dcOgma = sum(dc_ogma), deferral = sum(defer), dcDeferral = sum(dc_defer)) %>%
  mutate(abund=as.integer(abund), n_fish = as.integer(n_fish))
})


out2<-lapply(tsaBnds, function(x){ 
  cbind(st_drop_geometry(fetaPolySf[fetaTSA[fetaTSA$tsa %in% x,]$fid,]) %>% filter(ogma > 0) %>% summarise(nFetaOgma =n()),
        st_drop_geometry(fetaPolySf[fetaTSA[fetaTSA$tsa %in% x,]$fid,]) %>% filter(dc_ogma > 0) %>% summarise(nFetaDCOGMA =n()),
        st_drop_geometry(fetaPolySf[fetaTSA[fetaTSA$tsa %in% x,]$fid,]) %>% filter(defer > 0) %>% summarise(nFetaDefer =n()),
        st_drop_geometry(fetaPolySf[fetaTSA[fetaTSA$tsa %in% x,]$fid,]) %>% filter(dc_defer > 0) %>% summarise(nFetaDCDefer =n())
  )
})


out3<-lapply(1:length(tsaBnds), function(x){ 
  cbind(tsaBnds[[x]], out[[x]], out2[[x]])
})

out4<-rbindlist(out3)

write.csv(out4, "sumamryStats.csv")