vehicle_loader = function(){
  library(readxl)
  Ruteprogram_Materiel = read_excel("Ruteprogram_Materiel.xls", 
                                    skip = 1)
  names(Ruteprogram_Materiel) = c('ID',
                                  'vehicle',
                                  'spreader',
                                  'spreadertype',
                                  'spreadwidth',
                                  'capacity_m3',
                                  'lage',
                                  'routetype',
                                  'vinterclass',
                                  'note')
  Ruteprogram_Materiel = Ruteprogram_Materiel %>% 
    mutate(ID = 1:nrow(Ruteprogram_Materiel)) %>% 
    select(-c(spreader, spreadertype, lage, note))
  return(Ruteprogram_Materiel)
}
