server <- function(input, output,session) {
  isolate(
  observeEvent(input$Calculate,{
  periodos <- input$periodos
  periodo <- input$periodo_actual
  indice <- input$index
  adress <- input$adress
  Server <- input$Server
  Table <- input$Table
  User <- input$User
  Password <- input$Password
  Server_1 <- input$Server_1
  Table_1 <- input$Table_1
  User_1 <- input$User_1
  Password_1 <- input$Password_1
  
  
  basket <- read.table(input$Basket$datapath,
                       header = TRUE,
                       sep = "\t",
                       quote = "\"",
                       skipNul = TRUE)
  
  constant <- read.table(input$Constant$datapath,
                         header = TRUE,
                         sep = "\t",
                         quote = "\"",
                         skipNul = TRUE)
  
  ibds <- read.table(input$IBD$datapath,
                     header = TRUE,
                     sep = "\t",
                     quote = "\"",
                     skipNul = TRUE)
  
  outliers <- read.table(input$Outlier$datapath,
                        header = TRUE,
                        sep = "\t",
                        quote = "\"",
                        skipNul = TRUE)
  
  # Conection for SMS and SALES Information
  #SMSH<-odbcDriverConnect(paste("driver={SQL Server};server=",Server_1,";database=",Table_1,"; uid=",User_1,"; pwd=",Password_1))
  SMSH<-odbcDriverConnect('driver={SQL Server};server=acn056stgsql22;database=smshistory; uid=srv_proc; pwd=procesador')
  VEND<-odbcDriverConnect('driver={SQL Server};server=ACNDQC;database=Nrim; uid=srv_proc; pwd=procesador')
  
  # Information from SMS
  lojas_ch <- sqlQuery(SMSH,paste("SELECT source_id, status_id, cell_id, tipo_de_tienda FROM index_period_source WHERE period_id= ",periodo," and index_id=",indice))
  lojas_ch <- mutate(lojas_ch, duplicados = duplicated(lojas_ch))
  lojas_ch <- filter(lojas_ch, duplicados %in% F) # eliminando duplicados
  
  #Information of SALES for CHILE
  vendas_ch <- sqlQuery(VEND,paste('SELECT index_id, pc_id, period_id, pc_sales, shop_id, celula FROM rtpcshop WHERE period_id=',periodo,' and index_id=',indice))
  vendas_ch <- mutate(vendas_ch, duplicados = duplicated(vendas_ch))
  vendas_ch <- filter(vendas_ch, duplicados %in% F) # eliminando cuplicados
  
  # MERGE de information
  names(vendas_ch) <- c("index_id","pc_id","period_id","pc_sales","source_id","celula","duplicados") # cambiamos source_id para poder hace merge
  info_lojas <- merge(x=lojas_ch, y=vendas_ch, by=c('source_id'), all.x=T)
  head(info_lojas) # Revisamos consistencia en las columnas (quitar al final)
  summary(info_lojas) # Revisamos consistencia en los datos (quitar al final)
  info_lojas_vf <- filter(info_lojas, index_id != "NA") # eliminamos los NA
  summary(info_lojas_vf) # Revisamos consistencia en los datos *quitar al final*
  
  # Period information to check the stores POR LO PRONTO ESA INFORMACION DEBERA SER PROPORCIONADA MANUALMENTE
  period_con1 <- 2016015
  period_con2 <- 2016014
  #period_con3 <- 2016013#
  #period_con4 <- 2015024 # debemos acortar el conteo de periodos hastala ultima corrida de VUE
  periods_ch <- sqlQuery(SMSH,paste("SELECT source_id, status_id, period_id FROM index_period_source WHERE (period_id=(",period_con1,") OR period_id=(",period_con2,")) and index_id=",indice," and status_id in (6,7,8,9)"))
  table(periods_ch$period_id)
  
  # COUNT OF LOJAS BY PERIOD
  conteo <- table(periods_ch$source_id)
  df.conteo <- as.data.frame(conteo)
  names(df.conteo) <- c("store_id","cuenta")
  
  #LAS COLUMNAS DEL ARCHIVO ventas.z <- "index_id","period_id","ventas","store_id","category_code","cell_id","prod_store_status"
  ventas.z0 <- data.frame(info_lojas_vf$index_id,info_lojas_vf$period_id,info_lojas_vf$pc_sales,info_lojas_vf$source_id,info_lojas_vf$pc_id,info_lojas_vf$cell_id,info_lojas_vf$status_id)
  names(ventas.z0) <- c("index_id","period_id","ventas","store_id","category_code","cell_id","status_id")
  ventas.z <- merge(x=ventas.z0, y=df.conteo, by=c("store_id"), all.x=T)
  #head(ventas.z)
  #write.csv(ventas.z,"G:/My Drive/VUE repro INCO ene-feb 2018/modulo_de_tiendas/ventas_fabrica.csv")
  ventas.z <- mutate(ventas.z,prod_store_status=rep(F,length(ventas.z$store_id)))
  names(ventas.z) # are the columns names ok? 
  summary(ventas.z) # have we NA's ? yes!
  na.s <- filter(ventas.z, cuenta %in% NA) %>% select(store_id,status_id)
  table(na.s$status_id) # are the inactive stores
  
  # colocamos cuenta 0 a las tiendas inactivas, de cualquier manera no las vamos a usar
  for (i in 1:length(ventas.z$store_id)) {
    if(ventas.z$cuenta[i] %in% NA) {
      ventas.z$cuenta[i] <- 0
    }
  }
  # dejamos el formato de activo o inactivo como se dejo en el otro programa
  for (i in 1:length(ventas.z$store_id)) {
    if(ventas.z$status_id[i] %in% c(6,7,8,9)){
      ventas.z$prod_store_status[i] <- "ACTIVE"
    } else {
      ventas.z$prod_store_status[i] <- "INACTIVE"
    }
  }
  
  
  ###********** vamos a dar el formato anterior al archivo de ventas *********###
  head(ventas.z)
  table(ventas.z$cuenta)
  
  
  # comparamos el archivo anterior
  #SALVADOR##ventas.zold <- read.csv("G:/My Drive/VUE repro INCO ene-feb 2018/ventas_zero_15.csv", header = TRUE, sep = ",", quote = "\"",
  #                        dec = ".", fill = TRUE, comment.char = "") # Sales per store, from HUE or Sample Inspection
 #SALVADOR##head(ventas.zold) # vemos que las columnas tienen distinto orden, para aprovechar lo que ya se hizo se ordenan asi:
  # index_id period_id     ventas store_id category_code cell_id prod_store_status
  
  
 #SALVADOR# ventas.znew <- data.frame(ventas.z$index_id,ventas.z$period_id,ventas.z$ventas,ventas.z$store_id,ventas.z$category_code,ventas.z$cell_id,ventas.z$prod_store_status,ventas.z$cuenta)
#SALVADOR#  names(ventas.znew) <- c("index_id","period_id","ventas","store_id","category_code","cell_id","prod_store_status","cuenta")
#SALVADOR#   head(ventas.znew) # tenemos ya nuestras ventas finales para usar el programa enterior.
  
  
  ################################################################################################
  ibds <- data.frame(ibds[ ,1],ibds[ ,2],ibds[ ,3],ibds[ ,4],ibds[ ,7],ibds[ ,8],ibds[ ,5],ibds[ ,6]) # We order the columns, in order to use vlookup function
  names(ibds) <- c("Period","Period_Nm","Sample_ID","Sample_Nm","cell_Id","Cell_Name_ibds","IBD_ID","IBD_Name")
  
  #** OUTLIERS
  outliers <- mutate(outliers,checa = rep(F,length(outliers$Outlier))) # We generate a logical vector with F’s, in order to replace the blanks with 0’s
  # Puts 0´s on the blanks in outlier column, in Outlier file.
  for(i in 1:length(outliers$checa)){
    if (outliers$Outlier[i] %in% c("")) {
      outliers$checa[i] <- 0
    } else {
      outliers$checa[i] <- outliers$Outlier[i]
    }
  }
  # it rename the levels of the column of outliers
  for(i in 1:length(outliers$checa)){
    if (outliers$checa[i] %in% 2) {
      outliers$checa[i] <- c("High")
    } else {
      if (outliers$checa[i] %in% 3) {
        outliers$checa[i] <- c("Low")
      }
    }
  }
  
  
  ### Correction of sales sheet ###
  #** The names of the columns of the sales file, need be lowercase
  ventas1 <- mutate(ventas.z, ibd_name = vlookup(ventas.z$cell_id, ibds, result_column = 8, lookup_column = 5)) # For each cell_id, it paste the name of ibd
  canasto <- basket$Category # Our basket, from VUE files
  ventas1 <- mutate(ventas1, Basket = ifelse(ventas1$category_code %in% canasto, "SI", "NO")) #the flag that indicate us if the category is in the basket or not
  ventas1 <- mutate(ventas1, Outlier = vlookup(ventas1$store_id, outliers, result_column = 13, lookup_column = 8)) #the flag that indicate if the store is an outlier. Information from VUE file “outliers”
  #** Is it a new store?
  ventas1 <- mutate(ventas1, new_store = is.na(ventas1$Outlier)) # pegamos si es o no outlier desde el archivo de vue
  summary(ventas1)
  ### Generate the “base_bJ” sheet ###
  pivot <- filter(ventas1, Basket %in% "SI")
  pivot <- filter(pivot, Outlier %in% 0)
  pivot <- filter(pivot, new_store %in% 0)
  pivot <- filter(pivot, prod_store_status %in% "ACTIVE")
  pivot <- mutate(pivot, ibd_fact = as.character(pivot$ibd_name))
  pivot <- mutate(pivot, source_fact = as.character(pivot$store_id))
  
  ##It will generate our mini pivot table to paste the nspc’s values from vue file
  # Grouping the sales per ibd_names and stores
  pivote <- melt(pivot,id=c("ibd_name","store_id"),measure="ventas",value.name="sales");
  head(pivote)
  # Then, we generate our table with Zi value
  pivotes <- dcast(pivote, ibd_name + store_id ~ variable, sum, value.var="value",margins=F)
  head(pivotes)
  # We paste the nspc directly from outliers file, the Xi value
  pivotes <- mutate(pivotes, nspc = vlookup(pivotes$store_id,outliers,9,8))
  # Generating the ratio Zi/Xi
  pivotes <- mutate(pivotes, Zi.Xi = pivotes$nspc/pivotes$ventas)
  
  ## it is the mini pivot table of minimums, which we need to exclude from the data
  mini <- aggregate(pivotes[,5], by=list(pivotes$ibd_name), FUN=min)
  ## it is the mini pivot table of maximums, which we need to exclude from the data
  maxi <- aggregate(pivotes[,5], by=list(pivotes$ibd_name), FUN=max)
  names(mini) <- c("ibd_name","minimos")
  names(maxi) <- c("ibd_name","maximos")
  # This values are the minimums and maximums per ibd
  pivotes <- mutate(pivotes, maximo = vlookup(pivotes$ibd_name,maxi,2,1))
  pivotes <- mutate(pivotes, minimo = vlookup(pivotes$ibd_name,mini,2,1))
  # we collocate the flag to indicate if the store is a minimums or maximums, in order to exclude them from the data
  pivotes <- mutate(pivotes, max.min = rep(F, length(pivotes$store_id)))
  summary(pivotes) # hay tiendas para consultar al equipo de GDDC? si
  
  # colocamos la etiqueta de "Reach out GDDC team"
  for(i in 1:length(pivotes$store_id)){
    if(pivotes$ibd_name[i] %in% NA){
      pivotes$max.min[i] <- "Reach out GDDC team"
    }
  }
  
  # filtramos las tiendas que deben ser revisadas por el equipo de GDDC
  pivotes <- filter(pivotes, max.min != "Reach out GDDC team")
  
  # indicamos cuales son los minimos y maximos
  for(i in 1:length(pivotes$store_id)){
    if(pivotes$Zi.Xi[i] == pivotes$maximo[i]) {
      pivotes$max.min[i] <- c("MAX")
    } else {
      if(pivotes$Zi.Xi[i] == pivotes$minimo[i]) {
        pivotes$max.min[i] <- c("MIN")
      }
    }
  }
  # finally, the minimums and maximums gone
  pivotes <- filter(pivotes, max.min %in% F)
  
  ## we need generate other mini pivot table with the sums of sales per ibd, this pivot table not include the minimums and maximums.
  sum.venta.si <- aggregate(pivotes[,3], by=list(pivotes$ibd_name), FUN=sum)
  names(sum.venta.si) <- c("ibd_name","Ventas.SI")
  ## we need generate other mini pivot table with the sums of nspc's per ibd, this pivot table not include the minimums and maximums.
  sum.nspc <- aggregate(pivotes[,4], by=list(pivotes$ibd_name), FUN=sum)
  names(sum.nspc) <- c("ibd_name","NSPC")
  # merging the tables
  suma.ventas.si.nspc <- merge(sum.venta.si,sum.nspc,by="ibd_name")
  suma.ventas.si.nspc <- mutate(suma.ventas.si.nspc, nspc.ventas = NSPC/Ventas.SI)
  
  
  ### Generating the New control volume sheet ###
  solo.tiendas <- filter(ventas.z, prod_store_status %in% "ACTIVE")
  new.ctrl.vol <- data.frame(unique(solo.tiendas$store_id))
  names(new.ctrl.vol) <- c("store_id")
  names(ventas1)
  names(new.ctrl.vol)
  new.ctrl.vol <- mutate(new.ctrl.vol, cell_id = vlookup(new.ctrl.vol$store_id,ventas.z,result_column=6,lookup_column=4))
  new.ctrl.vol <- mutate(new.ctrl.vol, status_id = vlookup(new.ctrl.vol$store_id,ventas.z,result_column=7,lookup_column=4))
  new.ctrl.vol <- mutate(new.ctrl.vol, ibd_name = vlookup(new.ctrl.vol$cell_id,ventas1,result_column=8,lookup_column=6))
  new.ctrl.vol <- mutate(new.ctrl.vol, new_store = vlookup(new.ctrl.vol$store_id,ventas1,result_column=11,lookup_column=4))
  new.ctrl.vol <- mutate(new.ctrl.vol, nspc.ventas = vlookup(new.ctrl.vol$ibd_name,suma.ventas.si.nspc,result_column=4,lookup_column=1))
  
  # The pivot table that give us the sales per store
  ventas2 <- filter(ventas1, Basket %in% "SI")
  ventas.tienda <- aggregate(ventas2$ventas,by=list(ventas2$store_id),FUN=sum)
  names(ventas.tienda) <- c("store_id","ventas")
  # the finish of the dataset to calculate the nspc and control volume for the new stores
  new.ctrl.vol <- mutate(new.ctrl.vol, ventas = vlookup(new.ctrl.vol$store_id,ventas.tienda,result_column=2,lookup_column=1))
  new.ctrl.vol <- mutate(new.ctrl.vol, nspc = vlookup(new.ctrl.vol$store_id,outliers,result_column=9,lookup_column=8))
  new.ctrl.vol <- mutate(new.ctrl.vol, Control_Volume = vlookup(new.ctrl.vol$store_id,outliers,result_column=10,lookup_column=8))
  
  ## Calculus of nspc of the new stores
  for (i in 1:length(new.ctrl.vol$store_id)){
    if(new.ctrl.vol$new_store[i] %in% T){
      new.ctrl.vol$nspc[i] <- round(new.ctrl.vol$nspc.ventas[i]*new.ctrl.vol$ventas[i])
    }
  }
  ## Calculus of control volume of the new stores
  C <- constant$Constant
  for (i in 1:length(new.ctrl.vol$store_id)){
    if(new.ctrl.vol$new_store[i] %in% T){
      new.ctrl.vol$Control_Volume[i] <- round(new.ctrl.vol$nspc[i]+C,digits = 0)
    }
  }
  
  new_ctrl_vol <- new.ctrl.vol
  
  #renderizar tabla######
  output$tabloide <- renderTable(new.ctrl.vol)
  #write.csv(new.ctrl.vol,"C:/Users/ullosa01/Desktop/Proyecto NSPC Tiendas Nuevas/nspc_sal_fin_con_fabric.csv")
  #output$download <- downloadHandler(
   # filename = function(){"new.ctrl.vol"}, 
    #content = function(fname){
     # write.csv(new.ctrl.vol(), fname)
    #}
  #)
  
  #renderizar tabla######
  
  }))
  
  #output$download <- downloadHandler(
  #  filename = "new_ctrl_vol.csv", 
  #  content = function(file){
  #    write.csv(new.ctrl.vol(), fname)
  #  }
  #)
  
  #output$download <- downloadHandler(
  #  filename <- function() {
  #  'new_ctrl_vol.csv'
  #  },
  #  
  #  content <- function(file) {
  #    write.csv("new_ctrl_vol.csv", file)
  #  },
  #  contentType = "text/csv"
  #)
  
  output$download <- downloadHandler(
    filename <- function() {
      paste("new.ctrl.vol-",syc.date(),".csv", sep="")
    },
    
    content <- function(file) {
      write.csv("new.ctrl.vol", file)
    },
    contentType = "text/csv"
  )
  
  }
