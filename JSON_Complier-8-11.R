#script that loops through folder
#creates list of json files
#pulls info out from jsons into data.frame
#for easier manipulation in R

#try to loop thru jsons to get info needed

cur.dir = "G:/BRC/SimResults/test2/jsons/"


compile <- function(cur.dir){
  jsons <- list.files(path = cur.dir,
                      pattern = '*.json')
  
  #create empty data.frame with variable names
  names = c("FILE", "BUSTED.LR", "BUSTED.SRV.LR", "BUSTED.omega3.MLE", "BUSTED.SRV.omega3.MLE", "BUSTED.omega3.prop",
            "BUSTED.SRV.omega3.prop", 'CV.SRV', 'BUSTED.P', 'BUSTED.SRV.P','BUSTED.AICc','BUSTED.SRV.AICc',
            'BUSTED.treelength' ,'BUSTED.SRV.treelength', 'Sites', 'Sequences', 
            'BUSTED.omega1.MLE','BUSTED.SRV.omega1.MLE', 'BUSTED.omega1.prop','BUSTED.SRV.omega1.prop',
            'BUSTED.omega2.MLE','BUSTED.SRV.omega2.MLE', 'BUSTED.omega2.prop','BUSTED.SRV.omega2.prop', 'SRV.alpha3.MLE',
            'SRV.alpha3.prop','SRV.alpha1.MLE','SRV.alpha1.prop','SRV.alpha2.MLE','SRV.alpha2.prop')
  
  classes = c("character", "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
              "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
              "numeric","numeric","numeric")
  df =read.table(text="", col.names = names, colClasses = classes)
  for (i in  seq(from=1, to=length(jsons), by=2)){
    filepath = paste(cur.dir,jsons[i], sep="")
    
    test = fromJSON(filepath)
    
    
    
    FILE = jsons[i]
    Sites = length(test$profiles$unconstrained)
    tree_string = test$fits$`Unconstrained model`$`tree string`
    Sequences = "NaN"
    
    if (grepl("SRV",jsons[i])){
      filepath = paste(cur.dir,jsons[i], sep="")
      test = fromJSON(filepath)
      
      CV.SRV = "NaN"
      BUSTED.SRV.P = test$`test results`$p
      BUSTED.SRV.LR =test$`test results`$LR
      BUSTED.SRV.AICc = test$fits$`Unconstrained model`$`AIC-c`
      BUSTED.SRV.treelength = test$fits$`Unconstrained model`$`tree length`
      
      #OMEGA values for BUSTED.SRV
      BUSTED.SRV.omega3.MLE = test$fits$`Unconstrained model`$`rate distributions`$FG[3,1]
      BUSTED.SRV.omega3.prop = test$fits$`Unconstrained model`$`rate distributions`$FG[3,2]
      BUSTED.SRV.omega2.MLE = test$fits$`Unconstrained model`$`rate distributions`$FG[2,1]
      BUSTED.SRV.omega2.prop = test$fits$`Unconstrained model`$`rate distributions`$FG[2,2]
      BUSTED.SRV.omega1.MLE = test$fits$`Unconstrained model`$`rate distributions`$FG[1,1]
      BUSTED.SRV.omega1.prop = test$fits$`Unconstrained model`$`rate distributions`$FG[1,2]
      #ALPHA values for BUSTED.SRV
      SRV.alpha3.MLE = test$fits$`Unconstrained model`$`rate distributions`$SRV[3,1]
      SRV.alpha3.prop = test$fits$`Unconstrained model`$`rate distributions`$SRV[3,2]
      SRV.alpha2.MLE = test$fits$`Unconstrained model`$`rate distributions`$SRV[2,1]
      SRV.alpha2.prop = test$fits$`Unconstrained model`$`rate distributions`$SRV[2,2]
      SRV.alpha1.MLE = test$fits$`Unconstrained model`$`rate distributions`$SRV[1,1]
      SRV.alpha1.prop = test$fits$`Unconstrained model`$`rate distributions`$SRV[1,2]
      
      
      
    }
    if (grepl("SRV",jsons[i+1])==FALSE){
      filepath = paste(cur.dir,jsons[i+1], sep="")
      
      test = fromJSON(filepath)
      BUSTED.P = test$`test results`$p
      BUSTED.LR = test$`test results`$LR
      BUSTED.AICc = test$fits$`Unconstrained model`$`AIC-c`
      BUSTED.treelength = test$fits$`Unconstrained model`$`tree length`
      
      #OMEGA values for BUSTED
      BUSTED.omega3.MLE = test$fits$`Unconstrained model`$`rate distributions`$FG[3,1]
      BUSTED.omega3.prop = test$fits$`Unconstrained model`$`rate distributions`$FG[3,2]
      BUSTED.omega2.MLE = test$fits$`Unconstrained model`$`rate distributions`$FG[2,1]
      BUSTED.omega2.prop = test$fits$`Unconstrained model`$`rate distributions`$FG[2,2]
      BUSTED.omega1.MLE = test$fits$`Unconstrained model`$`rate distributions`$FG[1,1]
      BUSTED.omega1.prop = test$fits$`Unconstrained model`$`rate distributions`$FG[1,2]
      #ALPHA values for BUSTED
      #       BUSTED.alpha3.MLE = test$fits$`Unconstrained model`$`rate distributions`$SRV[3,1]
      #       BUSTED.alpha3.prop = test$fits$`Unconstrained model`$`rate distributions`$SRV[3,2]
      #       BUSTED.alpha2.MLE = test$fits$`Unconstrained model`$`rate distributions`$SRV[2,1]
      #       BUSTED.alpha2.prop = test$fits$`Unconstrained model`$`rate distributions`$SRV[2,2]
      #       BUSTED.alpha1.MLE = test$fits$`Unconstrained model`$`rate distributions`$SRV[1,1]
      #       BUSTED.alpha1.prop = test$fits$`Unconstrained model`$`rate distributions`$SRV[1,2]
      
    }
    
    df[nrow(df)+1,] <- c(FILE, BUSTED.LR, BUSTED.SRV.LR, BUSTED.omega3.MLE, BUSTED.SRV.omega3.MLE, BUSTED.omega3.prop,
                         BUSTED.SRV.omega3.prop, CV.SRV, BUSTED.P, BUSTED.SRV.P,BUSTED.AICc,BUSTED.SRV.AICc,
                         BUSTED.treelength ,BUSTED.SRV.treelength, Sites, Sequences, 
                         BUSTED.omega1.MLE,BUSTED.SRV.omega1.MLE, BUSTED.omega1.prop,BUSTED.SRV.omega1.prop,
                         BUSTED.omega2.MLE,BUSTED.SRV.omega2.MLE, BUSTED.omega2.prop,BUSTED.SRV.omega2.prop, SRV.alpha3.MLE,
                         SRV.alpha3.prop,SRV.alpha1.MLE,SRV.alpha1.prop,SRV.alpha2.MLE,SRV.alpha2.prop)
    
  }
  df[,2:30]=as.numeric(unlist(df[,2:30]))
    return(df)
}