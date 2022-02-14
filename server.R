inst_package <- function(pkg_name_list) {
  instld_pkgs <- as.vector(installed.packages()[, 1])
  for (pkg in pkg_name_list) {
    ifelse(pkg %in% instld_pkgs, print(paste('Already installed package - ',pkg)), 
           print(paste('installing package..',pkg, install.packages(pkg)))
    )
    library(pkg, character.only = TRUE)
  }
}

pkg_name_list <- c('magrittr','caret','psych','cowplot','C50','randomForest','gmodels','neuralnet','e1071','shiny','shinydashboard','dplyr','rsconnect')
inst_package(pkg_name_list)

pdb_full <- read.csv("/Users/akankshakhorgade/Desktop/Bioinfo/DA5030/Proj/protein-data-set/prot_full.csv")

str(pdb_full)
summary(pdb_full)

pdb_no_dups <- pdb_full[!duplicated(pdb_full$structureId), ]
print(paste('No. of Rows in original data:', nrow(pdb_full)))
print(paste('No. of Rows with duplicate structureIds removed:', nrow(pdb_no_dups)))

pdb_no_dups$secStruct <- ifelse(pdb_no_dups$`helix%` > pdb_no_dups$`turn%` & pdb_no_dups$`helix%` >pdb_no_dups$`sheet%`,'H',
                                ifelse(pdb_no_dups$`turn%` > pdb_no_dups$`helix%` & pdb_no_dups$`turn%` > pdb_no_dups$`sheet%`,'T',
                                       ifelse(pdb_no_dups$`sheet%`>pdb_no_dups$`helix%` & pdb_no_dups$`sheet%` > pdb_no_dups$`turn%`,'S','NA')))

pdb_final <- pdb_no_dups[,c('structureId',
                            'classification',
                            'residueCount',
                            'structureMolecularWeight',
                            'densityMatthews',
                            'densityPercentSol',
                            'phValue',
                            'Aromaticity',
                            'Instability.Index',
                            'pI',
                            'Hydrophobicity',
                            'secStruct')]

pairs.panels(pdb_final)

structIdsWithNA <- as.vector(pdb_final[c(which(pdb_final$secStruct=="NA")),"structureId"])

pdb_final <- pdb_final[!(pdb_final$structureId %in% structIdsWithNA),]
table(pdb_final$secStruct)

pdb_final$secStruct <- factor(pdb_final$secStruct)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

pdb_final_norm <- pdb_final
pdb_final_norm[3:11] <- as.data.frame(lapply(pdb_final[3:11], normalize))

Comp_Pred_Val <- data.frame(testing$secStruct, c5_pred10, nnet_pred, nb_pred, rf_pred_k10)
Accur <- data.frame(ModelName = c("C5 Decision Tree", "Naive Bayes", "Neural Net", "Random Forest"), Accuracy = c("86.697%","73.169%","77.36%","87.641"))

function(input, output) {
  output$plot1 <- renderPlot({
    ggplot(as.data.frame(pdb_final$classification))+geom_bar(aes(x = pdb_final$classification), color = "#FF6666")+labs(title="Classification curve",x="classification", y = "count")
  })
  output$plot2 <- renderPlot({
    ggplot(as.data.frame(pdb_final$residueCount), aes(x=pdb_final$residueCount)) + 
      geom_density(alpha=.2, fill="#FF6666")+
      labs(title="ResCount density curve",x="Residue Count", y = "Density")
  })
  output$plot3 <- renderPlot({
    ggplot(as.data.frame(pdb_final$structureMolecularWeight), aes(x=pdb_final$structureMolecularWeight)) + 
      geom_density(alpha=.4, fill="#A6761D")+
      labs(title="Structmolecular wt density curve",x="Structural molecular weight", y = "Density")
  })
  output$plot4 <- renderPlot({
    ggplot(as.data.frame(pdb_final$densityMatthews), aes(x=pdb_final$densityMatthews)) + 
      geom_density(alpha=.3, fill="#1B9E77")+
      labs(title="Density Matthews density curve",x="Density Matthews", y = "Density")
  })
  output$plot5 <- renderPlot({
    ggplot(as.data.frame(pdb_final$densityPercentSol), aes(x=pdb_final$densityPercentSol)) + 
      geom_density(alpha=.6, fill="#D95F02")+
      labs(title="Density PctSol density curve",x="Density Pctsol", y = "Density")
  })
  output$plot6 <- renderPlot({
    ggplot(as.data.frame(pdb_final$phValue), aes(x=pdb_final$phValue)) + 
      geom_density(alpha=.8, fill="#7570B3")+
      labs(title="phValue density curve",x="phValue", y = "Density")
  })
  output$plot7 <- renderPlot({
    ggplot(as.data.frame(pdb_final$Aromaticity), aes(x=pdb_final$Aromaticity)) + 
      geom_density(alpha=.6, fill="#E7298A")+
      labs(title="Aromaticity density curve",x="Aromaticity", y = "Density")
  })
  output$plot8 <- renderPlot({
    ggplot(as.data.frame(pdb_final$Instability.Index), aes(x=pdb_final$Instability.Index)) + 
      geom_density(alpha=.6, fill="#FFCC00")+
      labs(title="Instability.Index density curve",x="InstabilityIndex", y = "Density")
  })
  output$plot9 <- renderPlot({
    ggplot(as.data.frame(pdb_final$pI), aes(x=pdb_final$pI)) + 
      geom_density(alpha=.6, fill="#0000FF")+
      labs(title="pI vs density curve",x="pI", y = "Density")
  })
  output$plot10 <- renderPlot({
    ggplot(as.data.frame(pdb_final$Hydrophobicity), aes(x=pdb_final$Hydrophobicity)) + 
      geom_density(alpha=.6, fill="#FFFF00")+
      labs(title="Hydrophobicity vs density curve",x="Hydrophobicty", y = "Density")
  })
  output$plot11 <- renderPlot({
    ggplot(as.data.frame(pdb_final$secStruct))+geom_bar(aes(x = pdb_final$secStruct), color = "#E0FFFF")+labs(title="secStruct density curve",x="secStruct", y = "count")
  })
  output$contents <- renderTable({
    dataset <- pdb_final[1:100,]
  })
  output$predTab <- renderTable({
    dataset <- Comp_Pred_Val
  })
  output$AccurTab <- renderTable({
    dataset <- Accur
  })
}