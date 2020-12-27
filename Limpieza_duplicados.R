#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

###
stfu = suppressPackageStartupMessages
stfu(library(crayon))
stfu(library(data.table))
stfu(library(stringr))
stfu(library(tidyverse))
green = make_style(rgb(red=0,green=250,blue=0,maxColorValue=255))



display_help <- function(){
  message("
  ## SCRIPT QUE ADECUA EL PED DE UNA MUESTRA PARA CONTENER LOS FENOTIPOS DEL TIPO SANGUINEO O. 
	## Método de Uso:
        $ Limpieza_duplicados.r [datos blood type] [ped] [outfilename.ped]"
  )}



if (length(args)==0) {
  display_help()
  stop("At least four arguments must be supplied (final report file, associacion table, variants table, barcode ).", call.=FALSE)
  display_help()
} else if (length(args)==1) {
  # default output file
  display_help()
  stop("At least four arguments must be supplied (final report file, associacion table, variants table, barcode ).", call.=FALSE)  
  display_help()
} else if (length(args)>=2) {
  file_1 = args[1]; #Datos blood type UAQ
  file_2 = args[2]; #PED 
  file_3 = args[3]; #outfilename
  #file3 = args[3]; #DIRECTORY WHERE THE GZ's ARE 
  #arg1 = args[4];
}


main <- function(file1=file_1,file3=file_2,outputfile=file_3){
###
### EN ESTA PARTE SE HIZO UNA REVISION MANUAL DE LOS CODIGOS DE ESTUDIANTES QUE VENIAN DUPLICADOS 
### Y SIN INFORMACION DEL TIPO SANGUINEO DE LA DB 
#file  = "/Users/Robot/Documents/Descargas_Codigo46/Investigación/UAQ/Datos_Blood_type/14dic2020_COMPLETA.codigos_uaq.txt"
#codigos_uaq  = as.data.frame(fread(file,header=T))
#duplicados=sort(table(codigos_uaq))
#print(duplicados)
##
##
#file2 = "/Users/Robot/Documents/Descargas_Codigo46/Investigación/UAQ/Datos_Blood_type/14dic2020_COMPLETA.tsv"
uaq_data  = as.data.frame(fread(file1,header=T))
nrow(uaq_data)
blood_subset = uaq_data %>% select('Código',Sexo,'Tipo de sangre')
colnames(blood_subset)=c('Codigo','Sexo','Tipo de sangre');
##
#Donde_indices_duplicados=grep('C7 B5_UAQ',blood_subset$'Código')
#Valores_revision_manual=blood_subset[c(520,539),]
## indices seleccionados para eliminacion
uaq_data_no_dups=uaq_data[-c(110,127,138,147,158,168,280,520),]
#nrow(uaq_data_no_dups)
blood_subset = uaq_data_no_dups %>% select('Código',Sexo,'Tipo de sangre')
colnames(blood_subset)=c('Codigo','Sexo','Tipo de sangre');

n1=nrow(blood_subset)
#### TIPO O
O_vector=grep("O",blood_subset$'Tipo de sangre')
O_vector_negated=grep("O",blood_subset$'Tipo de sangre',invert=TRUE)
####
blood_subset$O_test=str_detect('O',blood_subset$'Tipo de sangre')
O_vector_na=which(is.na(blood_subset$O_test))
####
blood_subset$P_O=""
blood_subset[O_vector,]$P_O=1;
blood_subset[O_vector_negated,]$P_O=0;
blood_subset[O_vector_na,]$P_O=-2;
#####
table(blood_subset$P_O)
#### TIPO A
A_vector=grep("A",blood_subset$'Tipo de sangre')
A_vector_negated=grep("A",blood_subset$'Tipo de sangre',invert=TRUE)
####
blood_subset$A_test=str_detect('A',blood_subset$'Tipo de sangre')
A_vector_na=which(is.na(blood_subset$A_test))
####
blood_subset$P_A=""
blood_subset[A_vector,]$P_A=1;
blood_subset[A_vector_negated,]$P_A=0;
blood_subset[A_vector_na,]$P_A=-2;
#####
table(blood_subset$P_A)
#### TIPO B
B_vector=grep("B",blood_subset$'Tipo de sangre')
B_vector_negated=grep("B",blood_subset$'Tipo de sangre',invert=TRUE)
####
blood_subset$B_test=str_detect('B',blood_subset$'Tipo de sangre')
B_vector_na=which(is.na(blood_subset$B_test))
####
blood_subset$P_B=""
blood_subset[B_vector,]$P_B=1;
blood_subset[B_vector_negated,]$P_B=0;
blood_subset[B_vector_na,]$P_B=-2;
#####
table(blood_subset$P_B)
#### TIPO AB
AB_vector=grep("AB",blood_subset$'Tipo de sangre')
AB_vector_negated=grep("AB",blood_subset$'Tipo de sangre',invert=TRUE)
####
blood_subset$AB_test=str_detect('AB',blood_subset$'Tipo de sangre')
AB_vector_na=which(is.na(blood_subset$AB_test))
####
blood_subset$P_AB=""
blood_subset[AB_vector,]$P_AB=1;
blood_subset[AB_vector_negated,]$P_AB=0;
blood_subset[AB_vector_na,]$P_AB=-2;
#####
table(blood_subset$P_AB)
# REACOMODAMOS LOS NOMBRES DE LOS LAB CODES 
phenotypes_blood_subset_separated_names=separate(blood_subset,Codigo,sep="[[:blank:]]",remove=FALSE,into=c('A1','A2'))
phenotypes_blood_subset_separated_names$SampleID=paste(phenotypes_blood_subset_separated_names$A1,phenotypes_blood_subset_separated_names$A2,sep="")
nrow(phenotypes_blood_subset_separated_names)
length(phenotypes_blood_subset_separated_names$SampleID)
length(unique(phenotypes_blood_subset_separated_names$SampleID))
#########################
######################### PED MUESTRAS 
#########################
#########################
ped_muestras_file = file3;
#ped_muestras_file = "/Users/Robot/Documents/Descargas_Codigo46/Investigación/UAQ/Datos_Blood_type/SAMPLES_UAQ_sample.ped"
ped_uaq_data  = as.data.frame(fread(ped_muestras_file,header=F))
print(colnames(ped_uaq_data))

########################
#length(phenotypes_blood_subset_separated_names$SampleID)
#length(unique(phenotypes_blood_subset_separated_names$SampleID))
########################
#n2=nrow(ped_uaq_data)
#for(j in 1:n2){
#  print(paste('Sample',ped_uaq_data[j,]$V2,'Feno(O)=',ped_uaq_data[j,]$V6))
#  where=which(str_detect(ped_uaq_data[j,]$V2,phenotypes_blood_subset_separated_names$SampleID))
#  Isthis_user_onPED=any(where)
#  if(Isthis_user_onPED == 'TRUE'){
#    print(where)
#    print(phenotypes_blood_subset_separated_names[where,]$P_O )
#    ped_uaq_data[j,]$V6 = phenotypes_blood_subset_separated_names[where,]$P_O;
#  }
#  if(Isthis_user_onPED == 'FALSE'){
#    print(where)
#    ped_uaq_data[j,]$V6 = -2;
#  }
#}
#print(table(ped_uaq_data$V6))
#print(ped_uaq_data)
#message("\n\n",yellow$underline$bold(paste("Writing output at:",outputfile)))
#write.table(ped_uaq_data, file = outputfile, sep = "\t",col.names = FALSE,row.names=FALSE,quote = FALSE)

}


## MAIN
if (!interactive()){
  message("\n\n",yellow$underline$bold("Código 46 S.A. de C.V."),green("PED Phenotypes V. 2"),"\n")
  arg=commandArgs(TRUE)
  if(length(arg)!=0){
    input_file=arg[1] # Finalreport de lectura
    tm = system.time(main(input_file))
    message("\nTiempo de Procesamiento: ")
    print(tm)
    message("Script Completado\t",green("OK! :)"))
  } else {
    display_help()
  }
} else {
  message("Sourcing ...")
}


