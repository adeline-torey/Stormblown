HDF5_File_Parsing_Table_With_GC_Multiline_SC<-function(File) {
  
  h5errorHandling(type="suppress")
  File<-H5Fopen(File)
  idtab<-h5ls(File, recursive=FALSE, datasetinfo=FALSE)[2]
  Table<-c(Read_Id="Read_Id",Channel="Channel",Mux="Mux",Unix_Time="Unix_Time",Length="Length",Qscore="Qscore",GC_Content="GC_Content")
  List <- rep(list(Table),nrow(idtab))
  
  for (l in 1:nrow(idtab)) {
    
    GroupAnalyses<-try(H5Gopen(File,paste0(idtab[l,],'/Analyses/Basecall_1D_000/BaseCalled_template')), silent=TRUE) #check if fastq exists. It is supposed to exists both for failed and passed
    if (inherits(GroupAnalyses,"try-error")) {
      H5Gclose(GroupAnalyses)
      List[[l]]<-Table
    }
    
    else {
      
      Pre_Fastq<-try(Read_DataSet(GroupAnalyses, 'Fastq'), silent=TRUE)
      
      if (inherits(Pre_Fastq,"try-error")) {
        H5Gclose(GroupAnalyses)
        List[[l]]<-Table
      }
      
      else {
        
        Sequence_Fastq<-unlist(strsplit(Pre_Fastq,"\n"))[2]
        List[[l]]['GC_Content']<-sum(gregexpr('[GgCc]',Sequence_Fastq)[[1]] > 0)/nchar(Sequence_Fastq)
        H5Gclose(GroupAnalyses)
        
        GroupQuality<-H5Gopen(File,paste0(idtab[l,],'/Analyses/Basecall_1D_000/Summary/basecall_1d_template'))
        List[[l]]['Qscore']<-Read_Attributes(GroupQuality, 'mean_qscore')
        List[[l]]['Length']<-Read_Attributes(GroupQuality, 'sequence_length')
        H5Gclose(GroupQuality)
        
        GroupId<-H5Gopen(File,paste0(idtab[l,],'/Raw/'))
        List[[l]]['Read_Id']<-Read_Attributes(GroupId, 'read_id')
        List[[l]]['Mux']<-Read_Attributes(GroupId, 'start_mux')
        Generation<-Read_Attributes(GroupId, 'start_time')
        H5Gclose(GroupId)
        
        GroupChannel<-H5Gopen(File,paste0(idtab[l,],'/channel_id/'))
        List[[l]]['Channel']<-Read_Attributes(GroupChannel, 'channel_number')
        H5Gclose(GroupChannel)
        
        GroupStartTime<-H5Gopen(File,paste0(idtab[l,],'/tracking_id/'))
        
        Time<-Read_Attributes(GroupStartTime, 'exp_start_time')
        AlternativeStart<-floor(Generation/4000) ##actual sampling rate
        
        if (length(unlist(strsplit(Time,"T")))==2) {### avoid problems with UNIX exp start times found on EGA samples
          Y_M_D<-substr(Time,1,10)
          H_M_S<-substr(Time,12,19)
          Time_Vector<-paste(c(Y_M_D,H_M_S), collapse=" ")
          List[[l]]['Unix_Time']<-as.numeric(as.POSIXct(strptime(Time_Vector, "%Y-%m-%d %H:%M:%S")))+AlternativeStart
          
        }
        else {
          List[[l]]['Unix_Time']<-(as.numeric(AlternativeStart)+as.numeric(Time))
        }
        
        H5Gclose(GroupStartTime)
      }
    }
  }
  
  H5Fclose(File)
  Table<-do.call(rbind,List)
  return(Table)
}