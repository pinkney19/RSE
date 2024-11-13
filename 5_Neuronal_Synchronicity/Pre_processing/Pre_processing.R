
# Functions

exp.id<-function(flnm){
  substr(flnm,1,nchar(flnm)-1)
}


reduce.data<-function(data,path,exp.idx,ID,control.indices,treatment.indices){
  n.cell=length(data$efd[,,1]$LaserSpikes[,,1]$RasterAlign);
  control=array(list(),dim=c(length(control.indices),n.cell ));
  trt=array(list(),dim=c(length(control.indices),n.cell ));
  for(i.cell in 1:n.cell){
    for(i.trial in control.indices){ # idx for laser level idx
      control[i.trial,i.cell][[1]]=data$efd[,,1]$LaserSpikes[,,1]$RasterAlign[[i.cell]][[1]][[i.trial]][[1]];
    }
    for(i.trial in treatment.indices){ # idx for laser level idx 
      trt[i.trial-min(treatment.indices)+1,i.cell][[1]]=data$efd[,,1]$LaserSpikes[,,1]$RasterAlign[[i.cell]][[1]][[i.trial]][[1]];
    }
  }
  saveRDS(control, file=paste('./reduced/',exp.idx,'/', ID,'_control.rds',sep=''))
  saveRDS(trt, file=paste('./reduced/',exp.idx,'/', ID,'_trt.rds',sep=''))
}


setwd("~/luna/Bolding_Data/Pre_processing")
library(R.matlab) 

# Read the conditions (from Ali):
exps=read.csv('ExperimentCatalog_THY1-TeLC_forR.txt',sep='',stringsAsFactors=F);


exps.by.id <- data.frame(matrix(ncol = 6, nrow = 0))
x <- c("path",'FP','LP','Condition',"B bank", "P bank")
colnames(exps.by.id) <- x

counts=0;
for(i in 1:dim(exps)[1]){
  this.name=exps[i,1]
  m.id=match(exp.id(this.name), exps.by.id[,1]);
  if (is.na(m.id)){
    counts=counts+1;  
    m.id=counts;
    exps.by.id[m.id,1]=exp.id(this.name);
    exps.by.id[m.id,2]=exps[i,2];
    exps.by.id[m.id,3]=exps[i,3];
    exps.by.id[m.id,4]=exps[i,5]; 
  }
  if (identical(exps[i,4],'B')){
    exps.by.id[m.id,5]=this.name;
  }else{
    exps.by.id[m.id,6]=this.name;
  }
}  


head(exps.by.id)
saveRDS(exps.by.id, file='ExpCatalog.rds')

# We want OB region - i.e. B_bank at all laser intensities
# laser intensities are as follows
indices.0= 1:10; 
indices.1 = 11:20
indices.5 = 21:30
indices.10 = 31:40
indices.20 = 41:50
indices.30 = 51:60
indices.40 = 61:70
indices.50 = 71:80

Ndata = readMat("151213-1/151213-1_bank2.efd")

reduce.data2<-function(data,control.indices,treatment.indices, control_name, trt_name){
  n.cell=length(data$efd[,,1]$LaserSpikes[,,1]$RasterAlign);
  control=array(list(),dim=c(length(control.indices),n.cell ));
  trt=array(list(),dim=c(length(control.indices),n.cell ));
  for(i.cell in 1:n.cell){
    for(i.trial in control.indices){ # idx for laser level idx
      control[i.trial,i.cell][[1]]=data$efd[,,1]$LaserSpikes[,,1]$RasterAlign[[i.cell]][[1]][[i.trial]][[1]];
    }
    for(i.trial in treatment.indices){ # idx for laser level idx 
      trt[i.trial-min(treatment.indices)+1,i.cell][[1]]=data$efd[,,1]$LaserSpikes[,,1]$RasterAlign[[i.cell]][[1]][[i.trial]][[1]];
    }
  }
  saveRDS(control, file=control_name)
  saveRDS(trt, file=trt_name)
}

reduce.data2(Ndata,indices.0,indices.1, "OB_0.RDS", "OB_1.RDS")
reduce.data2(Ndata,indices.0,indices.5, "OB_0.RDS", "OB_5.RDS")
reduce.data2(Ndata,indices.0,indices.10, "OB_0.RDS", "OB_10.RDS")
reduce.data2(Ndata,indices.0,indices.20, "OB_0.RDS", "OB_20.RDS")
reduce.data2(Ndata,indices.0,indices.30, "OB_0.RDS", "OB_30.RDS")
reduce.data2(Ndata,indices.0,indices.40, "OB_0.RDS", "OB_40.RDS")
reduce.data2(Ndata,indices.0,indices.50, "OB_0.RDS", "OB_50.RDS")
