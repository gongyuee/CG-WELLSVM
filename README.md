# CG-WELLSVM
The DEM folder: data file-the digital elevation model of the study area in 2016 obtained from Geographical Information Monitoring Cloud Platform. Slope, curvature, aspect, topographic wetness index (TWI), stream power index (SPI) can be calculated or directly extracted from DEM.  
The ISP folder: data file-Impervious Surface Percentage of the study area in 2017 obtained from Global Man-made Impervious Surface data product.  
The LAI folder： data file-Leaf Area Index of the study area in 2016 obtained from Geographical Information Monitoring Cloud Platform.  
The precitation_2013 folder: data file-the hourly precipitaion of the study area during 2013/07/01/17-2013/07/07/16. The precipitation of the day, accumulative precipitation in the first three days during the event can be calculated based on the precipitation data.  
The precitation_2016 folder: data file-the hourly precipitaion of the study area during 2016/07/03/17-2016/07/06/16.  
The precitation_2019 folder: data file-the hourly precipitaion of the study area during 2019/06/18/17-2019/06/21/16.  
The Road network folder: data file-Road line of the study area in 2016 obtained from OpenStreetMap. The road distance can be calculated based on the data.  
The River network folder: data file-River line of the study area in 2016 obtained from OpenStreetMap. The River distance can be calculated based on the data.  
The Waterlogging sites folder: data file-the waterlogging sites of the rainstorm on July 6, 2016(case 1),July 7, 2013 (case 2) and June 21, 2019 (case 3).  
LR for waterlogging prediction.R: code file-Construct LR model, predict the possibility to waterlogging and assese the model.  
PSO-WELLSVM.R: code file-Get the optimal parameter of WELLSVM(gamma,c1,c2).  
SVM for waterlogging prediction.R: code file-Construct SVM model, predict the possibility to waterlogging and assese the model.  
wellsvm for waterlogging prediction.R: code file-Construct wellsvm model, predict the possibility to waterlogging and assese the model.  
WELLSVM function.R: code file- the functoin of wellsvm.
