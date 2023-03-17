#use with the abind library.
#set the right working directory
compute_mfccs <- function(dir,ncep) {

  file_list <- list.files(dir)  
  mfccs <- NULL
  for (file in file_list) {
    # load file .wav
    wav <- readWave(file.path(dir, file))
    if(length(wav@left) == 16000){ 
      # compute the MFCC
      mfcc <- melfcc(wav, numcep = ncep)
      #Check that the mfcc not have NA value
      if(!any(is.na(mfcc))) {
        mfccs <- abind(mfccs, mfcc, along = 3)
      }
    }
  }
  return(mfccs)
}