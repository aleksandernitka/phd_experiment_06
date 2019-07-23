hdf5.extractor = function(files,
                          events,
                          import.messages = TRUE,
                          logs = TRUE) {
    # This funciton takes three arguments:
    # 'files'             which is a vector or filenames to be processed and
    # 'events'            which is a vector of events to be extracted from the HDF5 file
    # 'import.messages'   which is a boolean - do you want event messages and event logs imported?
    # 'logs'              which is a boolean - do you want to get vectors with all DF names AND a general log of import?
    
    
    # To use this function:
    # source("/path/to/file/edf.extractor.R")
    
    # To install:
    # source("http://bioconductor.org/biocLite.R")
    # biocLite("rhdf5")
    
    require("rhdf5")
    
    # TODO
    # Add exception handling for the source/install of the rhdf5
    # DF naming - atm it's from the order of file names in 'files' - extract subject id from the filename?
    
    # For easier handling later on I would create lists of imported DFs
    # This can make looping easier in analysis:
    hdf5.import.df  = c()     # Log for all DFs with ET data (events)
    hdf5.import.evt = c()     # Log for all MessageEvent DFs created
    
    # Log beggining of loading
    t.start = Sys.time()
    
    for (f in 1:length(files)) {
        ##### Import ET events
        for (e in 1:length(events)) {
            tmp.name = strsplit(events[e], "/")
            tmp.name = tmp.name[[1]][length(tmp.name[[1]])]
            tmp.data = h5read(files[f],
                              paste("/data_collection/events", events[e], sep = ""))
            
            tmp.df.name = sprintf("ss%i.%s", f, tmp.name)
            
            # save the DF to the GLOBAL ENVI
            assign(tmp.df.name, tmp.data, envir = .GlobalEnv)
            
            tmpm = sprintf("file %s extracted to ss%i.%s", files[f], f, tmp.name)
            message(tmpm)
            
            # append to log
            if (logs == TRUE) {
                hdf5.import.df  = append(hdf5.import.df, tmp.df.name)
            }
            
            ##### Import messages
            if (import.messages == TRUE) {
                tmp.msgs = h5read(files[f],
                                  "/data_collection/events/experiment/MessageEvent")
            }
            
            # Save as GLOBAL
            assign(sprintf("ss%i.MessageEvent", f), tmp.msgs, envir = .GlobalEnv)
            tmpe = sprintf("file %s extracted to ss%i.MessageEvent", files[f], f)
            message(tmpe)
            
            # append to log
            if (logs == TRUE) {
                hdf5.import.evt = append(hdf5.import.evt,
                                         sprintf("ss%i.MessageEvent", f))
            }
        }
        
    }
    
    
    # Log end time of import
    t.end = Sys.time()
    message(sprintf("This took me: %.3f seconds!", (t.end - t.start)))
    
    # Save log vectors if needed
    if (logs == TRUE) {
        assign("hdf5.import.df", hdf5.import.df, envir = .GlobalEnv)
        assign("hdf5.import.evt", hdf5.import.evt, envir = .GlobalEnv)
    }
}