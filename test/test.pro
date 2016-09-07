PRO test
    ;initrmnlib
    initburplib
    MyFile    = Obj_New('burp_obj_file')                 ; file object
    err       = MyFile->SetProperty(FILE='2016072212_')                     
    MyReport  = Obj_New('burp_obj_rpt')                  ; report object     
    MyBlock   = Obj_New('burp_obj_block')                ; block object                                                                                    
    handle    = 0                                     ;beginning of the file
    handle    = MyFile->Find_Report(REPORT=MyReport,HANDLE=handle )          
    WHILE (handle  GT 0) DO BEGIN
        params    = MyReport->GetProperty(/ALL)      ;all parameters                  help, params,/structure                                             
        station   = MyReport->GetProperty(/STNID)    ;one parameter                   help, station                                                       
        blk       = 0                                ;beginning of the report
        blk       = MyReport->Find_Block(BLOCK=MyBlock,BLK=blk)
        WHILE(blk GT 0) DO BEGIN                                            
            params    = MyBlock->GetProperty(/ALL)   ;all parameters                      help, params,/structure                                         
            nelem     = MyBlock->GetProperty(/NELEM)  ;one parameter                                                                                      
            dliste    = MyBlock->Get_Elements_List(/CONV)
            rval      = MyBlock->Get_Elements_values(/CONV)                 
            blk        = MyReport->Find_Block(BLOCK=MyBlock,BLK=blk)            
        ENDWHILE                                                                                                                                          
        handle    = MyFile->Find_Report(REPORT=MyReport,HANDLE=handle )
    ENDWHILE
    print, station
    obj_destroy,[MyReport,MyBlock,MyFile]    
END
