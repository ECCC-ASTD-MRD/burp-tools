FUNCTION OMM_Template_structure
template = {$
   VERSION         : 1.0, $
   DATASTART       : 0, $
   DELIMITER       : 32B, $
   MISSINGVALUE    : !values.f_nan, $
   COMMENTSYMBOL   : '', $
   FIELDCOUNT      : [3L],$
   FIELDTYPES      : [7,7,7],$
   FIELDNAMES      : ['Station', 'Latitude', 'Longitude'], $
   FIELDLOCATIONS  : [3,15,25], $
   FIELDGROUPS     : [0,1,2]}



RETURN,Template
END;-------------------------------------------------------------------

