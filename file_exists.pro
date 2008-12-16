;+
; NAME:
;	file_exists
;
; PURPOSE:
;	tests, if a file exists
;   
; CATEGORY:
;	input/output
;
; CALLING SEQUENCE:
;	ok=file_exists(filename)
;
; EXAMPLE:
;
; INPUTS:
;	filename: string, the filename 
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;	ok:	=1 if file exists, 0 else
;
; COMMON BLOCKS:
;	
; SIDE EFFECTS:
;	
; RESTRICTIONS:
;	
; PROCEDURE:
;	
; MODIFICATION HISTORY:
;
; David N. Bresch, 950101
;-

FUNCTION file_exists,filename

DUM = findfile(expand_path(filename), COUNT=CNT)

return,cnt

end
