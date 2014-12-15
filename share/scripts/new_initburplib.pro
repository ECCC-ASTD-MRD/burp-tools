pro new_initburplib
   print,'acces to idl_burp library'
   if (!VERSION.RELEASE GE 6.3) then begin
      print,"Appel burpprocs version 6.3"
         new_burprocs2001,63
         init_burp_objects
   endif else begin
      if (!VERSION.RELEASE GE 5.3) then begin
         print,"Appel burpprocs version 5.4"
         burprocs2001
         init_burp_objects
      endif else begin
      print,"ancien appel"
      burprocs2001
      endelse
   endelse
end
