
function colocation_points_select,struc1,struc2,indices1,indices2,rayon =rayon,help=help


 if (KEYWORD_SET(help)) then begin
    print,''
    print,' USAGE  : Status = COLOCATION_POINTS_SELECT(struct_array1,struct_array2,$'
    print,'                    subscripts1,subscripts2 [,RAYON=your_distance] )'
    print,' PURPOSE:'
    print,'        This IDL function finds the subset of  two input vectors of'
    print,'subscripts of two geographical position vectors.  The output subscripts'
    print,'of respective position vectors are those that are mutually located'
    print,'within a certain distance(rayon) on earth.
    print,''
    print,'INPUT         :','struct_array1 position vector'
    print,'INPUT         :','struct_array2 position vector'
    print,' two (2) tags of an element sructure should be LAT and LON' 
    print,' and the geographical coordinates -90<= lat <= 90 , -180 <= lon <= 180'
    print,''
    print,'INPUT         :','rayon is in KM'
    print,'INPUT/OUTPUT  :','subscripts1 of struct_array1'
    print,'INPUT/OUTPUT  :','subscripts2 of struct_array2'
    print,'RETURNS status =  1 if position vectors are mutually located'
    print,'        status = -1 otherwise'
    return,0
  endif

; on s'attend a 4 arguments positionnels
  nombre_params = N_Params()

  if  nombre_params NE 4 then begin
     print,'nombre arguments positionnels non correct'
     return,-1
   endif

; On s'attend a un vecteur d'indices sinon retourne le status -1

    if size(indices1,/n_dimensions) GT 0 then $
                 dim_indices1  = n_elements(indices1) else return,-1

; On s'attend a un vecteur d'indices sinon sinon retourne le status -1

    if size(indices2,/n_dimensions) GT 0 then  $
                 dim_indices2  = n_elements(indices2) else return,-1

if dim_indices2 GT dim_indices1 THEN BEGIN
   err = colocation_points_select(struc2,struc1,indices2,indices1,rayon =rayon)
   return, err
ENDIF

; on s'attend a une distance sinon assigner une valeur par defaut de 100km

  if (KeyWord_Set(rayon)) then begin
    distance_mtr = 1000. * rayon
  endif else begin
    distance_mtr =100000.     ; 100km
  endelse



; i est l'indice pour parcourir les points des donnees de  la famille struc2
     i  = 0

; distances est vecteur qui contient toutes les distances des point j de struct1
; par rapport a un point i de struct2

     distances    =  MAKE_ARRAY(dim_indices1,  /FLOAT, VALUE = distance_mtr+10.)

     indices1_out = [ dim_indices1 ]
     indices2_out = [ dim_indices2 ]

; j est l'indice pour parcourir les points des donnees de  la famille struc1

     while ( i LT dim_indices2 ) do begin

     latlon = box_latlon(distance_mtr, LAT_REF=struc2[indices2[i]].lat,LON_REF=struc2[indices2[i]].lon)
     in_box = WHERE((struc1[indices1].lat GE latlon[0])  AND (struc1[indices1].lat LE latlon[2]) AND (struc1[indices1].lon GE latlon[1])  AND (struc1[indices1].lon LE latlon[3]) ,count_in, COMPLEMENT=count_out)

if count_in GT 0 Then begin
       for j=0,count_in -1 do begin
        distances[in_box[j]] = map_2points_new(struc1[indices1[in_box[j]]].lon,struc1[indices1[in_box[j]]].lat, struc2[indices2[i]].lon,struc2[indices2[i]].lat,/METERS)
       endfor

       valides = where(distances LE distance_mtr,count)
                IF count GT  0 THEN BEGIN
                  indices1_out =[valides,indices1_out]
      indices1_out =  indices1_out[UNIQ( indices1_out,SORT( indices1_out))]
                  indices2_out =[i,indices2_out]
                ENDIF
endif
       i=i+1

     endwhile

; s'il n ya pas de collocation retourne status -1

     IF  N_elements(indices2_out) GT 1 then $
              indices2_out = indices2_out[0:N_elements(indices2_out)-2] else return, -1

; s'il n ya pas de collocation retourne status -1

     IF  N_elements(indices1_out) GT 1 then $
              indices1_out = indices1_out[0:N_elements(indices1_out)-2] else return, -1

      ;indices1_out =  indices1_out[UNIQ( indices1_out,SORT( indices1_out))]

; on met a jour les vecteurs de collocations

      indices1 =indices1[indices1_out]
      indices2 =indices2[indices2_out]

; on retourne le status ok

      return,1
end;;__________________________________________________________________________________

