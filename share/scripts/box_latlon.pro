FUNCTION box_latlon, rayon, LAT_REF=LAT_REF,LON_REF=LON_REF,MILES=MILES,KM=KM,$
                     BAND=BAND,help =help

 if (KEYWORD_SET(help)) then begin
    print,''
    print,' USAGE  : Coord = BOX_LATLON( rayon, LAT_REF=LAT_REF,$'
    print,' LON_REF=LON_REF[,MILES=MILES][,KM=KM][, BAND=BAND]'
    print,''
    print,' PURPOSE:'
    print,'        This IDL function returns the geographical coordinates'
    print,'(latitude:[-90.,90.],longitude:[-180.,180.]) of the SW and NE corners of'
    print,'a box defined by a distance (rayon) from a given reference point on'
    print,'earth.'
    print,''
    print,'INPUT         : rayon'
    print,'INPUT         : LAT_REF'
    print,'INPUT         : LON_REF'
    print,'KeyWord MILES : input rayon is in MILES'
    print,'KeyWord KM    : input rayon is in KM'
    print,'KeyWord BAND  : the coordinates of the box define a band of latitude. lon0 and'
    print,'                  lon1 are forced to -180 and 180.' 
    print,''
    print,'RETURNS  4 elemenst array [lat0,lon0,lat1,lon1]'
    print,''
    print,'             x-------x (lat1,lon1)'
    print,'             |       |'
    print,'             |   X LAT_REF,LON_REF'
    print,'             |       |'
    print,' (lat0,lon0) x-------x'

    return,0
  endif

 distance = rayon

IF KEYWORD_SET(MILES) AND KEYWORD_SET(KM) THEN  BEGIN
    print, 'ATTENTION! Les parametres MILES et KM sont incompatibles' & exit
ENDIF


distance=FLOAT(distance)
IF KEYWORD_SET(MILES) THEN  distance=distance/0.6213712d-3
IF KEYWORD_SET(KM)    THEN  distance=distance*1000.0

r_earth = 6378206.4d0 ; rayon de la terre en metres
IF double(distance ) GE r_earth THEN BEGIN
   message,'ATTENTION : distance entree superieure au rayon de la terre.',/INFORMATIONAL
   return,-1
ENDIF


;; traitement des latitudes
IF NOT KEYWORD_SET(LAT_REF) THEN LAT_REF=0.0 ELSE LAT_REF=FLOAT(LAT_REF)
dlat=(asin(distance/r_earth)*180.0d0/!dpi)

lat0=LAT_REF-dlat
lat0=((lat0) LT -90.) ? -90.0 : lat0
lat1=LAT_REF+dlat
lat1=((lat1) GT  90.) ?  90.0 : lat1

;; traitement des longitudes
IF KEYWORD_SET(BAND) THEN BEGIN

    lon0=-180.0 & lon1=180.0

ENDIF ELSE BEGIN

    IF NOT KEYWORD_SET(LON_REF) THEN LON_REF=0.0 ELSE LON_REF=FLOAT(LON_REF)

    mx_lat=MAX(ABS([lat0,lat1]))
    radius=r_earth*cos(mx_lat*!dpi/180.0)
k=distance/radius
    IF ((radius LE 1.0e-7) OR k GT 1.) THEN BEGIN
        dlon=180. & LON_REF=0.0
    ENDIF ELSE BEGIN
        dlon=(asin(k)*180.0/!dpi)
    ENDELSE

    lon0=LON_REF-dlon
    lon0=((lon0) LT -180.) ? -180.0 : lon0
    lon1=LON_REF+dlon
    lon1=((lon1) GT  180.) ?  180.0 : lon1


ENDELSE

coord=[lat0,lon0,lat1,lon1]

RETURN,coord
;return
;             x-------x (lat1,lon1)
;             |       |
;             |   X LAT_REF,LON_REF
;             |       |
; (lat0,lon0) x-------x
;
;
END;____________________________________________________________

