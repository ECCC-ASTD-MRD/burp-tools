
FUNCTION Get_OMM_Stations,FILE=file, TEMPLATE=template,help=help

  if (KEYWORD_SET(help)) then begin
    print,''
    print,' USAGE :MyStations =GET_OMM_STATIONS(FILE=my_file_list )'
    print,''
    print,' INPUT: FILE'
    print,''
    print,'PURPOSE:'
    print,'       This IDL function reads a file list of stations'
    print,'       and returns a structure. '
    print,''
    print,' example of reading a list of 463 stations'
    print,''
    print,' IDL> help,MyStations,/structure'
    print,' ** Structure <100a60f8>, 4 tags, length=7412, refs=1:'
    print,' STATION         STRING    Array[463]'
    print,' LATITUDE        LONG      Array[463]'
    print,' LONGITUDE       LONG      Array[463]'
    print,' NBRE_STATIONS   LONG               463'
    print,''
    print,' MyStations.STATION[i]   --> name of the station i'
    print,' MyStations.LATITUDE[i]  --> latitude of the  station i'
    print,' MyStations.LONGITUDE[i] --> longitude of the station i'
    return,0
  endif

IF NOT(keyWord_set(TEMPLATE)) then template = OMM_Template_structure()

data=read_ascii(file,TEMPLATE=template, COUNT=NBENREG)

north           = WHERE(STRMID(data.latitude,0,1,/REVERSE_OFFSET) EQ 'N',$
                  Count, Complement=south, Ncomplement=CountSouth)
;Help, 'count=',count
;Help, 'CountSouth   =',countsouth

; south           = WHERE(STRMID(data.latitude,0,1,/REVERSE_OFFSET) EQ 'S',$
; count)

latitude        = FIX(STRMID(data.latitude,0,4))
latitude        = MIN_TO_DECIMAL(latitude)
iF Count GT 0 THEN latitude[north] = 9000 + latitude[north]
iF CountSouth GT 0 THEN latitude[south] = 9000 - latitude[south]

west            = WHERE(STRMID(data.longitude,0,1,/REVERSE_OFFSET) EQ 'W')

longitude       = FIX(STRMID(data.longitude,0,5))
longitude       = MIN_TO_DECIMAL(longitude)
longitude[west]  = 36000 - longitude[west]

data.station    =data.station + string(REPLICATE(32B,4))
data.latitude   = Temporary(latitude)
data.longitude  = Temporary(longitude)
data = {station:data.station,latitude:LONG(data.latitude),$
        longitude:LONG(data.longitude),nbre_stations:LONG(NBENREG)}

RETURN,data
END;-------------------------------------------------------------------


