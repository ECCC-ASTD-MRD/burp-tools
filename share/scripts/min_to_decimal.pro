FUNCTION MIN_TO_DECIMAL, Val
   deg=Val/100
   min=Val-deg*100
   dec=min/60.0
   ValDec=deg*100 + CEIL(dec*100.0)

RETURN,ValDec
END;-------------------------------------------------------------------
