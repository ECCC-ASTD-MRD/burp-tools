!! These modules implement conversion of integer, floating-point, and complex values to a string.
module  DECIMAL_DIGITS
   implicit  none
   character (len=1), parameter, public :: Ten_Digits (0:9) = (/ "0", "1", "2",   &
                                             "3", "4", "5", "6", "7", "8", "9" /)
   integer, parameter, public       :: Double = selected_real_kind (14)
end  module  DECIMAL_DIGITS

module  CONVERSION_TO_STRING

   interface CVT
      module procedure ICVT
      module procedure FCVT
      module procedure F2CVT
      module procedure CCVT
      module procedure C2CVT
   end  interface

contains

!! This function converts a binary integer to a string.
!! The result string consists only of digits and a sign.
!! The result string does not contain any blank characters.
!! The function allows for 16-bit integers up to 64-bit integers as the default kind.
!! It allows the full range of integers (e.g. -2147483648 through 2147483647
!! on a 32-bit word machine).
!! Example: to print the value of INTEGER variable K (whose value is 30) in a sentence:
!! print *, "There are " // CVT (K) // " days in September."
!! prints
!! There are 30 days in September.
function ICVT (K) result (String)
   !! INCOMING: K = an integer to be converted to string form.
   !! RETURNS:  the string version of the argument.
   use  DECIMAL_DIGITS
   integer, intent (in)            :: K

   !! The following elaborate declaration is required to define a string
   !! having just enough positions to hold all the digits of the integer, and the sign character.
   !! It contains constants no greater than 10000, so as to be acceptable
   !! on machines having a default integer kind of 16 bits or more.
   character (len = 1 +                   &       !! For a one-digit number;
       min (abs(K/10),              1) +  &       !! For a 2-digit no. yields 1;
       min (abs(K/100),                         1) + & !! For a 3-digit no. yields 1;
       min (abs(K/1000),                        1) + & !! For a 4-digit no. yields 1;
       min (abs(K/10000),                       1) + & !! For a 5-digit no. yields 1;
       min (abs(K/10000)/10,                    1) + & !! For a 6-digit no. yields 1;
       min (abs(K/10000)/100,                   1) + & !! For a 7-digit no. yields 1;
       min (abs(K/10000)/1000,                  1) + & !! For an 8-digit no. yields 1;
       min (abs(K/10000)/10000,                 1) + & !! For a  9-digit no. yields 1;
       min (abs(K/10000)/10000/10,              1) + & !! For a 10-digit no. yields 1.
       min (abs(K/10000)/10000/100,             1) + & !! For 11 digits;
       min (abs(K/10000)/10000/1000,            1) + & !! For 12 digits;
       min (abs(K/10000)/10000/10000,           1) + & !! For 13 digits;
       min (abs(K/10000)/10000/10000/10,        1) + & !! For 14 digits;
       min (abs(K/10000)/10000/10000/100,       1) + & !! For 15 digits;
       min (abs(K/10000)/10000/10000/1000,      1) + & !! For 16 digits;
       min (abs(K/10000)/10000/10000/10000,     1) + & !! For 17 digits;
       min (abs(K/10000)/10000/10000/10000/10,  1) + & !! For 18 digits;
       min (abs(K/10000)/10000/10000/10000/100, 1) - & !! For 19 digits.
       min (max(K, -1), 0) )       :: String           !! For a negative value, yields -1,
                                                       !! otherwise yields 0.
   integer                         :: J, LK, Quotient, Digit

   LK = K
   J = len(String)
   !! A loop to form the decimal digits starting with the least significant.
   do
      Quotient = LK / 10
      Digit = abs (LK - Quotient*10)              !! Extract the next least-significant digit.
      LK = Quotient
      String(J:J) = Ten_Digits(Digit)             !! Convert the binary remainder to a character.
      J = J - 1
      if (LK == 0) then
         exit
      end  if
   end  do

   if (K < 0) then                                !! For a negative value, insert the sign.
      String (1:1) = "-"
   end  if

end  function  ICVT

!! This function converts the floating-point dummy argument to a left-adjusted string.
!! The number will have <Places> digits after the decimal point.
function  FCVT (F, Places) result (String)
   !! INCOMING: F      = a floating-point value;
   !!           Places = the number of digits required after the decimal point.
   !! RETURNS:  the argument F in fixed-point format, as a left adjusted string.
   use  DECIMAL_DIGITS

   real, intent (in)               :: F
   integer, intent (in)            :: Places

   character (len=30)              :: String

   String = F2CVT (real(F, kind=Double), Places)

end  function  FCVT

!! This function converts the floating-point dummy argument to a left-adjusted string.
!! The number will have <Places> digits after the decimal point.
function  F2CVT (F, Places) result (String)
   !! INCOMING: F      = a floating-point value;
   !!           Places = the number of digits required after the decimal point.
   !! RETURNS:  the argument F in fixed-point format, as a left adjusted string.
   use  DECIMAL_DIGITS

   real (kind = Double), intent (in) :: F
   integer, intent (in)            :: Places

   character (len=30)              :: String

   integer                         :: J, LPlaces, Digit
   real (kind=Double)              :: D, Quotient

   D = F
   do J = 1, Places                               !! Scale D, that is, multiply by 10**Places.
      D = D * 10                                  !! Avoids using 10**Places which may overflow,
   end  do                                        !! and avoids 10.0**Places which could be inaccurate.
   D = anint(D)                                   !! Round off, and truncate the fraction.
!! {XE "ANINT built-in function"}
   if (Places > len(String)-1) then               !! Error: too many digits after the decimal point.
      String = repeat("*", len(String) )
      return
   end if

   LPlaces = Places
   String = " "
   J = len(String)
   !! A loop to form the decimal digits starting with the least significant.
   do
      if (LPlaces == 0) then                      !! Insert a decimal point.
         String(J:J) = "."
         J = J - 1
      end  if
      Quotient = aint (D / 10.0 + sign(0.01_Double, D) ) !! Round off in case of inexact division.
      Digit = abs (D - Quotient*10.0)             !! Extract the next least-significant digit.
      D = Quotient
      String(J:J) = Ten_Digits(Digit)             !! Convert the binary remainder to character.
      J = J - 1
      LPlaces = LPlaces - 1
      if (D == 0) then
         exit
      end  if
   end  do

   !! If the decimal point has not yet been inserted, this loop inserts it,
   !! and ensures that zeros are forced after the decimal point.
   if (LPlaces >= 0) then
      do
         if (LPlaces > 0) then
            String(J:J) = "0"
            J = J - 1
         end if
         LPlaces = LPlaces - 1
         if (LPlaces <= 0) then                   !! Insert a decimal point.
            String(J-1:J) = "0."
            J = J - 2
            exit
         end  if
      end  do
   end  if

   if (F < 0) then
      String (J:J) = "-"
   end  if

   String = adjustl (String)

end  function  F2CVT

!! Given a complex value, this function returns it in fixed-point form as a left-adjusted string.
!! The number of digits after the decimal point is specified as the second argument.
!! The example calling sequence: print *, trim (CCVT(Z, 3)) prints Z as 1.234 + j5.678
function CCVT (Z, Places) result (String)
   !! INCOMING: Z      = a complex floating-point value;
   !!           Places = the number of digits required after the decimal point.
   !! RETURNS:  the complex number Z in fixed-point electrical engineering form,
   !!           as a left-adjusted string, e.g., 1.234 - j9.876.
   use  DECIMAL_DIGITS
   complex, intent (in)            :: Z
   integer, intent (in)            :: Places

   character (len=70)              :: String

   character (len=30)              :: S1, S2

   S1 = F2CVT (real(Z, kind=Double), Places)      !! Convert the real part to a string.
   S2 = F2CVT (real (aimag(Z), kind=Double), Places)  !! Convert the imaginary part to a string.

   if (S2(1:1) == "-") then                       !! The imaginary part is negative.
      String = S1(1:len_trim(S1)+1) // S2(1:1) // " j" // S2(2: )
   else if (S2(1:1) == "*") then                  !! The field contains asterisks indicating that
      String = trim(S1) // S2                     !! the value is too large.
   else                                           !! Manufacture a plus (+) sign.
      String = trim(S1) // " + j" // S2
   end  if
end  function  CCVT

!! Given a complex value, this function returns it in fixed-point form as a left-adjusted string.
!! The number of digits after the decimal point is specified as the second argument.
!! The example calling sequence: print *, trim (CCVT(Z, 3)) prints Z as 1.234 + j5.678
function C2CVT (Z, Places) result (String)
   !! INCOMING: Z      = a complex floating-point value;
   !!           Places = the number of digits required after the decimal point.
   !! RETURNS:  the complex number Z in fixed-point electrical engineering form,
   !!           as a left-adjusted string, e.g., 1.234 - j9.876.
   use  DECIMAL_DIGITS
   complex (kind=Double), intent (in) :: Z
   integer, intent (in)            :: Places

   character (len=70)              :: String

   character (len=30)              :: S1, S2

   S1 = F2CVT (real(Z), Places)                   !! Convert the real part to a string.
   S2 = F2CVT (aimag(Z), Places)                  !! Convert the imaginary part to a string.

   if (S2(1:1) == "-") then                       !! The imaginary part is negative.
      String = S1(1:len_trim(S1)+1) // S2(1:1) // " j" // S2(2: )
   else if (S2(1:1) == "*") then                  !! The field contains asterisks indicating that
      String = trim(S1) // S2                     !! the value is too large.
   else                                           !! Manufacture a plus (+) sign.
      String = trim(S1) // " + j" // S2
   end  if
end  function  C2CVT

end  module  CONVERSION_TO_STRING

!! Figure 4.23  Functions to convert numeric values (including COMPLEX) to a string.
