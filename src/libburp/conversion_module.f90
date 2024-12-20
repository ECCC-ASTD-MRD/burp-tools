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

function STR_TO_UPPER(string_in) result(string_out)
!
implicit none
character (len=*),intent(in):: string_in
character (len =Len(string_in)) :: string_out
integer                     :: len_string,m,n
character (len=26)  :: low = 'abcdefghijklmnopqrstuvwxyz', &
                       up  = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
len_string = LEN(string_in)
string_out = string_in
if (len_string == 0) return
do m = 1,len_string 
        do n = 1, 26
          if ( string_out(m:m) .eq. low(n:n) ) string_out(m:m) = up(n:n)
        end do
end do

return
end function

function STR_TO_LOWER(string_in) result(string_out)
!
implicit none
character (len=*),intent(in):: string_in
character (len =Len(string_in)) :: string_out
integer                     :: len_string,m,n
character (len=26)  :: low = 'abcdefghijklmnopqrstuvwxyz', &
                       up  = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
len_string = LEN(string_in)
string_out = string_in
if (len_string == 0) return
do m = 1,len_string 
        do n = 1, 26
          if ( string_out(m:m) .eq. up(n:n) ) string_out(m:m) = low(n:n)
        end do
end do

return
end function

function STR_IS_NUMERIC(string_in,LR_TRIM) result(numeric)
!
implicit none
character (len=*),intent(in):: string_in
logical, optional,intent(in):: LR_TRIM ! remove left and Right blks to test
integer                     :: len_string
character (len=10),parameter:: sym_digits = '0123456789'
character (len =Len(string_in)):: string_tmp
logical             :: numeric
numeric = .FALSE.
len_string = LEN(string_in)
if (LEN(TRIM(string_in)) == 0) return
string_tmp = string_in
if (present(LR_TRIM)) then
    if (LR_TRIM) then
        string_tmp = ADJUSTL(string_tmp)
        if (Verify(TRIM(string_tmp),sym_digits) == 0) numeric = .TRUE. 
    else
        if (Verify(string_tmp,sym_digits) == 0) numeric = .TRUE. 
    endif
else
    if (Verify(string_tmp,sym_digits) == 0) numeric = .TRUE. 
endif
return
end function

end  module  CONVERSION_TO_STRING

!! This module procedure implements automatic conversion from integer to character on assignment.
!! It also implements automatic conversion from character string to integer on assignment,
!! and conversion from character to real, on assignment.
!! The module overloads the new conversions on the assignment operator (=).
module  CONVERSION
   use App
   implicit none
   public :: INTEGER_TO_CHAR, CHAR_TO_INTEGER, CHAR_TO_FLOAT
   public :: assignment (=)

   interface  assignment (=)
      module  procedure  INTEGER_TO_CHAR
      module  procedure  CHAR_TO_INTEGER
      module  procedure  CHAR_TO_FLOAT
   end  interface

contains

   !! This subroutine converts an integer K to character form, storing the character
   !! equivalent left adjusted in the variable C.
   !! If C is not long enough, the conversion is terminated with an error message.
   subroutine  INTEGER_TO_CHAR (C, K)
      !! INCOMING: C = a character variable to receive the converted integer;
      !!           K = the integer to be converted to a character string.
      character (len=*), intent (out) :: C
      integer, intent (in)            :: K

      character (len=25)              :: B
      integer                         :: J

      write (unit=B, fmt="(i25)" ) K              !! Convert to CHARACTER.

      do J = 1, len(B)                            !! Loop to find the first non-blank character.
         if (B(J:J) /= " ") then
            exit
         end  if
      end  do

      if (len_trim(B(J: )) > len(C)) then
         WRITE(app_msg,*) "Character variable is not long enough to receive the converted integer ", K
         call app_log(APP_WARNING,app_msg)
         WRITE(app_msg,*) "The variable on the left of the assignment has a length of ", len(C)
         call app_log(APP_WARNING,app_msg)
         stop
      end  if
      C = B(J: )                                  !! Left adjusts the digits in C.
   end  subroutine  INTEGER_TO_CHAR

   !! This subroutine converts a character string to an integer.
   subroutine  CHAR_TO_INTEGER (K, C)
      !! INCOMING: K = an integer variable to receive the converted character string;
      !!           C = the character variable to be converted to an integer.
      integer, intent (out)          :: K
      character (len=*), intent (in) :: C

      integer                        :: L

      L = len (C)
      if (L < 1) then
         call app_log(APP_ERROR,"Character value has null length.  Cannot convert to integer")
         stop
      end  if
      read (unit=C, fmt=*) K
   end  subroutine  CHAR_TO_INTEGER

   !! This subroutine converts a character string to a floating-point value.
   subroutine  CHAR_TO_FLOAT (A, C)
      !! INCOMING:   A  = a floating=point variable to receive the converted character string;
      !!       C  = the character variable to be converted to a floating-point.
      real, intent (out)             :: A
      character (len=*), intent (in) :: C

      integer                        :: L

      L = len (C)
      if (L < 1) then
         call app_log(APP_ERROR,"Character value has null length.  Cannot convert to floating-point")
         stop
      end  if

      read (unit=C, fmt=*) A
   end  subroutine  CHAR_TO_FLOAT

end  module  CONVERSION
!! Figure 4.23  Functions to convert numeric values (including COMPLEX) to a string.

MODULE CONVERSION_MODULE
    use DECIMAL_DIGITS
    use CONVERSION_TO_STRING
    use CONVERSION
END MODULE CONVERSION_MODULE
