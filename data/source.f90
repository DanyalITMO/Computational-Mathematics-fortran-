module environment
   use ISO_Fortran_Env

   implicit none
    
   integer, parameter      :: I_ = int16                             ! Разновидность типа для целочисленных переменных.
   integer, parameter      :: C_ = R_                                ! Разновидность типа для компексных переменных.
   character(*), parameter :: E_ = "UTF-8"                           ! Кодировка файлов.

   interface operator (//)
      module procedure IntPlusString
   end interface

contains

   pure function IntPlusString(int, str) result(res)
      integer, intent(in)                                   :: int
      character(*), intent(in)                              :: str
      character(len(str)+Floor(Log10(Real(int, real64)))+1) :: res

      write (res,'(i0, a)') int, str
   end function IntPlusString

end module environment
