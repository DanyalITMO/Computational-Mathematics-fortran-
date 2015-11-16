module environment
   use ISO_Fortran_Env

   implicit none
    
   integer, parameter      :: I_ = int16                             ! Разновидность типа для целочисленных переменных.
   integer, parameter      :: R_ = real32                            ! Разновидность типа для вещественных переменных.
   integer, parameter      :: C_ = R_                                ! Разновидность типа для компексных переменных.
   integer, parameter      :: CH_ = Selected_Char_Kind("ISO_10646")  ! Разновидность типа для символов.
   character(*), parameter :: E_ = "UTF-8"                           ! Кодировка файлов.

   interface operator (//)
      module procedure IntPlusString
      module procedure StringPlusInt
   end interface

contains

   pure function IntPlusString(int, str) result(res)
      integer, intent(in)                                   :: int
      character(*), intent(in)                              :: str
      character(len(str)+Floor(Log10(Real(int, real64)))+1) :: res

      write (res,'(i0, a)') int, str
   end function IntPlusString

   pure function StringPlusInt(str, int) result(res)
      character(*), intent(in)                               :: str
      integer, intent(in)                                    :: int
      character(len(str)+Floor(Log10(Real(int, real64)))+1)  :: res

      write (res,'(a, i0)') str, int
   end function StringPlusInt

end module environment

! Нечто.
