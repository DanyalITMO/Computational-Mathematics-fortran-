module Source_IO
  use Source_Process

   implicit none

contains
   ! Чтение исходного кода. 
!!!!!!!!!!!!!!!!!!!!in
subroutine input(INPUTFILE)
   integer :: i, j
	character(*),parameter :: ENC = "UTF-8"
	character(*), intent(in)      :: INPUTFILE
   integer  :: in

   open (file=INPUTFILE, encoding=ENC, newunit=in)
	do i = 1, 3
	   read(in, *) (a(i, j), j = 1, 3)
	end do
	close(in)

end subroutine  input


subroutine output(OutputFile)
   integer :: i, j
	character(*),parameter :: ENC = "UTF-8"
	character(*), intent(in)      :: OutputFile 
   integer  :: Out

   open (file=OUTPUTFILE, encoding=ENC, newunit=out)

      write(out, *) "A"	         	
         do i = 1, 3
            write(out, *) (a(i, j), j = 1, 3)
         end do

      write(out, *) "L"	         	
         do i = 1, 3
            write(out, *) (l(i, j), j = 1, 3)
         end do
      
      write(out, *) "D"	   
         do i = 1, 3
            write(out, *) (d(i, j), j = 1, 3)
         end do

      write(out, *) "U"	   
         do i = 1, 3
            write(out, *) (u(i, j), j = 1, 3)
         end do
	   
	close(out)

end subroutine  output

end module Source_IO 
