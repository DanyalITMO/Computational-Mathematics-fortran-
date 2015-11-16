! Во входном файле F1 находится исходный код программы на Fortran, а в файле F2 —
! тот же код, но с добавлением некоторых строк. Сформировать файл из новых
! строк, пометив их в начале как «++ ».

program reference_lab_2

   use Source_Process
   use Source_IO

   implicit none
   character(:), allocatable :: ifile, ofile

   ifile = "../data/input.txt"
   ofile = "output.txt"


   call input(ifile)
   call decomposition
   call inverse()
   call output(ofile)
end program reference_lab_2
