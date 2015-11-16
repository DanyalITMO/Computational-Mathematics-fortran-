module Source_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.

   implicit none

real, DIMENSION(3,3)    :: a, l, d, u, a_inverse
real :: e(3,3)= RESHAPE((/1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0/),SHAPE(e))
REAL :: x(2,2) 
integer :: N = 3


contains




!!!!!!!!!!!!!!!!!!!!!decomposition
subroutine decomposition()
   
   integer :: i, j      
	do i = 1, N
	   do j = 1, N

         if(j < i) then
            l(i, j) = (a(i, j) - summ(i, j, 1)) / d(j, j)
            d(i, j) = 0
            u(i, j) = 0     
         end if           

         if(j == i) then
            l(i, j) = 1
            d(i, j) = (a(i, i) - summ(i ,j, 0))
            u(i, j) = 1
         end if 

         if(j > i) then
            l(i, j) = 0
            d(i, j) = 0
            u(i, j) = (a(i, j) - summ(i, j, 0)) / d(i, i)
         end if           
          
          
   	end do
   end do   

   end subroutine decomposition

!!!!!!!!!!!!!!!!!!!summ
function summ(i, j, f) result(res)
      integer  :: i, j, f, k 
      real     :: res
res = 0

      if(f ==1) then
         do k = 1, j
                res = res + l(i, k) * d(k, k) * u(k, j)
         end do         
      else
         do k = 1, i
                res = res + l(i, k) * d(k, k) * u(k, j)
         end do         
      end if
      
 !write(*,*)count , number, InitialCode%string,  DiffCode%string
   end function summ




   
end module Source_process
