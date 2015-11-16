module Source_Process
  
   implicit none

real, DIMENSION(3,3)    :: a, l, d, u, A_inverse
real :: e(3,3)= RESHAPE((/1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0/),SHAPE(e))
REAL :: x(2,2) 
integer :: N = 3


contains


!!!!!!!!!!!!!!!!!!!!! inverse
   subroutine inverse()
      integer :: i, j
      REAL :: x(3), y(3), z(3)

      do i = 1, N
       y(1) = (e(1, i) / l(1, 1))
       y(2) = (-1 * (l(2, 1) * y(1)) +  e(2, i)) / l(2, 2)
       y(3) = (-1 * (l(3, 1) * y(1) + l(3, 2) * y(2)) + e(3, i)) / l(3, 3)
          
          do j = 1, N
            z(j) = y(j) / d(j, j)
          end do

       x(3) = z(3) / u(3, 3)
       x(2) = (-1 * u(2, 3) * x(3) + z(2)) / u(2, 2)
       x(1) = (-1 * (u(1, 3) * x(3) + u(1, 2) * x(2)) + z(1)) / u(1, 1)

      A_inverse(3, i) = x(3)
      A_inverse(2, i) = x(2)
      A_inverse(1, i) = x(1)

      end do
       
   end subroutine inverse

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

   end function summ




   
end module Source_process
