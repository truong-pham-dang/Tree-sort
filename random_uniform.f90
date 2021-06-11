!
! To change this license header, choose License Headers in Project Properties.
! To change this template file, choose Tools | Templates
! and open the template in the editor.
!

!     
! File:   random_uniform.f90
! Author: truong
!
! Created on June 11, 2021, 10:52 AM
!

! assuming a < b

real function random_uniform(a,b)
   implicit none
   real ,intent(in) :: a,b
   real :: u
   call random_stduniform(u)
   random_uniform = (b-a) * u + a
end function random_uniform

subroutine random_stduniform(u)
   implicit none
   real,intent(out) :: u
   real :: r
   call random_number(r)
   u = 1 - r
end subroutine random_stduniform

