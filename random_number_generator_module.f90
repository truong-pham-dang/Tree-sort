!
! To change this license header, choose License Headers in Project Properties.
! To change this template file, choose Tools | Templates
! and open the template in the editor.
!

!     
! File:   random_number_generator_module.f90
! Author: truong
!
! Created on June 11, 2021, 10:52 AM
!

module random_number_generator_module
    implicit none
    private
    
    type, public :: random_number_generator
        private
        real :: x
        contains
        procedure, public :: get_uniform_distribution
        procedure, public :: get_result
    end type random_number_generator
    
    contains
    
    real function get_result(this)
        implicit none
        class (random_number_generator) :: this
        get_result = this % x
    end function get_result
    
    subroutine get_uniform_distribution(this, a, b)
        implicit none
        class (random_number_generator) :: this
        real ,intent(in) :: a, b
        this % x = random_uniform(a, b)
    end subroutine get_uniform_distribution

    ! assuming a < b

    real function random_uniform(a, b)
        implicit none
        real ,intent(in) :: a, b
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

end module