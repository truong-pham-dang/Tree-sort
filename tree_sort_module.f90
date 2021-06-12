!
! To change this license header, choose License Headers in Project Properties.
! To change this template file, choose Tools | Templates
! and open the template in the editor.
!

!     
! File:   tree_sort_module.f90
! Author: truong
!
! Created on June 11, 2021, 4:22 PM
!

! Copyright (c) 1994-2002 The Fortran Company
!
! Developed at Unicomp, Inc.
!
! Permission to use, copy, modify, and distribute this
! software is freely granted, provided that this notice
! is preserved.

module tree_sort_module
    implicit none
    private

    type :: node
        private
        integer :: value
        type (node), pointer :: left => null(), &
                                right => null()
    end type node

    type, public :: binary_tree
        private
        type (node), pointer :: root => null()

        contains
            procedure :: build
            procedure :: print
    end type binary_tree

contains

    subroutine build(tree, num)
        implicit none
        class (binary_tree)    :: tree
        integer, intent(inout) :: num

        call insert(tree%root, num)

    end subroutine build

    subroutine print(tree)
        implicit none
        class (binary_tree)    :: tree

        call print_tree(tree%root)
    end subroutine print

   recursive subroutine insert (t, number)
      implicit none
      type (node),    pointer        :: t  ! A tree
      integer,        intent(inout)  :: number

      ! If (sub)tree is empty, put number at root
      if (.not. associated (t)) then
         allocate (t)
         t % value = number
      ! Otherwise, insert into correct subtree
      else if (number < t % value) then
         call insert (t % left, number)
      else
         call insert (t % right, number)
      end if

   end subroutine insert

   recursive subroutine print_tree (t)
      ! Print tree in infix order
      implicit none
      type (node), pointer :: t  ! A tree

      if (associated (t)) then
         call print_tree (t % left)
         print *, t % value
         call print_tree (t % right)
      end if

   end subroutine print_tree

end module tree_sort_module


