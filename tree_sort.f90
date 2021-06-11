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

program tree_sort
! Sorts a file of integers by building a
! tree, sorted in infix order.
! This sort has expected behavior n log n,
! but worst case (input is sorted) n ** 2.

   use tree_sort_module
   
   interface
       real function random_uniform(a,b)
       implicit none
       real , intent(in) :: a, b
       end function random_uniform
   end interface

   implicit none

   type (binary_tree) :: t  ! A tree
   integer            :: i, j, number
   real               :: x
   
   ! Oracle F95
   open (unit=1, file='input.txt')
   do i = 1, 10
       x = random_uniform(0.0, 1.0)
       write (1, *) floor(x * 100)
   end do
   close (1)

   open (unit=1, file='input.txt')

   ! Start with empty tree
   do i = 1, 10
      read (unit=1, fmt=*) number
      call t % build (number) ! Put next number in tree
   end do
   close (1)


   ! Print nodes of tree in infix order
   call t % print ()

end program tree_sort

