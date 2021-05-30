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
    public :: insert, print_tree, node

    type, public :: node
        private
        integer :: value
        type (node), pointer :: left => null(), &
                                right => null()
    end type node

contains

   recursive subroutine insert (t, number)

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

   implicit none

   type (node), pointer :: t  ! A tree
   integer, parameter   :: seed = 86456
   integer              :: i, j, number

   call srand(seed)
   open (unit=1, file='input.txt')
   do i = 1, 5
     write (1, *) irand(i)
   end do
   do j = 10, 6, -1
     write (1, *) irand(j)
   end do
   close (1)

   nullify(t)

   open (unit=1, file='input.txt')

   ! Start with empty tree
   do i = 1, 10
      read (unit=1, fmt=*) number
      call insert (t, number) ! Put next number in tree
   end do
   close (1)


   ! Print nodes of tree in infix order
   call print_tree (t)

end program tree_sort

