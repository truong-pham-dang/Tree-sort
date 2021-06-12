! Copyright (c) 1994-2002 The Fortran Company
!
! Developed at Unicomp, Inc.
!
! Permission to use, copy, modify, and distribute this
! software is freely granted, provided that this notice
! is preserved.

program tree_sort
! Sorts a file of integers by building a
! tree, sorted in infix order.
! This sort has expected behavior n log n,
! but worst case (input is sorted) n ** 2.

   use tree_sort_module
   use random_number_generator_module

   implicit none

   type (binary_tree) :: t  ! A tree
   type (random_number_generator) :: rand_gen
   integer            :: i, j, number
   real               :: x
   
   ! Oracle F95
   open (unit=1, file='input.txt')
   do i = 1, 10
       call rand_gen % get_uniform_distribution(0.0, 1.0)
       x = rand_gen % get_result()
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

