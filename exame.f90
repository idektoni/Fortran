! PROGRAMA FOTRAN
program exame
implicit none
integer :: n, m, i
real,allocatable :: x(:)

print *, "n="
read *, n
allocate(x(n))

call subprog(x,n,m)

print *, x

deallocate(x)
end program exame

subroutine subprog(x,n,m)

integer,intent(in) :: n
real, intent(in) :: x
integer,intent(out) :: m

do i = 1 , n
  x(i) = i
  do j = 1, n
    m = x(:),j
  end do
end do
  print *, "m=", m
end subroutine subprog
  
