program vectest
  use NLengthVector
  implicit none

  type (Vector) :: vec
  double precision, dimension(10) :: vals
  integer :: i
  double precision :: add = 5
  integer :: index = 11

  vals = (/ (i,i=1,10) /)

  vec = Vector(vals)
  call vec%addElement(add,index)
  write(*,*) vec%vals, size(vec%vals), vec%populated
end program
