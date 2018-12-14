program subtester
  use IntNLengthVector
  implicit none

  type (intVector)                      ::    int_vec
  integer                               ::    column, position
  column = 5
  position = 1
  ! assign a maximum size of array
  int_vec = intVector(5)
  ! try and call values that haven't been set (expect 0 output)
  write(*,*) 'not set elements:', int_vec%getElement(2), int_vec%getElement(5)
  ! add an element in position 1 value 5
  call int_vec%addElement(column, position)
  ! add another element in position 2 value 1
  position = 3
  column = 1
  call int_vec%addElement(column, position)
  ! write an element out that has been set position 3 will go to 2 because 2 isn't filled
  write(*,*) 'set element: ', int_vec%getElement(1), int_vec%getElement(3)

  ! write out the number of populated elements
  write(*,*) 'populated elements:', int_vec%getPopulated()

  ! get an array of values

end program
