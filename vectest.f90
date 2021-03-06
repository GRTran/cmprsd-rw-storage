program vectest
  !!-------------------------------------------------------------------------!!
  !!-------------------------------------------------------------------------!!
  !! Author: Greg Jones ------------------------------------ Date: 14/12/18 -!!
  !! Intent: A test program for the compressed row storage class and --------!!
  !!         corresponding bespoke matrix operations ------------------------!!
  !! Requirements: SparseMatrixForm.f90 and contained classes ---------------!!
  !!-------------------------------------------------------------------------!!
  !! Inputs: Row and column data --------------------------------------------!!
  !! Outputs: A compressed row storage matrix capable of performing matrix --!!
  !!          operations ----------------------------------------------------!!
  !! Revisions: -------------------------------------------------------------!!
  !! 1st (14/12/18) - Development of a general test program showing the -----!!
  !!                  compressed storage capability and 2x2 matrix ----------!!
  !!                  multiplication ----------------------------------------!!
  !!-------------------------------------------------------------------------!!
  !!-------------------------------------------------------------------------!!
  use SparseMatrixForm
  implicit none

  type (sparseMatrix) :: sm
  double precision, dimension(10) :: vals
  integer, dimension(10)            ::    col_in
  integer :: i
  double precision :: add = 5
  integer :: index = 11
  integer                  ::    rows

  !! Test File Inputs !!
  write(*,*) "Enter Num rows: "
  read(*,*) rows

  col_in = (/ (i,i=1,10) /)
  vals = (/ (i,i=1,10) /)

  sm = sparseMatrix(5)
  do i=1, rows
    call sm%addRowVals(i, col_in, vals)
  enddo
  write(*,*) sm%getValue(1,1)
  !sm%destroySM()
end program
