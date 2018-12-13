module SparseVectorForm
  !!-------------------------------------------------------------------------!!
  !!-------------------------------------------------------------------------!!
  !! Author: Greg Jones ------------------------------------ Date: 12/11/18 -!!
  !! Intent: A Class to create and manipulate sparse vectors/arrays. --------!!
  !! Requirements: The DblNLengthVector and IntNLengthVector classes. -------!!
  !!-------------------------------------------------------------------------!!
  !! Inputs: Columns in ascending order and their corresponding values. -----!!
  !! Outputs: A sparse column vector which can be made into a sparse matrix -!!
  !!          by defining a type-array of instances of this class within ----!!
  !!          the main program. ---------------------------------------------!!
  !!-------------------------------------------------------------------------!!
  !! Revisions: -------------------------------------------------------------!!
  !! 1st (12/11/18) - Implementation of the constructor, add element and ----!!
  !!                  remove element logic as well as a means of getting a --!!
  !!                  value. ------------------------------------------------!!
  !! 2nd (19/11/18) - Bug fixes, error checks, population of columns in -----!!
  !!                  ascending order. --------------------------------------!!
  !!-------------------------------------------------------------------------!!
  !!-------------------------------------------------------------------------!!

  !--- dependencies ---!
  use DblNLengthVector
  use IntNLengthVector

  !--- variable scope ---!
  implicit none
  private
  public sparseVector

  !--- class definition ---!
  type sparseVector
    private
    type(dblVector)   ::    vals
    type(intVector)   ::    cols
  contains
    procedure, public, pass   ::    getSVValue ! inputs: sparse column, output: value
    procedure, public, pass   ::    addSVEl  ! inputs: sparse column, value, optional position, output: adjusted SM
    procedure, public, pass   ::    rmSVEl   ! inputs: position within SM, output: adjusted SM
    procedure, public, pass   ::    multDblVecSV
  end type

  interface sparseVector
    procedure constructor
  end interface

contains

  !--- class constructor ---!
  function constructor(vals_in, col_index_in) result(new_sparse_matrix)
    implicit none
    double precision, dimension(:), intent(in)    ::    vals_in
    integer, dimension(:), intent(in)             ::    col_index_in
    type(sparseVector)                            ::    new_sparse_matrix
    integer                                       ::    i

    if (size(vals_in) /= size(col_index_in)) then
      stop 'Error SVF: The values and column index arrays must be of equal size.'
    endif

    ! Perform a check to ensure the columns are added in ascending order and to
    ! ensure that no two column indices are the same
    do i=1, size(col_index_in)-1
      if ( (col_index_in(i) > col_index_in(i+1)) .or. (col_index_in(i) == col_index_in(i+1)) ) then
        stop 'Error SVF: Column index must be in ascending order with non duplicated column indices'
      endif
    enddo

    new_sparse_matrix%vals = dblVector(vals_in)
    new_sparse_matrix%cols = intVector(col_index_in)
  end function constructor

  !--- return a value in sparse matrix given a column ---!
  function getSVValue(this, col_in) result(val_out)
    implicit none
    integer, intent(in)               :: col_in
    class(sparseVector), intent(in)   :: this
    double precision                  :: val_out

    ! error checks
    if (col_in == 0) stop 'Error SVF: No zero column.'
    ! if the column is not present, the value equals 0
    if (.NOT.any(this%cols%vals == col_in)) then
      val_out = 0; return
    endif
    ! find the value out
    val_out = this%vals%vals(count(col_in >= this%cols%vals(1:this%cols%populated)))
  end function getSVValue

  !--- add element to sparse matrix ---!
  subroutine addSVEl(this, col_in, val_in)
    implicit none
    integer                                         ::    position
    integer, intent(in)                             ::    col_in
    double precision, intent(in)                    ::    val_in
    class(sparseVector), intent(inout)              ::    this

    ! check whether the column has already been assigned.
    if (col_in == 0) stop 'Error SVF: Cannot add zero column.'
    if (any(this%cols%vals == col_in)) stop 'Error SVF: Column already assigned.'
    ! find column location of where value shall be added
    position = count(col_in > this%cols%vals(1:this%cols%populated)) + 1
    ! add column and value to sparse matrix
    call this%cols%addElement(col_in, position)
    call this%vals%addElement(val_in, position)
  end subroutine addSVEl

  !--- remove an element from the sparse matrix ---!
  subroutine rmSVEl(this, col_in)
    implicit none
    integer, intent(in)                   ::    col_in
    class(sparseVector), intent(inout)    ::    this
    integer                               ::    position

    ! ensure "col_in" is in sparse matrix
    if (col_in == 0) stop 'Error SVF: Cannot remove zero column.'
    if (.NOT.any(this%cols%vals == col_in)) stop 'Error SVF: Column index not found.'
    ! find the position of the column
    position = count(col_in >= this%cols%vals(1:this%cols%populated))
    ! remove column and value from sparse matrix
    call this%cols%removeElement(position)
    call this%vals%removeElement(position)
  end subroutine rmSVEl

  !--- perform a sparse vector by non-sparse vector multiplication ---!
  function multDblVecSV(this, vec_in) result(val_out)
    implicit none
    double precision, dimension(:), intent(in)      ::    vec_in
    class(sparseVector), intent(in)                 ::    this
    double precision                                ::    val_out
    integer                                         ::    i
    integer                                         ::    max_col_index
    ! obtain the largest column number in sparse vector
    max_col_index = maxval(this%cols%vals)
    ! initialise the output value
    val_out = 0
    ! perform a matrix multiplication of a sparse vector by an input vector
    do i=1, max_col_index
      val_out = val_out + this%getSVValue(i)*vec_in(i)
    enddo
  end function multDblVecSV


end module SparseVectorForm
