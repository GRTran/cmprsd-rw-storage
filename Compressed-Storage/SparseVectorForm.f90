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
  integer, parameter                           :: start_elem_array = 1

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

  !------ class constructor ------!
  function constructor(max_elems) result(new_sparse_vector)
    implicit none
    integer, intent(in)                                 ::    max_elems
    type(sparseVector)                                  ::    new_sparse_vector

    ! assign the maximum number of elements
    new_sparse_vector%vals = dblVector(max_elems)
    new_sparse_vector%cols = intVector(max_elems)
  end function constructor

  !------ return a value in sparse matrix given a column ------!
  function getSVValue(this, col_in) result(val_out)
    implicit none
    integer, intent(in)               :: col_in
    class(sparseVector), intent(in)   :: this
    double precision                  :: val_out

    ! error checks
    if (col_in == 0) stop 'Error SVF: No zero column.'
    ! if the column is not present, the value equals 0
    if (.NOT.any(this%cols%getElementArray(start_elem_array,this%cols%getPopulated()) == col_in)) then
      val_out = 0; return
    endif
    ! find the value out
    val_out = this%vals%getElement(count(col_in >= this%cols%getElementArray(start_elem_array,this%cols%getPopulated())))
  end function getSVValue

  !------ add element to sparse matrix ------!
  subroutine addSVEl(this, col_in, val_in)
    implicit none
    integer                                         ::    position
    integer, intent(in)                             ::    col_in
    double precision, intent(in)                    ::    val_in
    class(sparseVector), intent(inout)              ::    this

    ! check whether the column has already been assigned.
    if (col_in == 0) stop 'Error SVF: Cannot add zero column.'
    if (any(this%cols%getElementArray(start_elem_array,this%cols%getPopulated()) == col_in)) stop 'Error SVF: Column already assigned.'
    ! find column location of where value shall be added
    position = count(col_in > this%cols%getElementArray(start_elem_array,this%cols%getPopulated())) + 1
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
    if (.NOT.any(this%cols%getElementArray(1,this%cols%getPopulated()) == col_in)) stop 'Error SVF: Column index not found.'
    ! find the position of the column
    position = count(col_in >= this%cols%getElementArray(1,this%cols%getPopulated()))
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
    max_col_index = maxval(this%cols%getElementArray(start_elem_array,this%cols%getPopulated()))
    ! initialise the output value
    val_out = 0
    ! perform a matrix multiplication of a sparse vector by an input vector
    do i=1, max_col_index
      val_out = val_out + this%getSVValue(i)*vec_in(i)
    enddo
  end function multDblVecSV

end module SparseVectorForm
