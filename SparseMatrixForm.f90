module SparseMatrixForm
  use SparseVectorForm
  implicit none

  private
  public sparseMatrix

  !--- class definition ---!
  type sparseMatrix
    private
    type(sparseVector), allocatable, dimension(:)         ::    sparse_vec
  contains
    procedure, public, pass   ::    getValue ! inputs: sparse column, output: value
    procedure, public, pass   ::    addRowVals
    procedure, public, pass   ::    addSMEl
    procedure, public, pass   ::    rmSMEl
    procedure, public, pass   ::    multDblVecSM
    procedure, public, pass   ::    destroySM
  end type

  interface sparseMatrix
    procedure constructor
  end interface

contains

  !--- class constructor ---!
  function constructor(num_rows) result(new_sparse_matrix)
    implicit none
    integer, intent(in)                           ::    num_rows
    type(sparseMatrix)                            ::    new_sparse_matrix
    integer                                       ::    i

    ! error checks
    if (num_rows < 1) stop 'Error SMF: Number of rows must be greater than 0'
    ! allocate the number of rows to the sparse vector
    allocate(new_sparse_matrix%sparse_vec(num_rows))
    ! assign row values to sparse matrix
  end function constructor

  function getValue(this, row_in, col_in) result(val_out)
    implicit none
    integer, intent(in)                           ::    row_in
    integer, intent(in)                           ::    col_in
    class(sparseMatrix)                           ::    this
    double precision                              ::    val_out

    ! error checks
    if (row_in == 0) stop 'Error SMF: Cannot assign a zero row'
    if (row_in > size(this%sparse_vec)) stop 'Error SMF: Index exceeds assigned size'
    ! obtain the correct value
    val_out = this%sparse_vec(row_in)%getSVValue(col_in)
  end function getValue

  subroutine addRowVals(this, row_in, col_in, vals_in)
    implicit none
    integer, intent(in)                           ::    row_in
    double precision, dimension(:), intent(in)    ::    vals_in
    integer, dimension(:), intent(in)             ::    col_in
    class(sparseMatrix)                           ::    this

    ! error checks
    if (row_in == 0) stop 'Error SMF: Cannot assign a zero row'
    if (row_in > size(this%sparse_vec)) stop 'Error SMF: Index exceeds assigned size'
    ! assign a row a particular set of values and corresponding indices
    this%sparse_vec(row_in) = sparseVector(vals_in,col_in)
  end subroutine addRowVals

  !--- add element to sparse matrix ---!
  subroutine addSMEl(this, row_in, col_in, val_in)
    implicit none
    integer, intent(in)                             ::    row_in
    integer, intent(in)                             ::    col_in
    double precision, intent(in)                    ::    val_in
    class(sparseMatrix), intent(inout)              ::    this

    ! error checks
    if (row_in == 0) stop 'Error SMF: Cannot assign a zero row'
    if (row_in > size(this%sparse_vec)) stop 'Error SMF: Index exceeds assigned size'
    ! add a single column index and value to a row
    call this%sparse_vec(row_in)%addSVEl(col_in, val_in)
  end subroutine addSMEl

  !--- remove an element from the sparse matrix ---!
  subroutine rmSMEl(this, row_in, col_in)
    implicit none
    integer, intent(in)                   ::    row_in
    integer, intent(in)                   ::    col_in
    class(sparseMatrix), intent(inout)    ::    this

    ! error checks
    if (row_in == 0) stop 'Error SMF: Cannot assign a zero row'
    if (row_in > size(this%sparse_vec)) stop 'Error SMF: Index exceeds assigned size'
    ! add a single column index and value to a row
    call this%sparse_vec(row_in)%rmSVEl(col_in)
  end subroutine rmSMEl

  function multDblVecSM(this, vec_in) result(vec_out)
    implicit none
    double precision, dimension(:), intent(in)      ::    vec_in
    class(sparseMatrix), intent(in)                 ::    this
    double precision, allocatable, dimension(:)     ::    vec_out
    integer                                         ::    i

    ! allocate and initialise the output vector i.e. number of rows
    allocate(vec_out(size(this%sparse_vec)))
    vec_out = 0
    ! perform a matrix multiplication of sparse matrix by non-sparse vector
    do i=1, size(this%sparse_vec)
      vec_out(i) = this%sparse_vec(i)%multDblVecSV(vec_in)
    enddo
  end function multDblVecSM

  subroutine destroySM(this)
    class(sparseMatrix), intent(inout)    ::    this
    deallocate(this%sparse_vec)
  end subroutine destroySM

end module
