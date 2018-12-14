module DblNLengthVector
  implicit none
  private
  public dblVector

  type dblVector
    private
    integer :: populated
    integer :: max_elems
    double precision, allocatable, dimension(:) :: vals
  contains
    procedure, public, pass :: addElement
    procedure, public, pass :: removeElement
    procedure, public, pass :: getElement
    procedure, public, pass :: getElementArray
    procedure, public, pass :: getPopulated
  end type

  interface dblVector
    procedure constructor
  end interface

contains
  function constructor(max_elems) result(new_vector)
    implicit none
    integer, intent(in)                                        ::    max_elems
    type(dblVector)                                            ::    new_vector

    ! error check - max array size must be above 0
    if (max_elems < 1) stop 'Error DblNVec: Max vector size must be above zero'

    new_vector%populated  = 0
    allocate(new_vector%vals(1))
    new_vector%vals(1) = 0
    new_vector%max_elems = max_elems
  end function constructor

  subroutine addElement(this, val_to_add, position)
    implicit none
    double precision, intent(in) :: val_to_add
    integer, intent(inout) :: position
    class (DblVector), intent(inout) :: this

    ! check to see if the maximum array size has been met
    if (this%populated >= this%max_elems) stop 'Error DblNVec: Exceeding array size'
    ! check whether column value is greater than the maximum size of vector
    if (val_to_add > this%max_elems) stop 'Error DblNVec: Exceeding array size'
    ! check whether the array can be increased in size
    call checkIncreaseArraySize(this)
    ! if the position is past the populated number then we must add at end not
    ! at some random value past the end
    if (position > this%populated+1) position = this%populated+1
    this%vals(position+1:this%populated+1) = this%vals(position : this%populated)
    this%vals(position) = val_to_add
    this%populated = this%populated + 1
  end subroutine addElement

  subroutine removeElement(this, position)
    implicit none
    integer, intent(in) :: position
    class (dblVector), intent(inout) :: this

    call checkIncreaseArraySize(this)
    this%vals(position : this%populated-1)  = this%vals(position+1 : this%populated)
    this%vals(this%populated) = 0
    this%populated = this%populated - 1
  end subroutine removeElement

  function getElement(this, position) result(val_out)
    implicit none
    integer, intent(in)                                          ::    position
    class (dblVector), intent(in)                                ::    this
    integer                                                      ::    val_out

    if ((position > this%populated) .and. (position < this%max_elems)) then
      val_out = 0
    elseif (position > this%max_elems) then
      stop "Error DblNVec: position exceeds matrix length"
    else
      val_out = this%vals(position)
    endif
  end function getElement

  !!------ Get an array of elements in vector ------!!
  function getElementArray(this, start_elem, end_elem) result(array_out)
    implicit none
    integer, intent(in)                                          ::   start_elem
    integer, intent(in)                                          ::   end_elem
    class(dblVector), intent(in)                                 ::   this
    double precision, dimension(end_elem-start_elem)             ::   array_out
    integer                                                      ::   array_size
    ! error checks
    if (start_elem > end_elem) stop 'Error DblNVec: incorrect index reference'
    if ((start_elem < 1) .or. (end_elem < 1)) stop 'Error DblNVec: incorrect index reference'
    if (end_elem > this%populated) stop 'Error DblNVec: incorrect index reference'
    ! assign the array
    array_out = this%vals(start_elem:end_elem)
  end function getElementArray

  !!------ Get the number of populated elements ------!!
  function getPopulated(this) result(populated_out)
    implicit none
    class(dblVector), intent(in)          ::    this
    integer                               ::    populated_out

    populated_out = this%populated
  end function getPopulated

  subroutine checkIncreaseArraySize(vec)
    implicit none
    class(dblVector), intent(inout) :: vec
    integer, allocatable, dimension(:) :: temp_array
    integer :: new_size
    ! increase allocated memory by 50% or up to maximum number of elements
    if (size(vec%vals) <= vec%populated) then
      allocate(temp_array(vec%populated))
      temp_array = vec%vals
      deallocate(vec%vals)
      new_size = vec%populated * 1.5
      if (new_size > vec%max_elems) new_size = vec%max_elems
      allocate(vec%vals(new_size))
      vec%vals(1:vec%populated) = temp_array
      vec%vals(vec%populated+1 : new_size) = 0
      deallocate(temp_array)
    endif
  end subroutine checkIncreaseArraySize

end module
