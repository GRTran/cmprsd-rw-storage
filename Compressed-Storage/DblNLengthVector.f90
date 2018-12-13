module DblNLengthVector
  implicit none
  private
  public dblVector

  type dblVector
    integer :: populated
    double precision, allocatable, dimension(:) :: vals
  contains
    procedure, public, pass :: addElement
    procedure, public, pass :: removeElement
  end type

  interface dblVector
    procedure constructor
  end interface

contains
  function constructor(vals_in) result(new_vector)
    implicit none
    double precision, dimension(:), intent(in) :: vals_in
    integer :: array_size
    type(dblVector) :: new_vector
    new_vector%populated = size(vals_in)
    array_size = new_vector%populated * 1.5
    allocate(new_vector%vals(array_size))
    new_vector%vals(1 : new_vector%populated) = vals_in
    new_vector%vals(new_vector%populated+1 : array_size) = 0
  end function constructor

  subroutine addElement(this, val_to_add, position)
    implicit none
    double precision, intent(in) :: val_to_add
    integer, intent(inout) :: position
    class (dblVector), intent(inout) :: this
    ! problem in increasing the array size
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

  subroutine checkIncreaseArraySize(vec)
    implicit none
    class(dblVector), intent(inout) :: vec
    double precision, allocatable, dimension(:) :: temp_array
    integer :: new_size
    new_size = vec%populated * 1.5
    if (size(vec%vals) <= vec%populated) then
      allocate(temp_array(vec%populated))
      temp_array = vec%vals
      deallocate(vec%vals)
      allocate(vec%vals(new_size))
      vec%vals(1:vec%populated) = temp_array
      vec%vals(vec%populated+1 : new_size) = 0
      deallocate(temp_array)
    endif
  end subroutine checkIncreaseArraySize

end module
