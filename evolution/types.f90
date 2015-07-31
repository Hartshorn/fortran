module types

    use constants
    
    implicit none
    private


    type, public :: Animal

        logical                 :: alive = .false.
        integer                 :: x, y, energy = 1000, direction = 0
        integer, dimension(0:8) :: genes

    contains

        procedure :: show   => describe_animal
        procedure :: copy_an   => get_new_animal
        procedure :: cpy_gn => copy_and_alter_genes
        procedure :: eat    => eat_animal
        procedure :: can_reproduce => able_bodied
        procedure :: turn   => turn_animal
        procedure :: move   => move_animal
        procedure :: init   => initialize_first_animal

    end type Animal


    type, public :: Plant

        logical :: alive = .false.
        integer :: x, y

    contains

        procedure :: show => describe_plant

    end type Plant


contains

    subroutine initialize_first_animal(this)
        
        class(Animal) :: this
        integer :: i
        
        this%alive = .true.
        this%x     = WIDTH / 2
        this%y     = HEIGHT / 2
        do i = 0,8
            this%genes(i) = random_for_types(10)
        end do
        
    end subroutine initialize_first_animal
    

    subroutine describe_animal(this)

        100 FORMAT(a,i3,a,i3,a)
        200 FORMAT(a,i4)
        300 FORMAT(a,i1)
        400 FORMAT(a)

        class(Animal) :: this

        if (this%alive) then
            write(*,100) "Location: (", this%x, ",", this%y, " )"
            write(*,200) "Energy:  ", this%energy
            write(*,300) "Direction: ", this%direction
            write(*,*) "Genes: ", this%genes
        else
            write(*,400) "This animal is dead!"
        end if

    end subroutine describe_animal


    subroutine describe_plant(this)

        100 FORMAT(a,i3,a,i3,a)
        400 FORMAT(a)

        class(Plant) :: this

        if (this%alive) then
            write(*,100) "Location: (", this%x, ",", this%y, " )"
        else
            write(*,400) "This plant is dead!"
        end if

    end subroutine describe_plant


    function get_new_animal(this) result(new_a)

        class(Animal) :: this
        type(Animal)  :: new_a
        
        this%energy = this%energy / 2
        
        new_a%alive     = .true.
        new_a%x         = this%x + 1
        new_a%y         = this%y + 1
        new_a%energy    = this%energy
        new_a%genes     = this%cpy_gn()

    end function get_new_animal


    function copy_and_alter_genes(this) result(c_genes)

        class(Animal), intent(in)   :: this
        integer, dimension(0:8)     :: c_genes
        integer                     :: i, n

        do i = 0, 8
            c_genes(i) = this%genes(i)
        end do
        
        n = random_for_types(8)
        
        c_genes(n) = c_genes(n) + 1

    end function copy_and_alter_genes


    subroutine eat_animal(this, p_arr)

        class(Animal) :: this
        type(Plant), dimension(0:MAX_ARR_SIZE) :: p_arr
        integer :: i
        
        do i = 0, MAX_ARR_SIZE
            if (p_arr(i)%alive         .and. &
                p_arr(i)%x .eq. this%x .and. &
                p_arr(i)%y .eq. this%y)      &
            then
                this%energy = this%energy + PLT_NRG
                p_arr(i)%alive = .false.
                exit
            end if
        end do

    end subroutine eat_animal
    
    
    pure function able_bodied(this) result(is_able_bodied)
    
        class(Animal), intent(in) :: this
        logical :: is_able_bodied
        
        is_able_bodied = this%energy > REPR_NRG
        
    end function able_bodied
    
    
    subroutine turn_animal(this)
        
        class(Animal) :: this
        !integer :: index
        
        this%direction = mod(this%direction + 1, 8)
        this%energy = this%energy - 1
        
        !index = index_of_max(this%genes)
        
        !this%direction = mod(this%direction + index, 8)
        
    end subroutine turn_animal
    
    
    subroutine move_animal(this)
    
        class(Animal) :: this
        integer      :: x_factor, y_factor
        
        if (this%direction >= 2 .and. this%direction < 5) then
            x_factor = 1
        else if (this%direction == 1 .or. this%direction == 5) then
            x_factor = 0
        else
            x_factor = -1
        end if
        
        if (this%direction >= 0 .and. this%direction < 3) then
            y_factor = -1
        else if (this%direction >= 4 .or. this%direction < 7) then
            y_factor = 1
        else
            y_factor = 0
        end if
        
        this%x = mod(this%x + x_factor, WIDTH)
        this%y = mod(this%y + y_factor, HEIGHT)
        
    end subroutine move_animal
    
    
    function random_for_types(r) result(n)
    
        integer :: n
        integer, intent(in) :: r
        real :: rnd
        
        call random_number(rnd)
        
        n = int(rnd * r)
    
    end function random_for_types
    
    
    function index_of_max(arr) result(r)
    
        integer, dimension(0:8) :: arr
        integer :: i, r
        
        do i = 0, 8
            if (arr(i) > r) then
                r = i
            end if
        end do
    
    end function index_of_max
        

end module types
