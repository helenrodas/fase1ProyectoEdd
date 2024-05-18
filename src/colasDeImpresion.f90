module impresoraPequena_module
    use pila_module
    implicit none
    private

    type,public :: impresoraPequena
    type(node), pointer :: head => null()
    contains
        procedure :: pushPequena
        procedure :: eliminarPequena
        procedure :: printImpresoraPeque
        procedure :: impresoraPeque_dot
    end type impresoraPequena

    type :: node
        character(len=:), allocatable :: tipoImg
        type(node), pointer :: next
    end type node

    contains

    subroutine pushPequena(self, tipoImg)
        class(impresoraPequena), intent(inout) :: self
        character(len=*), intent(in) :: tipoImg
        
    
        type(node), pointer :: current, newNode
    
        ! Crear un nuevo nodo
        allocate(newNode)
        newNode%tipoImg = tipoImg
        newNode%next => null()
    
        ! Si la lista está vacía, el nuevo nodo se convierte en la cabeza de la lista
        if (.not. associated(self%head)) then
            self%head => newNode
        else
            ! Encontrar el último nodo de la lista
            current => self%head
            do while (associated(current%next))
                current => current%next
            end do
    
            ! Insertar el nuevo nodo al final de la lista
            current%next => newNode
        end if
    end subroutine pushPequena

    subroutine eliminarPequena(self)
        class(impresoraPequena), intent(inout) :: self
        type(node), pointer :: temp
        if (.not. associated(self%head)) then
            print *, "No hay imagenes pequenas en impresora "
            return
        else
            temp => self%head
            self%head => self%head%next
            deallocate(temp)
        end if
    end subroutine eliminarPequena

    subroutine printImpresoraPeque(self)
        class(impresoraPequena), intent(in) :: self
        type(node), pointer :: actual
        print*, "Impresora Imagenes Pequenas:"
        actual => self%head
        do while (associated(actual))
            print *, "Imagen: ",actual%tipoImg
            actual => actual%next
        end do
    end subroutine printImpresoraPeque

    subroutine impresoraPeque_dot(self)
        class(impresoraPequena), intent(inout) :: self
        
        integer :: unit, contador
        type(node), pointer :: actual
        character(len=:), allocatable :: filepath
        if (.not. associated(self%head)) then
            print*,"cola img pequenas vacia."
            return
        end if
        filepath =  trim("impresoraPequena") 
        open(unit, file=filepath, status='replace')
        write(unit, *) "digraph G {"
        write(unit, *) "  node [shape=ellipse];"
        write(unit, *) "  rankdir=LR"
        actual => self%head
        contador = 0
        write(unit, *) '"Node', contador, '" [shape=tab, color=black, fillcolor="yellow" style=filled label="',&
        "Impresora Imagenes Pequenas", '"];'
        do while (associated(actual))
            contador = contador + 1
            write(unit, *) '    "Node', contador, '" [label="', &
                                "Imagen: ", actual%tipoImg, "\n",'",fillcolor=orange, style=filled];'
            if (associated(actual%next)) then
                write(unit, *) '    "Node', contador, '" -> "Node', contador+1, '";'
            end if
            actual => actual%next
        end do 
        write(unit, *) '}'
        close(unit)
        call system('dot -Tpng ' // trim(filepath) // ' -o ' // trim(adjustl(filepath)) // '.png')
        print *, 'Grafica Impresoras Img Pequenas generada: ', trim(adjustl(filepath)) // '.png'
    end subroutine impresoraPeque_dot


end module impresoraPequena_module


module impresoraGrande_module
    use pila_module
    implicit none
    private

    type, public :: impresoraGrande
    type(node), pointer :: head => null()
    contains
        procedure :: pushGrande
        procedure :: eliminarGrande
        procedure :: printImpresoraGrande
        procedure :: impresoraGrande_dot
    end type impresoraGrande

    type :: node
        character(len=:), allocatable :: tipoImg
        type(node), pointer :: next
    end type node

    contains
    subroutine pushGrande(self, tipoImg)
        class(impresoraGrande), intent(inout) :: self
        character(len=*), intent(in) :: tipoImg
        
    
        type(node), pointer :: current, newNode
    
        ! Crear un nuevo nodo
        allocate(newNode)
        newNode%tipoImg = tipoImg
        newNode%next => null()
    
        ! Si la lista está vacía, el nuevo nodo se convierte en la cabeza de la lista
        if (.not. associated(self%head)) then
            self%head => newNode
        else
            ! Encontrar el último nodo de la lista
            current => self%head
            do while (associated(current%next))
                current => current%next
            end do
    
            ! Insertar el nuevo nodo al final de la lista
            current%next => newNode
        end if
    end subroutine pushGrande

    subroutine eliminarGrande(self)
        class(impresoraGrande), intent(inout) :: self
        type(node), pointer :: temp
        if (.not. associated(self%head)) then
            print *, "no hay imagenes grandes en impresora"
            return
        else
            temp => self%head
            self%head => self%head%next
            deallocate(temp)
        end if
    end subroutine eliminarGrande

    subroutine printImpresoraGrande(self)
        class(impresoraGrande), intent(in) :: self
        type(node), pointer :: actual
        actual => self%head
        print*, "Cola Imagenes Grandes:"
        do while (associated(actual))
            print *, "Imagen: ",actual%tipoImg
            actual => actual%next
        end do
    end subroutine printImpresoraGrande

    subroutine impresoraGrande_dot(self)
        class(impresoraGrande), intent(inout) :: self
        
        integer :: unit, contador
        type(node), pointer :: actual
        character(len=:), allocatable :: filepath
        if (.not. associated(self%head)) then
            print*,"cola img grandes vacia."
            return
        end if
        filepath =  trim("impresoraGrande") 
        open(unit, file=filepath, status='replace')
        write(unit, *) "digraph G {"
        write(unit, *) "  node [shape=ellipse];"
        write(unit, *) "  rankdir=LR"
        actual => self%head
        contador = 0
        write(unit, *) '"Node', contador, '" [shape=tab, color=black, fillcolor="yellow" style=filled label="',&
        "Impresora Imagenes Grandes", '"];'
        do while (associated(actual))
            contador = contador + 1
            write(unit, *) '    "Node', contador, '" [label="', &
                                "Imagen: ", actual%tipoImg, "\n",'",fillcolor=orange, style=filled];'
            if (associated(actual%next)) then
                write(unit, *) '    "Node', contador, '" -> "Node', contador+1, '";'
            end if
            actual => actual%next
        end do 
        write(unit, *) '}'
        close(unit)
        call system('dot -Tpng ' // trim(filepath) // ' -o ' // trim(adjustl(filepath)) // '.png')
        print *, 'Grafica Impresoras Img Grandes generada: ', trim(adjustl(filepath)) // '.png'
    end subroutine impresoraGrande_dot
    
end module impresoraGrande_module