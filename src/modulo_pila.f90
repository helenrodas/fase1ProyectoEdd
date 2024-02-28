module pila_module
    implicit none
    
    private
    
    type, public :: pila
    type(node), pointer :: head => null() ! head of the list
    type(node), pointer :: lastNodeReturned => null()
  
    contains
        !procedure :: agregar_imagen
        procedure :: printPila
        procedure :: init_pila
        procedure :: tamano_pila
        procedure :: append
        procedure :: actualizarPila
        !procedure ::print
        !procedure :: eliminar_nodo
    end type pila

    type :: node
        character(len=:), allocatable :: imagen
        integer :: idCliente
        type(node), pointer :: next
    end type node

  
  
    contains

    subroutine init_pila(self)
        class(pila), intent(inout) :: self
        type(node), pointer :: nodo
        
        
    end subroutine init_pila
  
  
    subroutine append(self,idCliente ,imagen)
        class(pila), intent(inout) :: self
        character(len=*), intent(in):: imagen
        integer,intent(in) :: idCliente

        type(node), pointer :: current
        type(node), pointer :: new
        allocate(new)

        new%imagen = imagen
        new%idCliente = idCliente

        if(.not. associated(self%head)) then
            self%head => new
        else

            new%next => self%head
            self%head => new
        end if

    end subroutine append

    subroutine actualizarPila(self,idCliente ,imagen)
        class(pila), intent(inout) :: self
        !integer, intent(in) :: imagen
        character(len=*), intent(in):: imagen
        integer,intent(in) :: idCliente

        type(node), pointer :: current
        !type(node), pointer :: new

        current = self%head

        do while (associated(current))
            current%idCliente = idCliente
            current%imagen = imagen
        end do

        current => current%next

    end subroutine actualizarPila






    subroutine printPila(self)
        class(pila), intent(in) :: self
        type(node), pointer :: current
        current => self%head
        

        do while(associated(current))
            if (associated(current%next)) then
                print *,"-------Pila-------"
                print *,"id cliente: " ,current%idCliente
                print *,"tipo de imagen: " ,current%imagen
                current => current%next
            else
                exit
            end if
        end do
    end subroutine printPila

    function tamano_pila(self) result(tam)
        class(pila), intent(inout) :: self
        type(node), pointer :: current
        integer :: tam
        
        ! Implementa la lógica para calcular el tamaño de la pila aquí
        ! Por ejemplo, si la pila es una estructura enlazada, recorre la pila y cuenta los elementos
        
        tam = 0
        
        ! Aquí deberías colocar el código para recorrer la pila y contar los elementos
        ! Por ejemplo, si la pila es una lista enlazada:
        do while (associated(current))
            tam = tam + 1
            current => current%next
        end do
        
    end function tamano_pila
end module pila_module