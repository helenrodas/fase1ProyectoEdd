module pila_module
    implicit none
    
    private
    
    type, public :: pila
    type(node), pointer :: head => null() ! head of the list
  
    contains
        procedure :: agregar_imagen
        procedure :: printPila
        procedure :: init_pila
        procedure :: tamano_pila
        procedure :: append
        !procedure ::print
        !procedure :: eliminar_nodo
    end type pila

    type :: node
        character(len=:), allocatable :: imagen
        type(node), pointer :: next
    end type node

  
  
    contains

    subroutine init_pila(self,nodo)
        class(pila), intent(inout) :: self
        type(node), pointer :: nodo
        
        
    end subroutine init_pila
  
    subroutine agregar_imagen(self, imagen)
        class(pila), intent(inout) :: self
        character(len=*), intent(in)  :: imagen
    
        type(node), pointer :: newNode
    
        ! Crear un nuevo nodo
        allocate(newNode)
        newNode%imagen = imagen
        newNode%next => self%head ! El nuevo nodo apunta al nodo anterior (antiguo head)
    
        ! El nuevo nodo se convierte en el nuevo head de la pila
        self%head => newNode
    
        !print *, 'pushed:: ', id,nombre,img_g,img_p
    end subroutine agregar_imagen
  
    subroutine append(self, imagen)
        class(pila), intent(inout) :: self
        !integer, intent(in) :: imagen
        character(len=*), intent(in):: imagen

        type(node), pointer :: current
        type(node), pointer :: new
        allocate(new)

        new%imagen = imagen

        if(.not. associated(self%head)) then
            self%head => new
        else
            current => self%head
            do while(associated(current%next))
                current => current%next
            end do

            current%next => new
        end if

    end subroutine append

    subroutine printPila(self)
        class(pila), intent(in) :: self
        type(node), pointer :: current
        current => self%head

        do while(associated(current))
            print *, current%imagen, ","
            current => current%next
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