module pila_module
    implicit none
    private
    
    type, public :: pila
    type(node), pointer :: head => null() 
    type(node), pointer :: lastNodeReturned => null()

    contains
        !procedure :: agregar_imagen
        procedure :: printPila
        procedure :: init_pila
        procedure :: tamano_pila
        procedure :: append
        procedure :: actualizarPila
        procedure :: PilaEstaVacia
        procedure :: graficar_pila
        procedure :: clearPila
    end type pila

    type :: node
        character(len=:), allocatable :: imagen
        integer :: idCliente
        type(node), pointer :: next => null()
        !type(node), pointer :: head => null()
    end type node

    contains

    subroutine PilaEstaVacia(self, pilaVacia)
        class(pila), intent(inout) :: self
        logical :: pilaVacia 

        if(.not. associated(self%head)) then
            pilaVacia = .true.
        else
            pilaVacia = .false.
        end if    
    
        
    end subroutine PilaEstaVacia


    subroutine graficar_pila(self,intrucciones)
        class(pila), intent(inout) :: self
        character(:), allocatable :: intrucciones,uniones,nombreNodo
        character(len=20) :: indice,id_nuevo
        integer :: index
        type(node), pointer :: current
        current => self%head
        intrucciones = ""
        uniones = ""
        nombreNodo = ""
        index = 1
        do while(associated(current))
            write(indice, "(I5)" ) index
            write(id_nuevo, "(I5)" ) current%idCliente
            nombreNodo = '"node '// current%imagen // id_nuevo // indice // '"'

            intrucciones = intrucciones // nombreNodo // '[label="' // current%imagen // '"]'
            if(associated(current%next))then
                uniones = uniones // nombreNodo // "->"
            else 
                uniones = uniones // nombreNodo
            end if
            index =  index + 1
            
            current => current%next
        end do
        intrucciones = intrucciones // uniones

    end subroutine graficar_pila


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
        character(len=*), intent(in):: imagen
        integer,intent(in) :: idCliente

        type(node), pointer :: current

        current = self%head

        do while (associated(current))
            current%idCliente = idCliente
            current%imagen = imagen
        end do

        current => current%next

    end subroutine actualizarPila


    subroutine clearPila(self)
        class(pila), intent(inout) :: self
            type(node), pointer :: aux
        
            do while(associated(self%head))
                aux => self%head%next
                deallocate(self%head)
                if (associated(aux)) then
                    self%head => aux
                else
                    nullify(self%head)
                end if
            end do
        
    end subroutine clearPila

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
        
        tam = 0
        
        do while (associated(current))
            tam = tam + 1
            current => current%next
        end do
        
    end function tamano_pila

end module pila_module