module listaImgImpresas_module
    implicit none
    private
    type :: node
        character(len=:), allocatable :: tipoImagen
        type(node), pointer :: next
    end type node

    type, public :: listaImgImpresas
        type(node), pointer :: cabeza => null()
    contains
        procedure :: append_imgImpresa
        procedure :: printImgImpresas
    end type listaImgImpresas
    contains

    subroutine append_imgImpresa(self, tipoImagen)
        class(listaImgImpresas), intent(inout) :: self
        character(len=*), intent(in) :: tipoImagen
        type(node), pointer :: nuevo_nodo
        
        allocate(nuevo_nodo)

        nuevo_nodo%tipoImagen = tipoImagen
        nuevo_nodo%next => self%cabeza
        self%cabeza => nuevo_nodo
    end subroutine append_imgImpresa

    subroutine printImgImpresas(self)
        class(listaImgImpresas), intent(in) :: self
        type(node), pointer :: actual
        actual => self%cabeza
        if (.not. associated(actual)) then
            print *, "LISTA IMAGENES IMPRESAS VACIA"
            return
        end if
        do while (associated(actual))
            print *, "Imagen: ", actual%tipoImagen
            actual => actual%next
        end do
    end subroutine printImgImpresas
end module listaImgImpresas_module


module listaClientesEspera_module
    use listaImgImpresas_module
    implicit none
    private 

    type,public :: nodeEspera
    integer :: id_ventanilla, img_pAsInt, img_gAsInt,id_cliente,copiaImgG,copiaImgP
    character(len=:), allocatable :: nombre
    type(listaImgImpresas) :: listaImgCliente
    type(nodeEspera), pointer :: anterior, siguiente
    end type nodeEspera

    type,public :: listaClientesEspera
        type(nodeEspera), pointer :: cabeza => null()
        type(nodeEspera), pointer :: tail => null()
    contains
        procedure :: append_clienteEspera
        procedure :: delete_clienteEspera
        procedure :: print_listaEspera
        procedure :: listaEspera_dot
    end type listaClientesEspera

    contains
    subroutine append_clienteEspera(self,id_ventanilla ,id_cliente, nombre, img_gAsInt,img_pAsInt)
        class(listaClientesEspera), intent(inout) :: self
        character(len=*), intent(in) ::  nombre
        integer :: img_pAsInt, img_gAsInt,id_cliente,id_ventanilla
        type(nodeEspera), pointer :: nuevo_nodo

        allocate(nuevo_nodo)

        nuevo_nodo%id_ventanilla = id_ventanilla
        nuevo_nodo%id_cliente = id_cliente
        nuevo_nodo%nombre = nombre
        nuevo_nodo%img_pAsInt = img_pAsInt
        nuevo_nodo%img_gAsInt = img_gAsInt
        nuevo_nodo%copiaImgP = img_pAsInt
        nuevo_nodo%copiaImgG = img_gAsInt
        if (.not. associated(self%cabeza)) then
            self%cabeza => nuevo_nodo
            nuevo_nodo%anterior => nuevo_nodo
            nuevo_nodo%siguiente => nuevo_nodo
        else
            nuevo_nodo%anterior => self%cabeza%anterior
            nuevo_nodo%siguiente => self%cabeza
            self%cabeza%anterior%siguiente => nuevo_nodo
            self%cabeza%anterior => nuevo_nodo
        end if
    end subroutine append_clienteEspera

    subroutine delete_clienteEspera(self, id_cliente)
        class(listaClientesEspera), intent(inout) :: self
        integer :: id_cliente
        type(nodeEspera), pointer :: actual

        actual => self%cabeza
        
        do while (associated(actual) .and. actual%id_cliente /= id_cliente)
            actual => actual%siguiente
        end do
        if (associated(actual)) then
            actual%anterior%siguiente => actual%siguiente
            actual%siguiente%anterior => actual%anterior
            if (associated(self%cabeza, actual)) then
                if (associated(actual, actual%siguiente)) then
                    self%cabeza => null()
                else
                    self%cabeza => actual%siguiente
                end if
            end if
            deallocate(actual)
        end if
    end subroutine delete_clienteEspera

    subroutine print_listaEspera(self)
        class(listaClientesEspera), intent(in) :: self
        type(nodeEspera), pointer :: actual
        if (.not. associated(self%cabeza)) then
            print *, "No hay clientes en lista espera"
        else
            actual => self%cabeza
            do
                print *, "ID Cliente: ", actual%id_cliente
                print *, "Nombre: ", actual%nombre
                print *, "Pequena: ", actual%img_pAsInt
                print *, "Grande: ", actual%img_gAsInt
                call actual%listaImgCliente%printImgImpresas()
                print *, "----------"
                actual => actual%siguiente
                if (associated(actual, self%cabeza)) exit
            end do
        end if
    end subroutine print_listaEspera

    subroutine listaEspera_dot(self)
        class(listaClientesEspera), intent(inout) :: self
        integer :: unit, i
        type(nodeEspera), pointer :: actual
        character(len=:), allocatable :: filepath
        if (.not. associated(self%cabeza)) then
            print*,"Lista Clientes En Espera Vacia."
            return
        end if
        filepath = trim("ClientesEnEspera") 
        open(unit, file=filepath, status='replace')
        write(unit, *) 'digraph cola {node [fontname="Arial"] rankdir = LR'
        write(unit, *) '    node [shape=ellipse, style=filled, fillcolor="orange"];'
        actual => self%cabeza
        i = 0
        do while (associated(actual))
            i = i + 1
            write(unit, *) '    "Node', actual%id_cliente, '" [label="', &
                                "ID Cliente: ", actual%id_cliente, "\n", &
                                "Nombre: ",actual%nombre, "\n", &
                                "img pequenas: ",actual%img_pAsInt,"\n", &
                                "Img grandes: ",actual%img_gAsInt,"\n", '"];'
            write(unit,*) '"Node',actual%id_cliente,'"->"Node',actual%siguiente%id_cliente,'";'
            write(unit,*) '"Node',actual%id_cliente,'"->"Node',actual%anterior%id_cliente,'";'
            actual => actual%siguiente
            if (associated(actual, self%cabeza)) exit
        end do 
        write(unit, *) '}'
        close(unit)
        call system('dot -Tpng ' // trim(filepath) // ' -o ' // trim(adjustl(filepath)) // '.png')
        print *, 'Grafica generada Correctamente: ', trim(adjustl(filepath)) // '.png'
    end subroutine listaEspera_dot
    
end module listaClientesEspera_module