module listaClientesAtendidos_module
    implicit none
    private
    type :: node
        integer :: id_ventanilla,id_cliente,img_pequena,img_grande

        character(len=:), allocatable :: nombre

        ! integer :: cantidad_pasos
        type(node), pointer :: next => null()
    end type node

    type,public :: listaClientesAtendidos
        type(node), pointer :: head => null()
    contains
        procedure :: agregarClienteAtendido
        procedure :: printClienteAtendido
        ! procedure :: cliente_mayor_pasos
        procedure :: clientesAtendidos_dot
        ! procedure :: print_cliente_por_nombre
    end type listaClientesAtendidos

    contains
    subroutine agregarClienteAtendido(self, id_ventanilla, id_cliente, nombre, img_pequena, img_grande)
        class(listaClientesAtendidos), intent(inout) :: self
        character(len=*), intent(in) ::  nombre
        integer, intent(in) :: id_cliente, img_pequena, img_grande
        integer, intent(in) :: id_ventanilla
        type(node), pointer :: newNode
        allocate(newNode)
        newNode%id_ventanilla = id_ventanilla
        newNode%id_cliente = id_cliente
        newNode%nombre = nombre
        newNode%img_pequena = img_pequena
        newNode%img_grande = img_grande
        newNode%next => self%head
        self%head => newNode
    end subroutine agregarClienteAtendido

    subroutine printClienteAtendido(self)
        class(listaClientesAtendidos), intent(in) :: self
        type(node), pointer :: actual
        actual => self%head
        do while (associated(actual))
            print *, "Id Ventanila: ", actual%id_ventanilla
            print *, "Id Cliente: ", actual%id_cliente
            print *, "Nombre: ", actual%nombre
            print *, "Imagen Pequena: ", actual%img_pequena
            print *, "Imagen Grande: ", actual%img_grande
            print *, "----------"
            actual => actual%next
        end do
    end subroutine printClienteAtendido

    ! subroutine cliente_mayor_pasos(self, filename)
    !     class(listaClientesAtendidos), intent(in) :: self
    !     type(node), pointer :: actual
    !     type(node), pointer :: max_pasos_nodo => null()
    !     integer :: max_pasos = -1, unit
    !     character(len=*), intent(in) :: filename
    !     character(len=:), allocatable :: filepath
    !     if (.not. associated(self%head)) then
    !         print*,"Lista De Clientes Atendidos Vacia."
    !         return
    !     end if
    !     filepath = 'zgraph/' // trim(filename) 
    !     open(unit, file=filepath, status='replace')
    !     write(unit, *) 'digraph cola {node [fontname="Courier New"]'
    !     write(unit, *) '    node [shape=box3d, style=filled, color=blue, fillcolor="#65babf"];'
    !     write(unit, *) '"Node', 0, '" [shape=folder, color=black, fillcolor="#d43440" label="', &
    !     "Cliente Con Mayor Cantidad De Pasos En El Sistemas", '"];'
    !     actual => self%head
    !     do while (associated(actual))
    !         if (actual%cantidad_pasos > max_pasos) then
    !             max_pasos = actual%cantidad_pasos
    !             max_pasos_nodo => actual
    !         end if
    !         actual => actual%next
    !     end do
    !     if (associated(max_pasos_nodo)) then
    !         write(unit, *) '    "Node', 1, '" [label="', &
    !                             "ID Cliente: ", max_pasos_nodo%id_cliente, "\n", &
    !                             "Nombre: ",max_pasos_nodo%nombre, "\n", &
    !                             "Imagenes Pequenas: ",max_pasos_nodo%img_pequena,"\n", &
    !                             "Imagenes Grandes: ",max_pasos_nodo%img_grande,"\n", &
    !                             "CANTIDAD DE PASOS: ",max_pasos_nodo%cantidad_pasos,'"];'
    !     else
    !         print *, "Lista Clientes Atendidos Vacia."
    !     end if
    !     write(unit, *) '}'
    !     close(unit)
    !     call system('dot -Tpdf ' // trim(filepath) // ' -o ' // trim(adjustl(filepath)) // '.pdf')
    !     print *, 'Grafica Cliente Con Mayor Cantidad De Pasos Correctamente: ', trim(adjustl(filepath)) // '.pdf'
    ! end subroutine cliente_mayor_pasos

    ! subroutine print_cliente_por_nombre(self, nombre_cliente, filename)
    !     class(listaClientesAtendidos), intent(in) :: self
    !     character(len=*), intent(in) :: nombre_cliente
    !     type(node), pointer :: actual
    !     logical :: cliente_encontrado
    !     integer :: unit
    !     character(len=*), intent(in) :: filename
    !     character(len=:), allocatable :: filepath
    !     if (.not. associated(self%head)) then
    !         print*,"Lista De Clientes Atendidos Vacia."
    !         return
    !     end if
    !     filepath = 'zgraph/' // trim(filename) 
    !     open(unit, file=filepath, status='replace')
    !     write(unit, *) 'digraph cola {node [fontname="Courier New"]'
    !     write(unit, *) '    node [shape=box3d, style=filled, color=blue, fillcolor="#65babf"];'
    !     write(unit, *) '"Node', 0, '" [shape=folder, color=black, fillcolor="#d43440" label="', &
    !     "Cliente Encontrado Por Nombre", '"];'

    !     cliente_encontrado = .false.
    !     actual => self%head
    !     if (.not. associated(actual)) then
    !         print *, "Lista De Clientes Atendidos Vacia."
    !         return
    !     end if

    !     do while (associated(actual))
    !         if (actual%nombre == nombre_cliente) then
    !             write(unit, *) '    "Node', 1, '" [label="', &
    !                             "Ventanilla Atentido: ", actual%id_ventanilla, "\n", &
    !                             "ID Cliente: ", actual%id_cliente, "\n", &
    !                             "Nombre: ",actual%nombre, "\n", &
    !                             "Imagenes Pequenas: ",actual%img_pequena,"\n", &
    !                             "Imagenes Grandes: ",actual%img_grande,"\n", &
    !                             "Cantidad Pasos: ",actual%cantidad_pasos,'"];'
    !             cliente_encontrado = .true.
    !             exit
    !         end if
    !         actual => actual%next
    !     end do
    !     if (.not. cliente_encontrado) then
    !         print *, "No se encontro un cliente con el nombre: ", nombre_cliente
    !     end if
    !     write(unit, *) '}'
    !     close(unit)
    !     call system('dot -Tpdf ' // trim(filepath) // ' -o ' // trim(adjustl(filepath)) // '.pdf')
    !     print *, 'Grafica Cliente Encontrado Por Nombre Correctamente: ', trim(adjustl(filepath)) // '.pdf'
    ! end subroutine print_cliente_por_nombre
    

    subroutine clientesAtendidos_dot(self)
        class(listaClientesAtendidos), intent(inout) :: self
        character(len=:), allocatable :: filename
        integer :: unit
        integer :: contador
        type(node), pointer :: actual
        character(len=:), allocatable :: filepath
        if (.not. associated(self%head)) then
            print*,"No hay clientes atendidos en lista."
            return
        end if
        filepath =  trim("Clientes_Atendidos") 
        open(unit, file=filepath, status='replace')
        write(unit, *) "digraph G {"
        write(unit, *) "  node [shape=ellipse];"
        write(unit, *) "  rankdir=LR"
        actual => self%head
        contador = 0
        write(unit, *) '"Node', contador, '" [shape=box, color=black, fillcolor=orange, style=filled label="', &
                        "Clientes Atendidos", '"];'
        do while (associated(actual))
            contador = contador + 1
            write(unit, *) '    "Node', contador, '" [label="', &
                                "Ventanilla: ", actual%id_ventanilla, "\n", &
                                "Id Cliente: ", actual%id_cliente, "\n", &
                                "Nombre: ",actual%nombre, "\n", &
                                "Imagenes Pequenas: ",actual%img_pequena,"\n", &
                                "Imagenes Grandes: ",actual%img_grande,"\n",'",fillcolor=orange, style=filled];'
            if (associated(actual%next)) then
                write(unit, *) '    "Node', contador, '" -> "Node', contador+1, '";'
            end if
            actual => actual%next
        end do 
        write(unit, *) '}'
        close(unit)
        call system('dot -Tpng ' // trim(filepath) // ' -o ' // trim(adjustl(filepath)) // '.png')
        print *, 'Grafica Clientes Atendidos Generada: ', trim(adjustl(filepath)) // '.png'
    end subroutine clientesAtendidos_dot
end module listaClientesAtendidos_module