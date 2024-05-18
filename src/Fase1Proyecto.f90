module linkedList
    use pila_module
    use listaClientesEspera_module 
    use impresoraPequena_module
    use impresoraGrande_module
    use listaClientesAtendidos_module
    use listaImgImpresas_module
    implicit none
    private

    type, public :: linked_list
        type(node), pointer :: head => null() 
        type(node), pointer :: tail => null()
        type(node), pointer :: lastNodeReturned => null()
        type(listaClientesEspera) :: listaCEspera
        type(impresoraPequena) :: colaImpresoraP
        type(impresoraGrande) :: colaImpresoraG
        type(listaClientesAtendidos):: listaAtendidos
        type(listaImgImpresas)::listaImpresas

        contains
            procedure :: agregar_lista
            procedure :: print_ventanillas
            procedure :: ventanillaDisponible
            procedure :: getIndiceVentanilla
            procedure :: actualizar_ventanilla
            procedure :: printVent
            procedure :: insert
            procedure :: getIDCliente
            procedure :: getNombreCliente
            procedure :: getImgPequenas
            procedure :: getImgGrande
            procedure :: grafica_pilaImagenes
            procedure :: printClienteTerminado
    end type linked_list


    type :: node
        integer :: index
        integer :: id_ventanilla, id_cliente, cantidadImg_pila, cantidadImg_pequena, cantidadImg_grande, copiaGrande, copiaPequena
        character(len=:), allocatable :: nombreCliente
        logical :: estado
        type(node), pointer :: next => null()
        type(node), pointer :: prev => null()
        type(pila) :: pila_img
       
    end type node

contains

subroutine printClienteTerminado(self)
    class(linked_list), intent(inout) :: self
    ! use listaClientesEspera_module, only: nodeEspera
    type(nodeEspera), pointer :: actual, next
    integer :: contadorImgG, contadorImgP
    logical :: salirMetodo
    actual => self%listaCEspera%cabeza
    if (.not. associated(actual)) then
        return
    end if
    do while (associated(actual))
        contadorImgP = actual%img_pAsInt
        contadorImgG = actual% img_gAsInt
        if (contadorImgP>0) then
            call self%colaImpresoraP%eliminarPequena()
            call actual%listaImgCliente%append_imgImpresa("Pequena")
            contadorImgP=contadorImgP-1
            actual%img_pAsInt = contadorImgP
            ! actual%cantidad_paso=actual%cantidad_paso+1
        end if
        if (contadorImgG>0) then
            call self%colaImpresoraG%eliminarGrande()
            call actual%listaImgCliente%append_imgImpresa("Grande")
            contadorImgG=contadorImgG-1
            actual%img_gAsInt = contadorImgG
            ! actual%cantidad_paso=actual%cantidad_paso+1
        end if
        next => actual%siguiente
        if (actual%img_gAsInt==0 .and. actual%img_pAsInt==0) then
            print*, "Cliente: ", trim(actual%nombre), " atendido."
            print*, "--"
            call self%listaAtendidos%agregarClienteAtendido( &
                actual%id_ventanilla, actual%id_cliente, actual%nombre, actual%copiaImgP, &
                actual%copiaImgG)
            call self%listaCEspera%delete_clienteEspera(actual%id_cliente)
        end if
        salirMetodo = .not. associated(self%listaCEspera%cabeza) .or. &
                        associated(actual, self%listaCEspera%cabeza)
        actual => next
        if (salirMetodo) exit
    end do
end subroutine printClienteTerminado




subroutine grafica_pilaImagenes(self, io)
    class(linked_list), intent(inout) :: self ! referencia a la lista
    class(node), pointer :: current ! puntero al nodo actual
    integer :: id_ventanilla,i,index
    integer, intent(out) ::io ! id del nodo
    character(len=100), allocatable :: command
    character(:), allocatable :: instru,nodoven
    character(len=8) :: nombre_nodo , ban, valor
    logical :: bandera
    instru = ""
    nodoven = ""
    command = "dot -Tpng ./PasosImg.dot -o ./PasosImg.png"

    current => self%head
    index = 1

    open(newunit=io, file="PasosImg.dot", status='replace')

    write(io, *) "digraph G {" ! encabezado del archivo dot
    write(io, *) "  node [shape=box];"  ! forma de los nodos
    write(io, *) "  rankdir=TB" ! orientación del grafo
    write(io, *) ' subgraph cluster{ bgcolor=white'  ! orientación del grafo
    if ( .not. associated(current))  then ! si la lista está vacía
        write(io, *) "  EmptyList;" ! escribir un nodo que diga que la lista está vacía
    else ! si la lista no está vacía
        ! escribir la arista de la cabeza al primer nodo
        ! escribir la arista de la cola al último nodo

        do while (associated(current)) ! recorrer la lista
            
            id_ventanilla = current%id_ventanilla
            ! crear el nodo
            write(valor, '(I5)') id_ventanilla
            nodoven = '  "Node' // valor 
            write(io, *) nodoven // '"[label="ventanilla= ', id_ventanilla, '"];'
            ! escribir las aristas
            if (associated(current%next)) then
                write(ban, '(I5)') current%next%id_ventanilla
                write(io, *)  nodoven // '"-> "Node' // ban // '";' ! arista del nodo actual al siguiente nodo
            end if

            if( .not.(current%estado)) then
            write(nombre_nodo, '(I5)') index

            write(io, *) '"nodo'//trim(nombre_nodo)//'c"[label="', 'ID: ', current%id_cliente, &
                    '\n Nombre: ', current%nombreCliente,'\n IMG_G: ', current%cantidadImg_grande,'\n IMG_P: ', &
                    current%cantidadImg_pequena, '", fillcolor=orange, style=filled];'
            
                write(io, *)'"nodo'//trim(nombre_nodo)//'c" ->   '// nodoven //'"'
            end if


            call current%pila_img%PilaEstaVacia(bandera)

            if(.not. bandera)then
                
                call current%pila_img%graficar_pila(instru)
                instru =  instru // '-> '// nodoven // '"'

                write(io, *) instru
            end if


            current => current%next
            index = index + 1
        end do
    end if
    write(io, *) "}}" 
    close(io)

    call execute_command_line(command, exitstat=i)
        
        if(i == 1) then
            print *, "Ocurrió un error"
        else
            print *, "Grafica de lista ventanillas generada satisfactoriamente"
        end if
end subroutine grafica_pilaImagenes


subroutine insert(self,id_ventanilla)
    class(linked_list), intent(inout) :: self
    integer, intent(in) :: id_ventanilla

    type(node), pointer :: aux
    type(node), pointer :: new
    allocate(new)

    if(.not. associated(self%head)) then
        allocate(aux)
        aux%id_ventanilla = id_ventanilla
        self%head => aux
        self%tail => aux
        call aux%pila_img%init_pila()
    else
        if(id_ventanilla < self%head%id_ventanilla) then
            self%head%prev => new
            new%next => self%head
            self%head => new

            new%id_ventanilla = id_ventanilla
            call new%pila_img%init_pila()
        else
            aux => self%head
            do while (associated(aux%next))
                if(id_ventanilla < aux%next%id_ventanilla) then
                    if(id_ventanilla == aux%id_ventanilla) then
                        call aux%pila_img%init_pila()
                    else
                        new%next => aux%next
                        new%prev => aux
                        aux%next%prev => new
                        aux%next => new

                        new%id_ventanilla = id_ventanilla
                        call new%pila_img%init_pila()
                    end if
                    return
                end if
                aux => aux%next
            end do

            if(id_ventanilla == aux%id_ventanilla) then
                call aux%pila_img%init_pila()
            else
                self%tail%next => new
                new%prev => self%tail
                self%tail => new

                new%id_ventanilla = id_ventanilla
                call new%pila_img%init_pila()
            end if
        end if
    end if
end subroutine insert


subroutine printVent(self)
    class(linked_list) :: self
    type(node), pointer :: aux

    aux => self%head

    do while(associated(aux))
        print *, '----------------------------'
        print *, 'indice ventanilla en pila : ', aux%id_ventanilla
        call aux%pila_img%printPila()
        print *, ""
        aux => aux%next
    end do
end subroutine printVent


subroutine agregar_lista(self, id_ventanilla, id_cliente, nombreCliente, estado, &
    cantidadImg_pila, cantidadImg_pequena, cantidadImg_grande)
    class(linked_list), intent(inout) :: self
    integer, intent(in) :: id_ventanilla, id_cliente, cantidadImg_pila, cantidadImg_pequena, cantidadImg_grande
    logical, intent(in) :: estado
    character(len=*), intent(in) :: nombreCliente

    type(node), pointer :: current, newNode

    allocate(newNode)
    newNode%id_ventanilla = id_ventanilla
    newNode%id_cliente = id_cliente
    newNode%nombreCliente = nombreCliente
    newNode%estado = estado
    newNode%cantidadImg_pila = cantidadImg_pila
    newNode%cantidadImg_pequena = cantidadImg_pequena
    newNode%cantidadImg_grande = cantidadImg_grande
    newNode%copiaGrande = cantidadImg_grande
    newNode%copiaPequena = cantidadImg_pequena
    newNode%next => null()


    if (.not. associated(self%head)) then
        self%head => newNode
    else
        current => self%head
        do while (associated(current%next))
            current => current%next
        end do

        current%next => newNode
    end if

    print *, 'ventanilla creada ', id_ventanilla
end subroutine agregar_lista
    
    subroutine print_ventanillas(self)
        class(linked_list), intent(in) :: self
    
        type(node), pointer :: current
    
        current => self%head
    
        print *, "---Ventanillas---"
        do while (associated(current))


            print *, "id_Ventanilla: ", current%id_ventanilla
            print *, "id_Cliente: ",current%id_cliente
            print *, "Nombre Cliente: ",current%nombreCliente
            if (current%estado .eqv. .true.) then
                print *, "Estado Ventanilla: Disponible "
            else
                print *, "Estado Ventanilla: Ocupado "
            end if
            print *, "Imagenes pequenas por procesar: ",current%cantidadImg_pequena
            print *, "Imagenes grandes por procesar: ",current%cantidadImg_grande
            print *, "Imagenes en pila: ",current%cantidadImg_pila
            print *, "------------------------"
            current => current%next
        end do
    end subroutine print_ventanillas

    function getIDCliente(self) result(clienteID)
        class(linked_list), intent(inout) :: self
        type(node), pointer :: current
        integer :: clienteID
        
        current => self%head
        
        if (associated(current)) then
            clienteID = current%id_Cliente
        else
            clienteID = 0 
        end if
    end function getIDCliente

    function getNombreCliente(self) result(nombre)
        class(linked_list), intent(inout) :: self
        type(node), pointer :: current
        character(len=:), allocatable :: nombre
        
        nombre = ""
        
        current => self%head
        
        if (associated(current) .and. current%estado .eqv. .false.) then
            nombre = current%nombreCliente
        else
            nombre = "--"
        end if
    end function getNombreCliente

    function getImgPequenas(self) result(contadorImgP)
        class(linked_list), intent(inout) :: self
        type(node), pointer :: current
        integer :: contadorImgP
        
        contadorImgP = 0
        
        current => self%head
        
        if (associated(current) .and. current%estado .eqv. .false.) then
            contadorImgP = current%cantidadImg_pequena
        end if
    end function getImgPequenas
    
    function getImgGrande(self) result(contadorImgG)
        class(linked_list), intent(inout) :: self
        type(node), pointer :: current
        integer :: contadorImgG
        
        contadorImgG = 0
        
        current => self%head
        
        if (associated(current) .and. current%estado .eqv. .false.) then
            contadorImgG = current%cantidadImg_grande
        end if
    end function getImgGrande
    
    function ventanillaDisponible(self) result (disponible)
        class(linked_list), intent(inout) :: self
        type(node), pointer :: current
        type(node), pointer :: lastNodeReturned => null()
        logical :: disponible
        disponible = .false.

        if (.not. associated(lastNodeReturned)) then
            current => self%head
        else
            current => lastNodeReturned%next
        end if

        do while (associated(current))
            if (current%estado) then
                disponible = .true.  ! La ventanilla está disponible
                exit  
            end if
            current => current%next
        end do
    end function ventanillaDisponible


function getIndiceVentanilla(self) result(indice)
        class(linked_list), intent(inout) :: self
        type(node), pointer :: current
        type(node), pointer :: lastNodeReturned => null()
        integer :: indice
        
        if (.not. associated(lastNodeReturned)) then
            current => self%head
        else
            current => lastNodeReturned%next
        end if

        if (.not. associated(current)) then
            indice = -1   ! Retorna -1 para indicar que no hay más índices
            
        else
            indice = current%id_Ventanilla
            lastNodeReturned => current
        end if
end function getIndiceVentanilla

subroutine actualizar_ventanilla(self, id_Cliente, nombre_cliente, cantidad_pequenas, cantidad_grandes)
    class(linked_list), intent(inout) :: self
    integer, intent(in) :: id_Cliente, cantidad_pequenas, cantidad_grandes
    character(len=*), intent(in) :: nombre_cliente
    type(node), pointer :: current
    integer:: i
    current => self%head

    do while (associated(current))
        if (current%estado) then
            current%id_Cliente = id_Cliente
            current%nombreCliente = nombre_cliente
            current%estado = .false.
            current%cantidadImg_pequena = cantidad_pequenas
            current%cantidadImg_grande = cantidad_grandes
            current%copiaGrande = cantidad_grandes
            current%copiaPequena = cantidad_pequenas

            exit 
        else if (.not. current%estado) then
            if (current%cantidadImg_grande > 0) then
                call current%pila_img%append(current%id_Cliente, "img_g")
                current%cantidadImg_grande = current%cantidadImg_grande - 1
                current%cantidadImg_pila = current%cantidadImg_pila + 1
            else if (current%cantidadImg_pequena > 0) then
                call current%pila_img%append(current%id_Cliente, "img_p")
                current%cantidadImg_pequena = current%cantidadImg_pequena - 1
                current%cantidadImg_pila = current%cantidadImg_pila + 1
            else if (current%cantidadImg_grande == 0 .and. current%cantidadImg_pequena == 0) then
            
                do i = 1, current%copiaGrande
                    call self%colaImpresoraG%pushGrande("img_g")
                end do

                do i = 1, current%copiaPequena
                    call self%colaImpresoraP%pushPequena("img_p")
                end do
            
                if (current%id_Cliente /= 0)then
                    call self%listaCEspera%append_clienteEspera(current%id_ventanilla, & 
                    current%id_Cliente, current%nombreCliente,current%copiaGrande,current%copiaPequena )
                end if
                current%id_Cliente = 0
                current%nombreCliente = "--"
                current%estado = .true.
                current%cantidadImg_pequena = 0
                current%cantidadImg_grande = 0
                current%cantidadImg_pila = 0
                current%copiaGrande = 0
                current%copiaPequena = 0
                call current%pila_img%clearPila()
            end if
        end if
        current => current%next
    end do

end subroutine actualizar_ventanilla

subroutine init_linked_list(self)
        class(linked_list), intent(inout) :: self

    if (associated(self%head)) then
            print *, "La lista ya está inicializada."
            return
    end if
    self%head => null()
end subroutine init_linked_list

end module linkedList


module cola_module
  implicit none
  private

  type,public :: cola
  type(node), pointer :: head => null() ! head of the list
  type(nodeOrdenG), pointer :: head_G => null()
  type(nodeOrdenP), pointer :: head_P => null()


  contains
        procedure :: push
        procedure :: print
        procedure :: getIndiceCliente
        procedure :: getImgPequenas
        procedure :: getImgGrande
        procedure :: eliminar_nodo
        procedure :: clientes_dot
        procedure :: agregar_imgP
        procedure :: topImgPequena_dot
        procedure :: getNombreCliente
        procedure :: topImgGrande_dot
        procedure :: agregar_imgG
        procedure :: graficaIdCliente
        procedure :: img_pasosSistema
  end type cola

  type :: node
      integer :: id
      character(len=:), allocatable :: nombre
      integer :: img_g
      integer :: img_p
      integer :: total_imagenes
      type(node), pointer :: next
  end type node

  type :: nodeOrdenG
        integer :: id
        character(len=:),allocatable :: nombre
        integer :: img_g
        integer :: img_p
        type(nodeOrdenG),pointer :: next
    end type nodeOrdenG

    type :: nodeOrdenP
        integer :: id
        character(len=:),allocatable :: nombre
        integer :: img_g
        integer :: img_p
        type(nodeOrdenP),pointer :: next
    end type nodeOrdenP

  contains

  subroutine img_pasosSistema  (self, io)
    class(cola), intent(in) :: self
    integer, intent(out) :: io
    character(len=100) :: command
    character(len=8) :: nombre_nodo,pasos
    character(len=:), allocatable :: firsts
    character(len=:), allocatable :: connections
    type(nodeOrdenG), pointer :: current
    integer ::  index, i, conteo,total
    integer, dimension(5) :: id_array ! Array para almacenar los IDs de los primeros 5 nodos

    current => self%head_G
    command = "dot -Tpng ./img_pasosSistema.dot -o ./img_pasosSistema.png"
    io = 1
    index = 0
    total = 0
    conteo = 0
    connections = ""
    firsts = ""

    open(newunit=io, file='./img_pasosSistema.dot')
    write(io, *) "digraph G {"
    write(io, *) "  node [shape=record];"
    write(io, *) "  rankdir=LR"

    ! Set title and background color
    write(io, *) "  graph [ bgcolor=white];"

    if (.not. associated(self%head_G)) then
        write(io, *) "  EmptyQueue;"
    else
        ! Almacenar los IDs de los primeros 5 nodos en un array
        do while (associated(current) .and. conteo < 1)
            id_array(conteo + 1) = current%id
            current => current%next
            conteo = conteo + 1
        end do

        ! Recorrer la lista nuevamente para imprimir los nodos con los IDs almacenados en orden inverso
        do conteo = 1, 1, -1
            current => self%head_G
            do while (associated(current))
                total = current%img_g + current%img_g + current%img_p
                write(pasos, '(I5)') total
                
                if (current%id == id_array(conteo)) then
                    write(nombre_nodo, '(I5)') index

                    write(io, *) '"nodo'//trim(nombre_nodo)//'"[label="{ |{', 'ID: ', current%id, &
                            '\n Nombre: ', current%nombre,'\n pasos: '// pasos // '}| }", fillcolor=orange, style=filled];'

                    index = index + 1
                    exit ! Salir del bucle interno una vez que se encuentra el nodo correspondiente al ID almacenado
                end if
                current => current%next
            end do
        end do
    end if

    write(io, *) connections
    write(io, *) "rankdir = LR"
    write(io, *) "}"

    close(io)

    call execute_command_line(command, exitstat=i)

    if (i == 1) then
        print *, "Ocurrió un error"
    else
        print *, "Imagen generada satisfactoriamente"
    end if
end subroutine img_pasosSistema

  subroutine graficaIdCliente(self, io, id)
        
    integer, intent(in) :: id
    class(cola), intent(in) :: self
    integer, intent(out) :: io
    character(len=100) :: command
    character(len=8) :: nombre_nodo
    character(len=:), allocatable :: firsts
    character(len=:), allocatable :: connections
    type(nodeOrdenG), pointer :: current
    integer ::  index, i

    current => self%head_G
    command = "dot -Tpng ./IdCliente.dot -o ./IdCliente.png"
    io = 1
    index = 0

    connections = ""
    firsts = ""

    open(newunit=io, file='./IdCliente.dot')
    write(io, *) "digraph G {"
    write(io, *) "  node [shape=ellipse];"
    write(io, *) "  rankdir=LR"

    ! Set title and background color
    write(io, *) "  graph [ bgcolor=white];"

    if (.not. associated(self%head_G)) then
        write(io, *) "  EmptyQueue;"
    else
        do while (associated(current))
            if (current%id == id) then
                write(nombre_nodo, '(I5)') index
    
                write(io, *) '"nodo'//trim(nombre_nodo)//'"[label="', 'ID: ', current%id, &
                        '\n Nombre: ', current%nombre,'\n IMG_G: ', current%img_g,'\n IMG_P: ', &
                        current%img_p, '", fillcolor=orange, style=filled];'
    
                
            end if
            current => current%next
            index = index + 1
        end do
    end if

    write(io, *) connections
    write(io, *) "rankdir = LR"
    write(io, *) "}"

    close(io)

    call execute_command_line(command, exitstat=i)

    if (i == 1) then
        print *, "Ocurrió un error"
    else
        print *, "Imagen generada satisfactoriamente"
    end if

end subroutine graficaIdCliente

subroutine agregar_imgP(self, id, nombre, img_g, img_p)
    class(cola), intent(inout) :: self
    integer, intent(in) :: id, img_g, img_p
    character(len=*), intent(in) :: nombre
    
    type(nodeOrdenP), pointer :: current, newNode, prev
    
    ! Crear un nuevo nodo
    allocate(newNode)
    newNode%id = id
    newNode%nombre = nombre
    newNode%img_g = img_g
    newNode%img_p = img_p
    newNode%next => null()
    
    if (.not. associated(self%head_P)) then
        ! La cola está vacía, insertar el nuevo nodo como cabeza
        self%head_P => newNode
    else
        ! La cola no está vacía, encontrar la posición de inserción
        prev => null()
        current => self%head_P
        
        do while (associated(current))
            if (img_p < current%img_p) then
                ! Insertar antes del nodo actual
                newNode%next => current
                if (associated(prev)) then
                    prev%next => newNode
                else
                    ! Nuevo nodo es la nueva cabeza de la cola
                    self%head_P => newNode
                end if
                return
            else if (img_p == current%img_p) then
                if (img_g > current%img_g) then
                    ! Insertar antes del nodo actual
                    newNode%next => current
                    if (associated(prev)) then
                        prev%next => newNode
                    else
                        self%head_P => newNode
                    end if
                    return
                end if
            end if
            prev => current
            current => current%next
        end do
        prev%next => newNode
    end if

end subroutine agregar_imgP

subroutine topImgPequena_dot(self, io)
    class(cola), intent(in) :: self
    integer, intent(out) :: io
    character(len=100) :: command
    character(len=8) :: nombre_nodo
    character(len=:), allocatable :: firsts
    character(len=:), allocatable :: connections
    type(nodeOrdenP), pointer :: current
    integer ::  index, i, conteo
    integer, dimension(5) :: id_array 
    current => self%head_P
    command = "dot -Tpng ./TopImgPequena.dot -o ./TopImgPequena.png"
    io = 1
    index = 0
    conteo = 0
    connections = ""
    firsts = ""

    open(newunit=io, file='./TopImgPequena.dot')
    write(io, *) "digraph G {"
    write(io, *) "  node [shape=ellipse];"
    write(io, *) "  rankdir=LR"

    write(io, *) "  graph [ bgcolor=white];"

    if (.not. associated(self%head_P)) then
        write(io, *) "  EmptyQueue;"
    else
        do while (associated(current) .and. conteo < 5)
            id_array(conteo + 1) = current%id
            current => current%next
            conteo = conteo + 1
        end do

        do conteo = 5, 1, -1
            current => self%head_P
            do while (associated(current))
                if (current%id == id_array(conteo)) then
                    write(nombre_nodo, '(I5)') index

                    write(io, *) '"nodo'//trim(nombre_nodo)//'"[label="', 'ID: ', current%id, &
                            '\n Nombre: ', current%nombre,'\n IMG_P: ', current%img_p, '", fillcolor=orange, style=filled];'

                    index = index + 1
                    exit ! Salir del bucle interno una vez que se encuentra el nodo correspondiente al ID almacenado
                end if
                current => current%next
            end do
        end do
    end if

    write(io, *) connections
    write(io, *) "rankdir = LR"
    write(io, *) "}"

    close(io)

    call execute_command_line(command, exitstat=i)

    if (i == 1) then
        print *, "Ocurrió un error"
    else
        print *, "Imagen generada satisfactoriamente"
    end if
end subroutine topImgPequena_dot


  subroutine agregar_imgG(self, id_c, nombre_c, img_g, img_p)
    class(cola), intent(inout) :: self
    integer, intent(in) :: id_c, img_g, img_p
    character(len=*), intent(in) :: nombre_c
    
    type(nodeOrdenG), pointer :: current, newNode, prev
    
    ! Crear un nuevo nodo
    allocate(newNode)
    newNode%id = id_c
    newNode%nombre = nombre_c
    newNode%img_g = img_g
    newNode%img_p = img_p
    newNode%next => null()
    
    if (.not. associated(self%head_G)) then
        ! La cola está vacía, insertar el nuevo nodo como cabeza
        self%head_G => newNode
    else
        ! La cola no está vacía, encontrar la posición de inserción
        current => self%head_G
        prev => null()
        
        do while (associated(current))
            if (img_g > current%img_g) then
                ! Insertar antes del nodo actual
                newNode%next => current
                if (associated(prev)) then
                    prev%next => newNode
                else
                    ! Nuevo nodo es la nueva cabeza de la cola
                    self%head_G => newNode
                end if
                return
            else if (img_g == current%img_g) then
                ! Comprobar para imágenes pequeñas si son iguales
                if (img_p < current%img_p) then
                    ! Insertar antes del nodo actual
                    newNode%next => current
                    if (associated(prev)) then
                        prev%next => newNode
                    else
                        ! Nuevo nodo es la nueva cabeza de la cola
                        self%head_G => newNode
                    end if
                    return
                end if
            end if
            
            prev => current
            current => current%next
        end do
        
        ! Llegamos al final de la cola, insertar el nuevo nodo al final
        prev%next => newNode
    end if
end subroutine agregar_imgG

subroutine topImgGrande_dot(self, io)
    class(cola), intent(in) :: self
    integer, intent(out) :: io
    character(len=100) :: command
    character(len=8) :: nombre_nodo
    character(len=:), allocatable :: firsts
    character(len=:), allocatable :: connections
    type(nodeOrdenG), pointer :: current
    integer ::  index, i, conteo
    integer, dimension(5) :: id_array ! Array para almacenar los IDs de los primeros 5 nodos

    current => self%head_G
    command = "dot -Tpng ./TopImgGrande.dot -o ./TopImgGrande.png"
    io = 1
    index = 0
    conteo = 0
    connections = ""
    firsts = ""

    open(newunit=io, file='./TopImgGrande.dot')
    write(io, *) "digraph G {"
    write(io, *) "  node [shape=ellipse];"
    write(io, *) "  rankdir=LR"

    ! Set title and background color
    write(io, *) "  graph [ bgcolor=white];"

    if (.not. associated(self%head_G)) then
        write(io, *) "  EmptyQueue;"
    else
        ! Almacenar los IDs de los primeros 5 nodos en un array
        do while (associated(current) .and. conteo < 5)
            id_array(conteo + 1) = current%id
            current => current%next
            conteo = conteo + 1
        end do

        ! Recorrer la lista nuevamente para imprimir los nodos con los IDs almacenados en orden inverso
        do conteo = 5, 1, -1
            current => self%head_G
            do while (associated(current))
                if (current%id == id_array(conteo)) then
                    write(nombre_nodo, '(I5)') index

                    write(io, *) '"nodo'//trim(nombre_nodo)//'"[label="', 'ID: ', current%id, &
                            '\n Nombre: ', current%nombre,'\n IMG_G: ', current%img_g, '", fillcolor=orange, style=filled];'

                    index = index + 1
                    exit ! Salir del bucle interno una vez que se encuentra el nodo correspondiente al ID almacenado
                end if
                current => current%next
            end do
        end do
    end if

    write(io, *) connections
    write(io, *) "rankdir = LR"
    write(io, *) "}"

    close(io)

    call execute_command_line(command, exitstat=i)

    if (i == 1) then
        print *, "Ocurrió un error"
    else
        print *, "Imagen generada satisfactoriamente"
    end if
end subroutine topImgGrande_dot


  subroutine push(self, id,nombre,img_g,img_p,total_imagenes)
      class(cola), intent(inout) :: self
      integer, intent(in) :: id,img_g,img_p,total_imagenes
      character(len=*), intent(in) :: nombre
      
  
      type(node), pointer :: current, newNode
  
      ! Crear un nuevo nodo
      allocate(newNode)
      newNode%id = id
      newNode%nombre = nombre
      newNode%img_g = img_g
      newNode%img_p = img_p
      newNode%total_imagenes = total_imagenes
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
  
      !print *, 'pushed:: ', id,nombre,img_g,img_p
  end subroutine push


  subroutine print(self)
      class(cola), intent(in) :: self
  
      type(node), pointer :: current
  
      current => self%head
  
      ! Recorre la lista y imprime los valores
      do while (associated(current))
          print *, "id: ", current%id
          print *, "nombre: ",current%nombre
          print *, "imagenes_grandes: ",current%img_g
          print *, "imagenes pequenas: ",current%img_p
          print *, "total de imagenes: ",current%total_imagenes
          current => current%next
      end do
  end subroutine print

  function getIndiceCliente(self) result(indice)
    class(cola), intent(inout) :: self
    type(node), pointer :: current
    integer :: indice

    current => self%head

    do while(associated(current))
        ! Si hay un nodo actual, devuelve su ID y sale del bucle
        indice = current%id
        return
    end do

    ! Si no se encontró ningún nodo válido, se establece el índice en -1
    indice = -1
end function getIndiceCliente

function getNombreCliente(self) result(nombre)
    class(cola), intent(inout) :: self
    type(node), pointer :: current
    character(len=:), allocatable :: nombre
    type(node), pointer :: lastNodeReturned => null()
    
    current => self%head
    do while(associated(current))
        nombre = current%nombre
        return
    end do

end function getNombreCliente

function getImgPequenas(self) result(cantidad)
    class(cola), intent(inout) :: self
    type(node), pointer :: current
    type(node), pointer :: lastNodeReturned => null()
    integer :: cantidad
    

    current => self%head
    do while(associated(current))
        cantidad = current%img_p
        return
    end do
    
end function getImgPequenas

function getImgGrande(self) result(cantidad)
    class(cola), intent(inout) :: self
    type(node), pointer :: current
    integer :: cantidad
    
    current => self%head
    do while(associated(current))
        cantidad = current%img_g
        return
    end do
end function getImgGrande


subroutine eliminar_nodo(self, id)
    class(cola), intent(inout) :: self
    integer, intent(in) :: id

    type(node), pointer :: current
    type(node), pointer :: prev

    ! Verificar si la cola está vacía
    if (.not. associated(self%head)) then
        print *, "Error: La cola está vacía."
        return
    endif

    current => self%head
    prev => self%head

    do while (associated(current))
        if (current%id == id) then
            if (associated(prev, current)) then
                self%head => current%next
            else
                prev%next => current%next
            endif
            deallocate(current)
            return
        endif
        prev => current
        current => current%next
    end do

    print *, "Error: Nodo con el ID ", id, " no encontrado."
end subroutine eliminar_nodo


subroutine clientes_dot(self, io)
    class(cola), intent(in) :: self
    integer, intent(out) :: io
    character(len=100) :: command
    character(len=8) :: name
    character(len=:), allocatable :: firsts
    character(len=:), allocatable :: connections
    type(node), pointer :: current
    integer :: id_counter, index, i

    current => self%head
    command = "dot -Tpng ./listaClientes.dot -o ./listaClientes.png"
    io = 1
    index = 0

    connections = ""
    firsts = ""

    open(newunit=io, file='./listaClientes.dot')
    write(io, *) "digraph G {"
    write(io, *) "  node [shape=record];"
    write(io, *) "  rankdir=LR"

    if (.not. associated(self%head)) then
        write(io, *) "  EmptyQueue;"
    else
        do while (associated(current))
            write(name, '(I5)') index

            write(io, *) '"nodo'//trim(name)//'"[label="{ |{', 'ID: ', current%id, &
                    '\n Nombre: ', current%nombre,'\n IMG_G: ', current%img_g,'\n IMG_P: ', &
                    current%img_p, '}| }", fillcolor=white, style=filled];'

            if(associated(current%next)) then
                connections = connections//'"nodo'//trim(name)//'"->'
            else 
                connections = connections//'"nodo'//trim(name)//'"'
            end if

            current => current%next
            index = index + 1
        end do
    end if

    write(io, *) connections
    write(io, *) "rankdir = LR"
    write(io, *) "}"
    
    close(io)

    call execute_command_line(command, exitstat=i)

    if (i == 1) then
        print *, "Ocurrió un error"
    else
        print *, "Imagen generada satisfactoriamente"
    end if

end subroutine clientes_dot

end module cola_module

