module linkedList
    !use linkedList
    use pila_module 
    implicit none
    private

    type, public :: linked_list
        type(node), pointer :: head => null() ! head of the list
        type(node), pointer :: tail => null()
        type(node), pointer :: lastNodeReturned => null()

        contains
            procedure :: agregar_lista
            procedure :: print_ventanillas
            procedure :: delete_by_position
            procedure :: ventanillaDisponible
            procedure :: getIndiceVentanilla
            procedure :: actualizar_ventanilla
            procedure :: print_dot
            procedure :: printVent
            procedure :: insert
            procedure :: getIDCliente
            procedure :: getNombreCliente
            procedure :: getImgPequenas
            procedure :: getImgGrande
            ! procedure :: segundaActualizacion
    end type linked_list


    type :: node
        !type(pila) :: pila_img
        integer :: index
        integer :: id_ventanilla, id_cliente, cantidadImg_pila, cantidadImg_pequena, cantidadImg_grande
        character(len=:), allocatable :: nombreCliente
        logical :: estado
        !type(node), pointer :: next
        type(node), pointer :: next => null()
        type(node), pointer :: prev => null()
        type(pila) :: pila_img
    end type node

contains
subroutine insert(self,id_ventanilla, tipo_img)
    class(linked_list), intent(inout) :: self
    integer, intent(in) :: id_ventanilla
    character(len=*), intent(in) :: tipo_img

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

        ! Crear un nuevo nodo
        allocate(newNode)
        newNode%id_ventanilla = id_ventanilla
        newNode%id_cliente = id_cliente
        newNode%nombreCliente = nombreCliente
        newNode%estado = estado
        newNode%cantidadImg_pila = cantidadImg_pila
        newNode%cantidadImg_pequena = cantidadImg_pequena
        newNode%cantidadImg_grande = cantidadImg_grande
        newNode%next => null()

        ! Inicializar la pila dentro del nuevo nodo
        !call init_pila(newNode%pila_img)

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

        print *, 'ventanilla creada ', id_ventanilla
    end subroutine agregar_lista
    

subroutine delete_by_position(self, position)
    class(linked_list), intent(inout) :: self
    integer, intent(in) :: position
    type(node), pointer :: current, previous
    integer :: counter

    current => self%head
    previous => null()

    if(position == 1) then
        self%head => current%next
        deallocate(current)
        return
    end if

    counter = 1
    do while (associated(current) .and. counter < position)
        previous => current
        current => current%next
        counter = counter + 1
    end do

    if (.not. associated(current)) then
        print *, 'No se encontro la posicion'
        return
    end if

    previous%next => current%next
    deallocate(current)
    end subroutine delete_by_position


    subroutine print_ventanillas(self)
        class(linked_list), intent(in) :: self
    
        type(node), pointer :: current
    
        current => self%head
    
        ! Recorre la lista y imprime los valores
        print *, "---Ventanillas---"
        do while (associated(current))

            

            print *, "id_Ventanilla: ", current%id_ventanilla
            print *, "id_Cliente: ",current%id_cliente
            print *, "Nombre Cliente: ",current%nombreCliente
            ! print *, "Estado Ventanilla: ",current%estado
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
        
        ! Obtener el nodo actual de la lista enlazada
        current => self%head
        
        ! Si hay un nodo asociado, obtener el ID del cliente asociado
        if (associated(current)) then
            clienteID = current%id_Cliente
        else
            ! Si no hay un nodo asociado, devolver un valor predeterminado o manejar el caso de error según sea necesario
            clienteID = 0 ! Por ejemplo, devolver -1 si no hay ningún cliente asociado al nodo actual
        end if
    end function getIDCliente

    function getNombreCliente(self) result(nombre)
        class(linked_list), intent(inout) :: self
        type(node), pointer :: current
        character(len=:), allocatable :: nombre
        
        ! Inicializar la cadena de nombre
        nombre = ""
        
        ! Obtener el nodo actual de la lista enlazada
        current => self%head
        
        ! Si hay un nodo asociado y tiene un cliente válido, obtener el nombre del cliente
        if (associated(current) .and. current%estado .eqv. .false.) then
            nombre = current%nombreCliente
        else
            ! Si no hay un nodo asociado o si el cliente asociado al nodo actual está en estado de espera, devolver una cadena vacía o manejar el caso de error según sea necesario
            ! Por ejemplo, devolver una cadena que indique que no hay cliente asociado o que el cliente está en espera
            nombre = "--"
        end if
    end function getNombreCliente

    function getImgPequenas(self) result(num_img_pequenas)
        class(linked_list), intent(inout) :: self
        type(node), pointer :: current
        integer :: num_img_pequenas
        
        ! Inicializar el número de imágenes pequeñas
        num_img_pequenas = 0
        
        ! Obtener el nodo actual de la lista enlazada
        current => self%head
        
        ! Si hay un nodo asociado y tiene un cliente válido, obtener la cantidad de imágenes pequeñas del cliente
        if (associated(current) .and. current%estado .eqv. .false.) then
            num_img_pequenas = current%cantidadImg_pequena
        end if
    end function getImgPequenas
    
    function getImgGrande(self) result(num_img_grandes)
        class(linked_list), intent(inout) :: self
        type(node), pointer :: current
        integer :: num_img_grandes
        
        ! Inicializar el número de imágenes grandes
        num_img_grandes = 0
        
        ! Obtener el nodo actual de la lista enlazada
        current => self%head
        
        ! Si hay un nodo asociado y tiene un cliente válido, obtener la cantidad de imágenes grandes del cliente
        if (associated(current) .and. current%estado .eqv. .false.) then
            num_img_grandes = current%cantidadImg_grande
        end if
    end function getImgGrande
    
    


    function ventanillaDisponible(self) result (disponible)
        class(linked_list), intent(inout) :: self
        type(node), pointer :: current
        type(node), pointer :: lastNodeReturned => null()
        logical :: disponible
        !integer::idCliente
        disponible = .false.

        if (.not. associated(lastNodeReturned)) then
            current => self%head
        else
            current => lastNodeReturned%next
        end if

        do while (associated(current))
            ! Verifica si la ventanilla current está ocupada
            if (current%estado) then
                disponible = .true.  ! La ventanilla está disponible
                exit  ! Sal del bucle
            end if
            current => current%next
        end do
    end function ventanillaDisponible


function getIndiceVentanilla(self) result(indice)
        class(linked_list), intent(inout) :: self
        type(node), pointer :: current
        type(node), pointer :: lastNodeReturned => null()
        integer :: indice
        
        ! Si es la primera llamada o no hay un último nodo devuelto, comenzar desde la cabeza
        if (.not. associated(lastNodeReturned)) then
            current => self%head
        else
            current => lastNodeReturned%next
        end if

        ! Si no hay nodo current, significa que se ha alcanzado el final de la lista
        if (.not. associated(current)) then
            indice = -1   ! Retorna -1 para indicar que no hay más índices
            !print *,"No hay ventanillas disponibles"
            
        else
            ! Obtiene el valor del nodo current y establece lastNodeReturned en el nodo current
            indice = current%id_Ventanilla
            lastNodeReturned => current
        end if
end function getIndiceVentanilla

! subroutine actualizar_ventanilla(self, id_Cliente,nombre_cliente,cantidad_grandes,cantidad_pequenas)
!     class(linked_list), intent(inout) :: self
!     integer, intent(in) :: id_Cliente,cantidad_pequenas,cantidad_grandes
!     type(node), pointer :: current
!     character(len=*), intent(in) :: nombre_cliente
!     type(node), pointer :: lastNodeReturned 

!     ! Inicializa lastNodeReturned si es la primera llamada
!     if (.not. associated(self%lastNodeReturned)) then
!         self%lastNodeReturned => null()
!     endif

!     ! Si lastNodeReturned no está asociado, comienza desde el inicio
!     if (.not. associated(self%lastNodeReturned)) then
!         current => self%head
!     else
!         current => self%lastNodeReturned%next
!     endif

!     ! Recorre la lista hasta encontrar una ventana disponible
!     do while (associated(current))
!         if (current%estado) then
!             current%id_Cliente = id_Cliente
!             current%nombreCliente = nombre_cliente
!             current%estado = .false.
!             current%cantidadImg_pequena = cantidad_pequenas
!             current%cantidadImg_grande = cantidad_grandes
!             current%cantidadImg_pila = 0

!             exit
!         else
!             self%lastNodeReturned => current
!             current => current%next
!         endif
!     end do

! end subroutine actualizar_ventanilla

! subroutine segundaActualizacion(self, id_Cliente,cantidad_grandes,cantidad_pequenas)
!     class(linked_list), intent(inout) :: self
!     integer, intent(inout) :: id_Cliente,cantidad_pequenas,cantidad_grandes
!     type(node), pointer :: current

!     current => self%head

!     do while (associated(current))
!         if (current%estado .eqv. .false.) then
!             if (cantidad_grandes > 0) then
!                 call current%pila_img%actualizarPila(id_Cliente, "img_g")
!                 cantidad_grandes = cantidad_grandes - 1
!                 current%cantidadImg_grande = cantidad_grandes
!             else if (cantidad_pequenas > 0) then
!                 call current%pila_img%actualizarPila(id_Cliente, "img_p")
!                 cantidad_pequenas = cantidad_pequenas - 1
!                 current%cantidadImg_pequena = cantidad_pequenas
!             else if (cantidad_grandes == 0 .and. cantidad_pequenas == 0) then
!                 current%id_Cliente = 0
!                 current%nombreCliente = "--"
!                 current%estado = .true.
!                 current%cantidadImg_pequena = 0
!                 current%cantidadImg_grande = 0
!                 current%cantidadImg_pila = 0
!             end if
!             current => current%next
!         end if
!         current => current%next
!     end do

! end subroutine segundaActualizacion




subroutine actualizar_ventanilla(self, id_Cliente, nombre_cliente, cantidad_pequenas, cantidad_grandes)
    class(linked_list), intent(inout) :: self
    integer, intent(in) :: id_Cliente, cantidad_pequenas, cantidad_grandes
    character(len=*), intent(in) :: nombre_cliente
    type(node), pointer :: current

    current => self%head

    do while (associated(current))
        if (current%estado) then
            current%id_Cliente = id_Cliente
            current%nombreCliente = nombre_cliente
            current%estado = .false.
            current%cantidadImg_pequena = cantidad_pequenas
            current%cantidadImg_grande = cantidad_grandes
            exit ! Salir del bucle si se actualiza la ventana
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
                current%id_Cliente = 0
                current%nombreCliente = "--"
                current%estado = .true.
                current%cantidadImg_pequena = 0
                current%cantidadImg_grande = 0
                current%cantidadImg_pila = 0
            end if
        end if
        current => current%next
    end do

end subroutine actualizar_ventanilla


    
    
    
    
    

        subroutine init_linked_list(self)
            class(linked_list), intent(inout) :: self
    
            ! No es necesario inicializar la lista enlazada si ya está inicializada.
        if (associated(self%head)) then
                print *, "La lista ya está inicializada."
                return
        end if
        self%head => null()
    end subroutine init_linked_list


    subroutine print_dot(self, filename)
        class(linked_list), intent(inout) :: self ! referencia a la lista
        character(len=*), intent(in) :: filename ! nombre del archivo
        class(node), pointer :: current ! puntero al nodo current
        integer :: id_ventana ! id del nodo

        ! puntero al nodo current
        current => self%head

        ! abrir el archivo
        open(10, file=filename, status='replace')

        ! escribir el encabezado
        write(10, '(a)') "digraph G {" ! encabezado del archivo dot
        write(10, '(a)') "  node [shape=circle];"  ! forma de los nodos
        write(10, '(a)') "  rankdir=TB" ! orientación del grafo

        if ( .not. associated(current))  then ! si la lista está vacía
            write(10, '(a)') "  EmptyList;" ! escribir un nodo que diga que la lista está vacía
        else ! si la lista no está vacía
            ! escribir la arista de la cabeza al primer nodo
            ! escribir la arista de la cola al último nodo

            do ! recorrer la lista
                if (.not. associated(current)) exit ! si no hay más nodos, salir del bucle
                id_ventana = current%id_ventanilla
                ! crear el nodo
                write(10, '(a, i0, a, i0, a)') '  Node', id_ventana, ' [label="ventanilla= ', id_ventana, '"];'
                ! escribir las aristas
                if (associated(current%next)) then
                    write(10, '(a, i0, a, i0)') '  Node', current%id_ventanilla, ' -> Node', current%next%id_ventanilla, ';' ! arista del nodo current al siguiente nodo
                end if

                ! avanzar al siguiente nodo
                current => current%next
            end do
        end if
        ! escribir el pie del archivo
        write(10, '(a)') "}"
        ! cerrar el archivo
        close(10)
    end subroutine print_dot




end module linkedList

module cola_module
  implicit none
  private

  type,public :: cola
  type(node), pointer :: head => null() ! head of the list

  contains
        procedure :: push
        procedure :: print
        procedure :: getIndiceCliente
        procedure :: getImgPequenas
        procedure :: getImgGrande
        procedure :: eliminar_nodo
        procedure :: clientes_dot
        procedure :: orden_imagenPequena
        procedure :: topImgPequena_dot
        procedure :: getNombreCliente
        procedure :: topImgGrande_dot
      ! Agregamos los procedimientos restantes de la cola
  end type cola

  type :: node
      integer :: id
      character(len=:), allocatable :: nombre
      integer :: img_g
      integer :: img_p
      integer :: total_imagenes
      type(node), pointer :: next
  end type node


  contains

  subroutine orden_imagenPequena(self, id, nombre, img_g, img_p)
    class(cola), intent(inout) :: self
    integer, intent(in) :: id, img_g, img_p
    character(len=*), intent(in) :: nombre
    
    type(node), pointer :: current, newNode, prev
    
    ! Crear un nuevo nodo
    allocate(newNode)
    newNode%id = id
    newNode%nombre = nombre
    newNode%img_g = img_g
    newNode%img_p = img_p
    newNode%next => null()
    
    if (.not. associated(self%head)) then
        ! La cola está vacía, insertar el nuevo nodo como cabeza
        self%head => newNode
    else
        ! La cola no está vacía, encontrar la posición de inserción
        prev => null()
        current => self%head
        
        do while (associated(current))
            if (img_p < current%img_p) then
                ! Insertar antes del nodo actual
                newNode%next => current
                if (associated(prev)) then
                    prev%next => newNode
                else
                    ! Nuevo nodo es la nueva cabeza de la cola
                    self%head => newNode
                end if
                return
            else if (img_p == current%img_p) then
                ! Si las imágenes pequeñas son iguales, verificar imágenes grandes
                if (img_g > current%img_g) then
                    ! Insertar antes del nodo actual
                    newNode%next => current
                    if (associated(prev)) then
                        prev%next => newNode
                    else
                        ! Nuevo nodo es la nueva cabeza de la cola
                        self%head => newNode
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
end subroutine orden_imagenPequena


subroutine topImgPequena_dot(self, io)
    class(cola), intent(in) :: self
    integer, intent(out) :: io
    character(len=100) :: command
    character(len=8) :: nombre_nodo
    character(len=:), allocatable :: firsts
    character(len=:), allocatable :: connections
    type(node), pointer :: current
    integer :: index, i, j, min_idx
    integer :: imgP_array(5) ! Array para almacenar las cantidades de imágenes pequeñas de los primeros 5 nodos
    integer :: temp_id

    current => self%head
    command = "dot -Tpng ./TopImgPequena.dot -o ./TopImgPequena.png"
    io = 1
    index = 0
    connections = ""
    firsts = ""

    open(newunit=io, file='./TopImgPequena.dot')
    write(io, *) "digraph G {"
    write(io, *) "  node [shape=ellipse];"
    write(io, *) "  rankdir=TB" ! Cambiar el sentido del rankdir a "top-bottom"

    ! Almacenar las cantidades de imágenes pequeñas de los primeros 5 nodos en un array
    do i = 1, 5
        imgP_array(i) = current%img_p
        current => current%next
    end do

    ! Ordenar el array imgP_array en función de la cantidad de imágenes pequeñas (de menor a mayor)
    do i = 1, 4
        min_idx = i
        do j = i + 1, 5
            if (imgP_array(j) <= imgP_array(min_idx)) then
                min_idx = j
            end if
        end do
        ! Intercambiar elementos
        temp_id = imgP_array(i)
        imgP_array(i) = imgP_array(min_idx)
        imgP_array(min_idx) = temp_id
    end do

    ! Recorrer el array ordenado en orden inverso y escribir los nodos en el archivo DOT
    current => self%head
    do i = 5, 1, -1
        do while (associated(current))
            if (current%img_p == imgP_array(i)) then
                write(nombre_nodo, '(I5)') index
                write(io, *) '"nodo'//trim(nombre_nodo)//'"[label="', 'ID: ', current%id, &
                    '\n Nombre: ', current%nombre,'\n Img_pequena: ', current%img_p, '", fillcolor=white];'
                index = index + 1
                exit ! Salir del bucle interno una vez que se encuentra el nodo correspondiente
            end if
            current => current%next
        end do
        current => self%head ! Reiniciar el puntero para el siguiente nodo
    end do

    write(io, *) connections
    write(io, *) "}"

    close(io)

    call execute_command_line(command, exitstat=i)

    if (i == 1) then
        print *, "Ocurrió un error"
    else
        print *, "Imagen generada satisfactoriamente"
    end if
end subroutine topImgPequena_dot


subroutine topImgGrande_dot(self, io)
    class(cola), intent(in) :: self
    integer, intent(out) :: io
    character(len=100) :: command
    character(len=8) :: nombre_nodo
    character(len=:), allocatable :: firsts
    character(len=:), allocatable :: connections
    type(node), pointer :: current
    integer ::  index, i, conteo
    integer, dimension(5) :: imgG_array ! Array para almacenar los IDs de los primeros 5 nodos

    current => self%head
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
    !write(io, *) "  graph [label=""TopImgPequena"", bgcolor=lightblue];"

    if (.not. associated(self%head)) then
        write(io, *) "  EmptyQueue;"
    else
        ! Almacenar los IDs de los primeros 5 nodos en un array
        do while (associated(current) .and. conteo < 5)
            imgG_array(conteo + 1) = current%id
            current => current%next
            conteo = conteo + 1
        end do

        ! Recorrer la lista nuevamente para imprimir los nodos con los IDs almacenados en orden inverso
        do conteo = 5, 1, -1
            current => self%head
            do while (associated(current))
                if (current%id == imgG_array(conteo)) then
                    write(nombre_nodo, '(I5)') index

                    write(io, *) '"nodo'//trim(nombre_nodo)//'"[label="', 'ID: ', current%id, &
                            '\n Nombre: ', current%nombre,'\n Img_Grande: ', current%img_G, '", fillcolor=white];'

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
    
    if (.not. associated(lastNodeReturned)) then
        current => self%head
    else
        current => lastNodeReturned%next
    end if

    if (.not. associated(current)) then
        nombre = " -- "   
    else
        nombre = current%nombre 
        lastNodeReturned => current
    end if
end function getNombreCliente

function getImgPequenas(self) result(cantidad)
    class(cola), intent(inout) :: self
    type(node), pointer :: current
    type(node), pointer :: lastNodeReturned => null()
    integer :: cantidad
    
    ! Si es la primera llamada o no hay un último nodo devuelto, comenzar desde la cabeza
    if (.not. associated(lastNodeReturned)) then
        current => self%head
    else
        current => lastNodeReturned%next
    end if

    ! Si no hay nodo current, significa que se ha alcanzado el final de la lista
    if (.not. associated(current)) then
        cantidad = -1   ! Retorna -1 para indicar que no hay más índices
        !print *,"No hay clientes"
    else
        ! Obtiene el valor del nodo current y establece lastNodeReturned en el nodo current
        cantidad = current%img_p   
        lastNodeReturned => current
    end if
end function getImgPequenas

function getImgGrande(self) result(cantidad)
    class(cola), intent(inout) :: self
    type(node), pointer :: current
    type(node), pointer :: lastNodeReturned => null()
    integer :: cantidad
    
    ! Si es la primera llamada o no hay un último nodo devuelto, comenzar desde la cabeza
    if (.not. associated(lastNodeReturned)) then
        current => self%head
    else
        current => lastNodeReturned%next
    end if

    ! Si no hay nodo current, significa que se ha alcanzado el final de la lista
    if (.not. associated(current)) then
        cantidad = -1   ! Retorna -1 para indicar que no hay más índices
        !print *,"No hay clientes"
    else
        ! Obtiene el valor del nodo current y establece lastNodeReturned en el nodo current
        cantidad = current%img_g   
        lastNodeReturned => current
    end if
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



!   subroutine eliminar_nodo(self, id)
!       class(cola), intent(inout) :: self
!       integer, intent(in) :: id
  
!       type(node), pointer :: current
!       type(node), pointer :: prev
  
!       ! Verificar si la cola está vacía
!       if (.not. associated(self%head)) then
!           print *, "Error: La cola está vacía."
!           return
!       endif
  
!       current => self%head
!       prev => self%head
  
!       do while (associated(current))
!           if (current%id == id) then
!               if (associated(prev, current)) then
!                   self%head => current%next
!               else
!                   prev%next => current%next
!               endif
!               deallocate(current)
!               return
!           endif
!           prev => current
!           current => current%next
!       end do
  
!       print *, "Error: Nodo con el ID ", id, " no encontrado."
!   end subroutine eliminar_nodo

  
      ! subroutine printPila(self)
    !     class(pila), intent(in) :: self
    
    !     type(node), pointer :: current
    
    !     current => self%head
    
    !     ! Recorre la lista y imprime los valores
    !     do while (associated(current))
    !         print *, "Imagen: ", current%imagen
    !         current => current%next
    !     end do
    ! end subroutine printPila