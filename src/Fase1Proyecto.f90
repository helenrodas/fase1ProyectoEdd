module linkedList
    !use pila_module 
    implicit none

    type :: linked_list
        type(node), pointer :: head => null() ! head of the list
        type(node), pointer :: lastNodeReturned => null()

        contains
            procedure :: agregar_lista
            procedure :: print_ventanillas
            procedure :: delete_by_position
            procedure :: ventanillaDisponible
            procedure :: getIndiceVentanilla
            procedure :: actualizar_ventanilla
            procedure :: print_dot
    end type linked_list

    type :: node
        !type(pila) :: pila_img
        integer :: id_ventanilla, id_cliente, cantidadImg_pila, cantidadImg_pequena, cantidadImg_grande
        logical :: estado
        type(node), pointer :: next
    end type node

contains

    subroutine agregar_lista(self, id_ventanilla, id_cliente, estado, cantidadImg_pila, cantidadImg_pequena, cantidadImg_grande)
        class(linked_list), intent(inout) :: self
        integer, intent(in) :: id_ventanilla, id_cliente, cantidadImg_pila, cantidadImg_pequena, cantidadImg_grande
        logical, intent(in) :: estado

        type(node), pointer :: current, newNode

        ! Crear un nuevo nodo
        allocate(newNode)
        newNode%id_ventanilla = id_ventanilla
        newNode%id_cliente = id_cliente
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

        function ventanillaDisponible(self,idCliente) result (disponible)
            class(linked_list), intent(inout) :: self
            type(node), pointer :: current
            type(node), pointer :: lastNodeReturned => null()
            logical :: disponible
            integer::idCliente
            disponible = .true.

            if (.not. associated(lastNodeReturned)) then
                current => self%head
            else
                current => lastNodeReturned%next
            end if

            do while (associated(current))
                ! Verifica si la ventanilla current está ocupada
                if (idCliente/=0) then
                    disponible = .false.  ! La ventanilla está disponible
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



    subroutine actualizar_ventanilla(self, id_Cliente,cantidad_pequenas,cantidad_grandes,size)
        class(linked_list), intent(inout) :: self
        integer, intent(in) :: id_Cliente,cantidad_pequenas,cantidad_grandes,size
        type(node), pointer :: current
        type(node), pointer :: lastNodeReturned 

        ! Inicializa lastNodeReturned si es la primera llamada
        if (.not. associated(self%lastNodeReturned)) then
            self%lastNodeReturned => null()
        endif

        ! Si lastNodeReturned no está asociado, comienza desde el inicio
        if (.not. associated(self%lastNodeReturned)) then
            current => self%head
        else
            current => self%lastNodeReturned%next
        endif

        ! Recorre la lista hasta encontrar una ventana disponible
        do while (associated(current))
            if (current%estado) then
                current%id_Cliente = id_Cliente
                current%estado = .false.
                current%cantidadImg_pequena = cantidad_pequenas
                current%cantidadImg_grande = cantidad_grandes
                current%cantidadImg_pila = size

                exit
            else
                self%lastNodeReturned => current
                current => current%next
            endif
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
  
  type :: cola
  type(node), pointer :: head => null() ! head of the list

  contains
        procedure :: push
        procedure :: print
        procedure :: getIndiceCliente
        procedure :: getImgPequenas
        procedure :: getImgGrande
        procedure :: eliminar_nodo
        procedure :: clientes_dot
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
        !print *,"No hay clientes"
    else
        ! Obtiene el valor del nodo current y establece lastNodeReturned en el nodo current
        indice = current%id     
        lastNodeReturned => current
    end if
end function getIndiceCliente

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

module pila_module
    implicit none
    
    type :: node
        private
        character(len=:), allocatable :: imagen
        type(node), pointer :: next
    end type node

    type, public :: pila
    private
    type(node), pointer :: head => null() ! head of the list
  
    contains
        procedure :: agregar_imagen
        procedure :: printPila
        procedure :: init_pila
        procedure :: tamano_pila
        !procedure :: eliminar_nodo
    end type pila
  
  
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
  
  
    subroutine printPila(self)
        class(pila), intent(in) :: self
    
        type(node), pointer :: current
    
        current => self%head
    
        ! Recorre la lista y imprime los valores
        do while (associated(current))
            print *, "Imagen: ", current%imagen
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

  end module pila_module
  