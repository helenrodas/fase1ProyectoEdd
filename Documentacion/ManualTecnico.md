# Manual Tecnico

## Helen Rodas - 202200066

### El proyecto de Pixel Print Studio consta una aplicacion para poder llevar el registro de clientes con sus impresiones e imagenes, por medio de listas,pilas y colas. Utilizando fortran como lenguaje de programacion.

## main
En este archivo se encuentra en la carpeta app pues es donde inicia el proyecto, imprimo menus de opciones para que puedan mostrarse en consola
```fortran
program main
  use:: json_module
  use linkedList
  use cola_module
  !use pila_module
  
  implicit none
  integer :: option,id_asInt,img_g_asInt,img_p_asInt, contador_pasos,total_img,io
  integer :: windowsAmount
  character(len=1) :: dummy_char
  
  !integer :: option
  type(linked_list) :: mylista
  type(cola) :: cola_clientes
  !type(pila) :: pila_imagenes
  type(json_file) :: json
  type(json_core) :: jsonc
  type(json_value), pointer :: listPointer, animalPointer, attributePointer

  logical :: found
  integer :: size, i,cantidad_ventanillas,clienteId_temp
  character(:), allocatable :: id, nombre, img_p, img_g
  io = 1
    do
        call print_menu()
        read(*, *) option
  
        select case(option)
        case(1)
            call parametros_iniciales()
        case(2)
          !print *, "Se encuentra en pasos"
            !call pasos()
          call pasoUno()
        case(3)
            call pasosMemoria()
            
        !     call cantidad_ventanillas()
        case(4)
            call reportes()
            print *, "Ingrese el ID del cliente a buscar: "
            read*, clienteId_temp
            call cola_clientes%graficaIdCliente(io,clienteId_temp)
        print *, "----------------"
        ! case(5)
        !     call estado_memoria_estructuras()
        case(6)
            exit
        case default
            print *, "Error!. Por favor seleccione una opcion valida."
        end select
    end do

  contains

subroutine print_menu()
  print *, "...................................."
  print *, "         Menu Principal             "
  print *, "...................................."
  print *, "1. Parametros iniciales"
  print *, "2. Ejecutar paso"
  print *, "3. Estado en memoria de las estructuras"
  print *, "4. Reportes"
  print *, "5. Acerca de "
  print *, "6. Salir"
  print *, "...................................."
  print *, "Ingrese el numero de la opcion deseada:"
end subroutine print_menu

subroutine parametros_iniciales()
  integer :: option
  do
    call parameters_menu()
    read(*, *) option
    
    select case(option)
    case(1)
      call readFile()
    case(2)
      print *, "Ingrese el numero de ventanillas deseadas: "
        read*, cantidad_ventanillas
        print *, "----------------"
        do i = 1, cantidad_ventanillas
            call mylista%agregar_lista(i,0," ",.true.,0,0,0)
            call mylista%insert(i,"pendiente")
            ! print *, "ventanillas creada =>>>>>>",i
        end do
        !call mylista%print_ventanillas()
        call mylista%printVent()
        print *, "----------------"
    case(3)
          exit
        case default
          print *, "Error!. Seleccione una opcion valida."
        end select
      end do
    end subroutine parametros_iniciales
    
    
    subroutine parameters_menu()
      print *, "...................................."
      print *, "         Seleccione una opcion            "
      print *, "...................................."
      print *, "1. Carga masiva de clientes"
      print *, "2. Cantidad de ventanillas"
      print *, "3. Regresar a menu principal"
      print *, "...................................."
    end subroutine parameters_menu


    subroutine pasosMemoria()
      call mylista%print_dot("listaVentanillas")
      call cola_clientes%clientes_dot(io)
      call mylista%grafica_pilaImagenes(io)
    end subroutine pasosMemoria

    subroutine reportes()
    call cola_clientes%topImgPequena_dot(io)
    call cola_clientes%topImgGrande_dot(io)
    end subroutine reportes


    subroutine readFile()
      
        print *, "---------------------------------------"
        print *, "-- Lista Clientes --"
        call json%initialize()
        call json%load(filename='DatosPrueba.json')
        call json%info('',n_children=size)
        call json%get_core(jsonc)
        call json%get('', listPointer, found)
    
        do i = 1, size
            call jsonc%get_child(listPointer, i, animalPointer, found)

            call jsonc%get_child(animalPointer, 'id', attributePointer, found)
            call jsonc%get(attributePointer, id)
    
            call jsonc%get_child(animalPointer, 'nombre', attributePointer, found)
            call jsonc%get(attributePointer, nombre)
    
            call jsonc%get_child(animalPointer, 'img_p', attributePointer, found) 
            call jsonc%get(attributePointer, img_p)

            call jsonc%get_child(animalPointer, 'img_g', attributePointer, found) 
            call jsonc%get(attributePointer, img_g)

            read(id, *) id_asInt
            read(img_g, *) img_g_asInt
            read(img_p, *) img_p_asInt
            !total_img = img_g_asInt + img_p_asInt
            total_img = 0

            call cola_clientes%push(id_asInt, trim(nombre), img_g_asInt, img_p_asInt,total_img)
            call cola_clientes%agregar_imgG(id_asInt, trim(nombre), img_g_asInt, img_p_asInt)
            call cola_clientes%agregar_imgP(id_asInt, trim(nombre), img_g_asInt, img_p_asInt)

            ! print *, "------------------"
            ! print *, 'ID: ', id
            ! print *, 'Nombre: ', nombre
            ! print *, 'img_p: ', img_p
            ! print *, 'img_g: ', img_g
        end do
        call cola_clientes%print()
        call json%destroy()
    end subroutine readFile

    subroutine pasos()
      contador_pasos = 0
      do
        call menuTemp()
        read(*, *) option
  
        select case(option)
        case(1)
            contador_pasos  = contador_pasos + 1
            call pasoUno()
            
            print *, contador_pasos
        ! case(2)s
        !     call pasos()
        ! case(3)
        !     call cantidad_ventanillas()
        ! case(4)
        !     call ejecutar_paso()
        ! case(5)
        !     call estado_memoria_estructuras()
        case(6)
            exit
        case default
            print *, "Error!. Por favor seleccione una opcion valida."
        end select
    end do

    end subroutine pasos

    subroutine menuTemp()
      print *, "...................................."
      print *, "         Seleccione una opcion            "
      print *, "...................................."
      print *, "1. paso Uno"
      print *, "...................................."
    end subroutine menuTemp
end program main

```
- subroutine pasoUno(): funcion que maneja la administracion de las ventanillas cuando ingresa un cliente, valida que hayan ventanillas disponibles o clientes en la cola.
```fortran
    subroutine pasoUno()
      integer :: id_ventanillaActual, id_clienteActual, img_pequenas, img_grandes
      logical :: ventanilla_disponible
      character(len=:), allocatable :: nombre_clienteActual
  
      id_ventanillaActual = mylista%getIndiceVentanilla()
      ventanilla_disponible = mylista%ventanillaDisponible()
  
      if (id_ventanillaActual > 0 .or. ventanilla_disponible) then
          ! Hay ventanillas disponibles o no hay ventanillas, pero se puede agregar un cliente de la cola
          id_clienteActual = cola_clientes%getIndiceCliente()
          nombre_clienteActual = cola_clientes%getNombreCliente()
          img_pequenas = cola_clientes%getImgPequenas()
          img_grandes = cola_clientes%getImgGrande()
  
          if (id_clienteActual > 0) then
              ! Hay un cliente en espera en la cola
              call mylista%actualizar_ventanilla(id_clienteActual, nombre_clienteActual, img_pequenas, img_grandes)
              call cola_clientes%eliminar_nodo(id_clienteActual)
              call cola_clientes%print()
              call mylista%print_ventanillas()
          else
              ! No hay clientes en espera en la cola
            call mylista%actualizar_ventanilla(0, "", 0, 0) ! Mantener la ventanilla con los mismos datos actuales
            call mylista%print_ventanillas()
            print *, "No hay clientes en espera en la cola."
          end if
      else
          ! Hay ventanillas, pero no están disponibles para asignar clientes
          ! Mantener al mismo cliente en la ventanilla y no eliminar ningún cliente de la cola
        print *, "Espere, no hay ventanillas disponibles."
          call mylista%actualizar_ventanilla(0, "", 0, 0) ! Mantener la ventanilla con los mismos datos actuales
          call mylista%print_ventanillas()
      end if
  end subroutine pasoUno
  
```
## Fase1Proyecto
En este archivo estan definidos 2 modulos, el de ventanillas que es mi lista simple y el de cola clientes.

# modulo linkedlist 
mi modulo de linkedlist esta compuesto por su nodo y las subrutinas que hara la lista.
```fortran
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
            procedure :: grafica_pilaImagenes
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
```
- grafica_pilaImagenes() grafica cuando pasa un cliente a la ventanilla y empieza a mandar sus imagenes a la pila.
```fortran
subroutine grafica_pilaImagenes(self, io)
    class(linked_list), intent(inout) :: self ! referencia a la lista
    !character(len=*), intent(in) :: filename ! nombre del archivo
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
    ! puntero al nodo actual
    current => self%head
    index = 1
    ! abrir el archivo
    open(newunit=io, file="PasosImg.dot", status='replace')

    ! escribir el encabezado
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



            ! avanzar al siguiente nodo
            current => current%next
            index = index + 1
        end do
    end if
    ! escribir el pie del archivo
    write(io, *) "}}" 
    ! cerrar el archivo
    
    close(io)

    call execute_command_line(command, exitstat=i)
        
        if(i == 1) then
            print *, "Ocurrió un error"
        else
            print *, "Grafica de lista ventanillas generada satisfactoriamente"
        end if
end subroutine grafica_pilaImagenes
```
- insert(): va a inicializar la pila dentro de cada ventanilla.
```fortran
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
```
- printVent(): hace un imprimir de mis ventanillas y tambien llama a la funcion para impimir mi pila.
```fortran
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
```
- agregar_lista(): esta funcion va a agregar a la lista de ventanillas la informacion de cada cliente.
```fortran
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
```
- print_ventanillas(): va a imprimir las ventanillas pero ya con la informacion de los clientes que se le asignaron a cada nodo.
```fortran
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
```
- getIDCliente(): devuelve el id del cliente mientras va recorriendo la lista.
```fortran
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
```
- getNombreCliente(): devuelve el nombre del cliente mientras va recorriendo la lista.
```fortran
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
```
- getImgPequenas(): devuelve la cantidad de imagenes pequenas del cliente mientras va recorriendo la lista.
```fortran
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
```
- getImgGrande(): devuelve la cantidad de imagenes grandes del cliente mientras va recorriendo la lista.
```fortran
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
```
- ventanillaDisponible(): verifica si una ventanilla esta disponible.
```fortran
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
```
- getIndiceVentanilla(): devuelve el indice de una ventanilla.
```fortran
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
```
- actualizar_ventanilla(): este metodo va a actualizar tanto la ventanilla como la pila, y va a ir reduciendo las imagenes para pasarlas a la pila de una en una en cada paso.

```fortran
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
```
## modulo cola
En este modulo manejo la cola de clientes.
```fortran
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
```
- img_pasosSistema(): genera la imagen del cliente que haya hecho mas pasos.
```fortran
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
```
- graficaIdCliente(): genera la grafica de solo un cliente por id.
```fortran
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
```
- agregar_imgP() ordena imagenes pequenas.
```fortran
subroutine agregar_imgP(self, id_c, nombre_c, img_g, img_p)
    class(cola), intent(inout) :: self
    integer, intent(in) :: id_c, img_g, img_p
    character(len=*), intent(in) :: nombre_c
    
    type(nodeOrdenP), pointer :: current, newNode, prev
    
    ! Crear un nuevo nodo
    allocate(newNode)
    newNode%id = id_c
    newNode%nombre = nombre_c
    newNode%img_g = img_g
    newNode%img_p = img_p
    newNode%next => null()
    
    if (.not. associated(self%head_P)) then
        ! La cola está vacía, insertar el nuevo nodo como cabeza
        self%head_P => newNode
    else
        ! La cola no está vacía, encontrar la posición de inserción
        current => self%head_P
        prev => null()
        
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
                ! Comprobar para imágenes grandes si son iguales
                if (img_g > current%img_g) then
                    ! Insertar antes del nodo actual
                    newNode%next => current
                    if (associated(prev)) then
                        prev%next => newNode
                    else
                        ! Nuevo nodo es la nueva cabeza de la cola
                        self%head_P => newNode
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
end subroutine agregar_imgP
```
- topImgPequena_dot(): genera grafica top 5 imagenes pequenas
```fortran
subroutine topImgPequena_dot(self, io)
    class(cola), intent(in) :: self
    integer, intent(out) :: io
    character(len=100) :: command
    character(len=8) :: nombre_nodo
    character(len=:), allocatable :: firsts
    character(len=:), allocatable :: connections
    type(nodeOrdenP), pointer :: current
    integer ::  index, i, conteo
    integer, dimension(5) :: id_array ! Array para almacenar los IDs de los primeros 5 nodos

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

    ! Set title and background color
    write(io, *) "  graph [ bgcolor=white];"

    if (.not. associated(self%head_P)) then
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
            current => self%head_P
            do while (associated(current))
                if (current%id == id_array(conteo)) then
                    write(nombre_nodo, '(I5)') index

                    write(io, *) '"nodo'//trim(nombre_nodo)//'"[label="', 'ID: ', current%id, &
                            '\n Nombre: ', current%nombre,'\n IMG_P: ', current%img_g, '", fillcolor=orange, style=filled];'

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
```
- agregar_imgG() ordena imagenes grandes
```fortran
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
```
- topImgGrande_dot(): genera la grafica de imagenes grandes

```fortran
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
```
- push(): agrega nodos a la cola
```fortran
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
```
- eliminar_nodo(): va a eliminar un nodo de mi cola clientes
```fortran
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
```
- clientes_dot(): genera la grafica de la lista de clientes.
```fortran
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
```
# modulo_pila
Maneja todas las funciones de la pila.
```fortran
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
        procedure :: PilaEstaVacia
        procedure :: graficar_pila
        !procedure ::print
        !procedure :: eliminar_nodo
    end type pila

    type :: node
        character(len=:), allocatable :: imagen
        integer :: idCliente
        type(node), pointer :: next => null()
        !type(node), pointer :: head => null()
    end type node
```
- PilaEstaVacia(): verifica si la pila esta vacia.
```fortran
subroutine PilaEstaVacia(self, pilaVacia)
        class(pila), intent(inout) :: self
        logical :: pilaVacia 

        if(.not. associated(self%head)) then
            pilaVacia = .true.
        else
            pilaVacia = .false.
        end if    
    
        
    end subroutine PilaEstaVacia
```
- graficar_pila(): grafica la pila con sus movimientos al asignar imagenes.
```fortran
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
```
- init_pila(): Inicializa la pila.
```fortran
    subroutine init_pila(self)
        class(pila), intent(inout) :: self
        type(node), pointer :: nodo
    end subroutine init_pila
```
- append(): agrega imagenes a la pila.
```fortran
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
```
- printPila(): imprime la pila
```fortran
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
```
- tamano_pila(): devuelve el tamano de la pila
```fortran
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
```