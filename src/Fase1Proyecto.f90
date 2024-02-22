module linkedList
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
  end type linked_list

  type :: node
  integer :: id_ventanilla,id_cliente,cantidadImg_pila
  logical :: estado
  type(node), pointer :: next
  end type node

  contains

  subroutine agregar_lista(self, id_ventanilla,id_cliente,estado,cantidadImg_pila)
      class(linked_list), intent(inout) :: self
      integer, intent(in) :: id_ventanilla,id_cliente,cantidadImg_pila
      logical, intent(in) :: estado

    
      type(node), pointer :: current, newNode
  
      ! Crear un nuevo nodo
      allocate(newNode)
      newNode%id_ventanilla = id_ventanilla
      newNode%id_cliente = id_cliente
      newNode%estado = estado
      newNode%cantidadImg_pila = cantidadImg_pila
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
            ! Verifica si la ventanilla actual está ocupada
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

    ! Si no hay nodo actual, significa que se ha alcanzado el final de la lista
    if (.not. associated(current)) then
        indice = -1   ! Retorna -1 para indicar que no hay más índices
        !print *,"No hay ventanillas disponibles"
        
    else
        ! Obtiene el valor del nodo actual y establece lastNodeReturned en el nodo actual
        indice = current%id_Ventanilla
        lastNodeReturned => current
    end if
end function getIndiceVentanilla



subroutine actualizar_ventanilla(self, id_Cliente)
    class(linked_list), intent(inout) :: self
    integer, intent(in) :: id_Cliente
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

end module linkedList

module cola_module
  implicit none
  
  type :: cola
  type(node), pointer :: head => null() ! head of the list

  contains
      procedure :: push
      procedure :: print
      procedure :: getIndiceCliente
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

    ! Si no hay nodo actual, significa que se ha alcanzado el final de la lista
    if (.not. associated(current)) then
        indice = -1   ! Retorna -1 para indicar que no hay más índices
        !print *,"No hay clientes"
    else
        ! Obtiene el valor del nodo actual y establece lastNodeReturned en el nodo actual
        indice = current%id     
        lastNodeReturned => current
    end if
end function getIndiceCliente


end module cola_module



! module ventanillasDisponibles
!     implicit none
    
!     type :: ventanilla_clientes
!     type(node), pointer :: head => null() ! head of the list
  
!     contains
!         procedure :: push_ventanillaClientes
!         procedure :: print_ventanillaClientes
!         ! Agregamos los procedimientos restantes de la cola
!     end type ventanilla_clientes
  
!     type :: node
!         integer :: id_ventanilla, id_cliente
!         type(node), pointer :: next
!     end type node
  
!     contains

   
  
!     subroutine push_ventanillaClientes(self, id_ventanilla,id_cliente)
!         class(ventanilla_clientes), intent(inout) :: self
!         integer, intent(in) :: id_ventanilla,id_cliente
    
!         type(node), pointer :: current, newNode
    
!         ! Crear un nuevo nodo
!         allocate(newNode)
!         newNode%id_ventanilla = id_ventanilla
!         newNode%id_cliente = id_cliente
!         newNode%next => null()
    
!         ! Si la lista está vacía, el nuevo nodo se convierte en la cabeza de la lista
!         if (.not. associated(self%head)) then
!             self%head => newNode
!         else
!             ! Encontrar el último nodo de la lista
!             current => self%head
!             do while (associated(current%next))
!                 current => current%next
!             end do
    
!             ! Insertar el nuevo nodo al final de la lista
!             current%next => newNode
!         end if
    
!         !print *, 'pushed:: ', id,nombre,img_g,img_p
!     end subroutine push_ventanillaClientes
  
  
!     subroutine print_ventanillaClientes(self)
!         class(ventanilla_clientes), intent(in) :: self
    
!         type(node), pointer :: current
    
!         current => self%head
    
!         ! Recorre la lista y imprime los valores
!         do while (associated(current))
!             print *, "id Ventanilla: ", current%id_ventanilla
!             print *, "id Cliente: ",current%id_cliente
!             current => current%next
!         end do

!     end subroutine print_ventanillaClientes

!   end module ventanillasDisponibles
