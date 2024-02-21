program main
  use:: json_module
  use linkedList
  use cola_module
  implicit none
  integer :: option,id_asInt,img_g_asInt,img_p_asInt
  integer :: windowsAmount
  character(len=1) :: dummy_char
  !integer :: option
  type(linked_list) :: mylista
  type(json_file) :: json
  type(json_core) :: jsonc
  type(json_value), pointer :: listPointer, animalPointer, attributePointer
  logical :: found
  integer :: size, i,num_vetanilla
  character(:), allocatable :: id, nombre, img_p, img_g
    do
        call print_menu()
        read(*, *) option
  
        select case(option)
        case(1)
            call parametros_iniciales()
        ! case(2)
        !     call carga_masiva_clientes()
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

  contains
  subroutine initial_menu()
    do
        call print_menu()
        read(*, *) option
        
        select case(option)
        case(1)
          call parametros_iniciales()
          ! case(2)
          !     call carga_masiva_clientes()
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

end subroutine initial_menu

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
      print *, "Cuantas ventanillas quieres: "
        read*, num_vetanilla
        print *, "----------------"
        do i = 1, num_vetanilla
            call mylista%agregar_lista(i)
            print *, "ventanillas creada =>>>>>>",i
        end do
        call mylista%print()
        print *, "----------------"
    case(3)
          exit
        case default
          print *, "Error!. Por favor seleccione una opcion valida."
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
    end subroutine parameters_menu



    subroutine readFile()
      type(cola) :: cola_clientes
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

            call cola_clientes%push(id_asInt, trim(nombre), img_g_asInt, img_p_asInt)

            ! print *, "------------------"
            ! print *, 'ID: ', id
            ! print *, 'Nombre: ', nombre
            ! print *, 'img_p: ', img_p
            ! print *, 'img_g: ', img_g
        end do
        call cola_clientes%print
        call json%destroy()
    end subroutine readFile


  !   subroutine windowNumber()
  !         type(linked_list) :: windowsList
          
  
  !         print *, ">> Ingrese el numero de ventanillas disponibles:"
  !         read(*, *) windowsAmount
  
  !         ! Inicializar la lista de ventanillas
  !         call init_linked_list(windowsList)
  
  !         ! Agregar nodos a la lista para representar las ventanillas
  !         do i = 1, windowsAmount
  !             call push(windowsList, i)
  !         end do
  
  !         ! Mostrar las ventanillas creadas
  !         print *, "Ventanillas creadas exitosamente:"
  !         call print(windowsList)
  
  !         ! Esperar a que el usuario presione cualquier tecla antes de volver al menú
  !         print *, "Presione cualquier tecla para volver al menu..."
  !         read(*,*) dummy_char
  ! end subroutine windowNumber

end program main