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
          call opciones_reportes()
            
        print *, "----------------"
        case(5)
            call datos_estudiante()
        case(6)
            exit
        case default
            print *, "Error!. Por favor seleccione una opcion valida."
        end select
    end do

  contains

  subroutine datos_estudiante()
    print *, "........................................"
  print *, "         Datos del Estudiante             "
  print *, ".........................................."
    print *, "Nombre: Helen Janet Rodas Castro"
    print *, "Carnet: 202200066"
    print *, "Primer Semestre 2024"
    print *, "Lab.Estructura de Datos"
    print *, "...................................."
  end subroutine datos_estudiante



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

    subroutine reportes_menu()
      print *, "..........................................................."
      print *, "                  Seleccione una opcion                    "
      print *, "..........................................................."
      print *, "1. Top 5 de clientes con mayor cantidad de imagenes grandes"
      print *, "2.Top 5 de clientes con menor cantidad de imagenes pequenas"
      print *, "3. Informacion del cliente que mas pasos estuvo en el sistema"
      print *, "4. Buscar Cliente."
      print *, "5. Regresar Menu Principal."
      print *, "..........................................................."
    end subroutine reportes_menu

    subroutine opciones_reportes()
      integer :: option
      do
        call reportes_menu()
        read(*, *) option
        
        select case(option)
        case(1)
          call cola_clientes%topImgGrande_dot(io)
        case(2)
          call cola_clientes%topImgPequena_dot(io)
        case(3)
          call cola_clientes%img_pasosSistema(io)
        case(4)
          print *, "Ingrese el ID del cliente a buscar: "
          read*, clienteId_temp
          call cola_clientes%graficaIdCliente(io,clienteId_temp)
        case(5)
              exit
            case default
              print *, "Error!. Seleccione una opcion valida."
            end select
          end do
        end subroutine opciones_reportes




    subroutine pasosMemoria()
      call mylista%print_dot("listaVentanillas")
      call cola_clientes%clientes_dot(io)
      call mylista%grafica_pilaImagenes(io)
    end subroutine pasosMemoria




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
  
  

end program main