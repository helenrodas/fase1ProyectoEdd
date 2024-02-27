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
  integer :: size, i,cantidad_ventanillas
  character(:), allocatable :: id, nombre, img_p, img_g
  io = 1
    do
        call print_menu()
        read(*, *) option
  
        select case(option)
        case(1)
            call parametros_iniciales()
        case(2)
          print *, "Se encuentra en pasos"
            call pasos()
        case(3)
            call pasosMemoria()
        !     call cantidad_ventanillas()
        case(4)
            call reportes()
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
            !call cola_clientes%orden_imagenPequena(id_asInt, trim(nombre), img_g_asInt, img_p_asInt)

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
      character(len=:), allocatable :: nombre_clienteActual
      
      id_ventanillaActual = mylista%getIndiceVentanilla()
      !print *, "id ventanilla actual: ",id_ventanillaActual
      id_clienteActual = cola_clientes%getIndiceCliente()
      !print *, "id cliente actual: ",id_clienteActual
      nombre_clienteActual = cola_clientes%getNombreCliente()
      !print *, "nombre cliente actual: ",nombre_clienteActual
      img_pequenas = cola_clientes%getImgPequenas()
      !print *, "imagenes pequenas actual: ",img_pequenas
      img_grandes = cola_clientes%getImgGrande()
      !print *, "imagenes grandes actual: ",img_grandes
  
      if (id_ventanillaActual < 0) then
          print *, "Espere... no hay ventanillas disponibles."
          !return
      endif
  
      if (id_clienteActual < 0) then
          print *, "Error: No hay clientes en la cola."
          !return
      endif
  
      if (img_pequenas < 0) then
          print *, "Ya no hay imágenes pequeñas por procesar"
          !return
      endif
  
      
          call mylista%actualizar_ventanilla(id_clienteActual, nombre_clienteActual, img_pequenas, img_grandes)
          ! call mylista%segundaActualizacion(id_clienteActual,img_grandes,img_pequenas)
          call cola_clientes%eliminar_nodo(id_clienteActual)



          call cola_clientes%print()
          call mylista%print_ventanillas()
  end subroutine pasoUno
  

end program main