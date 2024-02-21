program main
  use:: json_module
  implicit none
  integer :: option
  !integer :: option
  type(json_file) :: json
  type(json_core) :: jsonc
  type(json_value), pointer :: listPointer, animalPointer, attributePointer
  logical :: found
  integer :: size, i
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
      !call windowNumber()
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
      
        print *, "---------------------------------------"
        print *, "-- Carga Masiva Cliente --"
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

            

            print *, "----"
            print *, 'ID: ', id
            print *, 'Nombre: ', nombre
            print *, 'img_p: ', img_p
            print *, 'img_g: ', img_g

            

        end do
        call json%destroy()
    end subroutine readFile

end program main