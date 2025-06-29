class Producto
    @@contador = 0

    def initialize(nombre, precio, cantidad)
        @@contador += 1
        @id = @@contador
        setNombre(nombre)
        setPrecio(precio)
        setCantidad(cantidad)
    end

    ### Sets ###
    def setNombre(nombre)
        if nombre.is_a?(String)
        @nombre = nombre
        return true
        else
        raise ArgumentError, "El nombre debe ser un String"
        end
    end

    def setPrecio(precio)
        if precio.is_a?(Numeric)
        @precio = precio
        return true
        else
        raise ArgumentError, "El precio debe ser un valor numérico"
        end
    end

    def setCantidad(cantidad)
        if cantidad.is_a?(Integer)
        @cantidad = cantidad
        return true
        else
        raise ArgumentError, "La cantidad debe ser un número entero (Integer)"
        end
    end

    ### Gets ###
    def getId
        @id
    end

    def getNombre
        @nombre
    end

    def getPrecio
        @precio
    end

    def getCantidad
        @cantidad
    end

    ### Metodos ###
    def agregarCantidad(cant)
        if cant.is_a?(Integer)
        @cantidad = @cantidad + cant
        return true
        else
        raise ArgumentError, "La cantidad a agregar debe ser un número entero (Integer)"
        end
    end

    def restarCantidad(cant)
        if cant.is_a?(Integer)
            if @cantidad >= cant
                @cantidad = @cantidad - cant
                return true
            else
                puts "Inventario insuficiente"
                return false
            end
        else
        raise ArgumentError, "La cantidad a eliminar debe ser un número entero (Integer)"
        end
    end
end