class Producto
    def initialize(id, nombre, precio, cantidad)
        @id = id
        @nombre = nombre
        @precio = precio
        @cantidad = cantidad
    end

    ### Sets ###
    def SetId(id)
        @id = id
        return true
    end

    def setNombre(nombre)
        @nombre = nombre
        return true
    end

    def setPrecio(precio)
        @precio = precio
        return true
    end

    def setCantidad(cantidad)
        @cantidad = cantidad
        return true
    end

    ### Gets ###
    def getId()
        return @id
    end

    def getNombre()
        return @nombre
    end

    def getPrecio()
        return @precio
    end

    def getCantidad()
        return @cantidad
    end
end