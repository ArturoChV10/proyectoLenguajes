require_relative 'Producto'
require_relative 'Cliente'

$listaProductos = Array.new()

def agregarProducto(nom, prec, cant)
    if nom == "" 
        return false
    end
    if cant < 0
        return false
    end
    if prec < 0
        return false
    end

    $listaProductos.push(Producto.new(nom, prec, cant))
    return true
end

def eliminarProducto(id)
  $listaProductos.delete_if { |producto| producto.getId == id }
end

def comprarProducto(id, cant)
  return false if cant <= 0

  producto = $listaProductos.find { |p| p.getId == id }
  return false unless producto

  producto.agregarCantidad(cant)
  true
end

def venderProducto(id, cant)
  return false if cant <= 0

  producto = $listaProductos.find { |p| p.getId == id }
  return false unless producto

    producto.restarCantidad(cant)
end

def modificarPrecioProducto(id, prec)
  return false if prec < 0

  producto = $listaProductos.find { |p| p.getId == id }
  return false unless producto

  producto.setPrecio(prec)
  true
end

agregarProducto("hola", 1, 1)
puts $listaProductos.first().getId()