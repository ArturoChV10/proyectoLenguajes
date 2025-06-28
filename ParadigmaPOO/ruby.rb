require_relative 'Producto'
require_relative 'Cliente'

p = Producto.new(1, "Calculadora", 100, 1)
puts p.getNombre()
puts p.getId()
puts p.getCantidad()
puts p.getPrecio()