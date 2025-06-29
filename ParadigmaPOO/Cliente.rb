class Cliente
  def initialize(id, nombre)
      @id = id
      @nombre = nombre
  end

  ### Sets ###
  def setId(id)
    if id.is_a?(Integer)
      @id = id
      return true
    else
      raise ArgumentError, "El id debe ser un número entero (Integer)"
    end
  end

  def setNombre(nombre)
    if nombre.is_a?(String)
      @nombre = nombre
      return true
    else
      raise ArgumentError, "El nombre debe ser un String"
    end
  end

  ### Gets ###
  def getId()
      return @id
  end

  def getNombre()
      return @nombre
  end
end