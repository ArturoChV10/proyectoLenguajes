class Cliente
  def initialize(id, nombre)
      @id = id
      @nombre = nombre
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

  ### Gets ###
  def getId()
      return @id
  end

  def getNombre()
      return @nombre
  end
end