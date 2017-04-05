use Amnesia

defdatabase Database do

  deftable User, [:id, :name, :password] do
  end

  def init do
    Amnesia.Table.copying(:schema, node(), :disk)
    Amnesia.Table.copying(:schema, othernode, :disk)
  end
end
