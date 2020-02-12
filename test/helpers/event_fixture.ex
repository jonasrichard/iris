defmodule Iris.Fixture.Event do
  import Iris.Fixture, only: [id: 0, now_to_utc: 1]

  def channel_created_event(opts \\ []) do
    owner = Faker.Name.En.name()
    %Iris.Event.ChannelCreated{
      id: opts[:id] || id(),
      channel: opts[:channel] || id(),
      name: Faker.Team.En.name(),
      owner: owner,
      members: [owner | members()]
    }
  end

  def members() do
    1..5 |> Enum.map(fn _ -> Faker.Name.En.name() end)
  end
end
