
defmodule Test do
  def start do
    parent = self()
    r = 1..20

    Enum.map(r, fn x ->
      spawn fn -> send(parent, {:hello, x*x}) end
    end)
  end

  def loop(state) do
    receive do
      {:hello, n} ->
        IO.inspect(state)
        loop([n|state])
    end
  end
end

Test.start()
Test.loop([])