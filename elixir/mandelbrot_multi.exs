defmodule Mandelbrot do
  def add({ a, b }, { c, d }) do
    { a+c, b+d }
  end
  def multiply({ a, b }, { c, d }) do
    { a*c - b*d, a*d + b*c }
  end
  def abs({ a, b }) do
    :math.sqrt(a*a + b*b)
  end
  def range(min, max, steps) do
    steps = steps - 1
    0..steps
      |> Enum.map(&(min + &1*(max - min)/steps))
  end
  def matrix(x1, x2, y1, y2, xres, yres) do
    range(y1, y2, yres)
    |> Enum.map(
      fn b -> range(x1, x2, xres)
      |> Enum.map(
        fn a -> { a, b } end
      )
      end
    )
  end
  def mandel(c, m) do
    brot(c, c, 0, m)
  end
  def brot(c, z, i, max) do
    z2 = multiply(z, z)
    cond do
      Mandelbrot.abs(z2) > 2 ->
        i
      i === max ->
        0
      true ->
        brot(c, add(z2, c), i+1, max)
    end
  end
  def mandel_row(row, m) do
    Enum.map(row, fn c -> mandel(c, m) |> Integer.to_string end)
    |>  Enum.join(",")
  end
end

defmodule Multi do
  def run(data, depth) do
    stream = Task.async_stream(data,
      fn row ->
        Mandelbrot.mandel_row(row, depth)
      end
    )
    content = Enum.reduce(stream, [], fn {:ok, str}, acc -> [str | acc] end)
    |> Enum.join("\n")
    File.write("results/elixir_multi.csv", content, [:write, :utf8])
  end
end

{cx, _} = if System.get_env("CENTER_X"), do: Float.parse(System.get_env("CENTER_X")), else: 0
{cy, _} = if System.get_env("CENTER_Y"), do: Float.parse(System.get_env("CENTER_Y")), else: 0
{zoom, _} = if System.get_env("ZOOM"), do: Float.parse(System.get_env("ZOOM")), else: 1
{iterations, _} = if System.get_env("ITERATIONS"), do: Integer.parse(System.get_env("ITERATIONS")), else: 50
{width, _} = if System.get_env("WIDTH"), do: Integer.parse(System.get_env("WIDTH")), else: 600
{height, _} = if System.get_env("HEIGHT"), do: Integer.parse(System.get_env("HEIGHT")), else: 600



Mandelbrot.matrix(cx - zoom, cx + zoom, cy - zoom, cy + zoom, width, height)
|> Multi.run(iterations)
