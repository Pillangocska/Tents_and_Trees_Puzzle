# lib/math/simple_calculator.ex
defmodule Math.SimpleCalculator do

  @doc """
  Adds two numbers.
  """
  def add(a, b) do
    a + b
  end

  @doc """
  Subtracts the second number from the first.
  """
  def subtract(a, b) do
    a - b
  end

  @doc """
  Multiplies two numbers.
  """
  def multiply(a, b) do
    a * b
  end

  @doc """
  Divides the first number by the second
  """
  def divide(a, b) when b != 0 do
    a / b
  end
  def divide(_, 0), do: "Cannot divide by zero"
end
