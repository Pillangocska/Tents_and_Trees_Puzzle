defmodule Khf3 do
  @moduledoc """
  Kemping helyessége
  @author "Jankó András 2000.andras.janko@gmail.com"
  @date   "2023-10-02"
  """

  @type row   :: integer    # sor száma (1-től n-ig)
  @type col   :: integer    # oszlop száma (1-től m-ig)
  @type field :: {row, col} # egy parcella koordinátái

  @type tents_count_rows :: [integer] # a sátrak száma soronként
  @type tents_count_cols :: [integer] # a sátrak száma oszloponként

  @type trees       :: [field]   # a fákat tartalmazó parcellák koordinátái lexikálisan rendezve
  @type puzzle_desc :: {tents_count_rows, tents_count_cols, trees} # a feladványleíró hármas

  @type dir       :: :n | :e | :s | :w # a sátorpozíciók iránya: north, east, south, west
  @type tent_dirs :: [dir]             # a sátorpozíciók irányának listája a fákhoz képest

  @type cnt_tree  :: integer                         # a fák száma a kempingben
  @type cnt_tent  :: integer                         # az elemek száma a sátorpozíciók irányának listájában
  @type err_rows  :: %{err_rows:  [integer]}         # a sátrak száma rossz a felsorolt sorokban
  @type err_cols  :: %{err_cols:  [integer]}         # a sátrak száma rossz a felsorolt oszlopokban
  @type err_touch :: %{err_touch: [field]}           # a felsorolt koordinátájú sátrak másikat érintenek
  @type errs_desc :: {err_rows, err_cols, err_touch} # hibaleíró hármas

  @spec check_sol(pd::puzzle_desc, ds::tent_dirs) :: ed::errs_desc
  # Az {rs, cs, ts} = pd feladványleíró és a ds sátorirány-lista
  # alapján elvégzett ellenőrzés eredménye a ed hibaleíró, ahol
  #   rs a sátrak soronként elvárt számának a listája,
  #   cs a sátrak oszloponként elvárt számának a listája,
  #   ts a fákat tartalmazó parcellák koordinátájának a listája
  # Az {e_rows, e_cols, e_touch} = ed hármas elemei olyan
  # kulcs-érték párok, melyekben a kulcs a hiba jellegére utal, az
  # érték pedig a hibahelyeket felsoroló lista (üres, ha nincs hiba)
  def check_sol({expected_rows, expected_cols, trees}, dirs) do
    grid = generate_grid(expected_rows, expected_cols, trees, dirs)
    {err_rows, err_cols} = check_rows_cols(grid, expected_rows, expected_cols)
    err_touch = check_tents(grid)
    {%{err_rows: err_rows}, %{err_cols: err_cols}, %{err_touch: err_touch}}
  end

  @spec generate_grid(tents_count_rows, tents_count_cols, trees, tent_dirs) :: [[String.t()]]
  # A rácsot inicializálja '-' karakterekkel, majd elhelyezi a fákat és a sátrakat.
  defp generate_grid(expected_rows, expected_cols, trees, dirs) do
    # A rács inicializálása '-' karakterekkel
    grid = List.duplicate(List.duplicate("-", length(expected_cols)), length(expected_rows))
    # Fák elhelyezése
    grid = place_trees(grid, trees)
    # Sátrak elhelyezése
    grid = place_tents(grid, trees, dirs)
    grid
  end

  @spec place_trees([[String.t()]], trees) :: [[String.t()]]
  # Elhelyezi a fákat a rácsban.
  defp place_trees(grid, ts) do
    Enum.reduce(ts, grid, fn {row, col}, acc ->
      List.replace_at(acc, row - 1, List.replace_at(Enum.at(acc, row - 1), col - 1, "*"))
    end)
  end

  @spec place_tents([[String.t()]], trees, tent_dirs) :: [[String.t()]]
  # Elhelyezi a sátrakat a rácsban a megadott irányok alapján.
  defp place_tents(grid, ts, ds) do
    Enum.reduce(Enum.zip(ts, ds), grid, fn {{row, col}, dir}, acc ->
      new_row = case dir do
        :n -> row - 1
        :s -> row + 1
        _ -> row
      end
      new_col = case dir do
        :e -> col + 1
        :w -> col - 1
        _ -> col
      end
      List.replace_at(acc, new_row - 1, List.replace_at(Enum.at(acc, new_row - 1), new_col - 1, Atom.to_string(dir) |> String.upcase()))
    end)
  end

  @spec count_tents([[String.t()]]) :: {tents_count_rows, tents_count_cols}
  # Megszámolja a sátrakat soronként és oszloponként.
  defp count_tents(grid) do
    rows = Enum.map(grid, &Enum.count(&1, fn x -> x in ["N", "E", "S", "W"] end))
    cols = Enum.with_index(grid)
    |> Enum.flat_map(fn {row, _row_index} ->
      Enum.with_index(row)
      |> Enum.map(fn {cell, col_index} ->
        {col_index, cell in ["N", "E", "S", "W"]}
      end)
    end)
    |> Enum.group_by(fn {col_index, _} -> col_index end, fn {_, is_tent} -> is_tent end)
    |> Enum.map(fn {_, vals} -> Enum.count(vals, fn x -> x end) end)
    {rows, cols}
  end

  @spec check_rows_cols([[String.t()]], tents_count_rows, tents_count_cols) :: {err_rows, err_cols}
  # Ellenőrzi, hogy a sátrak száma megfelel-e a várt értékeknek soronként és oszloponként.
  defp check_rows_cols(grid, expected_rows, expected_cols) do
    {actual_rows, actual_cols} = count_tents(grid)
    err_rows = for {{actual, expected}, index} <- Enum.zip(Enum.zip(actual_rows, expected_rows), 1..length(actual_rows)), expected >= 0 and actual != expected, do: index
    err_cols = for {{actual, expected}, index} <- Enum.zip(Enum.zip(actual_cols, expected_cols), 1..length(actual_cols)), expected >= 0 and actual != expected, do: index
    {err_rows, err_cols}
  end

  @spec check_tents([[String.t()]]) :: [field]
  # Ellenőrzi, hogy a sátrak nem érintkeznek-e egymással.
  defp check_tents(grid) do
    grid
    |> Enum.with_index(1)
    |> Enum.flat_map(fn {row, row_index} ->
      row
      |> Enum.with_index(1)
      |> Enum.flat_map(fn {cell, col_index} ->
        if cell in ["N", "E", "S", "W"] do
          adjacent = [
            {row_index - 1, col_index},     # N
            {row_index - 1, col_index + 1}, # NE
            {row_index, col_index + 1},     # E
            {row_index + 1, col_index + 1}, # SE
            {row_index + 1, col_index},     # S
            {row_index + 1, col_index - 1}, # SW
            {row_index, col_index - 1},     # W
            {row_index - 1, col_index - 1}  # NW
          ]
          Enum.filter(adjacent, fn {r, c} ->
            r >= 1 and r <= length(grid) and c >= 1 and c <= length(Enum.at(grid, r - 1)) and Enum.at(Enum.at(grid, r - 1), c - 1) in ["N", "E", "S", "W"]
          end)
        else
          []
        end
      end)
    end)
    |> Enum.map(fn {row, col} -> {row, col} end) # Koordinátákat helyesen kell formázni
    |> Enum.uniq()
  end

end
