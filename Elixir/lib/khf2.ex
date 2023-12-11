defmodule Khf2 do
  @moduledoc """
  Kemping-Térképe
  @author "Jankó András 2000.andras.janko@gmail.com"
  @date   "2023-09-24"
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

  @spec to_external(pd::puzzle_desc, ds::tent_dirs, file::String.t) :: :ok
  # A pd = {rs, cs, ts} feladványleíró és a ds sátorirány-lista alapján
  # a feladvány szöveges ábrázolását írja ki a file fájlba, ahol
  #   rs a sátrak soronkénti számának a listája,
  #   cs a sátrak oszloponkénti számának a listája,
  #   ts a fákat tartalmazó parcellák korrdinátájának listája
  def to_external({rs, cs, ts}, ds, file_path) do
    # Inicializáljuk a rácsot: '-'
    grid = List.duplicate(List.duplicate("-", length(cs)), length(rs))
    # Elhelyezzük először a fákat (*)
    grid_with_trees = place_trees(grid, ts)
    # Aztán elhelyezzük a sátrakat (N,E,S,W)
    grid_with_tents = place_tents(grid_with_trees, ts, ds)
    # Aztán összegyúrjuk az eddigieket
    output_lines = construct_output_lines(rs, cs, grid_with_tents)
    # Kiiratjuk fájlba
    File.write!(file_path, Enum.join(output_lines, "\n"))
    # Végezetül visszatérünk az :ok atommal
    :ok
  end

  @spec place_trees(grid :: [[String.t]], trees :: [{integer, integer}]) :: [[String.t]]
  # Ez a függvény megkapja a gridet, és a fák koordináta listáját.
  # Lerak egy *-ot mindegyik helyre a ts listában.
  # Visszatér egy updatelt griddel ahol a beolvasott listahelyeken fák vannak (*).
  defp place_trees(grid, ts) do
    Enum.reduce(ts, grid, fn {row, col}, acc ->
      List.replace_at(acc, row - 1, List.replace_at(Enum.at(acc, row - 1), col - 1, "*"))
    end)
  end

  @spec place_tents(grid :: [[String.t]], trees :: [{integer, integer}], dirs :: [atom]) :: [[String.t]]
  # Ez a függvény megkapja a gridet, fák listáját, és az irányok listáját.
  # Lerakja a sátrakat relatív a fákhoz (N,E,W,S).
  # Visszatér az updatelt listával benne a sátrakkal.
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
      dir_string = Atom.to_string(dir) |> String.upcase()
      List.replace_at(acc, new_row - 1, List.replace_at(Enum.at(acc, new_row - 1), new_col - 1, dir_string))
    end)
  end

  @spec construct_output_lines(rs :: [integer], cs :: [integer], grid :: [[String.t]]) :: [String.t]
  # Ez a függvény megkapja a sátrak számát soronként (rs), és oszloponként (cs), illetve a gridet
  # Ezeket összegyúrja a specifikációnak megfelelően, és visszatér vele
  defp construct_output_lines(rs, cs, grid) do
  first_line = Enum.join(cs, " ")

  other_lines = Enum.map(Enum.zip(rs, grid), fn {r, g} ->
    Enum.join([r | Enum.map(g, &to_string/1)], " ")
  end)

  [first_line | other_lines]
  end
end
