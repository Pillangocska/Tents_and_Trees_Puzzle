# Egy modul függvények konténere hasonló a class-hoz
defmodule Khf1 do
  # Dokumentáció nice
  @moduledoc """
  Kemping
  @author "Jankó András 2000.andras.janko@gmail.com"
  @date   "2023-09-19"
  """
  # Típus annotációk dokumentációhoz és típusellenőrzéshez
  @type row   :: integer    # sor száma (1-től n-ig)
  @type col   :: integer    # oszlop száma (1-től m-ig)
  @type field :: {row, col} # egy parcella koordinátái

  @type tents_count_rows :: [integer] # a sátrak száma soronként
  @type tents_count_cols :: [integer] # a sátrak száma oszloponként

  @type trees       :: [field]   # a fákat tartalmazó parcellák koordinátái lexikálisan rendezve
  @type puzzle_desc :: {tents_count_rows, tents_count_cols, trees} # a feladványleíró hármas

  # Dokumentáció a függvényhez zárójelek között kapott érték :: után visszatérési érték
  # A fájlban szövegesen ábrázolt feladvány leírója pd (annotációknál)
  @spec to_internal(file::String.t) :: pd::puzzle_desc
  def to_internal(file_path) do

    # Beolvassuk a txt-t az elérési útvonala alapján
    file_content = File.read!(file_path) |> String.replace("\r\n", "\n")

    # Szétosztjuk soronként, majd az üreseket töröljük
    lines = String.split(file_content, "\n") |> Enum.filter(&String.trim(&1) != "")

    # Pattern matching (így lesz ráillesztve)
    [first_line | rest_lines] = lines
    # Felső sorban lévő sátor értékeket kiolvsasuk
    tents_count_cols = String.split(first_line) |> Enum.map(&String.to_integer/1)

    # A maradék a tuple többi részéhez kell (anonim függvényhívás)
    {tents_count_rows, tree_coordinates} = Enum.with_index(rest_lines)
    |> Enum.reduce({[], []}, fn {line, index}, {tent_rows_acc, tree_coords_acc} ->
      # Az első szó bekerül a függőleges sátor értékek közé
      [tent_count_row | plots] = String.split(line)
      # Akkumulátor kell ide mert elixirben a változók immutable-ek
      new_tent_rows_acc = tent_rows_acc ++ [String.to_integer(tent_count_row)]

      new_tree_coords_acc = Enum.with_index(plots)
      # Egy másik nested reduce ami az egyes csillagok előfordulását vizsgálja
      |> Enum.reduce(tree_coords_acc, fn {plot, j}, acc ->
        case plot do
          "*" -> acc ++ [{index + 1, j + 1}]
          _ -> acc
        end
      end)

      {new_tent_rows_acc, new_tree_coords_acc}
    end)

    # Visszatérünk a tuple értékekkel
    {tents_count_rows, tents_count_cols, Enum.sort(tree_coordinates)}
  end
end
