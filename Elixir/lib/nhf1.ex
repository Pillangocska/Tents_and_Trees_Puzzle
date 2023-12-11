defmodule Nhf1 do
  @moduledoc """
  Sátrak
  @author "Jankó András 2000.andras.janko@gmail.com"
  @date   "2023-10-06"
  """
@type row   :: integer               # sor száma (1-től n-ig)
@type col   :: integer               # oszlop száma (1-től m-ig)
@type field :: {row, col}            # egy parcella koordinátái
@type tents_count_rows :: [integer]  # a sátrak száma soronként
@type tents_count_cols :: [integer]  # a sátrak száma oszloponként
@type trees       :: [field]         # a fákat tartalmazó parcellák koordinátái lexikálisan rendezve
@type puzzle_desc :: {tents_count_rows, tents_count_cols, trees} # a feladványleíró hármas
@type dir       :: :n | :e | :s | :w # a sátorpozíciók iránya: north, east, south, west
@type tent_dirs :: [dir]             # a sátorpozíciók irányának listája a fákhoz képest

@spec satrak(pd::puzzle_desc) :: tss::[tent_dirs]
# tss: a pd feladványleíróval megadott feladvány összes megoldásának listája, tetszőleges sorrendben.
def satrak({tents_count_rows, tents_count_cols, trees}) do
  # A bemenetből kinyerjük a kemping dimenzióit.
  n = length(tents_count_rows)
  m = length(tents_count_cols)
  # Legeneráljuk a lehetséges sátorpozíciókat.
  tent_coord_map = Enum.reduce(trees, Map.new, fn t, acc ->
    gen_tent_coordinates(acc, t, n, m, trees)
  end)

  # Végül megpróbáljuk megoldani a rejtvényt.
  dfs(tent_coord_map, trees, [], tents_count_rows, tents_count_cols, List.duplicate(0, n), List.duplicate(0, m), length(trees))
end

@spec dfs(map, trees, [field], [integer], [integer], [integer], [integer], integer) :: [tent_dirs]
# DFS-t alkalmazva megnézzük minden fára, hogy érvényes-e az adott fára a megoldás.
# Ha érvényes továbbmegyünk egy fával, ha nem akkor nem foglalkozunk többet a megoldással.
# Ha már nincs több fa visszatérünk egy üres megoldással:
defp dfs(_, _, _, _, _, _, _, 0), do: [[]]
defp dfs(all_tents, trees, previous_tents, target_tents_count_rows, target_tents_count_cols, current_target_count_rows, current_target_count_cols, i) do
  # Kikapjuk először a fák lista fejét.
  current_tree = hd(trees)
  # Végigmegyünk az égtájakon, és visszatérünk a sátrak koordinátájával.
  valid_directions_with_coordinates = [:n, :e, :s, :w] |> Enum.map(fn dir ->
      # Adott irányra mik lesznek a koordináták.
      coordinates = Map.get(all_tents, {current_tree, dir})
      # Az előbbi koordinátákra megnézzük validak-e.
      validation = validate_tent(target_tents_count_rows, target_tents_count_cols, current_target_count_rows, current_target_count_cols, previous_tents, i, coordinates)
      {dir, coordinates, validation}
    end)
    # Kidobjuk az invalid eseteket.
    |> Enum.filter(fn {_, _, validation} -> validation != :invalid end)

  # Minden valid esetre kiszámoljuk rekurzívan az előzőket.
  Enum.flat_map(valid_directions_with_coordinates, fn {dir, coordinates, new_counts} ->
    recursive_solutions = dfs(
      all_tents,
      tl(trees),
      [coordinates | previous_tents],
      target_tents_count_rows,
      target_tents_count_cols,
      Enum.at(new_counts, 0),
      Enum.at(new_counts, 1),
      i-1
    )
    # Végigmappelünk a recursive_solutions-ön, és hozzáadjuk a jelenlegi valid irányt.
    Enum.map(recursive_solutions, fn solution -> [dir | solution] end)
  end)
end

@spec validate_tent([integer], [integer], [integer], [integer], [field], integer, field) :: list
# Az alábbi függvényekkel megnézzük, hogy nem helyezünk egymás mellé-e sátrakat.
# target_tents_count_rows: Elvárt sátrak száma soronként[], target_tents_count_cols: Elvárt sátrak száma oszloponként[]
# new_target_count_rows: Jelenlegi sátrak száma soronként[], new_target_count_cols: Jelenlegi sátrak száma oszloponként[]
# previous_tents: Eddig lerakott sátrak[{}], x0/y0: Jelenlegi sátor koordinátái
# Ha maga a sátor invalid:
defp validate_tent(_, _, _, _, _, _, :invalid), do: :invalid
# Ha maga a sátor invalid, és az utolsó:
defp validate_tent(_, _, _, _, _, 1, {:invalid, _}), do: :invalid
# Ha bármilyen mennyiségű sátorban valamelyik invalid:
defp validate_tent(_, _, _, _, _, _, {:invalid, _}), do: :invalid
# Utolsó sátor esete:
defp validate_tent(target_tents_count_rows, target_tents_count_cols, current_target_count_rows, current_target_count_cols, previous_tents, 1, {x0, y0}) do
  # Ki kell vonni mert a játékban 1-től indul az index. :(
  {x, y} = {x0 - 1, y0 - 1}
  # Inkrementálunk a jelenlegi, soronkénti & oszloponkénti sátorszámlálón.
  new_target_count_rows = List.replace_at(current_target_count_rows, x, Enum.at(current_target_count_rows, x) + 1)
  new_target_count_cols = List.replace_at(current_target_count_cols, y, Enum.at(current_target_count_cols, y) + 1)
  # Végezetül megnézzük nem romlott-e el a megoldás.
  check_conditions(target_tents_count_rows, target_tents_count_cols, new_target_count_rows, new_target_count_cols, previous_tents, x, y, true)
end
# Bármilyen más sátor esete:
defp validate_tent(target_tents_count_rows, target_tents_count_cols, current_target_count_rows, current_target_count_cols, previous_tents, _, {x0, y0}) do
  {x, y} = {x0 - 1, y0 - 1}
  new_target_count_rows = List.replace_at(current_target_count_rows, x, Enum.at(current_target_count_rows, x) + 1)
  new_target_count_cols = List.replace_at(current_target_count_cols, y, Enum.at(current_target_count_cols, y) + 1)
  check_conditions(target_tents_count_rows, target_tents_count_cols, new_target_count_rows, new_target_count_cols, previous_tents, x, y, false)
end

@spec check_conditions([integer], [integer], [integer], [integer], [{integer, integer}], integer, integer, boolean)
  :: {:invalid | [integer], [integer]}
# Ez a függvény nézi meg jó helyre raktuk-e a sátrat. (Nem romlott-e el a megoldás)
# target_tents_count_rows: Elvárt sátrak száma soronként[], target_tents_count_cols: Elvárt sátrak száma oszloponként[]
# new_target_count_rows: Jelenlegi sátrak száma soronként[], new_target_count_cols: Jelenlegi sátrak száma oszloponként[]
# previous_tents: Eddig lerakott sátrak[{}], x/y: Jelenlegi sátor koordinátái
# is_last_tent: Utolsó sátrat jelzi
defp check_conditions(target_tents_count_rows, target_tents_count_cols, new_target_count_rows, new_target_count_cols, previous_tents, x, y, is_last_tent) do
  # Query: jelenlegi sátrak az adott oszlop & sorban.
  i = Enum.at(new_target_count_rows, x)
  j = Enum.at(new_target_count_cols, y)
  # Query: mennyi sátornak kéne lenni a megoldásban.
  it = Enum.at(target_tents_count_rows, x)
  jt = Enum.at(target_tents_count_cols, y)
  cond do
    # Ha az utolsó sátor.
    is_last_tent and
    # Akkor megnézzük hogy egyezik-e a target-el vagy a target negatív.
    Enum.all?(Enum.zip(target_tents_count_cols, new_target_count_cols), fn {q, w} -> q == w or q < 0 end) and
    Enum.all?(Enum.zip(target_tents_count_rows, new_target_count_rows), fn {q, w} -> q == w or q < 0 end) and
    # És nem lehet szomszédos egy sátor sem.
    Enum.all?(previous_tents, fn t -> !are_neighbours(t, {x + 1, y + 1}) end) -> [new_target_count_rows, new_target_count_cols]
    # Ha nem az utolsó akkor csak azt kell checkolnunk hogy kisebb mint a target szám, vagy a target negatív.
    not is_last_tent and ((i <= it) or it < 0) and ((j <= jt) or jt < 0) and
    Enum.all?(previous_tents, fn t -> !are_neighbours(t, {x + 1, y + 1}) end) -> [new_target_count_rows, new_target_count_cols]
    # Default just in case?
    true -> :invalid
  end
end

@spec are_neighbours(field, field) :: boolean
# Megvizsgáljuk 2 mező szomszédos vagy egybeesik-e.
# Igazzal térünk vissza ha igaz az előző, és hamissal ha nem.
defp are_neighbours({x1, y1}, {x2, y2}) do
  # A 2 mező relatív távolsága.
  dx = x1 - x2
  dy = y1 - y2
  # Mintaillesztéssel megnézzük az összes lehetséges variációt.
  case {dx, dy} do
    {0, 0}   -> true
    {0, 1}   -> true
    {0, -1}  -> true
    {1, 0}   -> true
    {-1, 0}  -> true
    {1, 1}   -> true
    {1, -1}  -> true
    {-1, 1}  -> true
    {-1, -1} -> true
    _ -> false
  end
end

@spec gen_tent_coordinates(map, field, integer, integer, trees) :: map
# Bejárjuk mind a 4 égtáj felé a kempinget, és megnézzük az adott fához merre rakható sátor.
# prev: eddig megtalált helyek, tree: vizsgált fa, n/m: pálya dimenziói, all_trees: összes fa listája
# A függvény végén kiegészítjük az eddig megtalált helyek Map-jét.
defp gen_tent_coordinates(prev, tree, n, m, all_trees) do
  # Felsoroljuk a lehetséges irányokat.
  directions = [:n, :e, :s, :w]
  # Felvesszük a lehetséges irányok koordinátáit.
  new_coordinates = Enum.reduce(directions, %{}, fn direction, acc ->
    tent_coord = gen_tent_coord_for_direction(tree, direction, n, m, all_trees)
    # A végén megkapjuk melyik irányokba lehet sátrat rakni.
    Map.put(acc, {tree, direction}, tent_coord)
  end)
  # Mergeljük a megtalált koordinátákat az eddigiekkel.
  Map.merge(prev, new_coordinates)
end

@spec gen_tent_coord_for_direction(field, atom, integer, integer, trees) :: field | :invalid
# Legeneráljuk a sátor koordinátáját, egy irány alapján (invalid-ot dobunk ha kiesne a pályáról).
# {x,y}: fa koordinátái, d: sátor iránya, n/m: pálya dimenziói, all_trees: összes fa listája
defp gen_tent_coord_for_direction({x, y}, direction, n, m, all_trees) do
  # Deltázunk {x,y}-ra az irány függvényében.
  delta = %{
    n: {-1, 0},
    e: {0, 1},
    s: {1, 0},
    w: {0, -1}
  }
  # Tehát ha például észak jön akkor itt {-1, 0} lesz.
  {dx, dy} = Map.get(delta, direction)
  # A delta alapján kiszámoljuk a sátor koordinátáit.
  {i, j} = {x + dx, y + dy}
  cond do
    # Először megnézzük hogy egyáltalán bentvan-e a pályán.
    i <= 0 or j <= 0 or i > n or j > m -> :invalid
    # Aztán megnézzük overlappel-e a többi fával.
    Enum.member?(all_trees, {i, j}) -> :invalid
    # Ha egyik sem akkor jók vagyunk.
    true -> {i, j}
    end
  end
end
