let enableGenericTransform = ref false

let splitPath str =
  let regex = Str.regexp_string ".res" in
  try
    let index = Str.search_forward regex str 0 in
    Str.string_before str index
  with Not_found -> str
