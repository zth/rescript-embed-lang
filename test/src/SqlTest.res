let findBooksRaw = %sql(`
  /* @name BooksByAuthorRaw */
  select x from x
`)

module FindBooksRaw = %sql(`
  /* @name BooksByAuthorRaw */
  select x from x
`)

module FindBooksRawIncluded = {
  include %sql(`
    /* @name BooksByAuthorRaw */
    select x from x
  `)
}

let findBooksOne = %sql.one(`
  /* @name BooksByAuthorOne */
  select x from x
`)

let findBooksExpectOne = %sql.expectOne(`
  /* @name BooksByAuthorExpectOne */
  select x from x
`)

let findBooksNoName1 = %sql.expectOne(`
  select x from x
`)

let findBooksMany = %sql.many(`
  /* @name BooksByAuthorMany */
  select x from x
`)

let findBooksExecute = %sql.execute(`
  /* @name BooksByAuthorExecute */
  select x from x
`)

let findBooksNoName2 = %sql.expectOne(`
  select x from x
`)
