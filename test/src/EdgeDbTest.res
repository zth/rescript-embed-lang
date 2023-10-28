let edgeDbAsBinding = %edgeql(`
  # @name someBinding
  select Movie {
    title
  }
`)

// let commentedOut = %edgeql(`
//  # @name someBinding
//  select Movie {
//    title
//  }
//`)

let _query = edgeDbAsBinding

module EdgeQL_as_module = %edgeql(`
  # @name someModule
  select Movie {
    title
  }
`)

let _query = EdgeQL_as_module.query
