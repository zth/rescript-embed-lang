let edgeDbAsBinding = %edgeql(`
  # @name someBinding
  select Movie {
    title
  }
`)

module Nested = {
  let edgeDbAsBinding = %edgeql(`
  # @name someBindingNested
  select Movie {
    title
  }
`)

  module EdgeQL_as_module = %edgeql(`
    # @name someModuleNested
    select Movie {
      title
    }
  `)

  module Nested = {
    let edgeDbAsBinding = %edgeql(`
        # @name someBindingNested2
        select Movie {
          title
        }
      `)

    module EdgeQL_as_module = %edgeql(`
    # @name someModuleNested2
    select Movie {
      title
    }
  `)
  }
}

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
