let genericAsLetBinding = %generated.css(`
  some css here
`)

// let commentedOut = %generated.css(`
//   some css here
// `)

let _default = genericAsLetBinding

module Generic_as_module = %generated.css(`
  some css here
`)

let _query = Generic_as_module.default
type t = Generic_as_module.t
