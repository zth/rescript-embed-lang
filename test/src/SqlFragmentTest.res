module Active = %sql.fragment(`SELECT 1 AS value`)

module Outer = {
  module Value = %sql.fragment(`SELECT 2 AS value`)
}

module Sibling = %sql.fragment(`SELECT 3 AS value`)

let active: Active.result = {value: 1}
let nested: Outer.Value.result = {value: 2}
let sibling: Sibling.result = {value: 3}

if active.value !== 1 || nested.value !== 2 || sibling.value !== 3 {
  panic("SQL fragment companion module mapping failed")
}
