module Active = {
  type result = {value: int}
}

module Outer = {
  module Value = {
    type result = {value: int}
  }
}

module Sibling = {
  type result = {value: int}
}
