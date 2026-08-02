module Named = %generated.fixture(`query Named { viewer { id } }`)

let namedValue = %generated.fixture(`query Value { viewer { id } }`)

include %generated.fixture(`query Included { viewer { id } }`)

let moduleValue = Named.default
let value = namedValue
let includedValue = included
