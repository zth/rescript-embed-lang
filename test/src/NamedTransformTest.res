module Named = %generated.fixture(`query Named { viewer { id } }`)

type namedVariables = Named.variables
type namedResponse = Named.response

let namedVariables: namedVariables = {id: "named"}
let namedResponse: namedResponse = {id: "response"}
let moduleOperation = Named.operation
let moduleValue = Named.default

let query = %generated.fixture(`query Value { viewer { id } }`)
let value = query

let run = (operation, variables) => operation + variables
let inlineValue = run(%generated.fixture(`query Inline { viewer { id } }`), 1)

include %generated.fixture(`query Included { viewer { id } }`)
let includedValue = included
