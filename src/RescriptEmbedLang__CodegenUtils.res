let disallowedIdentifiers = [
  "and",
  "as",
  "assert",
  "constraint",
  "else",
  "exception",
  "external",
  "false",
  "for",
  "if",
  "in",
  "include",
  "lazy",
  "let",
  "module",
  "mutable",
  "of",
  "open",
  "rec",
  "switch",
  "true",
  "try",
  "type",
  "when",
  "while",
  "with",
  "private",
]

let textRegexp = %re("/[a-zA-Z_]/")
let legalIdentifierRegexp = %re("/^[a-z][a-zA-Z0-9_]*$/")

let removeIllegalCharacters = (input: string) => {
  let result = ref("")

  for i in 0 to input->String.length - 1 {
    let char = input->String.charAt(i)
    if textRegexp->RegExp.test(char) {
      result := if result.contents === "" {
          char->String.toLowerCase
        } else {
          result.contents ++ char
        }
    } else if textRegexp->RegExp.test(char) && result.contents === "" {
      result := result.contents ++ char->String.toLowerCase
    }
  }

  result.contents
}

@live
type safeName = Safe(string) | NeedsAnnotation({actualName: string, safeName: string})

let toReScriptSafeName = (ident: string) => {
  let isIllegalIdentifier =
    !(legalIdentifierRegexp->RegExp.test(ident)) || disallowedIdentifiers->Array.includes(ident)

  if isIllegalIdentifier {
    NeedsAnnotation({actualName: ident, safeName: removeIllegalCharacters(ident)})
  } else {
    Safe(ident)
  }
}
