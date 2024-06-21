type safeName =
  | /** Safe to use as-is. */ Safe(string)
  | /** Not safe to use directly. Needs wrapping in for example an `@as` annotation if it's a record property, and so on. */
  NeedsAnnotation({
      /** The original, unsafe name. */
      actualName: string,
      /** The safe name that can be used. */
      safeName: string,
    })

/** Turns a name that's potentially unsafe to use in ReScript into a safe name.

 ## Example
 ```rescript
 toReScriptSafeName("PropName") // NeedsAnnotation({actualName: "PropName", safeName: "propName"})
 toReScriptSafeName("$propName") // NeedsAnnotation({actualName: "$propName", safeName: "propName"})
 toReScriptSafeName("propName") // Safe("propName")
 ```
 */
@live
let toReScriptSafeName: string => safeName
