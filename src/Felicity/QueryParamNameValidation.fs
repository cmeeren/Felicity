namespace Felicity


module internal QueryParam =

  open System.Text.RegularExpressions

  let private isKnownJsonApiName =
    let r = Regex("^sort$|^include$|^page\[|^filter\[|^fields\[.+?\]$", RegexOptions.Compiled)
    fun s -> r.IsMatch s

  let private containsNonLowercase =
    let r = Regex("[^a-z]", RegexOptions.Compiled)
    fun s -> r.IsMatch s

  let isValidName s =
    isKnownJsonApiName s
    || (MemberName.isValid s && containsNonLowercase s)
