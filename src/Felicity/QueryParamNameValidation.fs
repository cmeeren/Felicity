namespace Felicity


module internal QueryParam =

  open System.Text.RegularExpressions

  let private isKnownJsonApiName =
    let r = Regex("^sort$|^include$|^page\[|^filter\[|^fields\[.+?\]$", RegexOptions.Compiled)
    r.IsMatch

  let private containsNonLowercase =
    let r = Regex("[^a-z]", RegexOptions.Compiled)
    r.IsMatch

  let isValidName s =
    isKnownJsonApiName s
    || (MemberName.isValid s && containsNonLowercase s)
