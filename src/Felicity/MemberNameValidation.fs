namespace Felicity


module internal MemberName =

    open System.Text.RegularExpressions

    let private globallyAllowed = "[a-zA-Z0-9\u0080-\uFFFF]"
    let private allowedInside = "[a-zA-Z0-9\u0080-\uFFFF\-_ ]"

    let private startsWithGloballyAllowed =
        let r = Regex("^" + globallyAllowed, RegexOptions.Compiled)
        r.IsMatch

    let private endsWithGloballyAllowed =
        let r = Regex(globallyAllowed + "$", RegexOptions.Compiled)
        r.IsMatch

    let private containsOnlyAllowed =
        let r = Regex(allowedInside + "+", RegexOptions.Compiled)
        r.IsMatch

    /// Indicates whether the string is a valid member name
    let isValid s =
        startsWithGloballyAllowed s
        && endsWithGloballyAllowed s
        && containsOnlyAllowed s
