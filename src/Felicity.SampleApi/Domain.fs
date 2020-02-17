module Domain

open System

(*
This module contains all domain entities and logic. Felicity calls your domain code and
places some requirements on it. For example, all "setters" must be immutable (e.g. with a
signature like 'a -> 'entity -> 'entity), because if any setter fails, the request will
fail and no observable state must be modified.

In a large, non-trivial API, you might want to place everything not directly API-related
(i.e. Domain.fs and Db.fs in this case) in a separate project. How you organize stuff is
entirely up to you.
*)


type PersonId = private PersonId of Guid with
  static member toString (PersonId x) = string x
  static member fromString = Guid.tryParse >> Option.map PersonId

type FirstName = private FirstName of string with
  static member toString (FirstName x) = x
  static member fromString = FirstName

type LastName = private LastName of string with
  static member toString (LastName x) = x
  static member fromString = LastName

type TwitterHandle = private TwitterHandle of string with
  static member toString (TwitterHandle x) = x
  static member fromString = TwitterHandle


type Gender =
  | Male
  | Female
  | Other
  with

  static member toString = function
    | Male -> "male"
    | Female -> "female"
    | Other -> "other"

  static member fromStringMap = [
    "male", Male
    "female", Female
    "other", Other
  ]


type Person = {
  Id: PersonId
  FirstName: FirstName
  LastName: LastName
  Twitter: TwitterHandle option
  Gender: Gender option
}


[<RequireQualifiedAccess>]
type PersonSort =
  | FirstName
  | LastName

  static member fromStringMap = [
    "firstName", LastName
    "lastName", LastName
  ]


// Arguments used for searching for persons (in GET /persons)
type PersonSearchArgs = {
  FirstName: FirstName option
  LastName: LastName option
  Twitter: TwitterHandle option
  Genders: Gender list option
  SortBy: PersonSort
  SortDescending: bool
  Offset: int
  Limit: int
}


module Person =

  let create firstName lastName = {
    Id = Guid.NewGuid () |> PersonId
    FirstName = firstName
    LastName = lastName
    Twitter = None
    Gender = None
  }

  let setFirstName firstName (person: Person) =
    { person with FirstName = firstName }

  let setLastName lastName (person: Person) =
    { person with LastName = lastName }

  let setTwitter twitter (person: Person) =
    { person with Twitter = twitter }

  let setGender gender (person: Person) =
    { person with Gender = gender }


module PersonSearchArgs =

  let empty = {
    FirstName = None
    LastName = None
    Twitter = None
    Genders = None
    SortBy = PersonSort.LastName
    SortDescending = false
    Offset = 0
    Limit = 10
  }

  let setFirstName firstName (args: PersonSearchArgs) =
    { args with FirstName = Some firstName }

  let setLastName lastName (args: PersonSearchArgs) =
    { args with LastName = Some lastName }

  let setTwitter twitter (args: PersonSearchArgs) =
    { args with Twitter = Some twitter }

  let setGenders genders (args: PersonSearchArgs) =
    { args with Genders = Some genders }

  let setSort (sortBy, sortDesc) (args: PersonSearchArgs) =
    { args with SortBy = sortBy; SortDescending = sortDesc }

  let setOffset offset (args: PersonSearchArgs) =
    { args with Offset = offset }

  let setLimit limit (args: PersonSearchArgs) =
    { args with Limit = limit }


type ArticleId = private ArticleId of Guid with
  static member toString (ArticleId x) = string x
  static member fromString = Guid.tryParse >> Option.map ArticleId

type ArticleTitle = private ArticleTitle of string with
  static member toString (ArticleTitle x) = x
  static member fromString = ArticleTitle

type ArticleBody = private ArticleBody of string with
  static member toString (ArticleBody x) = x
  static member fromString = ArticleBody


type ArticleType =
  | Personal
  | Commercial
  with

  static member toString = function
    | Personal -> "personal"
    | Commercial -> "commercial"

  static member fromStringMap = [
    "personal", Personal
    "commercial", Commercial
  ]




type Article = {
  Id: ArticleId
  AuthorId: PersonId
  Title: ArticleTitle
  Body: ArticleBody
  Type: ArticleType
  Created: DateTimeOffset
  Updated: DateTimeOffset option
}


[<RequireQualifiedAccess>]
type ArticleSort =
  | Title
  | Created

  static member fromStringMap = [
    "title", Title
    "created", Created
  ]


// Arguments used for searching for articles (in GET /articles)
type ArticleSearchArgs = {
  Title: ArticleTitle option
  Types: ArticleType list option
  CreatedAfter: DateTimeOffset option
  CreatedBefore: DateTimeOffset option
  SortBy: ArticleSort
  SortDescending: bool
  Offset: int
  Limit: int
}


module Article =

  let create authorId title body = {
    Id = Guid.NewGuid () |> ArticleId
    AuthorId = authorId
    Title = title
    Body = body
    Type = Personal
    Created = DateTimeOffset.Now
    Updated = None
  }

  let setAuthor authorId (article: Article) =
    { article with AuthorId = authorId }

  let setTitle title (article: Article) =
    { article with Title = title }

  let setBody body (article: Article) =
    { article with Body = body }

  let setType articleType (article: Article) =
    { article with Type = articleType }

  let setUpdated updatedAt (article: Article) =
    { article with Updated = updatedAt }


module ArticleSearchArgs =

  let empty = {
    Title = None
    Types = None
    CreatedAfter = None
    CreatedBefore = None
    SortBy = ArticleSort.Created
    SortDescending = true
    Offset = 0
    Limit = 10
  }

  let setTitle title (args: ArticleSearchArgs) =
    { args with Title = Some title }

  let setTypes types (args: ArticleSearchArgs) =
    { args with Types = Some types }

  let setCreatedAfter createdAfter (args: ArticleSearchArgs) =
    { args with CreatedAfter = Some createdAfter }

  let setCreatedBefore createdBefore (args: ArticleSearchArgs) =
    { args with CreatedBefore = Some createdBefore }

  let setSort (sortBy, sortDesc) (args: ArticleSearchArgs) =
    { args with SortBy = sortBy; SortDescending = sortDesc }

  let setOffset offset (args: ArticleSearchArgs) =
    { args with Offset = offset }

  let setLimit limit (args: ArticleSearchArgs) =
    { args with Limit = limit }


type CommentId = private CommentId of Guid with
  static member toString (CommentId x) = string x
  static member fromString = Guid.tryParse >> Option.map CommentId

type CommentBody = private CommentBody of string with
  static member toString (CommentBody x) = x
  static member fromString = CommentBody


type Comment = {
  Id: CommentId
  AuthorId: PersonId
  ArticleId: ArticleId
  Body: CommentBody
  Created: DateTimeOffset
  Updated: DateTimeOffset option
}


[<RequireQualifiedAccess>]
type CommentSort =
  | Created

  static member fromStringMap = [
    "created", Created
  ]


// Arguments used for searching for comments (in GET /comments)
type CommentSearchArgs = {
  Author: PersonId option
  AuthorFirstName: FirstName option
  SortBy: CommentSort
  SortDescending: bool
  Offset: int
  Limit: int
}


module Comment =

  // This function accepts an entire article object instead of just an article ID, to
  // demonstrate how simple it is to use related entities instead of just IDs.

  let create authorId (article: Article) body = {
    Id = Guid.NewGuid () |> CommentId
    AuthorId = authorId
    ArticleId = article.Id
    Body = body
    Created = DateTimeOffset.Now
    Updated = None
  }

  let setBody body (comment: Comment) =
    { comment with Body = body }

  let setUpdated updatedAt (comment: Comment) =
    { comment with Updated = updatedAt }


module CommentSearchArgs =

  let empty = {
    Author = None
    AuthorFirstName = None
    SortBy = CommentSort.Created
    SortDescending = true
    Offset = 0
    Limit = 10
  }

  let setAuthorId authorId (args: CommentSearchArgs) =
    { args with Author = Some authorId }

  let setAuthorFirstName authorFirstName (args: CommentSearchArgs) =
    { args with AuthorFirstName = Some authorFirstName }

  let setSort (sortBy, sortDesc) (args: CommentSearchArgs) =
    { args with SortBy = sortBy; SortDescending = sortDesc }

  let setOffset offset (args: CommentSearchArgs) =
    { args with Offset = offset }

  let setLimit limit (args: CommentSearchArgs) =
    { args with Limit = limit }
