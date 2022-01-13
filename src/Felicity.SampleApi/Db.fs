module Db

// This module is just a mock data store and functions to query and modify it
// (pretending they're async to make it realistic). It has nothing directly to
// do with Felicity.


open System.Collections.Concurrent
open Domain


// Pre-defined data

let private ada =
  Person.create (FirstName.fromString "Ada") (LastName.fromString "Lovelace")
  |> Person.setGender (Some Female)

let private eddie =
  Person.create (FirstName.fromString "Eddie") (LastName.fromString "Izzard")
  |> Person.setTwitter (Some (TwitterHandle.fromString "@eddieizzard"))

let private tom =
  Person.create (FirstName.fromString "Tom") (LastName.fromString "Hanks")
  |> Person.setTwitter (Some (TwitterHandle.fromString "@tomhanks"))
  |> Person.setGender (Some Male)

let private article =
  Article.create
    tom.Id
    (ArticleTitle.fromString "Life is like a box of chocolates")
    (ArticleBody.fromString "<p>My mom always said life was like a box of chocolates. You never know what you're gonna get. Well, as long as it's Ol' MacGuffin's Chocolate Truffles™, I could eat about a million and a half!</p>")
  |> Article.setType Commercial

let private comment1 =
  Comment.create
    eddie.Id
    article
    (CommentBody.fromString "First! Also, cross-dressing makes me twice as awesome.")

let private comment2 =
  Comment.create
    ada.Id
    article
    (CommentBody.fromString "u there babbage?")


// Data storage

let private persons =
  [ada; eddie; tom]
  |> List.map (fun p -> p.Id, p)
  |> dict
  |> ConcurrentDictionary

let private articles =
  [article]
  |> List.map (fun a -> a.Id, a)
  |> dict
  |> ConcurrentDictionary

let private comments =
  [comment1; comment2]
  |> List.map (fun c -> c.Id, c)
  |> dict
  |> ConcurrentDictionary



// Functions to query/modify data

module Person =

  let search (searchArgs: PersonSearchArgs) =
    async {
      let sortF f xs =
        if searchArgs.SortDescending then Seq.sortByDescending f xs else Seq.sortBy f xs
      let sort (xs: Person seq) =
        match searchArgs.SortBy with
        | PersonSort.FirstName -> xs  |> sortF (fun p -> p.FirstName)
        | PersonSort.LastName -> xs  |> sortF (fun a -> a.LastName)
      return
        persons.Values
        |> Seq.filter (fun p ->
            searchArgs.FirstName |> Option.map ((=) p.FirstName) |> Option.defaultValue true
            && searchArgs.LastName |> Option.map ((=) p.LastName) |> Option.defaultValue true
            && searchArgs.Twitter |> Option.map (Some >> (=) p.Twitter) |> Option.defaultValue true
            && searchArgs.Genders |> Option.map (List.map Some >> List.contains p.Gender) |> Option.defaultValue true
        )
        |> sort
        |> Seq.safeSkip searchArgs.Offset
        |> Seq.truncate searchArgs.Limit
        |> Seq.toList
    }

  let byId personId =
    async { return persons.TryFind personId }

  let authorForArticle (a: Article) =
    async {
      return
        match persons.TryFind a.AuthorId with
        | None -> failwith "Author missing; this should never happen"
        | Some p -> p
    }

  let authorForComment (c: Comment) =
    async {
      return
        match persons.TryFind c.AuthorId with
        | None -> failwith "Author missing; this should never happen"
        | Some p -> p
    }

  let save (p: Person) =
    async { persons[p.Id] <- p }

  let delete (p: Person) =
    async { persons.TryRemove p.Id |> ignore }


module Article =

  let search (searchArgs: ArticleSearchArgs) =
    async {
      let sortF f xs =
        if searchArgs.SortDescending then Seq.sortByDescending f xs else Seq.sortBy f xs
      let sort (xs: Article seq) =
        match searchArgs.SortBy with
        | ArticleSort.Title -> xs  |> sortF (fun a -> a.Title)
        | ArticleSort.CreatedAt -> xs  |> sortF (fun a -> a.CreatedAt)
      return
        articles.Values
        |> Seq.filter (fun a ->
            searchArgs.Title |> Option.map ((=) a.Title) |> Option.defaultValue true
            && searchArgs.Types |> Option.map (List.contains a.Type) |> Option.defaultValue true
            && searchArgs.CreatedAfter |> Option.map ((<=) a.CreatedAt) |> Option.defaultValue true
            && searchArgs.CreatedBefore |> Option.map ((>=) a.CreatedAt) |> Option.defaultValue true
        )
        |> sort
        |> Seq.safeSkip searchArgs.Offset
        |> Seq.truncate searchArgs.Limit
        |> Seq.toList
    }

  let byId articleId =
    async { return articles.TryFind articleId }

  let forComment (c: Comment) =
    async {
      return
        match articles.TryFind c.ArticleId with
        | None -> failwith "Comment missing; this should never happen"
        | Some a -> a
    }

  let allByAuthor (p: Person) =
    async {
      return
        articles.Values
        |> Seq.filter (fun a -> a.AuthorId = p.Id)
        |> Seq.sortByDescending (fun a -> a.CreatedAt)
        |> Seq.toList
    }

  let save (a: Article) =
    async { articles[a.Id] <- a }

  let delete (a: Article) =
    async { articles.TryRemove a.Id |> ignore }


module Comment =

  let search (searchArgs: CommentSearchArgs) =
    async {
      let sortF f xs =
        if searchArgs.SortDescending then Seq.sortByDescending f xs else Seq.sortBy f xs
      let sort (xs: Comment seq) =
        match searchArgs.SortBy with
        | CommentSort.CreatedAt -> xs |> sortF (fun c -> c.CreatedAt)
      return
        comments.Values
        |> Seq.filter (fun c ->
            searchArgs.Author |> Option.map ((=) c.AuthorId) |> Option.defaultValue true
            && searchArgs.AuthorFirstName
               |> Option.map (fun fn -> persons.TryFind c.AuthorId |> Option.map (fun p -> p.FirstName) = Some fn)
               |> Option.defaultValue true
        )
        |> sort
        |> Seq.safeSkip searchArgs.Offset
        |> Seq.truncate searchArgs.Limit
        |> Seq.toList
    }

  let byId commentId =
    async { return comments.TryFind commentId }

  let allForArticle (a: Article) =
    async {
      return
        comments.Values
        |> Seq.filter (fun c -> c.ArticleId = a.Id)
        |> Seq.sortByDescending (fun c -> c.CreatedAt)
        |> Seq.toList
    }

  let save (c: Comment) = 
    async { comments[c.Id] <- c }

  let delete (c: Comment) =
    async { comments.TryRemove c.Id |> ignore }
