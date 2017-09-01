module Common

type Either<'a, 'b>
    = Left of 'a
    | Right of 'b
