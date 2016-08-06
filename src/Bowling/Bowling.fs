module Bowling

let frameIsStrike (line: int list) = line.[0] = 10
let frameIsSpare (line: int list) = line.[0] + line.[1] = 10

let scoreUsingIfs line =
  let rec calculateScore (line: int list) score =
    if line |> List.isEmpty then score
    elif frameIsStrike line then
      let newScore = (line |> List.take 3 |> List.sum) + score
      if line |> List.length > 3 then calculateScore (line |> List.tail) newScore
      else newScore
    elif frameIsSpare line then
      let newScore = (line |> List.take 3 |> List.sum) + score
      if line |> List.length > 3 then calculateScore (line |> List.skip 2) newScore
      else newScore
    else calculateScore (line |> List.skip 2) ((line |> List.take 2 |> List.sum) + score)
  calculateScore line 0

let scoreUsingPM (line: int list) =
  let rec calculateScore (line: int list) score =
    match line with
    | 10::x::y::[] -> score + 10 + x + y
    | 10::x::y::tail -> calculateScore (x::y::tail) score + 10 + x + y
    | x::y::z::tail when x + y = 10 -> calculateScore (z::tail) score + 10 + z
    | x::y::tail -> calculateScore tail score + x + y
    | _ -> score
  calculateScore line 0

let scoreUsingReduce (line: int list) =
  let expandFrameScores line =
    let rec expandFrameScore (line: int list) scores =
      let newListParams = match line with
                          | 10::x::y::tail -> (1, 3)
                          | x::y::z::tail when x+y = 10 -> (2, 3)
                          | _ -> (2, 2)
      if line |> List.length < snd newListParams then scores
      else expandFrameScore (line |> List.skip (fst newListParams)) ((line |> List.take (snd newListParams)) :: scores)
    expandFrameScore line List.empty
  line |> expandFrameScores |> List.rev |> List.take 10 |> List.concat |> List.sum


let score = scoreUsingReduce
