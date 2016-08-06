module Bowling.Tests

open NUnit.Framework
open FsUnit
open Bowling

let consecutiveMissedRolls n =
  List.init n (fun _ -> 0)

let multiplyFrame (frame: int list) times =
  let rec concatFrame acc countDown =
    if countDown = 0 then acc
    else concatFrame (acc @ frame) (countDown - 1)
  concatFrame List.empty times

[<Test>]
let ``score for empty line is 0``() =
  consecutiveMissedRolls 20 |> score |> should equal 0

[<Test>]
let ``score for a frame is the count of pins``() =
  [2; 3] @ consecutiveMissedRolls 18 |> score |> should equal 5

[<Test>]
let ``score for strike is 10 plus score of next 2 rolls``() =
  [10; 3; 5] @ consecutiveMissedRolls 16 |> score |> should equal 26

[<Test>]
let ``score for spare is 10 plus score of next roll``() =
  [5; 5; 3; 6] @ consecutiveMissedRolls 16 |> score |> should equal 22

[<Test>]
let ``strike at last frame opens up for 2 more rolls``() =
  consecutiveMissedRolls 18 @ [10; 5; 3] |> score |> should equal 18

[<Test>]
let ``spare at last frame opens up for 1 more roll``() =
  consecutiveMissedRolls 18 @ [5; 5; 5] |> score |> should equal 15

[<Test>]
let ``score for full strike game``() =
  List.init 12 (fun _ -> 10) |> score |> should equal 300

[<Test>]
let ``score for full spare game``() =
  List.init 21 (fun _ -> 5) |> score |> should equal 150

[<Test>]
let ``all 9``() =
  multiplyFrame [9; 0] 10 |> score |> should equal 90

