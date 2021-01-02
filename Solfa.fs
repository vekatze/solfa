open System
open System.IO

let iterateCount =
  100

let standardScale =
  [0; 2; 4; 5; 7; 9; 11]

let admit<'a> =
  failwith<'a> "admit"

let doWhen bool f =
  if bool
  then
    f ()
  else
    ()

let rem x m =
  let tmp = x % m
  if tmp >= 0
  then
    tmp
  else
    tmp + m

let noteAt stringIndex fretIndex =
  rem (5 + 7 * stringIndex + fretIndex) 12

let fretOf stringIndex note =
  rem (note - 5 - 7 * stringIndex) 12

let exitWith<'a> i =
  let _ = Environment.Exit i
  admit<'a> // unreachable

let quitWith<'T> str =
  printf str
  Environment.Exit(0)

let parseInt (str : string) =
  match System.Int32.TryParse str with
  | true, i ->
    Some i
  | _, _ ->
    None

let baseNoteOf stringIndex =
  match stringIndex with
  | 0 ->
    26
  | 1 ->
    21
  | 2 ->
    16
  | 3 ->
    11
  | 4 ->
    6
  | 5 ->
    1
  | _ ->
    failwith "baseNoteOf"

let play stringIndex fretIndex =
  let basename = string (baseNoteOf stringIndex + fretIndex)
  let arg = sprintf "./sine/%s.wav" basename
  let p = new System.Diagnostics.Process ()
  p.StartInfo.FileName <- "paplay"
  p.StartInfo.Arguments <- arg
  p.StartInfo.RedirectStandardError <- true
  let _ = p.Start ()
  p

let getInput stringIndexOrNone noteOrNone =
  let rawInputStr = Console.ReadLine ()
  if String.IsNullOrEmpty rawInputStr
  then
    printf "\n"
    None
  else
    let inputStr = rawInputStr.Trim ()
    match inputStr, stringIndexOrNone, noteOrNone with
    | "show", _, Some note ->
      printf "note: %d\n" note
      None
    | "p", Some stringIndex, Some note ->
      let _ = play stringIndex (fretOf stringIndex note)
      None
    | "exit", _, _ ->
      exitWith 0
    | _ ->
      parseInt inputStr

let sum xs =
  let rec helper acc xs =
    match xs with
    | [] ->
      acc
    | y :: ys ->
      helper (y + acc) ys
  helper 0. xs

let save name (values : List<float>) =
  if values.Length <= 0
  then
    ()
  else
    let value = (sum values) / (float values.Length)
    let dirPath = sprintf "./result/%s/" name
    let _ = System.IO.Directory.CreateDirectory dirPath
    let dateStr = string (DateTimeOffset(DateTime.Now).ToUnixTimeSeconds ())
    let path = dirPath + dateStr
    File.WriteAllLines(path, [string value])

module Interval =

  let intervalAt stringIndex offset =
    (5 * stringIndex + offset) % 12

  let printRow stringIndex offset i range =
    for j = -range to range do
      match i, j with
      | 0, 0 ->
        printf "|  \u001b[33m0\u001b[0m "
      | 0, _ ->
        printf "|  * "
      | _, _ when i = stringIndex && j = offset ->
        printf "|  \u001b[33m?\u001b[0m "
      | _ ->
        printf "|    "
    printf "|\n"

  let printRows stringIndex offset range =
    printf "\n"
    for i = 5 downto 0 do
      printRow stringIndex offset i range

  let rec challenge stringIndex offset count =
    let t1 = DateTime.Now
    let rec f _ =
      printf "(%d/%d) > " (iterateCount - count + 1) iterateCount
      match getInput None None with
      | Some input when input = intervalAt stringIndex offset ->
        let t2 = DateTime.Now
        (t2 - t1).TotalSeconds
      | _ ->
        f ()
    f ()

  let run i =
    let rec helper acc i =
      if i <= 0
      then
        acc
      else
        let stringIndex = (new System.Random()).Next(1, 5)
        let offset = (new System.Random()).Next(-1, 1)
        printRows stringIndex offset 1
        helper (challenge stringIndex offset i :: acc) (i - 1)
    helper [] i

module Fretboard =

  let rec selectPoint _ =
    let questionStringIndex = (new System.Random()).Next(0, 5)
    let questionFretIndex = (new System.Random()).Next(0, 11)
    if List.contains (noteAt questionStringIndex questionFretIndex) [0; 2; 4; 5; 7; 9; 11]
    then
      (questionStringIndex, questionFretIndex)
    else
      selectPoint ()

  let printFooter _ =
    printf "   "
    for fretIndex = 0 to 11 do
      if List.contains fretIndex [3; 5; 7; 9]
      then
        printf "   * "
      else
        printf "     "
    printf "\n"

  let printRow questionStringIndex questionFretIndex stringIndex =
    printf "%d " (stringIndex + 1)
    for fretIndex = 0 to 11 do
      if questionStringIndex = stringIndex && questionFretIndex = fretIndex
      then
        printf "|  \u001b[33m?\u001b[0m "
      else
        printf "|    "
      doWhen (fretIndex = 0) (fun _ -> printf "|")
    printf "|\n"

  let printRows questionStringIndex questionFretIndex =
    printf "\n"
    for stringIndex = 0 to 5 do
      printRow questionStringIndex questionFretIndex stringIndex
    printFooter ()

  let challenge questionStringIndex questionFretIndex count =
    let t1 = DateTime.Now
    let rec f _ =
      printf "(%d/%d) > " (iterateCount - count + 1) iterateCount
      match getInput None None with
      | Some input when input = noteAt questionStringIndex questionFretIndex ->
        let t2 = DateTime.Now
        (t2 - t1).TotalSeconds
      | _ ->
        f ()
    f ()

  let run i =
    let rec helper acc i =
      if i <= 0
      then
        acc
      else
        let (questionStringIndex, questionFretIndex) = selectPoint ()
        printRows questionStringIndex questionFretIndex
        helper (challenge questionStringIndex questionFretIndex i :: acc) (i - 1)
    helper [] i

module Inverse =

  let printFooter _ =
    Fretboard.printFooter ()

  let printRow questionStringIndex stringIndex =
    if questionStringIndex = stringIndex
    then
      printf "\u001b[33m? %d \u001b[0m" (stringIndex + 1)
    else
      printf "  %d " (stringIndex + 1)
    for fretIndex = 0 to 11 do
      if questionStringIndex = stringIndex
      then
        printf "\u001b[33m|\u001b[0m    "
        doWhen (fretIndex = 0) (fun _ -> printf "\u001b[33m|\u001b[0m")
      else
        printf "|    "
        doWhen (fretIndex = 0) (fun _ -> printf "|")
    if questionStringIndex = stringIndex
    then
      printf "\u001b[33m|\u001b[0m\n"
    else
      printf "|\n"

  let printRows questionStringIndex =
    printf "\n"
    for stringIndex = 0 to 5 do
      printRow questionStringIndex stringIndex
    printFooter

  let challenge questionStringIndex questionNote count =
    let p = play questionStringIndex (fretOf questionStringIndex questionNote)
    let t1 = DateTime.Now
    let rec f _ =
      printf "> "
      match getInput (Some questionStringIndex) (Some questionNote) with
      | Some input when questionNote = noteAt questionStringIndex input ->
        let t2 = DateTime.Now
        p.WaitForExit ()
        (t2 - t1).TotalSeconds
      | _ ->
        f ()
    f ()

  let run i =
    let rec helper acc i =
      if i <= 0
      then
        acc
      else
        let questionStringIndex = (new System.Random()).Next(1, 5)
        let questionNoteIndex = (new System.Random()).Next(0, standardScale.Length - 1)
        let questionNote = standardScale.[questionNoteIndex]
        printRows questionStringIndex questionNote
        helper (challenge questionStringIndex questionNote i :: acc) (i - 1)
    helper [] i

let lesson name tester =
  let result = tester iterateCount
  save name result

lesson "interval" Interval.run
lesson "fretboard" Fretboard.run
lesson "inverse" Inverse.run
