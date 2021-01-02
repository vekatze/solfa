open System
open System.IO

let iteration =
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

let findIndex (a : 'a) (xs : List<'a>) =
  let rec helper (a : 'a) (xs : List<'a>) i =
    match xs with
    | [] ->
      None
    | y :: ys when a = y ->
      Some i
    | _ :: ys ->
      helper a ys (i + 1)
  helper a xs 0

let noteAt stringIndex fretIndex =
  rem (5 + 7 * stringIndex + fretIndex) 12

let fretOf stringIndex note =
  rem (note - 5 - 7 * stringIndex) 12

let exitWith<'a> i =
  let _ = Environment.Exit i
  admit // unreachable

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

let play basename =
  let p = new System.Diagnostics.Process ()
  match Environment.OSVersion.Platform with
  | PlatformID.Unix ->
      let arg = sprintf "./sine/%s.wav" basename
      p.StartInfo.FileName <- "paplay"
      p.StartInfo.Arguments <- arg
      p.StartInfo.RedirectStandardError <- true
  | _ ->
      ()
  let _ = p.Start ()
  p

let basenameAt stringIndex fretIndex =
  string (baseNoteOf stringIndex + fretIndex)

let playAt stringIndex fretIndex =
  let basename = string (baseNoteOf stringIndex + fretIndex)
  play basename

let getInput basenameOrNone noteOrNone =
  let rawInputStr = Console.ReadLine ()
  if String.IsNullOrEmpty rawInputStr
  then
    printf "\n"
    None
  else
    let inputStr = rawInputStr.Trim ()
    match inputStr, basenameOrNone, noteOrNone with
    | "show", _, Some note ->
      printf "note: %d\n" note
      None
    | "p", Some basename, _ ->
      let _ = play basename
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
    let dateStr = DateTime.Now.ToString("yyyy-MM-dd-HHmmss")
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
      printf "(%d/%d) > " (iteration - count + 1) iteration
      match getInput None None with
      | Some input when input = intervalAt stringIndex offset ->
        let t2 = DateTime.Now
        (t2 - t1).TotalSeconds
      | _ ->
        f ()
    f ()

  let lesson i =
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

module FretToNote =

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
      printf "(%d/%d) > " (iteration - count + 1) iteration
      match getInput None None with
      | Some input when input = noteAt questionStringIndex questionFretIndex ->
        let t2 = DateTime.Now
        (t2 - t1).TotalSeconds
      | _ ->
        f ()
    f ()

  let lesson i =
    let rec helper acc i =
      if i <= 0
      then
        acc
      else
        let (questionStringIndex, questionFretIndex) = selectPoint ()
        printRows questionStringIndex questionFretIndex
        helper (challenge questionStringIndex questionFretIndex i :: acc) (i - 1)
    helper [] i

module NoteToFret =

  let printFooter _ =
    FretToNote.printFooter ()

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
    let basename = basenameAt questionStringIndex (fretOf questionStringIndex questionNote)
    let p = play basename
    let t1 = DateTime.Now
    let rec f _ =
      printf "(%d/%d) > " (iteration - count + 1) iteration
      match getInput (Some basename) (Some questionNote) with
      | Some input when questionNote = noteAt questionStringIndex input ->
        let t2 = DateTime.Now
        p.WaitForExit ()
        (t2 - t1).TotalSeconds
      | _ ->
        f ()
    f ()

  let lesson i =
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

module Chroma =

  let rec takeRandomNote _ =
    let questionFilename = (new System.Random()).Next(9, 38)
    if List.contains (rem (questionFilename + 3) 12) standardScale
    then
      questionFilename + 3
    else
      takeRandomNote ()

  let challenge questionNote count =
    let basename = (string (questionNote - 3))
    let p = play basename
    let t1 = DateTime.Now
    let rec f _ =
      printf "(%d/%d) > " (iteration - count + 1) iteration
      match getInput (Some basename) None with
      | Some input when rem questionNote 12 = input ->
        let t2 = DateTime.Now
        p.WaitForExit ()
        (t2 - t1).TotalSeconds
      | _ ->
        f ()
    f ()

  let lesson i =
    let rec helper acc i =
      if i <= 0
      then
        acc
      else
        let questionNote = takeRandomNote ()
        helper (challenge questionNote i :: acc) (i - 1)
    helper [] i

module Staff =

  let rec takeRandomNote _ =
    let questionNote = (new System.Random()).Next(9, 36)
    if List.contains (rem questionNote 12) standardScale
    then
      questionNote
    else
      takeRandomNote ()

  let noteToRow note =
    //              A       C                   A       C                   A       C
    let noteList = [9; 11; 12; 14; 16; 17; 19; 21; 23; 24; 26; 28; 29; 31; 33; 35; 36]
    match findIndex note noteList with
    | None ->
      failwith "(unreachable)"
    | Some rowIndex ->
      rowIndex

  let printEvenRow rowIndex questionRowIndex =
    if rowIndex <= 3 || 13 <= rowIndex
    then
      printf "    "
    else
      printf "----"
    if rowIndex = questionRowIndex
    then
      printf "- * -"
    else
      printf "-----"
    if rowIndex <= 3 || 13 <= rowIndex
    then
      printf "\n"
    else
      printf "----\n"

  let printOddRow rowIndex questionRowIndex =
    if rowIndex = questionRowIndex
    then
      printf "      *\n"
    else
      printf "\n"

  let printRow rowIndex questionRowIndex =
    if rowIndex % 2 = 0
    then
      printEvenRow rowIndex questionRowIndex
    else
      printOddRow rowIndex questionRowIndex

  let printRows questionRowIndex =
    printf "\n"
    for rowIndex = 16 downto 0 do
      printRow rowIndex questionRowIndex
    printf "\n"

  let challenge questionNote count =
    let basename = string (questionNote - 3)
    let p = play basename
    let t1 = DateTime.Now
    printRows (noteToRow questionNote)
    let rec f _ =
      printf "(%d/%d) > " (iteration - count + 1) iteration
      match getInput (Some basename) None with
      | Some input when rem questionNote 12 = input ->
        let t2 = DateTime.Now
        p.WaitForExit ()
        (t2 - t1).TotalSeconds
      | _ ->
        f ()
    f ()

  let lesson i =
    let rec helper acc i =
      if i <= 0
      then
        acc
      else
        let questionNote = takeRandomNote ()
        helper (challenge questionNote i :: acc) (i - 1)
    helper [] i

[<EntryPoint>]
let main args =
  for i = 0 to args.Length - 1 do
    let lessonOrNone =
      match args.[i] with
      | "fret-to-note" ->
        Some FretToNote.lesson
      | "note-to-fret" ->
        Some NoteToFret.lesson
      | "interval" ->
        Some Interval.lesson
      | "chroma" ->
        Some Chroma.lesson
      | "staff" ->
        Some Staff.lesson
      | _ ->
        None
    match lessonOrNone with
    | Some lesson ->
      save args.[i] (lesson iteration)
    | None ->
      ()
  0
