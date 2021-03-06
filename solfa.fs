open System
open System.IO
open System.Diagnostics

open Argu

[<Struct>]
type OptionalBuilder =
  member __.Return(v) =
    Some v
  member __.Bind(m, k) =
    match m with
    | Some value ->
      k value
    | None ->
      None

let optional =
  OptionalBuilder()

let mutable solfaCount =
  0

let mutable solfaSize =
  0

let mutable solfaLevel =
  0

let mutable outputDirPath =
  ""

// the directory of executable (required to load wav files relatively)
let mutable baseDirPath =
  ""

// 0 = C, 2 = D, 4 = E, etc.
let whiteNoteList =
  [0; 2; 4; 5; 7; 9; 11]

// 1 = C# = Db, 3 = D# = Eb, etc.
let blackNoteList =
  [1; 3; 6; 8; 10]

// the `undefined` in Haskell.
let admit<'a> =
  failwith<'a> "admit"

// the result of `%` in F# can be negative when the `x` in `x % m` is negative,
// which isn't appropriate for the purpose of this software.
// the purpose of `rem` is to compensate this.
let rem x m =
  let tmp = x % m
  if tmp >= 0
  then
    tmp
  else
    tmp + m

type Question = {
  answerList: List<int>;
  eraseCount: int;
  printer: unit -> unit;
  basenameList: List<string>;
}

let eraseLines lineCount =
  printf "\r"
  for i = 1 to lineCount do
    printf "\u001b[A"
  printf "\u001b[0J"

let promptWith currentIteration =
  printf "\n\u001b[A"
  printf "(%d/%d) > " (solfaCount - currentIteration + 1) solfaCount

let noteAt stringIndex fretIndex =
  rem (5 + 7 * stringIndex + fretIndex) 12

let fretOf stringIndex note =
  rem (note - 5 - 7 * stringIndex) 12

let parseInt (str : string) =
  match System.Int32.TryParse str with
  | true, i ->
    Some i
  | _, _ ->
    None

let sample (xs : List<'a>) =
  let i = (new System.Random()).Next(0, xs.Length)
  xs.[i]

// the notes of open strings.
//    [29; 24; 19; 14; 9; 4] mod 12
// ~> [5; 0; 7; 2; 9; 4]
// ~> [F; C; G; D; A; E]
// ~> (so-called "all fourths" tuning, although I prefer to refer it as 5-regular tuning)
let baseNoteOf stringIndex =
  [29; 24; 19; 14; 9; 4].[stringIndex]

let toRelPath path =
  sprintf "%d/%s.wav" solfaLevel path

let play relPathList =
  let p = new Process ()
  match Environment.OSVersion.Platform with
  | PlatformID.Unix ->
      p.StartInfo.FileName <- "bash"
      let argList = List.map (fun relPath -> sprintf "paplay %s/assets/%s" baseDirPath relPath) relPathList
      let arg = String.concat " && " argList
      p.StartInfo.Arguments <- sprintf "-c \"%s\"" arg
      p.StartInfo.RedirectStandardError <- true
  | _ ->
      ()
  let _ = p.Start ()
  p

// computes the note name of specific position on a fretboard
let basenameAt stringIndex fretIndex =
  sprintf "%02d" (baseNoteOf stringIndex + fretIndex)

let getInput basenameList =
  let rawInputStr = Console.ReadLine ()
  if String.IsNullOrEmpty rawInputStr
  then
    None
  else
    let inputStr = rawInputStr.Trim ()
    let wordList = Array.toList (inputStr.Split [|' '|])
    let rec f wordList =
      match wordList with
      | [] ->
        Some []
      | x :: xs ->
        match x, basenameList with
        | "p", _ ->
          let _ = play (List.map toRelPath basenameList)
          None
        | "exit", _ ->
          let _ = Environment.Exit 0
          None // unreachable
        | _ ->
          optional {
            let! x' = parseInt x
            let! xs' = f xs
            return (x' :: xs')
          }
    f wordList

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
    let dirPath = sprintf "%s/%s/" outputDirPath name
    let _ = System.IO.Directory.CreateDirectory dirPath
    let dateStr = DateTime.Now.ToString("yyyy-MM-dd-HHmmss")
    let path = dirPath + dateStr
    File.WriteAllLines(path, [string value])

let generateQuestionWith currentIteration info =
  let t1 = DateTime.Now
  info.printer ()
  let pid = play (List.map toRelPath info.basenameList)
  let rec f _ =
    promptWith currentIteration
    match getInput info.basenameList with
    | Some input when input = info.answerList ->
      eraseLines info.eraseCount
      pid.Kill ()
      // pid.WaitForExit ()
      let t2 = DateTime.Now
      (t2 - t1).TotalSeconds
    | Some _ ->
      eraseLines 1
      let _ = play ["beep.wav"]
      f ()
    | _ ->
      eraseLines 1
      f ()
  f ()

let accumulate i f =
  let rec helper acc i f =
    if i <= 0
    then
      acc
    else
      helper (f i :: acc) (i - 1) f
  helper [] i f

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
    for i = 5 downto 0 do
      printRow stringIndex offset i range

  let question stringIndex offset currentIteration =
    generateQuestionWith currentIteration {
      answerList = [intervalAt stringIndex offset];
      eraseCount = 7;
      printer = fun _ -> printRows stringIndex offset 1;
      basenameList = [];
    }

  let lesson _ =
    accumulate solfaCount <| fun currentIteration ->
      let stringIndex = (new System.Random()).Next(1, 6)
      let offset = (new System.Random()).Next(-1, 2)
      question stringIndex offset currentIteration

module FretToNote =

  let rec selectPoint _ =
    let questionStringIndex = (new System.Random()).Next(0, 6)
    let questionFretIndex = (new System.Random()).Next(0, 12)
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
      if fretIndex = 0 then printf "|"
    printf "|\n"

  let printRows questionStringIndex questionFretIndex =
    for stringIndex = 0 to 5 do
      printRow questionStringIndex questionFretIndex stringIndex
    printFooter ()

  let question questionStringIndex questionFretIndex currentIteration =
    generateQuestionWith currentIteration {
      answerList = [noteAt questionStringIndex questionFretIndex];
      eraseCount = 8;
      printer = fun _ -> printRows questionStringIndex questionFretIndex;
      basenameList = [];
    }

  let lesson _ =
    accumulate solfaCount <| fun currentIteration ->
      let (questionStringIndex, questionFretIndex) = selectPoint ()
      question questionStringIndex questionFretIndex currentIteration

module NoteToFret =

  let printFooter _ =
    printf "  "
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
        if fretIndex = 0 then printf "\u001b[33m|\u001b[0m"
      else
        printf "|    "
        if fretIndex = 0 then printf "|"
    if questionStringIndex = stringIndex
    then
      printf "\u001b[33m|\u001b[0m\n"
    else
      printf "|\n"

  let printRows questionStringIndex =
    for stringIndex = 0 to 5 do
      printRow questionStringIndex stringIndex
    printFooter

  let question questionStringIndex questionNote currentIteration =
    generateQuestionWith currentIteration {
      answerList = [fretOf questionStringIndex questionNote];
      eraseCount = 8;
      printer = fun _ -> printRows questionStringIndex questionNote;
      basenameList = [basenameAt questionStringIndex (fretOf questionStringIndex questionNote)];
    }

  let lesson _ =
    accumulate solfaCount <| fun currentIteration ->
      let questionStringIndex = (new System.Random()).Next(1, 6)
      let questionNote = sample whiteNoteList
      question questionStringIndex questionNote currentIteration

module Chroma =

  let rec takeRandomNote noteList =
    let questionFilename = (new System.Random()).Next(12, 41)
    if List.contains (rem questionFilename 12) noteList
    then
      questionFilename
    else
      takeRandomNote noteList

  let question questionNoteList currentIteration =
    generateQuestionWith currentIteration {
      answerList = List.map (fun x -> rem x 12) questionNoteList;
      eraseCount = 1;
      printer = fun _ -> ();
      basenameList = List.map (fun x -> sprintf "%02d" x) questionNoteList;
    }

  let lesson noteList =
    accumulate solfaCount <| fun currentIteration ->
      let questionNoteList = List.map (fun _ -> takeRandomNote noteList) [0 .. solfaSize - 1]
      question questionNoteList currentIteration

module Staff =

  let rec takeRandomNote _ =
    let questionNote = (new System.Random()).Next(9, 37)
    if List.contains (rem questionNote 12) whiteNoteList
    then
      questionNote
    else
      takeRandomNote ()

  let noteToRow note =
    let noteList = [9; 11; 12; 14; 16; 17; 19; 21; 23; 24; 26; 28; 29; 31; 33; 35; 36]
    List.findIndex ((=) note) noteList

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
    for rowIndex = 16 downto 0 do
      printRow rowIndex questionRowIndex
    printf "\n"

  let question questionNote currentIteration =
    generateQuestionWith currentIteration {
      answerList = [rem questionNote 12];
      eraseCount = 19;
      printer = fun _ -> printRows (noteToRow questionNote);
      basenameList = [];
    }

  let lesson _ =
    accumulate solfaCount <| fun currentIteration ->
      let questionNote = takeRandomNote ()
      question questionNote currentIteration

module Convention =

  let conventionInfoList =
    [("m2", 1);
     ("M2", 2);
     ("m3", 3);
     ("M3", 4);
     ("P4", 5);
     ("P5", 7);
     ("m6", 8);
     ("M6", 9);
     ("m7", 10);
     ("M7", 11);
     ("b9", 1);
     ("9", 2);
     ("#9", 3);
     ("b11", 4);
     ("11", 5);
     ("#11", 6);
     ("b13", 8);
     ("13", 9);
     ("#13", 10)
     ]

  let question conventionInfo currentIteration =
    generateQuestionWith currentIteration {
      answerList = [snd conventionInfo];
      eraseCount = 2;
      printer = fun _ -> printf "%s:\n" (fst conventionInfo);
      basenameList = [];
    }

  let lesson _ =
    accumulate solfaCount <| fun currentIteration ->
      question (sample conventionInfoList) currentIteration

type Arguments =
  | [<CliPrefix(CliPrefix.None)>] Interval
  | [<CliPrefix(CliPrefix.None)>] Fret_To_Note
  | [<CliPrefix(CliPrefix.None)>] Note_To_Fret
  | [<CliPrefix(CliPrefix.None)>] White
  | [<CliPrefix(CliPrefix.None)>] Black
  | [<CliPrefix(CliPrefix.None)>] Chroma
  | [<CliPrefix(CliPrefix.None)>] Staff
  | [<CliPrefix(CliPrefix.None)>] Convention
  | [<AltCommandLine("-s")>] [<Mandatory>] Size of int
  | [<AltCommandLine("-c")>] [<Mandatory>] Count of int
  | [<AltCommandLine("-l")>] [<Mandatory>] Level of int
  | [<AltCommandLine("-o")>] [<Mandatory>] Output of string
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Interval ->
        "find the interval between two given notes."
      | Fret_To_Note ->
        "find the note for given fret position."
      | Note_To_Fret ->
        "find the fret position for a note."
      | White ->
        "find the white note name from its actual sound."
      | Black ->
        "find the black note name from its actual sound."
      | Chroma ->
        "find the note name from its actual sound."
      | Staff ->
        "find the note name for given position in a staff."
      | Convention ->
        "translate conventional names of intervals to integers."
      | Size _ ->
        "the \"size\" of each question of a test."
      | Level _ ->
        "the \"level\" of each question of a test."
      | Count _ ->
        "repeat each test for the number specified by this option."
      | Output _ ->
        "where to output the result."

let args = System.Environment.GetCommandLineArgs ()

baseDirPath <- (Directory.GetParent (Array.head args)).ToString ()

let withSuffix str =
  sprintf "%s-level-%d-size-%d" str solfaLevel solfaSize

try
  let parser = ArgumentParser.Create<Arguments>()
  let results = parser.Parse (Array.tail args)
  for dirPath in results.GetResults Output do
    outputDirPath <- dirPath
  for i in results.GetResults Count do
    solfaCount <- i
  for i in results.GetResults Size do
    solfaSize <- i
  for l in results.GetResults Level do
    solfaLevel <- l
  for item in results.GetAllResults() do
    match item with
    | Interval ->
      save "interval" (Interval.lesson ())
    | Fret_To_Note  ->
      save "fret-to-note" (FretToNote.lesson ())
    | Note_To_Fret ->
      save "note-to-fret" (NoteToFret.lesson ())
    | White ->
      save (withSuffix "white") (Chroma.lesson whiteNoteList)
    | Black ->
      save (withSuffix "black") (Chroma.lesson blackNoteList)
    | Chroma ->
      save (withSuffix "chroma") (Chroma.lesson (List.append whiteNoteList blackNoteList))
    | Staff ->
      save "staff" (Staff.lesson ())
    | Convention  ->
      save "convention" (Convention.lesson ())
    | _ ->
      ()
with e ->
  printfn "%s" e.Message
