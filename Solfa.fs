open System
open System.IO
open System.Diagnostics

open Argu

let mutable upperBound =
  0

let mutable outputDirPath =
  ""

let mutable baseDirPath =
  ""

let standardScale =
  [0; 2; 4; 5; 7; 9; 11]

let admit<'a> =
  failwith<'a> "admit"

let rem x m =
  let tmp = x % m
  if tmp >= 0
  then
    tmp
  else
    tmp + m

type Question = {
  answer: int;
  eraseCount: int;
  printer: unit -> unit;
  basename: Option<string>;
}

let eraseLines lineCount =
  printf "\r"
  for i = 1 to lineCount do
    printf "\u001b[A"
  printf "\u001b[0J"

let promptWith currentIteration =
  printf "\n\u001b[A"
  printf "(%d/%d) > " (upperBound - currentIteration + 1) upperBound

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

let baseNoteOf stringIndex =
  [29; 24; 19; 14; 9; 4].[stringIndex]

let play basename =
  let p = new Process ()
  match Environment.OSVersion.Platform with
  | PlatformID.Unix ->
      let arg = sprintf "%s/sine/%s.wav" baseDirPath basename
      p.StartInfo.FileName <- "paplay"
      p.StartInfo.Arguments <- arg
      p.StartInfo.RedirectStandardError <- true
  | _ ->
      ()
  let _ = p.Start ()
  p

let basenameAt stringIndex fretIndex =
  sprintf "%02d" (baseNoteOf stringIndex + fretIndex)

let getInput basenameOrNone =
  let rawInputStr = Console.ReadLine ()
  if String.IsNullOrEmpty rawInputStr
  then
    None
  else
    let inputStr = rawInputStr.Trim ()
    match inputStr, basenameOrNone with
    | "p", Some basename ->
      let _ = play basename
      None
    | "exit", _ ->
      let _ = Environment.Exit 0
      None // unreachable
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
    let dirPath = sprintf "%s/%s/" outputDirPath name
    let _ = System.IO.Directory.CreateDirectory dirPath
    let dateStr = DateTime.Now.ToString("yyyy-MM-dd-HHmmss")
    let path = dirPath + dateStr
    File.WriteAllLines(path, [string value])

let generateQuestionWith currentIteration info =
  let t1 = DateTime.Now
  info.printer ()
  let pidOrNone = Option.bind (play >> Some) info.basename
  let rec f _ =
    promptWith currentIteration
    match getInput info.basename with
    | Some input when input = info.answer ->
      eraseLines info.eraseCount
      let _ = Option.bind (fun (p : Process) -> Some (p.WaitForExit ())) pidOrNone
      let t2 = DateTime.Now
      (t2 - t1).TotalSeconds
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

  let challenge stringIndex offset currentIteration =
    generateQuestionWith currentIteration {
      answer = intervalAt stringIndex offset;
      eraseCount = 7;
      printer = fun _ -> printRows stringIndex offset 1;
      basename = None;
    }

  let lesson _ =
    accumulate upperBound <| fun currentIteration ->
      let stringIndex = (new System.Random()).Next(1, 6)
      let offset = (new System.Random()).Next(-1, 2)
      challenge stringIndex offset currentIteration

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

  let challenge questionStringIndex questionFretIndex currentIteration =
    generateQuestionWith currentIteration {
      answer = noteAt questionStringIndex questionFretIndex;
      eraseCount = 8;
      printer = fun _ -> printRows questionStringIndex questionFretIndex;
      basename = None;
    }

  let lesson _ =
    accumulate upperBound <| fun currentIteration ->
      let (questionStringIndex, questionFretIndex) = selectPoint ()
      challenge questionStringIndex questionFretIndex currentIteration

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

  let challenge questionStringIndex questionNote currentIteration =
    generateQuestionWith currentIteration {
      answer = fretOf questionStringIndex questionNote;
      eraseCount = 8;
      printer = fun _ -> printRows questionStringIndex questionNote;
      basename = Some (basenameAt questionStringIndex (fretOf questionStringIndex questionNote));
    }

  let lesson _ =
    accumulate upperBound <| fun currentIteration ->
      let questionStringIndex = (new System.Random()).Next(1, 6)
      let questionNote = sample standardScale
      challenge questionStringIndex questionNote currentIteration

module Chroma =

  let rec takeRandomNote _ =
    let questionFilename = (new System.Random()).Next(12, 41)
    if List.contains (rem questionFilename 12) standardScale
    then
      questionFilename
    else
      takeRandomNote ()

  let challenge questionNote currentIteration =
    generateQuestionWith currentIteration {
      answer = rem questionNote 12;
      eraseCount = 1;
      printer = fun _ -> ();
      basename = Some (sprintf "%02d" questionNote);
    }

  let lesson _ =
    accumulate upperBound <| fun currentIteration ->
      let questionNote = takeRandomNote ()
      challenge questionNote currentIteration

module Staff =

  let rec takeRandomNote _ =
    let questionNote = (new System.Random()).Next(9, 37)
    if List.contains (rem questionNote 12) standardScale
    then
      questionNote
    else
      takeRandomNote ()

  let noteToRow note =
    //              A       C                   A       C                   A       C
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

  let challenge questionNote currentIteration =
    generateQuestionWith currentIteration {
      answer = rem questionNote 12;
      eraseCount = 19;
      printer = fun _ -> printRows (noteToRow questionNote);
      basename = None;
    }

  let lesson _ =
    accumulate upperBound <| fun currentIteration ->
      let questionNote = takeRandomNote ()
      challenge questionNote currentIteration

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

  let challenge conventionInfo currentIteration =
    generateQuestionWith currentIteration {
      answer = snd conventionInfo;
      eraseCount = 2;
      printer = fun _ -> printf "%s:\n" (fst conventionInfo);
      basename = None;
    }

  let lesson _ =
    accumulate upperBound <| fun currentIteration ->
      challenge (sample conventionInfoList) currentIteration

type Arguments =
  | [<CliPrefix(CliPrefix.None)>] Fret_To_Note
  | [<CliPrefix(CliPrefix.None)>] Note_To_Fret
  | [<CliPrefix(CliPrefix.None)>] Interval
  | [<CliPrefix(CliPrefix.None)>] Chroma
  | [<CliPrefix(CliPrefix.None)>] Staff
  | [<CliPrefix(CliPrefix.None)>] Convention
  | [<AltCommandLine("-o")>] [<Mandatory>] Output of string
  | [<AltCommandLine("-i")>] [<Mandatory>] Iteration of int
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Fret_To_Note ->
        "do a solfege of finding the note for given fret position."
      | Note_To_Fret ->
        "do a solfege of finding the fret position for a note."
      | Interval ->
        "do a solfege of finding the interval between two given notes."
      | Chroma ->
        "do a solfege of finding the note name from its actual sound."
      | Staff ->
        "do a solfege of finding the note name for given position in a staff."
      | Convention ->
        "do a solfege of translating conventional names of intervals to integers."
      | Output _ ->
        "where to save the result."
      | Iteration _ ->
        "how many times do you want to iterate?"

let args = System.Environment.GetCommandLineArgs ()

baseDirPath <- (Directory.GetParent (Array.head args)).ToString ()

try
  let parser = ArgumentParser.Create<Arguments>()
  let results = parser.Parse (Array.tail args)
  for dirPath in results.GetResults Output do
    outputDirPath <- dirPath
  for i in results.GetResults Iteration do
    upperBound <- i
  for item in results.GetAllResults() do
    match item with
    | Fret_To_Note ->
      save "fret-to-note" (FretToNote.lesson ())
    | Note_To_Fret ->
      save "note-to-fret" (NoteToFret.lesson ())
    | Interval ->
      save "interval" (Interval.lesson ())
    | Chroma ->
      save "chroma" (Chroma.lesson ())
    | Staff ->
      save "staff" (Staff.lesson ())
    | Convention ->
      save "convention" (Convention.lesson ())
    | _ ->
      ()
with e ->
  printfn "%s" e.Message
