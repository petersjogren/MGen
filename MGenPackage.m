(* ::Package:: *)

(* ::Title:: *)
(*MGen Package*)


BeginPackage["MGenPackage`"]


fromNoteString::usage = 
	"fromNoteString[str] converts a note of the form F#3 to an ASCII representation"


fromNoteCharacter::usage =
	"fromNoteCharacter[ch] converts the note character to a note string"


fromMIDIFileToCodedNoteString::usage =
	"fromMIDIFileToCodedNoteString[fileName] converts the a MIDI file to a string representation"


fromCodedNoteString::usage =
	"fromCodedNoteString[noteString,tempoBPM] converts notes in string representation to a Sound object"


sampleText::usage =
	"sampleText[text,len,num] makes num number of examples at random positions
	in text with len long strings as input and the next character as output."


generate::usage =
	"generate[net,start,len] takes the net and feeds start as input to it and gets the output character.
		Then it appends the output character to the last input sequence and runs the ne again to get the
		next character. It does this len times and returns the entire concatenated string consisting of 
		start and all the output characters for all the times net was called."


generateSample::usage = 
	"generateSample[net,start,len] Same as generate[net,start,len] but samples from the output distribution of the net
		instead of taking the output character with the largest probability"


generateSampleTruncated::usage = 
	"generateSample[net,start,len] Same as generateSample[net,start,len] but the input to each step is truncated to a fixed length of 250"


toWAVSound::usage =
	"toWAVSound[sound_Sound] converts a Sound object to a WAV Sound object"


polyphonicMIDIToString::usage = 
	"polyphonicMIDIToString[fileName] converts a polyphonic MIDI file to string representation"


stringToSound::usage = 
	"stringToSound[string,tempoBPM] converts string to Sound object"


characters::usage = 
	"All possible characters in the string representation"


(* ::Chapter:: *)
(*Private definitions*)


Begin["`Private`"]


(* ::Subsubsection:: *)
(*Import MIDI*)


(* ::Input:: *)
(*fileName=FileNameJoin[{NotebookDirectory[],"flute.mid"}];*)


(* ::Input:: *)
(*microsecondsPerQuarterNote=First@Cases[Flatten@Import[fileName,"Metadata"],("SetTempo"->x_)->x];*)
(*fluteSound=Import[fileName]*)


(* ::Input:: *)
(*fluteSound[[1]]//Short[#,10]&*)


(* ::Subsubsection:: *)
(*Transform to {note_number, start_ppq_in_bar, length_ppq, velocity_0_to_100}*)


(* ::Text:: *)
(*Use EmbeddingLayer to embed integers*)


(* ::Text:: *)
(*Make generator function that randomly selects a position in the song and forms pairs of 50 notes input and next {start_ppq_in_bar, note, length_ppq, velocity} as output.*)


(* ::Text:: *)
(*TODO! Code output better so it can be formulated as softmax probabilities. Maybe {note, ppq_since_last_note, length_ppq, velocity}*)


(* ::Input:: *)
(*SoundNote["B4",{0.9`,1.0987500000000001`},"Piano",SoundVolume->0.807843137254902`]*)


(* ::Subsubsection:: *)
(*Extract notes, position and length in 32nd notes*)


(* ::Input:: *)
(*notes=fluteSound[[1]]/.SoundNote[note_,{start_,stop_},"Piano",SoundVolume->vel_]->{note,Round[1000000start/(microsecondsPerQuarterNote/8)],Max[1,Round[1000000(stop-start)/(microsecondsPerQuarterNote/8)]]};*)
(*notes//Short[#,20]&*)


(* ::Subsubsection:: *)
(*Transform to {note, 32nd notes to the next note, length in 32nd notes}. (' ' is no note)*)


(* ::Input:: *)
(*notesDifferences=Transpose[{#[[1]],Differences[Join[#[[2]],{16+#[[2,-1]]}]],#[[3]]}&@Transpose[Join[{{" ",0,1}},notes]]];*)
(*notesDifferences//Short[#,10]&*)


(* ::Input:: *)
(*FromCharacterCode[#+46]&/@Range[0,88]*)


(* ::Subsubsection:: *)
(*Transform note string to a character*)


(* ::Input::Initialization:: *)
noteStrings=Flatten[Function[{octave},#<>ToString[octave]&/@{"A","A#","B","C","C#","D","D#","E","F","F#","G","G#"}]/@{1,2,3,4,5,6,7}]~Join~{" "}


(* ::Input::Initialization:: *)
fromNoteString[str_]:=If[str==" "," ",FromCharacterCode[First[First[Position[noteStrings,str]]]+44]]
fromNoteString/@noteStrings


(* ::Input::Initialization:: *)
fromNoteCharacter[ch_]:=If[ch==" "," ",noteStrings[[First[ToCharacterCode[ch]]-44]]]
fromNoteCharacter/@fromNoteString/@noteStrings==noteStrings


(* ::Input:: *)
(*notesWithCharacters=Transpose[{fromNoteString/@#[[1]],#[[2]],#[[3]]}&@Transpose[notesDifferences]];*)
(*notesWithCharacters//Short[#,10]&*)


(* ::Subsubsection:: *)
(*All possible characters*)


characters=Union[fromNoteString/@noteStrings,{"+"}];


(* ::Subsubsection:: *)
(*Make sure notes don't overlap*)


(* ::Input:: *)
(*notesWithCharactersNoOverlap={#[[1]],#[[2]],Min[#[[2]],#[[3]]]}&/@notesWithCharacters;*)
(*notesWithCharactersNoOverlap//Short[#,10]&*)


(* ::Subsubsection:: *)
(*Transform to: New note is note character, same note on next 32nd step is '+', rest on the next 32nd step is ' '.*)


(* ::Input:: *)
(*stringCodedNotes=StringJoin@Flatten[Module[{},{#[[1]],Array["+"&,#[[3]]-1],Array[" "&,#[[2]]-#[[3]]]}]&/@notesWithCharacters]*)


(* ::Subsubsection:: *)
(*Make function that does conversion from MIDI Sound object to string*)


(* ::Input::Initialization:: *)
fromMIDIFileToCodedNoteString[fileName_String]:=Module[{microsecondsPerQuarterNote,midiSound,notes,notesDifferences,noteStrings,notesWithCharacters,notesWithCharactersNoOverlap,stringCodedNotes},
microsecondsPerQuarterNote=First@Cases[Flatten@Import[fileName,"Metadata"],("SetTempo"->x_)->x];
midiSound=Import[fileName];
notes=midiSound[[1]]/.SoundNote[note_,{start_,stop_},instrument_,SoundVolume->vel_]->{note,Round[1000000start/(microsecondsPerQuarterNote/8)],Max[1,Round[1000000(stop-start)/(microsecondsPerQuarterNote/8)]]};
notesDifferences=Transpose[{#[[1]],Differences[Join[#[[2]],{64+#[[2,-1]]}]],#[[3]]}&@Transpose[Join[{{" ",0,1}},notes]]];
noteStrings=Flatten[Function[{octave},#<>ToString[octave]&/@{"A","A#","B","C","C#","D","D#","E","F","F#","G","G#"}]/@{1,2,3,4,5,6,7}]~Join~{" "};
notesWithCharacters=Transpose[{fromNoteString/@#[[1]],#[[2]],#[[3]]}&@Transpose[notesDifferences]];
notesWithCharactersNoOverlap={#[[1]],#[[2]],Min[#[[2]],#[[3]]]}&/@notesWithCharacters;
stringCodedNotes=StringJoin@Flatten[Module[{},{#[[1]],Array["+"&,#[[3]]-1],Array[" "&,#[[2]]-#[[3]]]}]&/@notesWithCharacters]
]


(* ::Input:: *)
(*fromMIDIFileToCodedNoteString[fileName]*)


(* ::Subsubsection:: *)
(*Make function that converts from string back to Sound object. Listen.*)


(* ::Input::Initialization:: *)
fromCodedNoteString[noteString_,tempoBPM_]:=Module[{note32ToTime},
note32ToTime[notes32_]:=60. notes32/8 /tempoBPM;
Sound[SoundNote[#[[1]],#[[2]],"Flute"]&/@StringCases[noteString,{
(x:(" "..)):>{None,note32ToTime@StringLength[x]},
(x:(Except[Characters[" +"]]~~"+"...)):>{fromNoteCharacter@StringTake[x,{1}],note32ToTime@StringLength[x]}
}]]
]


(* ::Input:: *)
(*fromCodedNoteString[stringCodedNotes,120]*)


(* ::Subsubsection:: *)
(*Convert to coded string and back*)


(* ::Input:: *)
(*fromCodedNoteString[fromMIDIFileToCodedNoteString[fileName],100]*)


(* ::Subsubsection:: *)
(*Construct a LSTM net with 4 layers and 320 neurons in each layer or smaller*)


(* ::Input::Initialization:: *)
sampleText[text_,len_,num_]:=Module[
	{positions=RandomInteger[{len+1,StringLength[text]-1},num]},
	Thread[StringTake[text,{#-len,#}&/@positions]->
	StringPart[text,positions+1]]]


(* ::Input:: *)
(*text=fromMIDIFileToCodedNoteString[fileName];*)


(* ::Input:: *)
(*trainingData=sampleText[text,50,10000];*)
(*RandomSample[trainingData,5]*)


(* ::Input:: *)
(*characters=Union[Characters[text]]*)


(* ::Input:: *)
(*net=NetInitialize@NetChain[{*)
(*UnitVectorLayer[],*)
(*LongShortTermMemoryLayer[300,"Dropout"->0.2],*)
(*LongShortTermMemoryLayer[300],*)
(*LongShortTermMemoryLayer[300],*)
(*LongShortTermMemoryLayer[300],*)
(*SequenceLastLayer[],*)
(*LinearLayer[64],*)
(*Tanh,*)
(*LinearLayer[],*)
(*SoftmaxLayer[]},*)
(*"Input"->NetEncoder[{"Characters",characters}],*)
(*"Output"->NetDecoder[{"Class",characters}]*)
(*]*)


(* ::Subsubsection:: *)
(*Train the network (with checkpoints)*)


(* ::Input:: *)
(*trainedNet=net;*)


(* ::Input:: *)
(*trainedNet=NetTrain[trainedNet,trainingData,BatchSize->512,TargetDevice->"CPU",MaxTrainingRounds->Quantity[8,"Hours"],TrainingProgressCheckpointing->{"Directory","~/Desktop/slask/FluteCheckpoints"}]*)


(* ::Subsubsection:: *)
(*Listen to the results*)


(* ::Input::Initialization:: *)
generate[net_,start_,len_]:=
	Nest[StringJoin[#,net[#]]&,start,len]


(* ::Input:: *)
(*trainedNet=Import[FileNameJoin[{NotebookDirectory[],"FluteNets","2017-09-20T22:51:53_2_286_5718_4.66e-3.wlnet"}]]*)


(* ::Input:: *)
(*generated=generate[trainedNet,"+c+a+S+R+S e+++f+++++",200]*)


(* ::Input:: *)
(*fromCodedNoteString[generated,100]*)


(* ::Input::Initialization:: *)
generateSample[net_,start_,len_]:=
	Nest[StringJoin[#,net[#,"RandomSample"]]&,start,len]
generateSampleTruncated[net_,start_,len_]:=
	Nest[StringJoin[#,net[StringTake[#,-Min[250,StringLength[#]]],"RandomSample"]]&,start,len]


(* ::Input:: *)
(*sampled=generateSample[trainedNet,"+c+a+S+R+S e+++f+++++",200]*)


(* ::Input:: *)
(*fromCodedNoteString[sampled,100]*)


(* ::Input:: *)
(*Export["/tmp/a.mid",%]*)


(* ::Input:: *)
(*SystemOpen[DirectoryName[AbsoluteFileName["/tmp/a.mid"]]]*)


(* ::Subsubsection:: *)
(*Result: trainedNet was over trained and just learned the entire piece*)


(* ::Subsubsection:: *)
(*Convert polyphonic MIDI to string*)


timeTo32ndNote[time_,microsecondsPerQuarterNote_]:=Round[time*8.*1000000/microsecondsPerQuarterNote]


note32ndToTime[note32nd_,tempoBPM_]:=note32nd/8/tempoBPM 60


polyphonicMIDIToString[fileName_] := Module[{data,microsecondsPerQuarterNote,noteStartlength,clockAdvances,noteAdvancesLengths,resultString},
	data=Import[fileName];
	microsecondsPerQuarterNote=First@Cases[Import[fileName,"Metadata"],("SetTempo"->tempo_)->tempo,{3}];
	noteStartlength=SortBy[data[[1]]/.SoundNote[note_,{fromTime_,toTime_},__,SoundVolume->__]->{fromNoteString@note,timeTo32ndNote[fromTime,microsecondsPerQuarterNote],timeTo32ndNote[toTime-fromTime,microsecondsPerQuarterNote]},#[[2]]&];
	clockAdvances=Differences[Join[{{" ",0,0}},noteStartlength][[All,2]]];
	noteAdvancesLengths=Transpose[{#[[1]],clockAdvances,#[[3]]}&@Transpose[noteStartlength]];
	resultString=StringJoin[{Array[" "&,#[[2]]],#[[1]],Array["+"&,Max[#[[3]]-1,0]]}&/@noteAdvancesLengths]
]


(* ::Subsubsection:: *)
(*Convert string to Sound*)


stringToSound[string_,tempoBPM_]:=Module[{notesAndDurations,notesStartsAndDurations},
	notesAndDurations=StringCases[string,{b:(" "..):>{" ",StringLength[b]},note_~~p:("+"...):>{note,1+StringLength[p]}}];
	notesStartsAndDurations=Cases[FoldList[If[#2[[1]]==" ",{#2[[1]],#1[[2]]+#2[[2]],#2[[2]]},{#2[[1]],#1[[2]],#2[[2]]}]&,{" ",0,0},notesAndDurations],
	Except[{" ",_,_}]];
	Sound[Cases[SoundNote[fromNoteCharacter[#[[1]]],{note32ndToTime[#[[2]],tempoBPM],note32ndToTime[#[[2]],tempoBPM]+note32ndToTime[#[[3]],tempoBPM]}]&/@notesStartsAndDurations,
	Except[SoundNote[" ",{_,_}]]
	]
	]
]


(* ::Input::Initialization:: *)
toWAVSound[sound_Sound]:=ImportString[ExportString[sound,"WAV"],"WAV"]


End[]


EndPackage[]
