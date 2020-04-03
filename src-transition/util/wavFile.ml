(*
   The MIT License (MIT)

   Copyright (c) 2017 Leonardo Laguna Ruiz

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
   THE SOFTWARE.
*)

open Result

type buffer =
   { data : Buffer.t
   ; mutable index : int
   ; size : int
   }

(** Returns a character from the buffer and increases the index *)
let get (buffer : buffer) : char =
   let c = Buffer.nth buffer.data buffer.index in
   buffer.index <- buffer.index + 1 ;
   c


(** Returns a character (as integer) from the buffer *)
let get_int (buffer : buffer) : int32 = Int32.of_int (Char.code (get buffer))

(** Used to shift and combine characters into integers *)
let shift_or (b1 : int32) (b2 : int32) : int32 = Int32.logor (Int32.shift_left b2 8) b1

(** Reads two characters (16 bits) as an int *)
let read2 (buffer : buffer) : int32 =
   let b1 = get_int buffer in
   let b2 = get_int buffer in
   shift_or b1 b2


(** Reads three characters (24 bits) as an int *)
let read3 (buffer : buffer) : int32 =
   let b1 = get_int buffer in
   let b2 = get_int buffer in
   let b3 = get_int buffer in
   b3 |> shift_or b2 |> shift_or b1


(** Reads four characters (32 bits) as an int *)
let read4 (buffer : buffer) : int32 =
   let b1 = get_int buffer in
   let b2 = get_int buffer in
   let b3 = get_int buffer in
   let b4 = get_int buffer in
   b4 |> shift_or b3 |> shift_or b2 |> shift_or b1


(** Reads four characters as a string *)
let read4_chars (buffer : buffer) : string =
   let c1 = get buffer in
   let c2 = get buffer in
   let c3 = get buffer in
   let c4 = get buffer in
   let result = Bytes.create 4 in
   Bytes.set result 0 c1 ;
   Bytes.set result 1 c2 ;
   Bytes.set result 2 c3 ;
   Bytes.set result 3 c4 ;
   Bytes.to_string result


(** Moves chunk by chunk until it finds the "data" chunk *)
let searchData (buffer : buffer) : bool =
   let rec skipData n =
      if n = 0 then
         ()
      else
         let _ = get buffer in
         skipData (n - 1)
   in
   let rec loop () =
      if read4_chars buffer = "data" then
         true
      else
         let size = read4 buffer |> Int32.to_int in
         let () = skipData size in
         loop ()
   in
   match loop () with
   | found -> found
   | exception Invalid_argument _ -> false


(** constants to convert 16 bits to a float *)
let max_16 = (2.0 ** 16.0) /. 2.0

let sign_16 = Int32.shift_left Int32.one 15

let mask_16 = Int32.shift_left Int32.minus_one 15

(** Reads a 16 bit valua as a float *)
let readSample16 (buffer : buffer) : float =
   let v = read2 buffer in
   if Int32.logand sign_16 v <> Int32.zero then
      Int32.to_float (Int32.logor mask_16 v) /. max_16
   else
      Int32.to_float v /. max_16


(** constants to convert 24 bits to a float *)
let max_24 = (2.0 ** 24.0) /. 2.0

let sign_24 = Int32.shift_left Int32.one 23

let mask_24 = Int32.shift_left Int32.minus_one 23

(** Reads a 24 bit valua as a float *)
let readSample24 (buffer : buffer) : float =
   let v = read2 buffer in
   if Int32.logand sign_24 v <> Int32.zero then
      Int32.to_float (Int32.logor mask_24 v) /. max_24
   else
      Int32.to_float v /. max_24


let getReadSampleFunction (bits : int32) : (buffer -> float, string) result =
   match Int32.to_int bits with
   | 16 -> Ok readSample16
   | 24 -> Ok readSample24
   | _ -> Error ("Wave file encoded in an unsupported bits per sample: " ^ string_of_int (Int32.to_int bits))


(** Reads the given number of samples into the data arrays *)
let readSamples (buffer : buffer) (channels : int) (size : int) (data : float array array) (read_fn : buffer -> float) :
   int =
   (* iterates reading the channels *)
   let rec loop_channels index channel =
      if channel >= channels then
         true
      else
         try
            let value = read_fn buffer in
            let channel_data = data.(channel) in
            let () = channel_data.(index) <- value in
            loop_channels index (channel + 1)
         with
         | Invalid_argument _ -> false
   in
   (* iterates reading the samples *)
   let rec loop_samples index =
      if index >= size then
         size
      else if loop_channels index 0 then
         loop_samples (index + 1)
      else
         index - 1
   in
   loop_samples 0


(** Performs checks for valid wav format *)
let checkFormat (buffer : buffer) =
   if not (read4_chars buffer = "RIFF") then
      Error "Not a valid file"
   else
      let chunk_size = read4 buffer in
      if chunk_size < Int32.of_int 4 then
         Error "Invalid chunk size"
      else if not (read4_chars buffer = "WAVE") then
         Error "Not a supported wav file"
      else if not (read4_chars buffer = "fmt ") then
         Error "Not a supported wav file"
      else
         let sub_chunk_size = read4 buffer in
         let audio_format = read2 buffer in
         if sub_chunk_size <> Int32.of_int 16 || audio_format <> Int32.one then
            Error "Input file is not in PCM format"
         else
            Ok ()


type wave =
   { channels : int
   ; samples : int
   ; data : float array array
   }

(** Reads a wav file and returns an array containing the channels *)
let read (file : string) : (wave, string) result =
   match FileIO.read_bytes file with
   | None -> Error "failed to open the file"
   | Some data ->
      let buffer = { index = 0; size = Buffer.length data; data } in
      ( match checkFormat buffer with
        | Error _ as error -> error
        | Ok () ->
           let channels = read2 buffer |> Int32.to_int in
           let _sample_rate = read4 buffer in
           let _byte_rate = read4 buffer in
           let _block_align = read2 buffer in
           let bits_per_sample = read2 buffer in
           ( match getReadSampleFunction bits_per_sample with
             | Error _ as e -> e
             | Ok sample_fn ->
                if not (searchData buffer) then
                   Error "the file does not contain data"
                else
                   let size = read4 buffer |> Int32.to_int in
                   let no_samples = size / channels / (Int32.to_int bits_per_sample / 8) in
                   let data = Array.init channels (fun _ -> Array.make no_samples 0.0) in
                   let samples = readSamples buffer channels no_samples data sample_fn in
                   Ok { channels; samples; data } ) )
