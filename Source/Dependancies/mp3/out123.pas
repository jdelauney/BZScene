//
// out123.h header binding for the Free Pascal Compiler aka FPC
//
// Binaries and demos available at http://www.djmaster.com/
//

(*
    out123: audio output interface

    copyright 1995-2016 by the mpg123 project,
    free software under the terms of the LGPL 2.1

    see COPYING and AUTHORS files in distribution or http://mpg123.org
    initially written as audio.h by Michael Hipp, reworked into out123 API
    by Thomas Orgis
*)

(** \file out123.h The header file for the libout123 audio output facility. *)

unit out123;

{$mode objfpc}{$H+}
{$inline on}

interface

(* We only need size_t definition. *)
// #include <stddef.h>

(* Common audio encoding specification, including a macro for getting
 *  size of encodined samples in bytes. Said macro is still hardcoded
 *  into out123_encsize(). Relying on this one may help an old program
 *  know sizes of encodings added to fmt123.h later on.
 *  If you don't care, just use the macro.
 *)
// #include <fmt123.h>

uses
  sysutils,
  fmt123,
  ctypes;

type
  pppcchar = ^ppcchar;
  ppcchar = ^pcchar;
  ppcint = ^pcint;

{$IFDEF LINUX}
  size_t = DWORD;
{$ENDIF}

const
  LIB_OUT123 = 'libout123-0.dll';

(** A macro to check at compile time which set of API functions to expect.
 * This should be incremented at least each time a new symbol is added
 * to the header.
 *)
const
  OUT123_LIB_VERSION = '1.25.6';
  OUT123_API_VERSION = 2;
  OUT123_LIB_PATCHLEVEL = 1;

// (** Defines needed for MS Visual Studio(tm) DLL builds.
//  * Every public function must be prefixed with MPG123_EXPORT. When building
//  * the DLL ensure to define BUILD_MPG123_DLL. This makes the function accessible
//  * for clients and includes it in the import library which is created together
//  * with the DLL. When consuming the DLL ensure to define LINK_MPG123_DLL which
//  * imports the functions from the DLL.
//  *)
// #ifdef BUILD_MPG123_DLL
// (* The dll exports. *)
// #define MPG123_EXPORT __declspec(dllexport)
// #else
// #ifdef LINK_MPG123_DLL
// (* The exe imports. *)
// #define MPG123_EXPORT __declspec(dllimport)
// #else
// (* Nothing on normal/UNIX builds *)
// #define MPG123_EXPORT
// #endif
// #endif

(** \defgroup out123_api out123 library API
 *  This is out123, a library focused on continuous playback of audio streams
 *  via various platform-specific output methods. It glosses over details of
 *  the native APIs to give an interface close to simply writing data to a
 *  file. There might be the option to tune details like buffer (period) sizes
 *  and the number of them on the device side in future, but the focus of the
 *  library is to ease the use case of just getting that raw audio data out
 *  there, without interruptions.
 *
 *  The basic idea is to create a handle with out123_new() and open a certain
 *  output device (using a certain driver module, possibly build-time defaults)
 *  with out123_open(). Now, you can query the output device for supported
 *  encodings for given rate and channel count with out123_get_encodings() and
 *  decide what to use for actually starting playback with out123_start().
 *
 *  Then, you just need to provide (interleaved pcm) data for playback with
 *  out123_play(), which will block when the device's buffers are full. You get
 *  your timing from that (instead of callbacks). If your program does the
 *  production of the audio data just a little bit faster than the playback,
 *  causing out123_play() to block ever so briefly, you're fine.
 *
 *  You stop playback with out123_stop(), or just close the device and driver
 *  via out123_close(), or even just decide to drop it all and do out123_del()
 *  right away when you're done.
 *
 *  There are other functions for specific needs, but the basic idea should be
 *  covered by the above.
 @{
 *)

type
(** Typedef shortcut as preferrend name for the handle type. *)
  pout123_handle = ^out123_handle;
  out123_handle = ^out123_struct;
(** Opaque structure for the libout123 handle. *)
  out123_struct = record
  end;

type
(** Enumeration of codes for the parameters that it is possible to set/get. *)
  out123_parms = clong;
const
  OUT123_FLAGS_ = 1;       (**< integer, various flags, see enum out123_flags *)
  OUT123_PRELOAD = 2;      (**< float, fraction of buffer to fill before playback *)
  OUT123_GAIN = 3;         (**< integer, output device gain (module-specific) *)
  OUT123_VERBOSE = 4;      (**< integer, verbosity to stderr, >= 0 *)
  OUT123_DEVICEBUFFER = 5; (**<
  *  float, length of device buffer in seconds;
  *  This might be ignored, might have only a loose relation to actual
  *  buffer sizes and latency, depending on output driver. Try to tune
  *  this before opening a device if you want to influcence latency or reduce
  *  dropouts. Value <= 0 uses some default, usually favouring stable playback
  *  over low latency. Values above 0.5 are probably too much.
  *)
  OUT123_PROPFLAGS_ = 6;   (**< integer, query driver/device property flags (r/o) *)
  OUT123_NAME = 7;         (**< string, name of this instance (NULL restores default);
  * The value returned by out123_getparam() might be different if the audio
  * backend changed it (to be unique among clients, p.ex.).
  * TODO: The name provided here is used as prefix in diagnostic messages. *)
  OUT123_BINDIR = 8;       (**< string, path to a program binary directory to use
  * as starting point in the search for the output module directory
  * (e.g. ../lib/mpg123 or ./plugins). The environment variable MPG123_MODDIR
  * is always tried first and the in-built installation path last.
  *)

type
(** Flags to tune out123 behaviour *)
  out123_flags = clong;
const
  OUT123_HEADPHONES       = $01; (**< output to headphones (if supported) *)
  OUT123_INTERNAL_SPEAKER = $02; (**< output to speaker (if supported) *)
  OUT123_LINE_OUT         = $04; (**< output to line out (if supported) *)
  OUT123_QUIET            = $08; (**< no printouts to standard error *)
  OUT123_KEEP_PLAYING     = $10; (**<
  *  When this is set (default), playback continues in a loop when the device
  *  does not consume all given data at once. This happens when encountering
  *  signals (like SIGSTOP, SIGCONT) that cause interruption of the underlying
  *  functions.
  *  Note that this flag is meaningless when the optional buffer is employed,
  *  There, your program will always block until the buffer completely took
  *  over the data given to it via out123_play(), unless a communication error
  *  arises.
  *)

type
(** Read-only output driver/device property flags (OUT123_PROPFLAGS). *)
  out123_propflags = clong;
const
  OUT123_PROP_LIVE = $01;       (**< This is a live output, meaning that
  *  special care might be needed for pauses in playback (p.ex. stream
  *  of silence instead of interruption), as opposed to files on disk.
  *)
  OUT123_PROP_PERSISTENT = $02; (**< This (live) output does not need
  *  special care for pauses (continues with silence itself),
  *  out123_pause() does nothing to the device.
  *)

(** Create a new output handle.
 *  This only allocates and initializes memory, so the only possible
 *  error condition is running out of memory.
 * \return pointer to new handle or NULL on error
 *)
function out123_new(): pout123_handle; cdecl; external LIB_OUT123;

(** Delete output handle.
 *  This implies out123_close().
 *)
procedure out123_del(ao: pout123_handle); cdecl; external LIB_OUT123;

type
(** Error code enumeration
 * API calls return a useful (positve) value or zero (OUT123_OK) on simple
 * success. A negative value (-1 == OUT123_ERR) usually indicates that some
 * error occured. Which one, that can be queried using out123_errcode()
 * and friends.
 *)
  out123_error = clong;
const
  OUT123_ERR = -1;            (**< generic alias for verbosity, always == -1 *)
  OUT123_OK  = 0;             (**< just a name for zero, not going to change *)
  OUT123_DOOM = 1;            (**< dazzled, out of memory *)
  OUT123_BAD_DRIVER_NAME = 2; (**< bad driver name given *)
  OUT123_BAD_DRIVER = 3;      (**< unspecified issue loading a driver *)
  OUT123_NO_DRIVER = 4;       (**< no driver loaded *)
  OUT123_NOT_LIVE = 5;        (**< no active audio device *)
  OUT123_DEV_PLAY = 6;        (**< some device playback error *)
  OUT123_DEV_OPEN = 7;        (**< error opening device *)
  OUT123_BUFFER_ERROR = 8;    (**<
  * Some (really unexpected) error in buffer infrastructure.
  *)
  OUT123_MODULE_ERROR = 9;    (**< basic failure in module loading *)
  OUT123_ARG_ERROR = 10;      (**< some bad function arguments supplied *)
  OUT123_BAD_PARAM = 11;      (**< unknown parameter code *)
  OUT123_SET_RO_PARAM = 12;   (**< attempt to set read-only parameter *)
  OUT123_BAD_HANDLE = 13;     (**< bad handle pointer (NULL, usually) *)
  OUT123_ERRCOUNT = 14;       (**< placeholder for shaping arrays *)

(** Get string representation of last encountered error in the
 *  context of given handle.
 * \param ao handle
 * \return error string
 *)
function out123_strerror(ao: pout123_handle): pchar; cdecl; external LIB_OUT123;

(** Get the plain errcode intead of a string.
 * Note that this used to return OUT123_ERR instead of
 * OUT123_BAD_HANDLE in case of ao==NULL before mpg123-1.23.5 .
 * \param ao handle
 * \return error code recorded in handle or OUT123_BAD_HANDLE
 *)
function out123_errcode(ao: pout123_handle): cint; cdecl; external LIB_OUT123;

(** Return the error string for a given error code.
 * \param errcode the integer error code
 * \return error string
 *)
function out123_plain_strerror(errcode: cint): pchar; cdecl; external LIB_OUT123;

(** Set a desired output buffer size.
 *  This starts a separate process that handles the audio output, decoupling
 *  the latter from the main process with a memory buffer and saving you the
 *  burden to ensure sparing CPU cycles for actual playback.
 *  This is for applicatons that prefer continuous playback over small latency.
 *  In other words: The kind of applications that out123 is designed for.
 *  This routine always kills off any currently active audio output module /
 *  device, even if you just disable the buffer when there is no buffer.
 *
 *  Keep this in mind for memory-constrainted systems: Activating the
 *  buffer causes a fork of the calling process, doubling the virtual memory
 *  use. Depending on your operating system kernel's behaviour regarding
 *  memory overcommit, it might be wise to call out123_set_buffer() very
 *  early in your program before allocating lots of memory.
 *
 *  There _might_ be a change to threads in future, but for now this is
 *  classic fork with shared memory, working without any threading library.
 *  If your platform or build does not support that, you will always get an
 *  error on trying to set up a non-zero buffer (but the API call will be
 *  present).
 *
 *  Also, if you do intend to use this from a multithreaded program, think
 *  twice and make sure that your setup is happy with forking full-blown
 *  processes off threaded programs. Probably you are better off spawning a
 *  buffer thread yourself.
 *
 * \param ao handle
 * \param buffer_bytes size (bytes) of a memory buffer for decoded audio,
 *    a value of zero disables the buffer.
 * \return 0 on success, OUT123_ERR on error
 *)
function out123_set_buffer(ao: pout123_handle; buffer_bytes: size_t): cint; cdecl; external LIB_OUT123;

(** Set a specific parameter, for a specific out123_handle, using a parameter
 *  code chosen from the out123_parms enumeration, to the specified value.
 *  The parameters usually only change what happens on next out123_open, not
 *  incfluencing running operation.
 * \param ao handle
 * \param code parameter code
 * \param value input value for integer parameters
 * \param fvalue input value for floating point parameters
 * \param svalue input value for string parameters (contens are copied)
 * \return 0 on success, OUT123_ERR on error.
 *)
function out123_param(ao: pout123_handle; code: out123_parms; value: clong; fvalue: cdouble; const svalue: pchar): cint; cdecl; external LIB_OUT123;
function out123_param_int(ao: pout123_handle; code: out123_parms; value: clong): cint; cdecl; inline;
function out123_param_float(ao: pout123_handle; code: out123_parms; value: cdouble): cint; cdecl; inline;
function out123_param_string(ao: pout123_handle; code: out123_parms; value: pchar): cint; cdecl; inline;

(** Get a specific parameter, for a specific out123_handle, using a parameter
 *  code chosen from the out123_parms enumeration, to the specified value.
 * \param ao handle
 * \param code parameter code
 * \param ret_value output address for integer parameters
 * \param ret_fvalue output address for floating point parameters
 * \param ret_svalue output address for string parameters (pointer to
 *        internal memory, so no messing around, please)
 * \return 0 on success, OUT123_ERR on error (bad parameter name or bad handle).
 *)
function out123_getparam(ao: pout123_handle; code: out123_parms; ret_value: pclong; ret_fvalue: pcdouble; ret_svalue: ppchar): cint; cdecl; external LIB_OUT123;
function out123_getparam_int(ao: pout123_handle; code: out123_parms; value: pclong): cint; cdecl; inline;
function out123_getparam_float(ao: pout123_handle; code: out123_parms; value: pcdouble): cint; cdecl; inline;
function out123_getparam_string(ao: pout123_handle; code: out123_parms; value: ppchar): cint; cdecl; inline;

(** Copy parameters from another out123_handle.
 * \param ao handle
 * \param from_ao the handle to copy parameters from
 * \return 0 in success, -1 on error
 *)
function out123_param_from(ao: pout123_handle; from_ao: pout123_handle): cint; cdecl; external LIB_OUT123;

(** Get list of driver modules reachable in system in C argv-style format.
 *  The client is responsible for freeing the memory of both the individual
 *  strings and the lists themselves.
 *  A module that is not loadable because of missing libraries is simply
 *  skipped. You will get stderr messages about that unless OUT123_QUIET was
 *  was set, though. Failure to open the module directory is a serious error,
 *  resulting in negative return value.
 * \param ao handle
 * \param names address for storing list of names
 * \param descr address for storing list of descriptions
 * \return number of drivers found, -1 on error
 *)
function out123_drivers(ao: pout123_handle; names: pppchar; descr: pppchar): cint; cdecl; external LIB_OUT123;

(** Open an output device with a certain driver
 *  Note: Opening means that the driver code is loaded and the desired
 *  device name recorded, possibly tested for availability or tentatively
 *  opened. After out123_open(), you can ask for supported encodings
 *  and then really open the device for playback with out123_start().
 * \param ao handle
 * \param driver (comma-separated list of) output driver name(s to try),
 *               NULL for default (stdout for file-based drivers)
 * \param device device name to open, NULL for default
 * \return 0 on success, -1 on error.
 *)
function out123_open(ao: pout123_handle; const driver: pchar; const device: pchar): cint; cdecl; external LIB_OUT123;

(** Give info about currently loaded driver and device
 *  Any of the return addresses can be NULL if you are not interested in
 *  everything. You get pointers to internal storage. They are valid
 *  as long as the driver/device combination is opened.
 *  The device may be NULL indicating some unnamed default.
 *  TODO: Make the driver modules return names for such defaults.
 * \param ao handle
 * \param driver return address for driver name
 * \param device return address for device name
 * \return 0 on success, -1 on error (i.e. no driver loaded)
 *)
function out123_driver_info(ao: pout123_handle; driver: ppchar; device: ppchar): cint; cdecl; external LIB_OUT123;

(** Close the current output device and driver.
 *  This implies out123_drain() to ensure no data is lost.
 *  With a buffer, that might cause considerable delay during
 *  which your main application is blocked waiting.
 *  Call out123_drop() beforehand if you want to end things
 *  quickly.
 * \param ao handle
 *)
procedure out123_close(ao: pout123_handle); cdecl; external LIB_OUT123;

(** Get supported audio encodings for given rate and channel count,
 *  for the currently openend audio device.
 *  TODO: Reopening the underlying audio device for each query
 *        is dumb, at least when dealing with JACK. It takes
 *        a long time and is just a waste. Reconsider that.
 *        Make sure that all output modules are fine with it, though!
 *  Usually, a wider range of rates is supported, but the number
 *  of sample encodings is limited, as is the number of channels.
 *  So you can call this with some standard rate and hope that the
 *  returned encodings work also for others, with the tested channel
 *  count.
 *  The return value of -1 on some encountered error conveniently also
 *  does not match any defined format (only 15 bits used for encodings,
 *  so this would even work with 16 bit integers).
 *  This implies out123_stop() to enter query mode.
 * \param ao handle
 * \param rate sampling rate
 * \param channels number of channels
 * \return supported encodings combined with bitwise or, to be checked
 *         against your favourite bitmask, -1 on error
 *)
function out123_encodings(ao: pout123_handle; rate: clong; channels: cint): cint; cdecl; external LIB_OUT123;

(** Return the size (in bytes) of one mono sample of the named encoding.
 * \param encoding The encoding value to analyze.
 * \return positive size of encoding in bytes, 0 on invalid encoding. *)
function out123_encsize(encoding: cint): cint; cdecl; external LIB_OUT123;

(** Get list of supported formats for currently opened audio device.
 *  Given a list of sampling rates and minimal/maximal channel count,
 *  this quickly checks what formats are supported with these
 *  constraints. The first entry is always reserved for a default
 *  format for the output device. If there is no such default,
 *  all values of the format are -1.
 *  For each requested combination of rate and channels, a format entry is
 *  created, possible with encoding value 0 to indicate that this combination
 *  has been tested and rejected. So, when there is no basic error, the
 *  number of returned format entries should be
 *     (ratecount*(maxchannels-minchannels+1)+1)
 *  . But instead of forcing you to guess, this will be allocated by
 *  successful run.
 *  For the first entry, the encoding member is supposed to be a definite
 *  encoding, for the others it is a bitwise combination of all possible
 *  encodings.
 *  This function is more efficient than many calls to out123_encodings().
 * \param ao handle
 * \param rates pointer to an array of sampling rates, may be NULL for none
 * \param ratecount number of provided sampling rates
 * \param minchannels minimal channel count
 * \param maxchannels maximal channel count
 * \param fmtlist return address for array of supported formats
 *        the encoding field of each entry is a combination of all
 *        supported encodings at this rate and channel count;
 *        Memory shall be freed by user.
 * \return number of returned format enries, -1 on error
 *)
function out123_formats(ao: pout123_handle; const rates: pclong; ratecount: cint; minchannels: cint; maxchannels: cint; fmtlist: ppmpg123_fmt): cint; cdecl; external LIB_OUT123;

(** Get list of encodings known to the library.
 *  You are responsible for freeing the allocated array.
 * \param enclist return address for allocated array of encoding codes
 * \return number of encodings, -1 on error
 *)
function out123_enc_list(enclist: ppcint): cint; cdecl; external LIB_OUT123;

(** Find encoding code by name.
 * \param name short or long name to find encoding code for
 * \return encoding if found (enum mpg123_enc_enum), else 0
 *)
function out123_enc_byname(const name: pchar): cint; cdecl; external LIB_OUT123;

(** Get name of encoding.
 * \param encoding code (enum mpg123_enc_enum)
 * \return short name for valid encodings, NULL otherwise
 *)
function out123_enc_name(encoding: cint): pchar; cdecl; external LIB_OUT123;

(** Get long name of encoding.
 * \param encoding code (enum mpg123_enc_enum)
 * \return long name for valid encodings, NULL otherwise
 *)
function out123_enc_longname(encoding: cint): pchar; cdecl; external LIB_OUT123;

(** Start playback with a certain output format
 *  It might be a good idea to have audio data handy to feed after this
 *  returns with success.
 *  Rationale for not taking a pointer to struct mpg123_fmt: This would
 *  always force you to deal with that type and needlessly enlarge the
 *  shortest possible program.
 * \param ao handle
 * \param encoding sample encoding (values matching libmpg123 API)
 * \param channels number of channels (1 or 2, usually)
 * \param rate sampling rate
 * \return 0 on success, negative on error (bad format, usually)
 *)
function out123_start(ao: pout123_handle; rate: clong; channels: cint; encoding: cint): cint; cdecl; external LIB_OUT123;

(** Pause playback
 *  Interrupt playback, holding any data in the optional buffer.
 *
 *  This closes the audio device if it is a live sink, ready to be re-opened
 *  by out123_continue() or out123_play() with the existing parameters.
 * \param ao handle
 *)
procedure out123_pause(ao: pout123_handle); cdecl; external LIB_OUT123;

(** Continue playback
 *  The counterpart to out123_pause(). Announce to the driver that playback
 *  shall continue.
 *
 *  Playback might not resume immediately if the optional buffer is configured
 *  to wait for a minimum fill and close to being empty. You can force playback
 *  of the last scrap with out123_drain(), or just by feeding more data with
 *  out123_play(), which will trigger out123_continue() for you, too.
 * \param ao handle
 *)
procedure out123_continue(ao: pout123_handle); cdecl; external LIB_OUT123;

(** Stop playback.
 *  This waits for pending audio data to drain to the speakers.
 *  You might want to call out123_drop() before stopping if you want
 *  to end things right away.
 * \param ao handle
 *)
procedure out123_stop(ao: pout123_handle); cdecl; external LIB_OUT123;

(** Hand over data for playback and wait in case audio device is busy.
 *  This survives non-fatal signals like SIGSTOP/SIGCONT and keeps on
 *  playing until the buffer is done with if the flag
 *  OUT123_KEEP_PLAYING ist set (default). So, per default, if
 *  you provided a byte count divisible by the PCM frame size, it is an
 *  error when less bytes than given are played.
 *  To be sure if an error occured, check out123_errcode().
 *  Also note that it is no accident that the buffer parameter is not marked
 *  as constant. Some output drivers might need to do things like swap
 *  byte order. This is done in-place instead of wasting memory on yet
 *  another copy.
 * \param ao handle
 * \param buffer pointer to raw audio data to be played
 * \param bytes number of bytes to read from the buffer
 * \return number of bytes played (might be less than given, even zero)
 *)
function out123_play(ao: pout123_handle; buffer: pointer; bytes: size_t): size_t; cdecl; external LIB_OUT123;

(** Drop any buffered data, making next provided data play right away.
 *  This does not imply an actual pause in playback.
 *  You are expected to play something, unless you called out123_pause().
 *  Feel free to call out123_stop() afterwards instead for a quicker
 *  exit than the implied out123_drain().
 *  For live sinks, this may include dropping data from their buffers.
 *  For others (files), this only concerns data in the optional buffer.
 * \param ao handle
 *)
procedure out123_drop(ao: pout123_handle); cdecl; external LIB_OUT123;

(** Drain the output, waiting until all data went to the hardware.
 * This does imply out123_continue() before and out123_pause()
 * after draining.
 * This might involve only the optional buffer process, or the
 * buffers on the audio driver side, too.
 * \param ao handle
 *)
procedure out123_drain(ao: pout123_handle); cdecl; external LIB_OUT123;

(** Drain the output, but only partially up to the given number of
 *  bytes. This gives you the opportunity to do something while
 *  the optional buffer is writing remaining data instead of having
 *  one atomic API call for it all.
 *
 *  It is wholly expected that the return value of out123_buffered()
 *  before and after calling this has a bigger difference than the
 *  provided limit, as the buffer is writing all the time in the
 *  background.
 *
 *  This is just a plain out123_drain() if the optional buffer is not
 *  in use. Also triggers out123_continue(), but only out123_pause()
 *  if there is no buffered data anymore.
 * \param ao handle
 * \param bytes limit of buffered bytes to drain
 * \return number of bytes drained from buffer
 *)
procedure out123_ndrain(ao: pout123_handle; bytes: size_t); cdecl; external LIB_OUT123;

(** Get an indication of how many bytes reside in the optional buffer.
 * This might get extended to tell the number of bytes queued up in the
 * audio backend, too.
 * \param ao handle
 * \return number of bytes in out123 library buffer
 *)
function out123_buffered(ao: pout123_handle): size_t; cdecl; external LIB_OUT123;

(** Extract currently used audio format from handle.
 *  matching mpg123_getformat().
 *  Given return addresses may be NULL to indicate no interest.
 * \param ao handle
 * \param rate address for sample rate
 * \param channels address for channel count
 * \param encoding address for encoding
 * \param framesize size of a full PCM frame (for convenience)
 * \return 0 on success, -1 on error
 *)
function out123_getformat(ao: pout123_handle; rate: pclong; channels: pcint; encoding: pcint; framesize: pcint): cint; cdecl; external LIB_OUT123;

(* @} *)


implementation

function out123_param_int(ao: pout123_handle; code: out123_parms; value: clong): cint; cdecl; inline;
begin
  Result := out123_param(ao, code, value, 0.0, nil);
end;

function out123_param_float(ao: pout123_handle; code: out123_parms; value: cdouble): cint; cdecl; inline;
begin
  Result := out123_param(ao, code, 0, value, nil);
end;

function out123_param_string(ao: pout123_handle; code: out123_parms; value: pchar): cint; cdecl; inline;
begin
  Result := out123_param(ao, code, 0, 0.0, value);
end;

function out123_getparam_int(ao: pout123_handle; code: out123_parms; value: pclong): cint; cdecl; inline;
begin
  Result := out123_getparam(ao, code, value, nil, nil);
end;

function out123_getparam_float(ao: pout123_handle; code: out123_parms; value: pcdouble): cint; cdecl; inline;
begin
  Result := out123_getparam(ao, code, nil, value, nil);
end;

function out123_getparam_string(ao: pout123_handle; code: out123_parms; value: ppchar): cint; cdecl; inline;
begin
  Result := out123_getparam(ao, code, nil, nil, value);
end;


end.

