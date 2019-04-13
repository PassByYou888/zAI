{ ****************************************************************************** }
{ * sound engine with BASS api                                                 * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }
unit Bass;

{$IFDEF FPC}

{$MODE Delphi}
{$MODESWITCH AdvancedRecords}
{$NOTES OFF}

{$ASMMODE intel}

{$IFDEF FPC_BIG_ENDIAN}
{$DEFINE BIG_ENDIAN}
{$ENDIF}

{$UNDEF Delphi}
{$UNDEF FastMD5}
{$DEFINE parallel}
{$DEFINE OptimizationMemoryStreamMD5}
{$DEFINE CriticalSimulateAtomic}

{$ENDIF}

{$WARNINGS OFF}
{$HINTS OFF}
{$H+} { long string }
{$R-} { range check }
{$A+} { Word Align Data }
{$O-} { optimization }

interface

uses
  {$IFDEF MSWINDOWS}
  Windows
  {$ELSE}
    SysUtils
  {$ENDIF};

type
  {$IFDEF NEXTGEN}
  PByteChar = MarshaledAString;
  TByteChar = Byte;
  {$ELSE}
  PByteChar = PAnsiChar;
  TByteChar = AnsiChar;
  {$ENDIF}


const
  BASSVERSION     = $204; // API version
  BASSVERSIONTEXT = '2.4';

  // Use these to test for error from functions that return a DWORD or QWORD
  DW_ERROR = Cardinal(-1); // -1 (DWORD)
  QW_ERROR = Int64(-1);    // -1 (QWORD)

  // Error codes returned by BASS_ErrorGetCode()
  BASS_OK             = 0;  // all is OK
  BASS_ERROR_MEM      = 1;  // memory error
  BASS_ERROR_FILEOPEN = 2;  // can't open the file
  BASS_ERROR_DRIVER   = 3;  // can't find a free sound driver
  BASS_ERROR_BUFLOST  = 4;  // the sample buffer was lost
  BASS_ERROR_HANDLE   = 5;  // invalid handle
  BASS_ERROR_FORMAT   = 6;  // unsupported sample format
  BASS_ERROR_POSITION = 7;  // invalid position
  BASS_ERROR_INIT     = 8;  // BASS_Init has not been successfully called
  BASS_ERROR_START    = 9;  // BASS_Start has not been successfully called
  BASS_ERROR_ALREADY  = 14; // already initialized/paused/whatever
  BASS_ERROR_NOCHAN   = 18; // can't get a free channel
  BASS_ERROR_ILLTYPE  = 19; // an illegal type was specified
  BASS_ERROR_ILLPARAM = 20; // an illegal parameter was specified
  BASS_ERROR_NO3D     = 21; // no 3D support
  BASS_ERROR_NOEAX    = 22; // no EAX support
  BASS_ERROR_DEVICE   = 23; // illegal device number
  BASS_ERROR_NOPLAY   = 24; // not playing
  BASS_ERROR_FREQ     = 25; // illegal sample rate
  BASS_ERROR_NOTFILE  = 27; // the stream is not a file stream
  BASS_ERROR_NOHW     = 29; // no hardware voices available
  BASS_ERROR_EMPTY    = 31; // the MOD music has no sequence data
  BASS_ERROR_NONET    = 32; // no internet connection could be opened
  BASS_ERROR_CREATE   = 33; // couldn't create the file
  BASS_ERROR_NOFX     = 34; // effects are not enabled
  BASS_ERROR_NOTAVAIL = 37; // requested data is not available
  BASS_ERROR_DECODE   = 38; // the channel is a "decoding channel"
  BASS_ERROR_DX       = 39; // a sufficient DirectX version is not installed
  BASS_ERROR_TIMEOUT  = 40; // connection timedout
  BASS_ERROR_FILEFORM = 41; // unsupported file format
  BASS_ERROR_SPEAKER  = 42; // unavailable speaker
  BASS_ERROR_VERSION  = 43; // invalid BASS version (used by add-ons)
  BASS_ERROR_CODEC    = 44; // codec is not available/supported
  BASS_ERROR_ENDED    = 45; // the channel/file has ended
  BASS_ERROR_BUSY     = 46; // the device is busy
  BASS_ERROR_UNKNOWN  = -1; // some other mystery problem

  // BASS_SetConfig options
  BASS_CONFIG_BUFFER           = 0;
  BASS_CONFIG_UPDATEPERIOD     = 1;
  BASS_CONFIG_GVOL_SAMPLE      = 4;
  BASS_CONFIG_GVOL_STREAM      = 5;
  BASS_CONFIG_GVOL_MUSIC       = 6;
  BASS_CONFIG_CURVE_VOL        = 7;
  BASS_CONFIG_CURVE_PAN        = 8;
  BASS_CONFIG_FLOATDSP         = 9;
  BASS_CONFIG_3DALGORITHM      = 10;
  BASS_CONFIG_NET_TIMEOUT      = 11;
  BASS_CONFIG_NET_BUFFER       = 12;
  BASS_CONFIG_PAUSE_NOPLAY     = 13;
  BASS_CONFIG_NET_PREBUF       = 15;
  BASS_CONFIG_NET_PASSIVE      = 18;
  BASS_CONFIG_REC_BUFFER       = 19;
  BASS_CONFIG_NET_PLAYLIST     = 21;
  BASS_CONFIG_MUSIC_VIRTUAL    = 22;
  BASS_CONFIG_VERIFY           = 23;
  BASS_CONFIG_UPDATETHREADS    = 24;
  BASS_CONFIG_DEV_BUFFER       = 27;
  BASS_CONFIG_VISTA_TRUEPOS    = 30;
  BASS_CONFIG_IOS_MIXAUDIO     = 34;
  BASS_CONFIG_DEV_DEFAULT      = 36;
  BASS_CONFIG_NET_READTIMEOUT  = 37;
  BASS_CONFIG_VISTA_SPEAKERS   = 38;
  BASS_CONFIG_IOS_SPEAKER      = 39;
  BASS_CONFIG_HANDLES          = 41;
  BASS_CONFIG_UNICODE          = 42;
  BASS_CONFIG_SRC              = 43;
  BASS_CONFIG_SRC_SAMPLE       = 44;
  BASS_CONFIG_ASYNCFILE_BUFFER = 45;
  BASS_CONFIG_OGG_PRESCAN      = 47;

  // BASS_SetConfigPtr options
  BASS_CONFIG_NET_AGENT = 16;
  BASS_CONFIG_NET_PROXY = 17;

  // BASS_Init flags
  BASS_DEVICE_8BITS      = 1;     // 8 bit resolution, else 16 bit
  BASS_DEVICE_MONO       = 2;     // mono, else stereo
  BASS_DEVICE_3D         = 4;     // enable 3D functionality
  BASS_DEVICE_LATENCY    = $100;  // calculate device latency (BASS_INFO struct)
  BASS_DEVICE_CPSPEAKERS = $400;  // detect speakers via Windows control panel
  BASS_DEVICE_SPEAKERS   = $800;  // force enabling of speaker assignment
  BASS_DEVICE_NOSPEAKER  = $1000; // ignore speaker arrangement
  BASS_DEVICE_DMIX       = $2000; // use ALSA "dmix" plugin
  BASS_DEVICE_FREQ       = $4000; // set device sample rate

  // DirectSound interfaces (for use with BASS_GetDSoundObject)
  BASS_OBJECT_DS    = 1; // IDirectSound
  BASS_OBJECT_DS3DL = 2; // IDirectSound3DListener

  // BASS_DEVICEINFO flags
  BASS_DEVICE_ENABLED = 1;
  BASS_DEVICE_DEFAULT = 2;
  BASS_DEVICE_INIT    = 4;

  // BASS_INFO flags (from DSOUND.H)
  DSCAPS_CONTINUOUSRATE = $00000010;
  // supports all sample rates between min/maxrate
  DSCAPS_EMULDRIVER = $00000020;
  // device does NOT have hardware DirectSound support
  DSCAPS_CERTIFIED       = $00000040; // device driver has been certified by Microsoft
  DSCAPS_SECONDARYMONO   = $00000100; // mono
  DSCAPS_SECONDARYSTEREO = $00000200; // stereo
  DSCAPS_SECONDARY8BIT   = $00000400; // 8 bit
  DSCAPS_SECONDARY16BIT  = $00000800; // 16 bit

  // BASS_RECORDINFO flags (from DSOUND.H)
  DSCCAPS_EMULDRIVER = DSCAPS_EMULDRIVER;
  // device does NOT have hardware DirectSound recording support
  DSCCAPS_CERTIFIED = DSCAPS_CERTIFIED;
  // device driver has been certified by Microsoft

  // defines for formats field of BASS_RECORDINFO (from MMSYSTEM.H)
  WAVE_FORMAT_1M08 = $00000001; // 11.025 kHz, Mono,   8-bit
  WAVE_FORMAT_1S08 = $00000002; // 11.025 kHz, Stereo, 8-bit
  WAVE_FORMAT_1M16 = $00000004; // 11.025 kHz, Mono,   16-bit
  WAVE_FORMAT_1S16 = $00000008; // 11.025 kHz, Stereo, 16-bit
  WAVE_FORMAT_2M08 = $00000010; // 22.05  kHz, Mono,   8-bit
  WAVE_FORMAT_2S08 = $00000020; // 22.05  kHz, Stereo, 8-bit
  WAVE_FORMAT_2M16 = $00000040; // 22.05  kHz, Mono,   16-bit
  WAVE_FORMAT_2S16 = $00000080; // 22.05  kHz, Stereo, 16-bit
  WAVE_FORMAT_4M08 = $00000100; // 44.1   kHz, Mono,   8-bit
  WAVE_FORMAT_4S08 = $00000200; // 44.1   kHz, Stereo, 8-bit
  WAVE_FORMAT_4M16 = $00000400; // 44.1   kHz, Mono,   16-bit
  WAVE_FORMAT_4S16 = $00000800; // 44.1   kHz, Stereo, 16-bit

  BASS_SAMPLE_8BITS     = 1;      // 8 bit
  BASS_SAMPLE_FLOAT     = 256;    // 32-bit floating-point
  BASS_SAMPLE_MONO      = 2;      // mono
  BASS_SAMPLE_LOOP      = 4;      // looped
  BASS_SAMPLE_3D        = 8;      // 3D functionality
  BASS_SAMPLE_SOFTWARE  = 16;     // not using hardware mixing
  BASS_SAMPLE_MUTEMAX   = 32;     // mute at max distance (3D only)
  BASS_SAMPLE_VAM       = 64;     // DX7 voice allocation & management
  BASS_SAMPLE_FX        = 128;    // old implementation of DX8 effects
  BASS_SAMPLE_OVER_VOL  = $10000; // override lowest volume
  BASS_SAMPLE_OVER_POS  = $20000; // override longest playing
  BASS_SAMPLE_OVER_DIST = $30000; // override furthest from listener (3D only)

  BASS_STREAM_PRESCAN  = $20000; // enable pin-point seeking/length (MP3/MP2/MP1)
  BASS_MP3_SETPOS      = BASS_STREAM_PRESCAN;
  BASS_STREAM_AUTOFREE = $40000;
  // automatically free the stream when it stop/ends
  BASS_STREAM_RESTRATE = $80000;
  // restrict the download rate of internet file streams
  BASS_STREAM_BLOCK = $100000;
  // download/play internet file stream in small blocks
  BASS_STREAM_DECODE = $200000;
  // don't play the stream, only decode (BASS_ChannelGetData)
  BASS_STREAM_STATUS = $800000;
  // give server status info (HTTP/ICY tags) in DOWNLOADPROC

  BASS_MUSIC_FLOAT      = BASS_SAMPLE_FLOAT;
  BASS_MUSIC_MONO       = BASS_SAMPLE_MONO;
  BASS_MUSIC_LOOP       = BASS_SAMPLE_LOOP;
  BASS_MUSIC_3D         = BASS_SAMPLE_3D;
  BASS_MUSIC_FX         = BASS_SAMPLE_FX;
  BASS_MUSIC_AUTOFREE   = BASS_STREAM_AUTOFREE;
  BASS_MUSIC_DECODE     = BASS_STREAM_DECODE;
  BASS_MUSIC_PRESCAN    = BASS_STREAM_PRESCAN; // calculate playback length
  BASS_MUSIC_CALCLEN    = BASS_MUSIC_PRESCAN;
  BASS_MUSIC_RAMP       = $200;    // normal ramping
  BASS_MUSIC_RAMPS      = $400;    // sensitive ramping
  BASS_MUSIC_SURROUND   = $800;    // surround sound
  BASS_MUSIC_SURROUND2  = $1000;   // surround sound (mode 2)
  BASS_MUSIC_FT2MOD     = $2000;   // play .MOD as FastTracker 2 does
  BASS_MUSIC_PT1MOD     = $4000;   // play .MOD as ProTracker 1 does
  BASS_MUSIC_NONINTER   = $10000;  // non-interpolated sample mixing
  BASS_MUSIC_SINCINTER  = $800000; // sinc interpolated sample mixing
  BASS_MUSIC_POSRESET   = $8000;   // stop all notes when moving position
  BASS_MUSIC_POSRESETEX = $400000;
  // stop all notes and reset bmp/etc when moving position
  BASS_MUSIC_STOPBACK = $80000;  // stop the music on a backwards jump effect
  BASS_MUSIC_NOSAMPLE = $100000; // don't load the samples

  // Speaker assignment flags
  BASS_SPEAKER_FRONT      = $1000000;  // front speakers
  BASS_SPEAKER_REAR       = $2000000;  // rear/side speakers
  BASS_SPEAKER_CENLFE     = $3000000;  // center & LFE speakers (5.1)
  BASS_SPEAKER_REAR2      = $4000000;  // rear center speakers (7.1)
  BASS_SPEAKER_LEFT       = $10000000; // modifier: left
  BASS_SPEAKER_RIGHT      = $20000000; // modifier: right
  BASS_SPEAKER_FRONTLEFT  = BASS_SPEAKER_FRONT or BASS_SPEAKER_LEFT;
  BASS_SPEAKER_FRONTRIGHT = BASS_SPEAKER_FRONT or BASS_SPEAKER_RIGHT;
  BASS_SPEAKER_REARLEFT   = BASS_SPEAKER_REAR or BASS_SPEAKER_LEFT;
  BASS_SPEAKER_REARRIGHT  = BASS_SPEAKER_REAR or BASS_SPEAKER_RIGHT;
  BASS_SPEAKER_CENTER     = BASS_SPEAKER_CENLFE or BASS_SPEAKER_LEFT;
  BASS_SPEAKER_LFE        = BASS_SPEAKER_CENLFE or BASS_SPEAKER_RIGHT;
  BASS_SPEAKER_REAR2LEFT  = BASS_SPEAKER_REAR2 or BASS_SPEAKER_LEFT;
  BASS_SPEAKER_REAR2RIGHT = BASS_SPEAKER_REAR2 or BASS_SPEAKER_RIGHT;

  BASS_ASYNCFILE = $40000000;
  BASS_UNICODE   = $80000000;

  BASS_RECORD_PAUSE = $8000; // start recording paused

  // DX7 voice allocation & management flags
  BASS_VAM_HARDWARE  = 1;
  BASS_VAM_SOFTWARE  = 2;
  BASS_VAM_TERM_TIME = 4;
  BASS_VAM_TERM_DIST = 8;
  BASS_VAM_TERM_PRIO = 16;

  // BASS_CHANNELINFO types
  BASS_CTYPE_SAMPLE           = 1;
  BASS_CTYPE_RECORD           = 2;
  BASS_CTYPE_STREAM           = $10000;
  BASS_CTYPE_STREAM_OGG       = $10002;
  BASS_CTYPE_STREAM_MP1       = $10003;
  BASS_CTYPE_STREAM_MP2       = $10004;
  BASS_CTYPE_STREAM_MP3       = $10005;
  BASS_CTYPE_STREAM_AIFF      = $10006;
  BASS_CTYPE_STREAM_WAV       = $40000; // WAVE flag, LOWORD=codec
  BASS_CTYPE_STREAM_WAV_PCM   = $50001;
  BASS_CTYPE_STREAM_WAV_FLOAT = $50003;
  BASS_CTYPE_MUSIC_MOD        = $20000;
  BASS_CTYPE_MUSIC_MTM        = $20001;
  BASS_CTYPE_MUSIC_S3M        = $20002;
  BASS_CTYPE_MUSIC_XM         = $20003;
  BASS_CTYPE_MUSIC_IT         = $20004;
  BASS_CTYPE_MUSIC_MO3        = $00100; // MO3 flag

  // 3D channel modes
  BASS_3DMODE_NORMAL   = 0; // normal 3D processing
  BASS_3DMODE_RELATIVE = 1; // position is relative to the listener
  BASS_3DMODE_OFF      = 2; // no 3D processing

  // software 3D mixing algorithms (used with BASS_CONFIG_3DALGORITHM)
  BASS_3DALG_DEFAULT = 0;
  BASS_3DALG_OFF     = 1;
  BASS_3DALG_FULL    = 2;
  BASS_3DALG_LIGHT   = 3;

  // EAX environments, use with BASS_SetEAXParameters
  EAX_ENVIRONMENT_GENERIC         = 0;
  EAX_ENVIRONMENT_PADDEDCELL      = 1;
  EAX_ENVIRONMENT_ROOM            = 2;
  EAX_ENVIRONMENT_BATHROOM        = 3;
  EAX_ENVIRONMENT_LIVINGROOM      = 4;
  EAX_ENVIRONMENT_STONEROOM       = 5;
  EAX_ENVIRONMENT_AUDITORIUM      = 6;
  EAX_ENVIRONMENT_CONCERTHALL     = 7;
  EAX_ENVIRONMENT_CAVE            = 8;
  EAX_ENVIRONMENT_ARENA           = 9;
  EAX_ENVIRONMENT_HANGAR          = 10;
  EAX_ENVIRONMENT_CARPETEDHALLWAY = 11;
  EAX_ENVIRONMENT_HALLWAY         = 12;
  EAX_ENVIRONMENT_STONECORRIDOR   = 13;
  EAX_ENVIRONMENT_ALLEY           = 14;
  EAX_ENVIRONMENT_FOREST          = 15;
  EAX_ENVIRONMENT_CITY            = 16;
  EAX_ENVIRONMENT_MOUNTAINS       = 17;
  EAX_ENVIRONMENT_QUARRY          = 18;
  EAX_ENVIRONMENT_PLAIN           = 19;
  EAX_ENVIRONMENT_PARKINGLOT      = 20;
  EAX_ENVIRONMENT_SEWERPIPE       = 21;
  EAX_ENVIRONMENT_UNDERWATER      = 22;
  EAX_ENVIRONMENT_DRUGGED         = 23;
  EAX_ENVIRONMENT_DIZZY           = 24;
  EAX_ENVIRONMENT_PSYCHOTIC       = 25;
  // total number of environments
  EAX_ENVIRONMENT_COUNT = 26;

  BASS_STREAMPROC_END = $80000000; // end of user stream flag

  // BASS_StreamCreateFileUser file systems
  STREAMFILE_NOBUFFER   = 0;
  STREAMFILE_BUFFER     = 1;
  STREAMFILE_BUFFERPUSH = 2;

  // BASS_StreamPutFileData options
  BASS_FILEDATA_END = 0; // end & close the file

  // BASS_StreamGetFilePosition modes
  BASS_FILEPOS_CURRENT   = 0;
  BASS_FILEPOS_DECODE    = BASS_FILEPOS_CURRENT;
  BASS_FILEPOS_DOWNLOAD  = 1;
  BASS_FILEPOS_END       = 2;
  BASS_FILEPOS_START     = 3;
  BASS_FILEPOS_CONNECTED = 4;
  BASS_FILEPOS_BUFFER    = 5;

  // BASS_ChannelSetSync types
  BASS_SYNC_POS        = 0;
  BASS_SYNC_END        = 2;
  BASS_SYNC_META       = 4;
  BASS_SYNC_SLIDE      = 5;
  BASS_SYNC_STALL      = 6;
  BASS_SYNC_DOWNLOAD   = 7;
  BASS_SYNC_FREE       = 8;
  BASS_SYNC_SETPOS     = 11;
  BASS_SYNC_MUSICPOS   = 10;
  BASS_SYNC_MUSICINST  = 1;
  BASS_SYNC_MUSICFX    = 3;
  BASS_SYNC_OGG_CHANGE = 12;
  BASS_SYNC_MIXTIME    = $40000000; // FLAG: sync at mixtime, else at playtime
  BASS_SYNC_ONETIME    = $80000000; // FLAG: sync only once, else continuously

  // BASS_ChannelIsActive return values
  BASS_ACTIVE_STOPPED = 0;
  BASS_ACTIVE_PLAYING = 1;
  BASS_ACTIVE_STALLED = 2;
  BASS_ACTIVE_PAUSED  = 3;

  // Channel attributes
  BASS_ATTRIB_FREQ             = 1;
  BASS_ATTRIB_VOL              = 2;
  BASS_ATTRIB_PAN              = 3;
  BASS_ATTRIB_EAXMIX           = 4;
  BASS_ATTRIB_NOBUFFER         = 5;
  BASS_ATTRIB_CPU              = 7;
  BASS_ATTRIB_SRC              = 8;
  BASS_ATTRIB_MUSIC_AMPLIFY    = $100;
  BASS_ATTRIB_MUSIC_PANSEP     = $101;
  BASS_ATTRIB_MUSIC_PSCALER    = $102;
  BASS_ATTRIB_MUSIC_BPM        = $103;
  BASS_ATTRIB_MUSIC_SPEED      = $104;
  BASS_ATTRIB_MUSIC_VOL_GLOBAL = $105;
  BASS_ATTRIB_MUSIC_VOL_CHAN   = $200; // + channel #
  BASS_ATTRIB_MUSIC_VOL_INST   = $300; // + instrument #

  // BASS_ChannelGetData flags
  BASS_DATA_AVAILABLE      = 0;         // query how much data is buffered
  BASS_DATA_FLOAT          = $40000000; // flag: return floating-point sample data
  BASS_DATA_FFT256         = $80000000; // 256 sample FFT
  BASS_DATA_FFT512         = $80000001; // 512 FFT
  BASS_DATA_FFT1024        = $80000002; // 1024 FFT
  BASS_DATA_FFT2048        = $80000003; // 2048 FFT
  BASS_DATA_FFT4096        = $80000004; // 4096 FFT
  BASS_DATA_FFT8192        = $80000005; // 8192 FFT
  BASS_DATA_FFT16384       = $80000006; // 16384 FFT
  BASS_DATA_FFT_INDIVIDUAL = $10;
  // FFT flag: FFT for each channel, else all combined
  BASS_DATA_FFT_NOWINDOW = $20; // FFT flag: no Hanning window
  BASS_DATA_FFT_REMOVEDC = $40; // FFT flag: pre-remove DC bias
  BASS_DATA_FFT_COMPLEX  = $80; // FFT flag: return complex data

  // BASS_ChannelGetTags types : what's returned
  BASS_TAG_ID3   = 0; // ID3v1 tags : TAG_ID3 structure
  BASS_TAG_ID3V2 = 1; // ID3v2 tags : variable length block
  BASS_TAG_OGG   = 2; // OGG comments : series of null-terminated UTF-8 strings
  BASS_TAG_HTTP  = 3; // HTTP headers : series of null-terminated ANSI strings
  BASS_TAG_ICY   = 4; // ICY headers : series of null-terminated ANSI strings
  BASS_TAG_META  = 5; // ICY metadata : ANSI string
  BASS_TAG_APE   = 6; // APEv2 tags : series of null-terminated UTF-8 strings
  BASS_TAG_MP4   = 7;
  // MP4/iTunes metadata : series of null-terminated UTF-8 strings
  BASS_TAG_VENDOR   = 9;  // OGG encoder : UTF-8 string
  BASS_TAG_LYRICS3  = 10; // Lyric3v2 tag : ASCII string
  BASS_TAG_CA_CODEC = 11; // CoreAudio codec info : TAG_CA_CODEC structure
  BASS_TAG_MF       = 13;
  // Media Foundation tags : series of null-terminated UTF-8 strings
  BASS_TAG_WAVEFORMAT = 14; // WAVE format : WAVEFORMATEEX structure
  BASS_TAG_RIFF_INFO  = $100;
  // RIFF "INFO" tags : series of null-terminated ANSI strings
  BASS_TAG_RIFF_BEXT  = $101; // RIFF/BWF "bext" tags : TAG_BEXT structure
  BASS_TAG_RIFF_CART  = $102; // RIFF/BWF "cart" tags : TAG_CART structure
  BASS_TAG_RIFF_DISP  = $103; // RIFF "DISP" text tag : ANSI string
  BASS_TAG_APE_BINARY = $1000;
  // + index #, binary APEv2 tag : TAG_APE_BINARY structure
  BASS_TAG_MUSIC_NAME    = $10000; // MOD music name : ANSI string
  BASS_TAG_MUSIC_MESSAGE = $10001; // MOD message : ANSI string
  BASS_TAG_MUSIC_ORDERS  = $10002;
  // MOD order list : BYTE array of pattern numbers
  BASS_TAG_MUSIC_INST = $10100;
  // + instrument #, MOD instrument name : ANSI string
  BASS_TAG_MUSIC_SAMPLE = $10300; // + sample #, MOD sample name : ANSI string

  // BASS_ChannelGetLength/GetPosition/SetPosition modes
  BASS_POS_BYTE        = 0;         // byte position
  BASS_POS_MUSIC_ORDER = 1;         // order.row position, MAKELONG(order,row)
  BASS_POS_OGG         = 3;         // OGG bitstream number
  BASS_POS_DECODE      = $10000000; // flag: get the decoding (not playing) position
  BASS_POS_DECODETO    = $20000000;
  // flag: decode to the position instead of seeking

  // BASS_RecordSetInput flags
  BASS_INPUT_OFF = $10000;
  BASS_INPUT_ON  = $20000;

  BASS_INPUT_TYPE_MASK    = $FF000000;
  BASS_INPUT_TYPE_UNDEF   = $00000000;
  BASS_INPUT_TYPE_DIGITAL = $01000000;
  BASS_INPUT_TYPE_LINE    = $02000000;
  BASS_INPUT_TYPE_MIC     = $03000000;
  BASS_INPUT_TYPE_SYNTH   = $04000000;
  BASS_INPUT_TYPE_CD      = $05000000;
  BASS_INPUT_TYPE_PHONE   = $06000000;
  BASS_INPUT_TYPE_SPEAKER = $07000000;
  BASS_INPUT_TYPE_WAVE    = $08000000;
  BASS_INPUT_TYPE_AUX     = $09000000;
  BASS_INPUT_TYPE_ANALOG  = $0A000000;

  BASS_FX_DX8_CHORUS      = 0;
  BASS_FX_DX8_COMPRESSOR  = 1;
  BASS_FX_DX8_DISTORTION  = 2;
  BASS_FX_DX8_ECHO        = 3;
  BASS_FX_DX8_FLANGER     = 4;
  BASS_FX_DX8_GARGLE      = 5;
  BASS_FX_DX8_I3DL2REVERB = 6;
  BASS_FX_DX8_PARAMEQ     = 7;
  BASS_FX_DX8_REVERB      = 8;

  BASS_DX8_PHASE_NEG_180 = 0;
  BASS_DX8_PHASE_NEG_90  = 1;
  BASS_DX8_PHASE_ZERO    = 2;
  BASS_DX8_PHASE_90      = 3;
  BASS_DX8_PHASE_180     = 4;

type
  DWord = Cardinal;
  BOOL  = LongBool;
  FLOAT = Single;
  qword = Int64;

  HMUSIC   = DWord; // MOD music handle
  HSAMPLE  = DWord; // sample handle
  HCHANNEL = DWord; // playing sample's channel handle
  HSTREAM  = DWord; // sample stream handle
  HRECORD  = DWord; // recording handle
  HSYNC    = DWord; // synchronizer handle
  HDSP     = DWord; // DSP handle
  HFX      = DWord; // DX8 effect handle
  HPLUGIN  = DWord; // Plugin handle

  // Device info structure
  BASS_DEVICEINFO = packed record
    Name: PByteChar;   // description
    driver: PByteChar; // driver
    Flags: DWord;
  end;

  BASS_INFO = packed record
    Flags: DWord;   // device capabilities (DSCAPS_xxx flags)
    hwsize: DWord;  // size of total device hardware memory
    hwfree: DWord;  // size of free device hardware memory
    freesam: DWord; // number of free sample slots in the hardware
    free3d: DWord;  // number of free 3D sample slots in the hardware
    minrate: DWord; // min sample rate supported by the hardware
    maxrate: DWord; // max sample rate supported by the hardware
    eax: BOOL;
    // device supports EAX? (always FALSE if BASS_DEVICE_3D was not used)
    minbuf: DWord;
    // recommended minimum buffer length in ms (requires BASS_DEVICE_LATENCY)
    dsver: DWord; // DirectSound version
    latency: DWord;
    // delay (in ms) before start of playback (requires BASS_DEVICE_LATENCY)
    InitFlags: DWord; // BASS_Init "flags" parameter
    speakers: DWord;  // number of speakers available
    freq: DWord;      // current output rate
  end;

  // Recording device info structure
  BASS_RECORDINFO = packed record
    Flags: DWord;   // device capabilities (DSCCAPS_xxx flags)
    formats: DWord; // supported standard formats (WAVE_FORMAT_xxx flags)
    inputs: DWord;  // number of inputs
    singlein: BOOL; // only 1 input can be set at a time
    freq: DWord;    // current input rate
  end;

  // Sample info structure
  BASS_SAMPLE = packed record
    freq: DWord;     // default playback rate
    volume: FLOAT;   // default volume (0-100)
    pan: FLOAT;      // default pan (-100=left, 0=middle, 100=right)
    Flags: DWord;    // BASS_SAMPLE_xxx flags
    length: DWord;   // length (in samples, not bytes)
    Max: DWord;      // maximum simultaneous playbacks
    origres: DWord;  // original resolution
    chans: DWord;    // number of channels
    mingap: DWord;   // minimum gap (ms) between creating channels
    mode3d: DWord;   // BASS_3DMODE_xxx mode
    mindist: FLOAT;  // minimum distance
    MaxDist: FLOAT;  // maximum distance
    iangle: DWord;   // angle of inside projection cone
    oangle: DWord;   // angle of outside projection cone
    outvol: FLOAT;   // delta-volume outside the projection cone
    vam: DWord;      // voice allocation/management flags (BASS_VAM_xxx)
    Priority: DWord; // priority (0=lowest, $ffffffff=highest)
  end;

  // Channel info structure
  BASS_CHANNELINFO = packed record
    freq: DWord;     // default playback rate
    chans: DWord;    // channels
    Flags: DWord;    // BASS_SAMPLE/STREAM/MUSIC/SPEAKER flags
    ctype: DWord;    // type of channel
    origres: DWord;  // original resolution
    plugin: HPLUGIN; // plugin
    sample: HSAMPLE; // sample
    {$IFDEF CPUX64}
    Padding: DWord;
    {$ENDIF}
    FileName: PChar; // filename
  end;

  BASS_PLUGINFORM = packed record
    ctype: DWord; // channel type
    {$IFDEF CPUX64}
    Padding: DWord;
    {$ENDIF}
    Name: PByteChar; // format description
    exts: PByteChar; // file extension filter (*.ext1;*.ext2;etc...)
  end;

  PBASS_PLUGINFORMS = ^TBASS_PLUGINFORMS;
  TBASS_PLUGINFORMS = array [0 .. MaxInt div SizeOf(BASS_PLUGINFORM) - 1]
    of BASS_PLUGINFORM;

  BASS_PLUGININFO = packed record
    version: DWord;             // version (same form as BASS_GetVersion)
    formatc: DWord;             // number of formats
    formats: PBASS_PLUGINFORMS; // the array of formats
  end;

  PBASS_PLUGININFO = ^BASS_PLUGININFO;

  // 3D vector (for 3D positions/velocities/orientations)
  BASS_3DVECTOR = packed record
    x: FLOAT; // +=right, -=left
    y: FLOAT; // +=up, -=down
    z: FLOAT; // +=front, -=behind
  end;

  // User file stream callback functions
  FILECLOSEPROC = procedure(user: Pointer); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  FILELENPROC   = function(user: Pointer)                                 : qword; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  FILEREADPROC  = function(buffer: Pointer; length: DWord; user: Pointer): DWord;
  {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  FILESEEKPROC = function(Offset: qword; user: Pointer): BOOL;
  {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  BASS_FILEPROCS = packed record
    Close: FILECLOSEPROC;
    length: FILELENPROC;
    read: FILEREADPROC;
    Seek: FILESEEKPROC;
  end;

  // ID3v1 tag structure
  TAG_ID3 = packed record
    ID: array [0 .. 2] of TByteChar;
    Title: array [0 .. 29] of TByteChar;
    artist: array [0 .. 29] of TByteChar;
    album: array [0 .. 29] of TByteChar;
    Year: array [0 .. 3] of TByteChar;
    Comment: array [0 .. 29] of TByteChar;
    genre: Byte;
  end;

  // Binary APEv2 tag structure
  TAG_APE_BINARY = packed record
    key: PByteChar;
    Data: PByteChar;
    length: DWord;
  end;

  // BWF "bext" tag structure
  TAG_BEXT = packed record
    Description: array [0 .. 255] of TByteChar; // description
    Originator: array [0 .. 31] of TByteChar;   // name of the originator
    OriginatorReference: array [0 .. 31] of TByteChar;
    // reference of the originator
    OriginationDate: array [0 .. 9] of TByteChar;
    // date of creation (yyyy-mm-dd)
    OriginationTime: array [0 .. 7] of TByteChar; // time of creation (hh-mm-ss)
    TimeReference: qword;                         // first sample count since midnight (little-endian)
    version: Word;                                // BWF version (little-endian)
    UMID: array [0 .. 63] of Byte;                // SMPTE UMID
    Reserved: array [0 .. 189] of Byte;
    CodingHistory: array of TByteChar; // history
  end;

  BASS_DX8_CHORUS = packed record
    fWetDryMix: FLOAT;
    fDepth: FLOAT;
    fFeedback: FLOAT;
    FFrequency: FLOAT;
    lWaveform: DWord; // 0=triangle, 1=sine
    fDelay: FLOAT;
    lPhase: DWord; // BASS_DX8_PHASE_xxx
  end;

  BASS_DX8_COMPRESSOR = packed record
    fGain: FLOAT;
    fAttack: FLOAT;
    fRelease: FLOAT;
    FThreshold: FLOAT;
    fRatio: FLOAT;
    fPredelay: FLOAT;
  end;

  BASS_DX8_DISTORTION = packed record
    fGain: FLOAT;
    fEdge: FLOAT;
    fPostEQCenterFrequency: FLOAT;
    fPostEQBandwidth: FLOAT;
    fPreLowpassCutoff: FLOAT;
  end;

  BASS_DX8_ECHO = packed record
    fWetDryMix: FLOAT;
    fFeedback: FLOAT;
    fLeftDelay: FLOAT;
    fRightDelay: FLOAT;
    lPanDelay: BOOL;
  end;

  BASS_DX8_FLANGER = packed record
    fWetDryMix: FLOAT;
    fDepth: FLOAT;
    fFeedback: FLOAT;
    FFrequency: FLOAT;
    lWaveform: DWord; // 0=triangle, 1=sine
    fDelay: FLOAT;
    lPhase: DWord; // BASS_DX8_PHASE_xxx
  end;

  BASS_DX8_GARGLE = packed record
    dwRateHz: DWord;    // Rate of modulation in hz
    dwWaveShape: DWord; // 0=triangle, 1=square
  end;

  BASS_DX8_I3DL2REVERB = packed record
    lRoom: Integer;             // [-10000, 0]      default: -1000 mB
    lRoomHF: Integer;           // [-10000, 0]      default: 0 mB
    flRoomRolloffFactor: FLOAT; // [0.0, 10.0]      default: 0.0
    flDecayTime: FLOAT;         // [0.1, 20.0]      default: 1.49s
    flDecayHFRatio: FLOAT;      // [0.1, 2.0]       default: 0.83
    lReflections: Integer;      // [-10000, 1000]   default: -2602 mB
    flReflectionsDelay: FLOAT;  // [0.0, 0.3]       default: 0.007 s
    lReverb: Integer;           // [-10000, 2000]   default: 200 mB
    flReverbDelay: FLOAT;       // [0.0, 0.1]       default: 0.011 s
    flDiffusion: FLOAT;         // [0.0, 100.0]     default: 100.0 %
    flDensity: FLOAT;           // [0.0, 100.0]     default: 100.0 %
    flHFReference: FLOAT;       // [20.0, 20000.0]  default: 5000.0 Hz
  end;

  BASS_DX8_PARAMEQ = packed record
    FCenter: FLOAT;
    fBandwidth: FLOAT;
    fGain: FLOAT;
  end;

  BASS_DX8_REVERB = packed record
    fInGain: FLOAT;          // [-96.0,0.0]            default: 0.0 dB
    fReverbMix: FLOAT;       // [-96.0,0.0]            default: 0.0 db
    fReverbTime: FLOAT;      // [0.001,3000.0]         default: 1000.0 ms
    fHighFreqRTRatio: FLOAT; // [0.001,0.999]          default: 0.001
  end;

  // callback function types
  STREAMPROC = function(Handle: HSTREAM; buffer: Pointer; length: DWord;
    user: Pointer): DWord; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    User stream callback function. NOTE: A stream function should obviously be as
    quick as possible, other streams (and MOD musics) can't be mixed until
    it's finished.
    handle : The stream that needs writing
    buffer : Buffer to write the samples in
    length : Number of bytes to write
    user   : The 'user' parameter value given when calling BASS_StreamCreate
    RETURN : Number of bytes written. Set the BASS_STREAMPROC_END flag to end
    the stream.
  }

const
  // special STREAMPROCs
  STREAMPROC_DUMMY = Pointer(0);  // "dummy" stream
  STREAMPROC_PUSH  = Pointer(-1); // push stream

type

  DOWNLOADPROC = procedure(buffer: Pointer; length: DWord; user: Pointer);
  {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    Internet stream download callback function.
    buffer : Buffer containing the downloaded data... NULL=end of download
    length : Number of bytes in the buffer
    user   : The 'user' parameter value given when calling BASS_StreamCreateURL
  }

  SYNCPROC = procedure(Handle: HSYNC; channel, Data: DWord; user: Pointer);
  {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    Sync callback function. NOTE: a sync callback function should be very
    quick as other syncs cannot be processed until it has finished. If the
    sync is a "mixtime" sync, then other streams and MOD musics can not be
    mixed until it's finished either.
    handle : The sync that has occured
    channel: Channel that the sync occured in
    data   : Additional data associated with the sync's occurance
    user   : The 'user' parameter given when calling BASS_ChannelSetSync
  }

  DSPPROC = procedure(Handle: HDSP; channel: DWord; buffer: Pointer;
    length: DWord; user: Pointer); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    DSP callback function. NOTE: A DSP function should obviously be as quick
    as possible... other DSP functions, streams and MOD musics can not be
    processed until it's finished.
    handle : The DSP handle
    channel: Channel that the DSP is being applied to
    buffer : Buffer to apply the DSP to
    length : Number of bytes in the buffer
    user   : The 'user' parameter given when calling BASS_ChannelSetDSP
  }

  RECORDPROC = function(Handle: HRECORD; buffer: Pointer; length: DWord;
    user: Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    Recording callback function.
    handle : The recording handle
    buffer : Buffer containing the recorded sample data
    length : Number of bytes
    user   : The 'user' parameter value given when calling BASS_RecordStart
    RETURN : TRUE = continue recording, FALSE = stop
  }

{$IFDEF IOS}

      const bassdll = 'libbass.a';

      function BASS_SetConfig(option, Value: DWord): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_GetConfig(option: DWord): DWord; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_SetConfigPtr(option: DWord; Value: Pointer): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_GetConfigPtr(option: DWord): Pointer; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_GetVersion: DWord; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ErrorGetCode: Integer; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_GetDeviceInfo(Device: DWord; var Info: BASS_DEVICEINFO): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;

      {$IFDEF MSWINDOWS}
      function BASS_Init(Device: Integer; freq, Flags: DWord; Win: HWND; clsid: PGUID): BOOL; stdcall; external bassdll;
      {$ELSE}
      function BASS_Init(Device: Integer; freq, Flags: DWord; Win: Pointer; clsid: Pointer): BOOL; cdecl; external bassdll;
      {$ENDIF}

      function BASS_SetDevice(Device: DWord): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_GetDevice: DWord; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_Free: BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      {$IFDEF MSWINDOWS} function BASS_GetDSoundObject(Obj: DWord): Pointer; stdcall; external bassdll; {$ENDIF}

      function BASS_GetInfo(var Info: BASS_INFO): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_Update(length: DWord): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_GetCPU: FLOAT; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_Start: BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_Stop: BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_Pause: BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_SetVolume(volume: FLOAT): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_GetVolume: FLOAT; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_PluginLoad(FileName: PChar; Flags: DWord): HPLUGIN; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_PluginFree(Handle: HPLUGIN): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_PluginGetInfo(Handle: HPLUGIN): PBASS_PLUGININFO; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_Set3DFactors(distf, rollf, doppf: FLOAT): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_Get3DFactors(var distf, rollf, doppf: FLOAT): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_Set3DPosition(var Pos, vel, front, Top: BASS_3DVECTOR): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_Get3DPosition(var Pos, vel, front, Top: BASS_3DVECTOR): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      procedure BASS_Apply3D; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;

      {$IFDEF MSWINDOWS}
      function BASS_SetEAXParameters(env: Integer; vol, Decay, damp: FLOAT): BOOL; stdcall; external bassdll;
      function BASS_GetEAXParameters(var env: DWord; var vol, Decay, damp: FLOAT): BOOL; stdcall external bassdll;
      {$ENDIF}

      function BASS_MusicLoad(mem: BOOL; f: Pointer; Offset: qword; length, Flags, freq: DWord): HMUSIC; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_MusicFree(Handle: HMUSIC): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_SampleLoad(mem: BOOL; f: Pointer; Offset: qword; length, Max, Flags: DWord): HSAMPLE; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_SampleCreate(length, freq, chans, Max, Flags: DWord): HSAMPLE; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_SampleFree(Handle: HSAMPLE): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_SampleSetData(Handle: HSAMPLE; buffer: Pointer): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_SampleGetData(Handle: HSAMPLE; buffer: Pointer): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_SampleGetInfo(Handle: HSAMPLE; var Info: BASS_SAMPLE): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_SampleSetInfo(Handle: HSAMPLE; var Info: BASS_SAMPLE): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_SampleGetChannel(Handle: HSAMPLE; onlynew: BOOL): HCHANNEL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_SampleGetChannels(Handle: HSAMPLE; channels: Pointer): DWord; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_SampleStop(Handle: HSAMPLE): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_StreamCreate(freq, chans, Flags: DWord; proc: STREAMPROC; user: Pointer): HSTREAM; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_StreamCreateFile(mem: BOOL; f: Pointer; Offset, length: qword; Flags: DWord): HSTREAM; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_StreamCreateURL(url: Pointer; Offset: DWord; Flags: DWord; proc: DOWNLOADPROC; user: Pointer): HSTREAM; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_StreamCreateFileUser(System, Flags: DWord; var procs: BASS_FILEPROCS; user: Pointer): HSTREAM; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_StreamFree(Handle: HSTREAM): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_StreamGetFilePosition(Handle: HSTREAM; Mode: DWord): qword; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_StreamPutData(Handle: HSTREAM; buffer: Pointer; length: DWord): DWord; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_StreamPutFileData(Handle: HSTREAM; buffer: Pointer; length: DWord): DWord; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_RecordGetDeviceInfo(Device: DWord; var Info: BASS_DEVICEINFO): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_RecordInit(Device: Integer): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_RecordSetDevice(Device: DWord): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_RecordGetDevice: DWord; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_RecordFree: BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_RecordGetInfo(var Info: BASS_RECORDINFO): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_RecordGetInputName(Input: Integer): PByteChar; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_RecordSetInput(Input: Integer; Flags: DWord; volume: FLOAT): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_RecordGetInput(Input: Integer; var volume: FLOAT): DWord; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_RecordStart(freq, chans, Flags: DWord; proc: RECORDPROC; user: Pointer): HRECORD; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelBytes2Seconds(Handle: DWord; Pos: qword): Double; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelSeconds2Bytes(Handle: DWord; Pos: Double): qword; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelGetDevice(Handle: DWord): DWord; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelSetDevice(Handle, Device: DWord): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelIsActive(Handle: DWord): DWord; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelGetInfo(Handle: DWord; var Info: BASS_CHANNELINFO): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelGetTags(Handle: HSTREAM; Tags: DWord): PByteChar; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelFlags(Handle, Flags, Mask: DWord): DWord; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelUpdate(Handle, length: DWord): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelLock(Handle: DWord; lock: BOOL): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelPlay(Handle: DWord; restart: BOOL): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelStop(Handle: DWord): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelPause(Handle: DWord): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelSetAttribute(Handle, attrib: DWord; Value: FLOAT): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelGetAttribute(Handle, attrib: DWord; var Value: FLOAT): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelSlideAttribute(Handle, attrib: DWord; Value: FLOAT; Time: DWord): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelIsSliding(Handle, attrib: DWord): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelSet3DAttributes(Handle: DWord; Mode: Integer; Min, Max: FLOAT; iangle, oangle, outvol: Integer): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelGet3DAttributes(Handle: DWord; var Mode: DWord; var Min, Max: FLOAT; var iangle, oangle, outvol: DWord): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelSet3DPosition(Handle: DWord; var Pos, orient, vel: BASS_3DVECTOR): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelGet3DPosition(Handle: DWord; var Pos, orient, vel: BASS_3DVECTOR): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelGetLength(Handle, Mode: DWord): qword; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelSetPosition(Handle: DWord; Pos: qword; Mode: DWord): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelGetPosition(Handle, Mode: DWord): qword; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelGetLevel(Handle: DWord): DWord; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelGetData(Handle: DWord; buffer: Pointer; length: DWord): DWord; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelSetSync(Handle: DWord; type_: DWord; Param: qword; proc: SYNCPROC; user: Pointer): HSYNC; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelRemoveSync(Handle: DWord; Sync: HSYNC): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelSetDSP(Handle: DWord; proc: DSPPROC; user: Pointer; Priority: Integer): HDSP; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelRemoveDSP(Handle: DWord; DSP: HDSP): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelSetLink(Handle, chan: DWord): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelRemoveLink(Handle, chan: DWord): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelSetFX(Handle, type_: DWord; Priority: Integer): HFX; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_ChannelRemoveFX(Handle: DWord; fx: HFX): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_FXSetParameters(Handle: HFX; PAR: Pointer): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_FXGetParameters(Handle: HFX; PAR: Pointer): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_FXReset(Handle: HFX): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF}; external bassdll;
      function BASS_SPEAKER_N(n: DWord): DWord;

      {$IFDEF MSWINDOWS}
      function BASS_SetEAXPreset(env: Integer): BOOL;
      {
        This function is defined in the implementation part of this unit.
        It is not part of BASS.DLL but an extra function which makes it easier
        to set the predefined EAX environments.
        env    : a EAX_ENVIRONMENT_xxx constant
      }
      {$ENDIF}

{$ELSE} // Non IOS

        // Functions
      const
        {$IFDEF MSWINDOWS}
         {$IF Defined(WIN32)}
           bassdll = 'bass32.dll';
         {$ELSEIF Defined(WIN64)}
           bassdll = 'bass64.dll';
         {$IFEND}
        {$ENDIF}

        {$IFDEF MACOS}
          bassdll = 'libbass.dylib';
        {$ENDIF}

        {$IFDEF ANDROID}
         bassdll = 'libbass.so';
        {$ENDIF}

      var
        BASS_SetConfig    : function(option, Value: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_GetConfig    : function(option: Cardinal): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_SetConfigPtr : function(option: Cardinal; Value: Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_GetConfigPtr : function(option: Cardinal): Pointer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_GetVersion   : function(): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ErrorGetCode : function(): Integer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_GetDeviceInfo: function(Device: Cardinal; var Info: BASS_DEVICEINFO): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        {$IFDEF MSWINDOWS}
        BASS_Init      : function(Device: Integer; freq, Flags: Cardinal; Win: HWND; clsid: PGUID): BOOL; stdcall;
        {$ELSE}
        BASS_Init        : function(Device: Integer; freq, Flags: Cardinal; Win: Pointer; clsid: Pointer): BOOL; cdecl;
        {$ENDIF}
        BASS_SetDevice                        : function(Device: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_GetDevice                        : function(): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_Free                             : function(): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        {$IFDEF MSWINDOWS}BASS_GetDSoundObject: function(Obj: Cardinal): Pointer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; {$ENDIF}
        BASS_GetInfo                          : function(var Info: BASS_INFO): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_Update                           : function(length: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_GetCPU                           : function(): FLOAT; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_Start                            : function(): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_Stop                             : function(): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_Pause                            : function(): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_SetVolume                        : function(volume: FLOAT): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_GetVolume                        : function(): FLOAT; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

        BASS_PluginLoad   : function(FileName: PChar; Flags: Cardinal): HPLUGIN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_PluginFree   : function(Handle: HPLUGIN): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_PluginGetInfo: function(Handle: HPLUGIN): PBASS_PLUGININFO; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

        BASS_Set3DFactors : function(distf, rollf, doppf: FLOAT): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_Get3DFactors : function(var distf, rollf, doppf: FLOAT): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_Set3DPosition: function(var Pos, vel, front, Top: BASS_3DVECTOR): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_Get3DPosition: function(var Pos, vel, front, Top: BASS_3DVECTOR): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_Apply3D      : procedure(); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        {$IFDEF MSWINDOWS}
        BASS_SetEAXParameters: function(env: Integer; vol, Decay, damp: FLOAT): BOOL; stdcall;
        BASS_GetEAXParameters: function(var env: Cardinal; var vol, Decay, damp: FLOAT): BOOL; stdcall;
        {$ENDIF}
        BASS_MusicLoad: function(mem: BOOL; f: Pointer; Offset: qword; length, Flags, freq: Cardinal): HMUSIC; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_MusicFree: function(Handle: HMUSIC): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

        BASS_SampleLoad       : function(mem: BOOL; f: Pointer; Offset: qword; length, Max, Flags: Cardinal): HSAMPLE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_SampleCreate     : function(length, freq, chans, Max, Flags: Cardinal): HSAMPLE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_SampleFree       : function(Handle: HSAMPLE): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_SampleSetData    : function(Handle: HSAMPLE; buffer: Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_SampleGetData    : function(Handle: HSAMPLE; buffer: Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_SampleGetInfo    : function(Handle: HSAMPLE; var Info: BASS_SAMPLE): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_SampleSetInfo    : function(Handle: HSAMPLE; var Info: BASS_SAMPLE): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_SampleGetChannel : function(Handle: HSAMPLE; onlynew: BOOL): HCHANNEL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_SampleGetChannels: function(Handle: HSAMPLE; channels: Pointer): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_SampleStop       : function(Handle: HSAMPLE): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

        BASS_StreamCreate                          : function(freq, chans, Flags: Cardinal; proc: STREAMPROC; user: Pointer): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_StreamCreateFile                      : function(mem: BOOL; f: Pointer; Offset, length: qword; Flags: Cardinal): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_StreamCreateURL                       : function(url: Pointer; Offset: Cardinal; Flags: Cardinal; proc: DOWNLOADPROC; user: Pointer): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_StreamCreateFileUser                  : function(System, Flags: Cardinal; var procs: BASS_FILEPROCS; user: Pointer): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_StreamFree                            : function(Handle: HSTREAM): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_StreamGetFilePosition                 : function(Handle: HSTREAM; Mode: Cardinal): qword; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_StreamPutData                         : function(Handle: HSTREAM; buffer: Pointer; length: Cardinal): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_StreamPutFileData                     : function(Handle: HSTREAM; buffer: Pointer; length: Cardinal): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

        BASS_RecordGetDeviceInfo   : function(Device: Cardinal; var Info: BASS_DEVICEINFO): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_RecordInit            : function(Device: Integer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_RecordSetDevice       : function(Device: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_RecordGetDevice       : function(): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_RecordFree            : function(): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_RecordGetInfo         : function(var Info: BASS_RECORDINFO): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_RecordGetInputName    : function(Input: Integer): PByteChar; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_RecordSetInput        : function(Input: Integer; Flags: Cardinal; volume: FLOAT): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_RecordGetInput        : function(Input: Integer; var volume: FLOAT): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_RecordStart           : function(freq, chans, Flags: Cardinal; proc: RECORDPROC; user: Pointer): HRECORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelBytes2Seconds  : function(Handle: Cardinal; Pos: qword): Double; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelSeconds2Bytes  : function(Handle: Cardinal; Pos: Double): qword; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelGetDevice      : function(Handle: Cardinal): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelSetDevice      : function(Handle, Device: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelIsActive       : function(Handle: Cardinal): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelGetInfo        : function(Handle: Cardinal; var Info: BASS_CHANNELINFO): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelGetTags        : function(Handle: HSTREAM; Tags: Cardinal): PByteChar; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelFlags          : function(Handle, Flags, Mask: Cardinal): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelUpdate         : function(Handle, length: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelLock           : function(Handle: Cardinal; lock: BOOL): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelPlay           : function(Handle: Cardinal; restart: BOOL): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelStop           : function(Handle: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelPause          : function(Handle: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelSetAttribute   : function(Handle, attrib: Cardinal; Value: FLOAT): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelGetAttribute   : function(Handle, attrib: Cardinal; var Value: FLOAT): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelSlideAttribute : function(Handle, attrib: Cardinal; Value: FLOAT; Time: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelIsSliding      : function(Handle, attrib: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelSet3DAttributes: function(Handle: Cardinal; Mode: Integer; Min, Max: FLOAT; iangle, oangle, outvol: Integer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelGet3DAttributes: function(Handle: Cardinal; var Mode: Cardinal; var Min, Max: FLOAT; var iangle, oangle, outvol: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelSet3DPosition  : function(Handle: Cardinal; var Pos, orient, vel: BASS_3DVECTOR): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelGet3DPosition  : function(Handle: Cardinal; var Pos, orient, vel: BASS_3DVECTOR): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelGetLength      : function(Handle, Mode: Cardinal): qword; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelSetPosition    : function(Handle: Cardinal; Pos: qword; Mode: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelGetPosition    : function(Handle, Mode: Cardinal): qword; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelGetLevel       : function(Handle: Cardinal): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelGetData        : function(Handle: Cardinal; buffer: Pointer; length: Cardinal): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelSetSync        : function(Handle: Cardinal; type_: Cardinal; Param: qword; proc: SYNCPROC; user: Pointer): HSYNC; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelRemoveSync     : function(Handle: Cardinal; Sync: HSYNC): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelSetDSP         : function(Handle: Cardinal; proc: DSPPROC; user: Pointer; Priority: Integer): HDSP; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelRemoveDSP      : function(Handle: Cardinal; DSP: HDSP): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelSetLink        : function(Handle, chan: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelRemoveLink     : function(Handle, chan: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelSetFX          : function(Handle, type_: Cardinal; Priority: Integer): HFX; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_ChannelRemoveFX       : function(Handle: Cardinal; fx: HFX): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

        BASS_FXSetParameters: function(Handle: HFX; PAR: Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_FXGetParameters: function(Handle: HFX; PAR: Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
        BASS_FXReset        : function(Handle: HFX): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

      function BASS_SPEAKER_N(n: Cardinal): Cardinal;

      {$IFDEF MSWINDOWS}
      function BASS_SetEAXPreset(env: Integer): BOOL;
      {
        This is defined in the implementation part of this unit.
        It is not part of BASS.DLL but an extra which makes it easier
        to set the predefined EAX environments.
        env    : a EAX_ENVIRONMENT_xxx constant
      }
      {$ENDIF}

{$ENDIF} //end ios

function Bass_Available: Boolean;

implementation

{$IFNDEF FPC}
uses System.IOUtils {$IFDEF IOS}, Macapi.CoreFoundation{$ENDIF};
{$ENDIF FPC}

{$IFDEF IOS}

const
  libAudioToolbox = '/System/Library/Frameworks/AudioToolbox.framework/AudioToolbox';
  libCoreServices = '/System/Library/Frameworks/CFNetwork.framework/CFNetwork';

procedure AudioServicesPlaySystemSound(inSystemSoundID: Integer); cdecl;
  external libAudioToolbox Name _PU + 'AudioServicesPlaySystemSound';

function HTTPMessageAddAuthentication(): Boolean; cdecl;
  external libCoreServices Name _PU + 'HTTPMessageAddAuthentication';
{$ENDIF}


function BASS_FOLDER: string;
begin
  {$IFNDEF FPC}
    {$IF Defined(MSWINDOWS)}
    Result := '';
    {$ELSEIF Defined(OSX)}
    Result := '';
    {$ELSE}
    Result := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetLibraryPath);
    {$ENDIF}
  {$ELSE FPC}
    Result := '';
  {$ENDIF FPC}
end;

{$IFDEF IOS}
  function Bass_Available: Boolean;
  begin
    Result := True;
  end;
{$ELSE}

    var FBassDLL: THandle;

    function Bass_Available: Boolean;
    begin
      Result := FBassDLL <> 0;
    end;

    procedure LoadBassDLL;
    begin
      FBassDLL := LoadLibrary(PChar(BASS_FOLDER + bassdll));
      if FBassDLL = 0 then
          Exit;

      BASS_SetConfig := GetProcAddress(FBassDLL, 'BASS_SetConfig');
      BASS_GetConfig := GetProcAddress(FBassDLL, 'BASS_GetConfig');
      BASS_SetConfigPtr := GetProcAddress(FBassDLL, 'BASS_SetConfigPtr');
      BASS_GetConfigPtr := GetProcAddress(FBassDLL, 'BASS_GetConfigPtr');
      BASS_GetVersion := GetProcAddress(FBassDLL, 'BASS_GetVersion');
      BASS_ErrorGetCode := GetProcAddress(FBassDLL, 'BASS_ErrorGetCode');
      BASS_GetDeviceInfo := GetProcAddress(FBassDLL, 'BASS_GetDeviceInfo');
      BASS_Init := GetProcAddress(FBassDLL, 'BASS_Init');
      BASS_SetDevice := GetProcAddress(FBassDLL, 'BASS_SetDevice');
      BASS_GetDevice := GetProcAddress(FBassDLL, 'BASS_GetDevice');
      BASS_Free := GetProcAddress(FBassDLL, 'BASS_Free');
      {$IFDEF MSWINDOWS}
      BASS_GetDSoundObject := GetProcAddress(FBassDLL, 'BASS_GetDSoundObject');
      {$ENDIF}
      BASS_GetInfo := GetProcAddress(FBassDLL, 'BASS_GetInfo');
      BASS_Update := GetProcAddress(FBassDLL, 'BASS_Update');
      BASS_GetCPU := GetProcAddress(FBassDLL, 'BASS_GetCPU');
      BASS_Start := GetProcAddress(FBassDLL, 'BASS_Start');
      BASS_Stop := GetProcAddress(FBassDLL, 'BASS_Stop');
      BASS_Pause := GetProcAddress(FBassDLL, 'BASS_Pause');
      BASS_SetVolume := GetProcAddress(FBassDLL, 'BASS_SetVolume');
      BASS_GetVolume := GetProcAddress(FBassDLL, 'BASS_GetVolume');

      BASS_PluginLoad := GetProcAddress(FBassDLL, 'BASS_PluginLoad');
      BASS_PluginFree := GetProcAddress(FBassDLL, 'BASS_PluginFree');
      BASS_PluginGetInfo := GetProcAddress(FBassDLL, 'BASS_PluginGetInfo');
      BASS_Set3DFactors := GetProcAddress(FBassDLL, 'BASS_Set3DFactors');
      BASS_Get3DFactors := GetProcAddress(FBassDLL, 'BASS_Get3DFactors');
      BASS_Set3DPosition := GetProcAddress(FBassDLL, 'BASS_Set3DPosition');
      BASS_Get3DPosition := GetProcAddress(FBassDLL, 'BASS_Get3DPosition');
      BASS_Apply3D := GetProcAddress(FBassDLL, 'BASS_Apply3D');
      {$IFDEF MSWINDOWS}
      BASS_SetEAXParameters := GetProcAddress(FBassDLL, 'BASS_SetEAXParameters');
      BASS_GetEAXParameters := GetProcAddress(FBassDLL, 'BASS_GetEAXParameters');
      {$ENDIF}
      BASS_MusicLoad := GetProcAddress(FBassDLL, 'BASS_MusicLoad');
      BASS_MusicFree := GetProcAddress(FBassDLL, 'BASS_MusicFree');
      BASS_SampleLoad := GetProcAddress(FBassDLL, 'BASS_SampleLoad');
      BASS_SampleCreate := GetProcAddress(FBassDLL, 'BASS_SampleCreate');
      BASS_SampleFree := GetProcAddress(FBassDLL, 'BASS_SampleFree');
      BASS_SampleSetData := GetProcAddress(FBassDLL, 'BASS_SampleSetData');
      BASS_SampleGetData := GetProcAddress(FBassDLL, 'BASS_SampleGetData');
      BASS_SampleGetInfo := GetProcAddress(FBassDLL, 'BASS_SampleGetInfo');
      BASS_SampleSetInfo := GetProcAddress(FBassDLL, 'BASS_SampleSetInfo');
      BASS_SampleGetChannel := GetProcAddress(FBassDLL, 'BASS_SampleGetChannel');
      BASS_SampleGetChannels := GetProcAddress(FBassDLL, 'BASS_SampleGetChannels');
      BASS_SampleStop := GetProcAddress(FBassDLL, 'BASS_SampleStop');
      BASS_StreamCreate := GetProcAddress(FBassDLL, 'BASS_StreamCreate');
      BASS_StreamCreateFile := GetProcAddress(FBassDLL, 'BASS_StreamCreateFile');
      BASS_StreamCreateURL := GetProcAddress(FBassDLL, 'BASS_StreamCreateURL');
      BASS_StreamCreateFileUser := GetProcAddress(FBassDLL, 'BASS_StreamCreateFileUser');
      BASS_StreamFree := GetProcAddress(FBassDLL, 'BASS_StreamFree');
      BASS_StreamGetFilePosition := GetProcAddress(FBassDLL, 'BASS_StreamGetFilePosition');
      BASS_StreamPutData := GetProcAddress(FBassDLL, 'BASS_StreamPutData');
      BASS_StreamPutFileData := GetProcAddress(FBassDLL, 'BASS_StreamPutFileData');
      BASS_RecordGetDeviceInfo := GetProcAddress(FBassDLL, 'BASS_RecordGetDeviceInfo');
      BASS_RecordInit := GetProcAddress(FBassDLL, 'BASS_RecordInit');
      BASS_RecordSetDevice := GetProcAddress(FBassDLL, 'BASS_RecordSetDevice');
      BASS_RecordGetDevice := GetProcAddress(FBassDLL, 'BASS_RecordGetDevice');
      BASS_RecordFree := GetProcAddress(FBassDLL, 'BASS_RecordFree');
      BASS_RecordGetInfo := GetProcAddress(FBassDLL, 'BASS_RecordGetInfo');
      BASS_RecordGetInputName := GetProcAddress(FBassDLL, 'BASS_RecordGetInputName');
      BASS_RecordSetInput := GetProcAddress(FBassDLL, 'BASS_RecordSetInput');
      BASS_RecordGetInput := GetProcAddress(FBassDLL, 'BASS_RecordGetInput');
      BASS_RecordStart := GetProcAddress(FBassDLL, 'BASS_RecordStart');
      BASS_ChannelBytes2Seconds := GetProcAddress(FBassDLL, 'BASS_ChannelBytes2Seconds');
      BASS_ChannelSeconds2Bytes := GetProcAddress(FBassDLL, 'BASS_ChannelSeconds2Bytes');
      BASS_ChannelGetDevice := GetProcAddress(FBassDLL, 'BASS_ChannelGetDevice');
      BASS_ChannelSetDevice := GetProcAddress(FBassDLL, 'BASS_ChannelSetDevice');
      BASS_ChannelIsActive := GetProcAddress(FBassDLL, 'BASS_ChannelIsActive');
      BASS_ChannelGetInfo := GetProcAddress(FBassDLL, 'BASS_ChannelGetInfo');
      BASS_ChannelGetTags := GetProcAddress(FBassDLL, 'BASS_ChannelGetTags');
      BASS_ChannelFlags := GetProcAddress(FBassDLL, 'BASS_ChannelFlags');
      BASS_ChannelUpdate := GetProcAddress(FBassDLL, 'BASS_ChannelUpdate');
      BASS_ChannelLock := GetProcAddress(FBassDLL, 'BASS_ChannelLock');
      BASS_ChannelPlay := GetProcAddress(FBassDLL, 'BASS_ChannelPlay');
      BASS_ChannelStop := GetProcAddress(FBassDLL, 'BASS_ChannelStop');
      BASS_ChannelPause := GetProcAddress(FBassDLL, 'BASS_ChannelPause');
      BASS_ChannelSetAttribute := GetProcAddress(FBassDLL, 'BASS_ChannelSetAttribute');
      BASS_ChannelGetAttribute := GetProcAddress(FBassDLL, 'BASS_ChannelGetAttribute');
      BASS_ChannelSlideAttribute := GetProcAddress(FBassDLL, 'BASS_ChannelSlideAttribute');
      BASS_ChannelIsSliding := GetProcAddress(FBassDLL, 'BASS_ChannelIsSliding');
      BASS_ChannelSet3DAttributes := GetProcAddress(FBassDLL, 'BASS_ChannelSet3DAttributes');
      BASS_ChannelGet3DAttributes := GetProcAddress(FBassDLL, 'BASS_ChannelGet3DAttributes');
      BASS_ChannelSet3DPosition := GetProcAddress(FBassDLL, 'BASS_ChannelSet3DPosition');
      BASS_ChannelGet3DPosition := GetProcAddress(FBassDLL, 'BASS_ChannelGet3DPosition');
      BASS_ChannelGetLength := GetProcAddress(FBassDLL, 'BASS_ChannelGetLength');
      BASS_ChannelSetPosition := GetProcAddress(FBassDLL, 'BASS_ChannelSetPosition');
      BASS_ChannelGetPosition := GetProcAddress(FBassDLL, 'BASS_ChannelGetPosition');
      BASS_ChannelGetLevel := GetProcAddress(FBassDLL, 'BASS_ChannelGetLevel');
      BASS_ChannelGetData := GetProcAddress(FBassDLL, 'BASS_ChannelGetData');
      BASS_ChannelSetSync := GetProcAddress(FBassDLL, 'BASS_ChannelSetSync');
      BASS_ChannelRemoveSync := GetProcAddress(FBassDLL, 'BASS_ChannelRemoveSync');
      BASS_ChannelSetDSP := GetProcAddress(FBassDLL, 'BASS_ChannelSetDSP');
      BASS_ChannelSetLink := GetProcAddress(FBassDLL, 'BASS_ChannelSetLink');
      BASS_ChannelRemoveLink := GetProcAddress(FBassDLL, 'BASS_ChannelRemoveLink');
      BASS_ChannelSetFX := GetProcAddress(FBassDLL, 'BASS_ChannelSetFX');
      BASS_ChannelRemoveFX := GetProcAddress(FBassDLL, 'BASS_ChannelRemoveFX');
      BASS_FXSetParameters := GetProcAddress(FBassDLL, 'BASS_FXSetParameters');
      BASS_FXGetParameters := GetProcAddress(FBassDLL, 'BASS_FXGetParameters');
      BASS_FXReset := GetProcAddress(FBassDLL, 'BASS_FXReset');
    end;
{$ENDIF}


function BASS_SPEAKER_N(n: DWord): DWord;
begin
  Result := n shl 24;
end;

{$IFDEF MSWINDOWS}


function BASS_SetEAXPreset(env: Integer): BOOL;
begin
  case (env) of
    EAX_ENVIRONMENT_GENERIC: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_GENERIC, 0.5, 1.493, 0.5);
    EAX_ENVIRONMENT_PADDEDCELL: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_PADDEDCELL, 0.25, 0.1, 0);
    EAX_ENVIRONMENT_ROOM: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_ROOM, 0.417, 0.4, 0.666);
    EAX_ENVIRONMENT_BATHROOM: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_BATHROOM, 0.653, 1.499, 0.166);
    EAX_ENVIRONMENT_LIVINGROOM: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_LIVINGROOM, 0.208, 0.478, 0);
    EAX_ENVIRONMENT_STONEROOM: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_STONEROOM, 0.5, 2.309, 0.888);
    EAX_ENVIRONMENT_AUDITORIUM: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_AUDITORIUM, 0.403, 4.279, 0.5);
    EAX_ENVIRONMENT_CONCERTHALL: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_CONCERTHALL, 0.5, 3.961, 0.5);
    EAX_ENVIRONMENT_CAVE: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_CAVE, 0.5, 2.886, 1.304);
    EAX_ENVIRONMENT_ARENA: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_ARENA, 0.361, 7.284, 0.332);
    EAX_ENVIRONMENT_HANGAR: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_HANGAR, 0.5, 10.0, 0.3);
    EAX_ENVIRONMENT_CARPETEDHALLWAY: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_CARPETEDHALLWAY, 0.153, 0.259, 2.0);
    EAX_ENVIRONMENT_HALLWAY: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_HALLWAY, 0.361, 1.493, 0);
    EAX_ENVIRONMENT_STONECORRIDOR: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_STONECORRIDOR, 0.444, 2.697, 0.638);
    EAX_ENVIRONMENT_ALLEY: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_ALLEY, 0.25, 1.752, 0.776);
    EAX_ENVIRONMENT_FOREST: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_FOREST, 0.111, 3.145, 0.472);
    EAX_ENVIRONMENT_CITY: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_CITY, 0.111, 2.767, 0.224);
    EAX_ENVIRONMENT_MOUNTAINS: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_MOUNTAINS, 0.194, 7.841, 0.472);
    EAX_ENVIRONMENT_QUARRY: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_QUARRY, 1, 1.499, 0.5);
    EAX_ENVIRONMENT_PLAIN: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_PLAIN, 0.097, 2.767, 0.224);
    EAX_ENVIRONMENT_PARKINGLOT: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_PARKINGLOT, 0.208, 1.652, 1.5);
    EAX_ENVIRONMENT_SEWERPIPE: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_SEWERPIPE, 0.652, 2.886, 0.25);
    EAX_ENVIRONMENT_UNDERWATER: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_UNDERWATER, 1, 1.499, 0);
    EAX_ENVIRONMENT_DRUGGED: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_DRUGGED, 0.875, 8.392, 1.388);
    EAX_ENVIRONMENT_DIZZY: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_DIZZY, 0.139, 17.234, 0.666);
    EAX_ENVIRONMENT_PSYCHOTIC: Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_PSYCHOTIC, 0.486, 7.563, 0.806);
    else
      Result := False;
  end;
end;
{$ENDIF}

{$IFNDEF IOS}

initialization

LoadBassDLL;

finalization

FreeLibrary(FBassDLL);
{$ENDIF}

end. 
 
 
 
