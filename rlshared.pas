unit rlshared;
{$MODE DELPHI}{$H+}
//{$mode objfpc}{$H+}
{@unit RLShared - Carrega a funcao ZGetPrinterDevice retornando corretamente a device da impressora
Autor: Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
07/2009
}

interface

uses
  Classes, SysUtils,Graphics;
//function MyCube(x:integer) : integer; cdecl; External 'libmycube.so' name 'MyCube';

const
  {$IFDEF LINUX}
  RLLIB = 'librlreportshared.so';
  {$ELSE}
  RLLIB = 'rlreportshared.dll';
  {$ENDIF}

  DM_ORIENTATION      = $00000001;
  {$EXTERNALSYM DM_ORIENTATION}
  DM_PAPERSIZE        = $00000002;
  {$EXTERNALSYM DM_PAPERSIZE}
  DM_PAPERLENGTH      = $00000004;
  {$EXTERNALSYM DM_PAPERLENGTH}
  DM_PAPERWIDTH       = $00000008;
  {$EXTERNALSYM DM_PAPERWIDTH}
  DM_SCALE            = $00000010;
  {$EXTERNALSYM DM_SCALE}
  DM_POSITION         = $00000020;
  {$EXTERNALSYM DM_POSITION}
  DM_NUP              = $00000040;
  {$EXTERNALSYM DM_NUP}
//#if(WINVER >= 0x0501)
  DM_DISPLAYORIENTATION = $00000080;
  {$EXTERNALSYM DM_DISPLAYORIENTATION}
//#endif /* WINVER >= 0x0501 */
  DM_COPIES           = $00000100;
  {$EXTERNALSYM DM_COPIES}
  DM_DEFAULTSOURCE    = $00000200;
  {$EXTERNALSYM DM_DEFAULTSOURCE}
  DM_PRINTQUALITY     = $00000400;
  {$EXTERNALSYM DM_PRINTQUALITY}
  DM_COLOR            = $00000800;
  {$EXTERNALSYM DM_COLOR}
  DM_DUPLEX           = $00001000;
  {$EXTERNALSYM DM_DUPLEX}
  DM_YRESOLUTION      = $00002000;
  {$EXTERNALSYM DM_YRESOLUTION}
  DM_TTOPTION         = $00004000;
  {$EXTERNALSYM DM_TTOPTION}
  DM_COLLATE          = $00008000;
  {$EXTERNALSYM DM_COLLATE}
  DM_FORMNAME         = $00010000;
  {$EXTERNALSYM DM_FORMNAME}
  DM_LOGPIXELS        = $00020000;
  {$EXTERNALSYM DM_LOGPIXELS}
  DM_BITSPERPEL       = $00040000;
  {$EXTERNALSYM DM_BITSPERPEL}
  DM_PELSWIDTH        = $00080000;
  {$EXTERNALSYM DM_PELSWIDTH}
  DM_PELSHEIGHT       = $00100000;
  {$EXTERNALSYM DM_PELSHEIGHT}
  DM_DISPLAYFLAGS     = $00200000;
  {$EXTERNALSYM DM_DISPLAYFLAGS}
  DM_DISPLAYFREQUENCY = $00400000;
  {$EXTERNALSYM DM_DISPLAYFREQUENCY}
  DM_ICMMETHOD        = $00800000;
  {$EXTERNALSYM DM_ICMMETHOD}
  DM_ICMINTENT        = $01000000;
  {$EXTERNALSYM DM_ICMINTENT}
  DM_MEDIATYPE        = $02000000;
  {$EXTERNALSYM DM_MEDIATYPE}
  DM_DITHERTYPE       = $04000000;
  {$EXTERNALSYM DM_DITHERTYPE}
  DM_PANNINGWIDTH     = $08000000;
  {$EXTERNALSYM DM_PANNINGWIDTH}
  DM_PANNINGHEIGHT    = $10000000;
  {$EXTERNALSYM DM_PANNINGHEIGHT}
//#if(WINVER >= 0x0501)
  DM_DISPLAYFIXEDOUTPUT = $20000000;
  {$EXTERNALSYM DM_DISPLAYFIXEDOUTPUT}
//#endif /* WINVER >= 0x0501 */

// orientation selections

  DMORIENT_PORTRAIT  = 1;
  {$EXTERNALSYM DMORIENT_PORTRAIT}
  DMORIENT_LANDSCAPE = 2;
  {$EXTERNALSYM DMORIENT_LANDSCAPE}

// paper selections

  DMPAPER_LETTER                  = 1; // Letter 8 1/2 x 11 in
  {$EXTERNALSYM DMPAPER_LETTER}
  DMPAPER_FIRST                   = DMPAPER_LETTER;
  {$EXTERNALSYM DMPAPER_FIRST}
  DMPAPER_LETTERSMALL             = 2; // Letter Small 8 1/2 x 11 in
  {$EXTERNALSYM DMPAPER_LETTERSMALL}
  DMPAPER_TABLOID                 = 3; // Tabloid 11 x 17 in
  {$EXTERNALSYM DMPAPER_TABLOID}
  DMPAPER_LEDGER                  = 4; // Ledger 17 x 11 in
  {$EXTERNALSYM DMPAPER_LEDGER}
  DMPAPER_LEGAL                   = 5; // Legal 8 1/2 x 14 in
  {$EXTERNALSYM DMPAPER_LEGAL}
  DMPAPER_STATEMENT               = 6; // Statement 5 1/2 x 8 1/2 in
  {$EXTERNALSYM DMPAPER_STATEMENT}
  DMPAPER_EXECUTIVE               = 7; // Executive 7 1/4 x 10 1/2 in
  {$EXTERNALSYM DMPAPER_EXECUTIVE}
  DMPAPER_A3                      = 8; // A3 297 x 420 mm
  {$EXTERNALSYM DMPAPER_A3}
  DMPAPER_A4                      = 9; // A4 210 x 297 mm
  {$EXTERNALSYM DMPAPER_A4}
  DMPAPER_A4SMALL                 = 10; // A4 Small 210 x 297 mm
  {$EXTERNALSYM DMPAPER_A4SMALL}
  DMPAPER_A5                      = 11; // A5 148 x 210 mm
  {$EXTERNALSYM DMPAPER_A5}
  DMPAPER_B4                      = 12; // B4 (JIS) 250 x 354
  {$EXTERNALSYM DMPAPER_B4}
  DMPAPER_B5                      = 13; // B5 (JIS) 182 x 257 mm
  {$EXTERNALSYM DMPAPER_B5}
  DMPAPER_FOLIO                   = 14; // Folio 8 1/2 x 13 in
  {$EXTERNALSYM DMPAPER_FOLIO}
  DMPAPER_QUARTO                  = 15; // Quarto 215 x 275 mm
  {$EXTERNALSYM DMPAPER_QUARTO}
  DMPAPER_10X14                   = 16; // 10x14 in
  {$EXTERNALSYM DMPAPER_10X14}
  DMPAPER_11X17                   = 17; // 11x17 in
  {$EXTERNALSYM DMPAPER_11X17}
  DMPAPER_NOTE                    = 18; // Note 8 1/2 x 11 in
  {$EXTERNALSYM DMPAPER_NOTE}
  DMPAPER_ENV_9                   = 19; // Envelope #9 3 7/8 x 8 7/8
  {$EXTERNALSYM DMPAPER_ENV_9}
  DMPAPER_ENV_10                  = 20; // Envelope #10 4 1/8 x 9 1/2
  {$EXTERNALSYM DMPAPER_ENV_10}
  DMPAPER_ENV_11                  = 21; // Envelope #11 4 1/2 x 10 3/8
  {$EXTERNALSYM DMPAPER_ENV_11}
  DMPAPER_ENV_12                  = 22; // Envelope #12 4 \276 x 11
  {$EXTERNALSYM DMPAPER_ENV_12}
  DMPAPER_ENV_14                  = 23; // Envelope #14 5 x 11 1/2
  {$EXTERNALSYM DMPAPER_ENV_14}
  DMPAPER_CSHEET                  = 24; // C size sheet
  {$EXTERNALSYM DMPAPER_CSHEET}
  DMPAPER_DSHEET                  = 25; // D size sheet
  {$EXTERNALSYM DMPAPER_DSHEET}
  DMPAPER_ESHEET                  = 26; // E size sheet
  {$EXTERNALSYM DMPAPER_ESHEET}
  DMPAPER_ENV_DL                  = 27; // Envelope DL 110 x 220mm
  {$EXTERNALSYM DMPAPER_ENV_DL}
  DMPAPER_ENV_C5                  = 28; // Envelope C5 162 x 229 mm
  {$EXTERNALSYM DMPAPER_ENV_C5}
  DMPAPER_ENV_C3                  = 29; // Envelope C3  324 x 458 mm
  {$EXTERNALSYM DMPAPER_ENV_C3}
  DMPAPER_ENV_C4                  = 30; // Envelope C4  229 x 324 mm
  {$EXTERNALSYM DMPAPER_ENV_C4}
  DMPAPER_ENV_C6                  = 31; // Envelope C6  114 x 162 mm
  {$EXTERNALSYM DMPAPER_ENV_C6}
  DMPAPER_ENV_C65                 = 32; // Envelope C65 114 x 229 mm
  {$EXTERNALSYM DMPAPER_ENV_C65}
  DMPAPER_ENV_B4                  = 33; // Envelope B4  250 x 353 mm
  {$EXTERNALSYM DMPAPER_ENV_B4}
  DMPAPER_ENV_B5                  = 34; // Envelope B5  176 x 250 mm
  {$EXTERNALSYM DMPAPER_ENV_B5}
  DMPAPER_ENV_B6                  = 35; // Envelope B6  176 x 125 mm
  {$EXTERNALSYM DMPAPER_ENV_B6}
  DMPAPER_ENV_ITALY               = 36; // Envelope 110 x 230 mm
  {$EXTERNALSYM DMPAPER_ENV_ITALY}
  DMPAPER_ENV_MONARCH             = 37; // Envelope Monarch 3.875 x 7.5 in
  {$EXTERNALSYM DMPAPER_ENV_MONARCH}
  DMPAPER_ENV_PERSONAL            = 38; // 6 3/4 Envelope 3 5/8 x 6 1/2 in
  {$EXTERNALSYM DMPAPER_ENV_PERSONAL}
  DMPAPER_FANFOLD_US              = 39; // US Std Fanfold 14 7/8 x 11 in
  {$EXTERNALSYM DMPAPER_FANFOLD_US}
  DMPAPER_FANFOLD_STD_GERMAN      = 40; // German Std Fanfold 8 1/2 x 12 in
  {$EXTERNALSYM DMPAPER_FANFOLD_STD_GERMAN}
  DMPAPER_FANFOLD_LGL_GERMAN      = 41; // German Legal Fanfold 8 1/2 x 13 in
  {$EXTERNALSYM DMPAPER_FANFOLD_LGL_GERMAN}
  DMPAPER_ISO_B4                  = 42; // B4 (ISO) 250 x 353 mm
  {$EXTERNALSYM DMPAPER_ISO_B4}
  DMPAPER_JAPANESE_POSTCARD       = 43; // Japanese Postcard 100 x 148 mm
  {$EXTERNALSYM DMPAPER_JAPANESE_POSTCARD}
  DMPAPER_9X11                    = 44; // 9 x 11 in
  {$EXTERNALSYM DMPAPER_9X11}
  DMPAPER_10X11                   = 45; // 10 x 11 in
  {$EXTERNALSYM DMPAPER_10X11}
  DMPAPER_15X11                   = 46; // 15 x 11 in
  {$EXTERNALSYM DMPAPER_15X11}
  DMPAPER_ENV_INVITE              = 47; // Envelope Invite 220 x 220 mm
  {$EXTERNALSYM DMPAPER_ENV_INVITE}
  DMPAPER_RESERVED_48             = 48; // RESERVED--DO NOT USE
  {$EXTERNALSYM DMPAPER_RESERVED_48}
  DMPAPER_RESERVED_49             = 49; // RESERVED--DO NOT USE
  {$EXTERNALSYM DMPAPER_RESERVED_49}
  DMPAPER_LETTER_EXTRA            = 50; // Letter Extra 9 \275 x 12 in
  {$EXTERNALSYM DMPAPER_LETTER_EXTRA}
  DMPAPER_LEGAL_EXTRA             = 51; // Legal Extra 9 \275 x 15 in
  {$EXTERNALSYM DMPAPER_LEGAL_EXTRA}
  DMPAPER_TABLOID_EXTRA           = 52; // Tabloid Extra 11.69 x 18 in
  {$EXTERNALSYM DMPAPER_TABLOID_EXTRA}
  DMPAPER_A4_EXTRA                = 53; // A4 Extra 9.27 x 12.69 in
  {$EXTERNALSYM DMPAPER_A4_EXTRA}
  DMPAPER_LETTER_TRANSVERSE       = 54; // Letter Transverse 8 \275 x 11 in
  {$EXTERNALSYM DMPAPER_LETTER_TRANSVERSE}
  DMPAPER_A4_TRANSVERSE           = 55; // A4 Transverse 210 x 297 mm
  {$EXTERNALSYM DMPAPER_A4_TRANSVERSE}
  DMPAPER_LETTER_EXTRA_TRANSVERSE = 56; // Letter Extra Transverse 9\275 x 12 in
  {$EXTERNALSYM DMPAPER_LETTER_EXTRA_TRANSVERSE}
  DMPAPER_A_PLUS                  = 57; // SuperA/SuperA/A4 227 x 356 mm
  {$EXTERNALSYM DMPAPER_A_PLUS}
  DMPAPER_B_PLUS                  = 58; // SuperB/SuperB/A3 305 x 487 mm
  {$EXTERNALSYM DMPAPER_B_PLUS}
  DMPAPER_LETTER_PLUS             = 59; // Letter Plus 8.5 x 12.69 in
  {$EXTERNALSYM DMPAPER_LETTER_PLUS}
  DMPAPER_A4_PLUS                 = 60; // A4 Plus 210 x 330 mm
  {$EXTERNALSYM DMPAPER_A4_PLUS}
  DMPAPER_A5_TRANSVERSE           = 61; // A5 Transverse 148 x 210 mm
  {$EXTERNALSYM DMPAPER_A5_TRANSVERSE}
  DMPAPER_B5_TRANSVERSE           = 62; // B5 (JIS) Transverse 182 x 257 mm
  {$EXTERNALSYM DMPAPER_B5_TRANSVERSE}
  DMPAPER_A3_EXTRA                = 63; // A3 Extra 322 x 445 mm
  {$EXTERNALSYM DMPAPER_A3_EXTRA}
  DMPAPER_A5_EXTRA                = 64; // A5 Extra 174 x 235 mm
  {$EXTERNALSYM DMPAPER_A5_EXTRA}
  DMPAPER_B5_EXTRA                = 65; // B5 (ISO) Extra 201 x 276 mm
  {$EXTERNALSYM DMPAPER_B5_EXTRA}
  DMPAPER_A2                      = 66; // A2 420 x 594 mm
  {$EXTERNALSYM DMPAPER_A2}
  DMPAPER_A3_TRANSVERSE           = 67; // A3 Transverse 297 x 420 mm
  {$EXTERNALSYM DMPAPER_A3_TRANSVERSE}
  DMPAPER_A3_EXTRA_TRANSVERSE     = 68; // A3 Extra Transverse 322 x 445 mm
  {$EXTERNALSYM DMPAPER_A3_EXTRA_TRANSVERSE}

  DMPAPER_DBL_JAPANESE_POSTCARD         = 69; // Japanese Double Postcard 200 x 148 mm
  {$EXTERNALSYM DMPAPER_DBL_JAPANESE_POSTCARD}
  DMPAPER_A6                            = 70; // A6 105 x 148 mm
  {$EXTERNALSYM DMPAPER_A6}
  DMPAPER_JENV_KAKU2                    = 71; // Japanese Envelope Kaku #2
  {$EXTERNALSYM DMPAPER_JENV_KAKU2}
  DMPAPER_JENV_KAKU3                    = 72; // Japanese Envelope Kaku #3
  {$EXTERNALSYM DMPAPER_JENV_KAKU3}
  DMPAPER_JENV_CHOU3                    = 73; // Japanese Envelope Chou #3
  {$EXTERNALSYM DMPAPER_JENV_CHOU3}
  DMPAPER_JENV_CHOU4                    = 74; // Japanese Envelope Chou #4
  {$EXTERNALSYM DMPAPER_JENV_CHOU4}
  DMPAPER_LETTER_ROTATED                = 75; // Letter Rotated 11 x 8 1/2 11 in
  {$EXTERNALSYM DMPAPER_LETTER_ROTATED}
  DMPAPER_A3_ROTATED                    = 76; // A3 Rotated 420 x 297 mm
  {$EXTERNALSYM DMPAPER_A3_ROTATED}
  DMPAPER_A4_ROTATED                    = 77; // A4 Rotated 297 x 210 mm
  {$EXTERNALSYM DMPAPER_A4_ROTATED}
  DMPAPER_A5_ROTATED                    = 78; // A5 Rotated 210 x 148 mm
  {$EXTERNALSYM DMPAPER_A5_ROTATED}
  DMPAPER_B4_JIS_ROTATED                = 79; // B4 (JIS) Rotated 364 x 257 mm
  {$EXTERNALSYM DMPAPER_B4_JIS_ROTATED}
  DMPAPER_B5_JIS_ROTATED                = 80; // B5 (JIS) Rotated 257 x 182 mm
  {$EXTERNALSYM DMPAPER_B5_JIS_ROTATED}
  DMPAPER_JAPANESE_POSTCARD_ROTATED     = 81; // Japanese Postcard Rotated 148 x 100 mm
  {$EXTERNALSYM DMPAPER_JAPANESE_POSTCARD_ROTATED}
  DMPAPER_DBL_JAPANESE_POSTCARD_ROTATED = 82; // Double Japanese Postcard Rotated 148 x 200 mm
  {$EXTERNALSYM DMPAPER_DBL_JAPANESE_POSTCARD_ROTATED}
  DMPAPER_A6_ROTATED                    = 83; // A6 Rotated 148 x 105 mm
  {$EXTERNALSYM DMPAPER_A6_ROTATED}
  DMPAPER_JENV_KAKU2_ROTATED            = 84; // Japanese Envelope Kaku #2 Rotated
  {$EXTERNALSYM DMPAPER_JENV_KAKU2_ROTATED}
  DMPAPER_JENV_KAKU3_ROTATED            = 85; // Japanese Envelope Kaku #3 Rotated
  {$EXTERNALSYM DMPAPER_JENV_KAKU3_ROTATED}
  DMPAPER_JENV_CHOU3_ROTATED            = 86; // Japanese Envelope Chou #3 Rotated
  {$EXTERNALSYM DMPAPER_JENV_CHOU3_ROTATED}
  DMPAPER_JENV_CHOU4_ROTATED            = 87; // Japanese Envelope Chou #4 Rotated
  {$EXTERNALSYM DMPAPER_JENV_CHOU4_ROTATED}
  DMPAPER_B6_JIS                        = 88; // B6 (JIS) 128 x 182 mm
  {$EXTERNALSYM DMPAPER_B6_JIS}
  DMPAPER_B6_JIS_ROTATED                = 89; // B6 (JIS) Rotated 182 x 128 mm
  {$EXTERNALSYM DMPAPER_B6_JIS_ROTATED}
  DMPAPER_12X11                         = 90; // 12 x 11 in
  {$EXTERNALSYM DMPAPER_12X11}
  DMPAPER_JENV_YOU4                     = 91; // Japanese Envelope You #4
  {$EXTERNALSYM DMPAPER_JENV_YOU4}
  DMPAPER_JENV_YOU4_ROTATED             = 92; // Japanese Envelope You #4 Rotated
  {$EXTERNALSYM DMPAPER_JENV_YOU4_ROTATED}
  DMPAPER_P16K                          = 93; // PRC 16K 146 x 215 mm
  {$EXTERNALSYM DMPAPER_P16K}
  DMPAPER_P32K                          = 94; // PRC 32K 97 x 151 mm
  {$EXTERNALSYM DMPAPER_P32K}
  DMPAPER_P32KBIG                       = 95; // PRC 32K(Big) 97 x 151 mm
  {$EXTERNALSYM DMPAPER_P32KBIG}
  DMPAPER_PENV_1                        = 96; // PRC Envelope #1 102 x 165 mm
  {$EXTERNALSYM DMPAPER_PENV_1}
  DMPAPER_PENV_2                        = 97; // PRC Envelope #2 102 x 176 mm
  {$EXTERNALSYM DMPAPER_PENV_2}
  DMPAPER_PENV_3                        = 98; // PRC Envelope #3 125 x 176 mm
  {$EXTERNALSYM DMPAPER_PENV_3}
  DMPAPER_PENV_4                        = 99; // PRC Envelope #4 110 x 208 mm
  {$EXTERNALSYM DMPAPER_PENV_4}
  DMPAPER_PENV_5                        = 100; // PRC Envelope #5 110 x 220 mm
  {$EXTERNALSYM DMPAPER_PENV_5}
  DMPAPER_PENV_6                        = 101; // PRC Envelope #6 120 x 230 mm
  {$EXTERNALSYM DMPAPER_PENV_6}
  DMPAPER_PENV_7                        = 102; // PRC Envelope #7 160 x 230 mm
  {$EXTERNALSYM DMPAPER_PENV_7}
  DMPAPER_PENV_8                        = 103; // PRC Envelope #8 120 x 309 mm
  {$EXTERNALSYM DMPAPER_PENV_8}
  DMPAPER_PENV_9                        = 104; // PRC Envelope #9 229 x 324 mm
  {$EXTERNALSYM DMPAPER_PENV_9}
  DMPAPER_PENV_10                       = 105; // PRC Envelope #10 324 x 458 mm
  {$EXTERNALSYM DMPAPER_PENV_10}
  DMPAPER_P16K_ROTATED                  = 106; // PRC 16K Rotated
  {$EXTERNALSYM DMPAPER_P16K_ROTATED}
  DMPAPER_P32K_ROTATED                  = 107; // PRC 32K Rotated
  {$EXTERNALSYM DMPAPER_P32K_ROTATED}
  DMPAPER_P32KBIG_ROTATED               = 108; // PRC 32K(Big) Rotated
  {$EXTERNALSYM DMPAPER_P32KBIG_ROTATED}
  DMPAPER_PENV_1_ROTATED                = 109; // PRC Envelope #1 Rotated 165 x 102 mm
  {$EXTERNALSYM DMPAPER_PENV_1_ROTATED}
  DMPAPER_PENV_2_ROTATED                = 110; // PRC Envelope #2 Rotated 176 x 102 mm
  {$EXTERNALSYM DMPAPER_PENV_2_ROTATED}
  DMPAPER_PENV_3_ROTATED                = 111; // PRC Envelope #3 Rotated 176 x 125 mm
  {$EXTERNALSYM DMPAPER_PENV_3_ROTATED}
  DMPAPER_PENV_4_ROTATED                = 112; // PRC Envelope #4 Rotated 208 x 110 mm
  {$EXTERNALSYM DMPAPER_PENV_4_ROTATED}
  DMPAPER_PENV_5_ROTATED                = 113; // PRC Envelope #5 Rotated 220 x 110 mm
  {$EXTERNALSYM DMPAPER_PENV_5_ROTATED}
  DMPAPER_PENV_6_ROTATED                = 114; // PRC Envelope #6 Rotated 230 x 120 mm
  {$EXTERNALSYM DMPAPER_PENV_6_ROTATED}
  DMPAPER_PENV_7_ROTATED                = 115; // PRC Envelope #7 Rotated 230 x 160 mm
  {$EXTERNALSYM DMPAPER_PENV_7_ROTATED}
  DMPAPER_PENV_8_ROTATED                = 116; // PRC Envelope #8 Rotated 309 x 120 mm
  {$EXTERNALSYM DMPAPER_PENV_8_ROTATED}
  DMPAPER_PENV_9_ROTATED                = 117; // PRC Envelope #9 Rotated 324 x 229 mm
  {$EXTERNALSYM DMPAPER_PENV_9_ROTATED}
  DMPAPER_PENV_10_ROTATED               = 118; // PRC Envelope #10 Rotated 458 x 324 mm
  {$EXTERNALSYM DMPAPER_PENV_10_ROTATED}

  {$IFDEF WIN98ME_UP}
  DMPAPER_LAST = DMPAPER_PENV_10_ROTATED;
  {$EXTERNALSYM DMPAPER_LAST}
  {$ELSE}
  DMPAPER_LAST = DMPAPER_A3_EXTRA_TRANSVERSE;
  {$EXTERNALSYM DMPAPER_LAST}
  {$ENDIF WIN98ME_UP}

  DMPAPER_USER = 256;
  {$EXTERNALSYM DMPAPER_USER}


  VER_PLATFORM_WIN32s = 0;
  {$EXTERNALSYM VER_PLATFORM_WIN32s}
  VER_PLATFORM_WIN32_WINDOWS = 1;
  {$EXTERNALSYM VER_PLATFORM_WIN32_WINDOWS}
  VER_PLATFORM_WIN32_NT = 2;
  {$EXTERNALSYM VER_PLATFORM_WIN32_NT}
  SEM_FAILCRITICALERRORS = 1;
  {$EXTERNALSYM SEM_FAILCRITICALERRORS}
  SEM_NOGPFAULTERRORBOX = 2;
  {$EXTERNALSYM SEM_NOGPFAULTERRORBOX}
  SEM_NOALIGNMENTFAULTEXCEPT = 4;
  {$EXTERNALSYM SEM_NOALIGNMENTFAULTEXCEPT}
  SEM_NOOPENFILEERRORBOX = $8000;
  {$EXTERNALSYM SEM_NOOPENFILEERRORBOX}

  HANDLE_FLAG_INHERIT = 1;
  {$EXTERNALSYM HANDLE_FLAG_INHERIT}
  HANDLE_FLAG_PROTECT_FROM_CLOSE = 2;
  {$EXTERNALSYM HANDLE_FLAG_PROTECT_FROM_CLOSE}
  HINSTANCE_ERROR = $20;
  {$EXTERNALSYM HINSTANCE_ERROR}

type
  BOOL = LongBool;
  {$EXTERNALSYM BOOL}
  PBOOL = ^BOOL;
  {$EXTERNALSYM PBOOL}

  UINT = LongWord;
  {$EXTERNALSYM UINT}
  PUINT = ^UINT;
  {$EXTERNALSYM PUINT}

  _OSVERSIONINFOA = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of AnsiChar; { Maintenance string for PSS usage }
  end;
  {$EXTERNALSYM _OSVERSIONINFOA}
  _OSVERSIONINFOW = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of WideChar; { Maintenance string for PSS usage }
  end;
  {$EXTERNALSYM _OSVERSIONINFOW}
  _OSVERSIONINFO = _OSVERSIONINFOA;
  TOSVersionInfoA = _OSVERSIONINFOA;
  TOSVersionInfoW = _OSVERSIONINFOW;
  TOSVersionInfo = TOSVersionInfoA;
  OSVERSIONINFOA = _OSVERSIONINFOA;
  {$EXTERNALSYM OSVERSIONINFOA}
  {$EXTERNALSYM OSVERSIONINFO}
  OSVERSIONINFOW = _OSVERSIONINFOW;
  {$EXTERNALSYM OSVERSIONINFOW}
  {$EXTERNALSYM OSVERSIONINFO}
  OSVERSIONINFO = OSVERSIONINFOA;

  TPageSize = (psA4, psB5, psLetter, psLegal, psExecutive, psA0, psA1, psA2,
    psA3, psA5, psA6, psA7, psA8, psA9, psB0, psB1, psB10, psB2, psB3, psB4, psB6,
    psB7, psB8, psB9, psC5E, psComm10E, psDLE, psFolio, psLedger, psTabloid, psNPageSize);

//  procedure rlRotateBitmap(rlfileBitmap,rlfileBitmapResult:pchar;rlAngle:double);{$IFDEF WIN32}stdcall;{$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF} External RLLIB name 'rlRotateBitmap';

//  procedure rlRotateBitmap(rlAngle:double);{$IFDEF WIN32}stdcall;{$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF} External RLLIB name 'rlRotateBitmap';
//  procedure DoColorTable(aSource,aDest:TBitmap; aContrast:double=2); {$IFDEF WIN32}stdcall;{$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF} External RLLIB name 'DoColorTable';
//  procedure DoErrorDiffusion(aSource,aDest:TBitmap; aContrast:double=2); {$IFDEF WIN32}stdcall;{$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF} External RLLIB name 'DoErrorDiffusion';
//  function  CanvasGetDescent(aCanvas:TCanvas):integer;{$IFDEF WIN32}stdcall;{$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF} External RLLIB name 'CanvasGetDescent';
//  function  GetBitmapPixel(aBitmap:TBitmap; aX,aY:integer; aDefault:TColor):TColor; {$IFDEF WIN32}stdcall;{$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF} External RLLIB name 'GetBitmapPixel';
  {$IFDEF MSWINDOWS}
  function ZGetPrinterDevice(Printername:pchar):pchar;stdcall; External RLLIB name 'ZGetPrinterDevice';
  {$ENDIF}
  {$ifdef FPC}
  function RGB(r, g, b: Byte): TColor;
  {$endif}

implementation
{
procedure RotateBitmap; External RLLIB name 'RotateBitmap';
procedure DoColorTable; External RLLIB name 'DoColorTable';
procedure DoErrorDiffusion; External RLLIB name 'DoErrorDiffusion';
function  CanvasGetDescent:integer;External RLLIB name 'CanvasGetDescent';
function GetBitmapPixel:TColor;External RLLIB name 'GetBitmapPixel';
}
{$IFDEF MSWINDOWS}
{
function GetPrinterDevice:string;External RLLIB name 'GetPrinterDevice';
}
{$ENDIF}

{$ifdef FPC}
function RGB(r, g, b: Byte): TColor;
begin
  Result := (r or (g shl 8) or (b shl 16));
end;
{$endif}


end.
