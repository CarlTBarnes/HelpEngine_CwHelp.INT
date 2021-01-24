  include('SLSBaseClass.inc'),ONCE 
  include('NinaTrackClass.inc'),ONCE

  INCLUDE('CWHELP.INT'),ONCE

TXHelpClass                   Class(SLSBaseClass),TYPE,IMPLEMENTS(HelpEngine),module('TXHelpClass.CLW'),LINK('TXHelpClass.CLW',_SLSLinkMode_),DLL(_SLSDllMode_)
Tracker                         &NinaTrackClass
HelpFile                        STRING(FILE:MaxFilePath)

OriginalHelpExtension           CSTRING(16)
OriginalHelpEngineIndex         LONG
IOriginalHelpEngine             &HelpEngine

Inited                          BYTE,PROTECTED

Construct                       PROCEDURE
Destruct                        PROCEDURE

Init                            procedure(STRING p_HelpFile, NinaTrackClass p_Tracker),VIRTUAL

SetHelpFile                     procedure(STRING p_HelpFile),VIRTUAL

kill                            PROCEDURE(),VIRTUAL

Match                           PROCEDURE(CONST *CSTRING _HelpFileEXT),BOOL,VIRTUAL,PROTECTED  !Do we match .ChekFileEXT ?
HelpCmd                         PROCEDURE(UNSIGNED _HWnd, <CONST *CSTRING _HelpFile>, UNSIGNED _HCmd, LONG _Data=0),BOOL,PROC,VIRTUAL,PROTECTED

FindMatch                       PROCEDURE(STRING HelpFileExten),LONG      !Search 16 slots for Match(EXT) and return Index
HCmdName                        PROCEDURE(LONG HCmd),STRING    
                              end!class