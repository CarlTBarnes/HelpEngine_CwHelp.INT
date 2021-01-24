                              MEMBER

  include('TXHelpClass.inc'),ONCE

                              MAP
                              end!map


TXHelpClass.Construct  procedure()
  CODE  
  
TXHelpClass.Destruct   procedure()
  CODE
  SELF.Kill()
  
TXHelpClass.init              procedure(STRING p_HelpFile, NinaTrackClass p_Tracker)
stTmp                           StringTheory
  CODE
  if ~SELF.inited
    SELF.HelpFile = UPPER(p_HelpFile)
    SELF.Tracker &= p_Tracker
  
    !find the extension
    stTmp.SetValue(SELF.HelpFile,true)
    SELF.OriginalHelpExtension = stTmp.ExtensionOnly()
    SELF.Log('original help extension: ' & SELF.OriginalHelpExtension)

    HELP(SELF.HelpFile)  !makes sure that the appropriate help engine is loaded
    SELF.OriginalHelpEngineIndex = SELF.FindMatch(SELF.OriginalHelpExtension)

    if SELF.OriginalHelpEngineIndex = 0
      SELF.Log('Failed to find help engine of original file: ' & SELF.OriginalHelpExtension)
    ELSE
      !only continue if we've found the original engine    
      SELF.IOriginalHelpEngine &= SYSTEM{PROP:HelpEngine,SELF.OriginalHelpEngineIndex}
      SELF.Log('address orig helpengine: ' & SYSTEM{PROP:HelpEngine,SELF.OriginalHelpEngineIndex})
      
      SYSTEM{PROP:HelpEngine,SELF.OriginalHelpEngineIndex} = ADDRESS(SELF.HelpEngine) !overwrite it with our address
      SELF.Log('address helpengine after assignment: ' & SYSTEM{PROP:HelpEngine,SELF.OriginalHelpEngineIndex})

      SELF.inited = TRUE !needs to be set before re-assigning the help file

      !by re-assigning the Clarion RTL will pick up this new address
      HELP(SELF.HelpFile)  !makes sure that the appropriate help engine is loaded

      SELF.Log('Inited help engine: ' & SELF.OriginalHelpEngineIndex & ' File: ' & CLIP(SELF.HelpFile)) ! & '.' & SELF.HelpExtension)
    end!If
  end!If
  
TXHelpClass.SetHelpFile       procedure(STRING p_HelpFile)
  CODE
  SELF.HelpFile = p_HelpFile  
 
TXHelpClass.kill              procedure()  
  CODE
  if SELF.Inited
    SELF.inited = false
    SYSTEM{PROP:HelpEngine,SELF.OriginalHelpEngineIndex} = SELF.IOriginalHelpEngine
    HELP(SELF.HelpFile)  !re-assigning needed for the RTL to pick up the changed interface address                                   
  end!If
  
!============================= Help Engine Tools ===============================  
! CODE was copied from CbHelpEngCheck.clw written by Carl Barnes
! CODE was shared by Carl Barnes with CIDC 2017 attendees: 
! For DevCon 2017 Attendees
! By Carl Barnes, Barrington, Illinois 
! www.CarlBarnes.com  carl@carlbarnes.com
!============================= Help Engine Tools ===============================
    
TXHelpClass.Match             PROCEDURE(CONST *CSTRING _HelpFileEXT)!,BOOL,VIRTUAL,PROTECTED
  CODE
  SELF.Log('Matching: ' & _HelpFileEXT & ' with ' & SELF.OriginalHelpExtension & ' inited: ' & SELF.Inited)
  IF SELF.inited AND UPPER(_HelpFileEXT)=SELF.OriginalHelpExtension !& SELF.HelpExtension THEN    
    SELF.log('Matched help engine: ' & _HelpFileEXT)
    RETURN True 
  END     
  RETURN False  
  
TXHelpClass.HelpCmd PROCEDURE(UNSIGNED _HWnd, <CONST *CSTRING _HelpFile>, UNSIGNED _HCmd, LONG _Data=0)!,BOOL,PROC,VIRTUAL,,PROTECTED
cChmFile        CSTRING(261)    !_HelpFile with CHEK cutoff of ChmChek
LnStr           LONG 
cDbg            CSTRING(512)
cData           &CSTRING
cBlank          CSTRING(2)   
RetBool         BOOL 
  CODE
  SELF.Log('HelpCMD: ' & _Hwnd & _HelpFile & ' ' & _HCmd & ' ' & _Data)
  
  if ~SELF.inited   
    SELF.Log('HelpCMD while not inited')
    return  0
  end!if
  
  cData &= cBlank
  IF OMITTED(_HelpFile) THEN                      !No File passed, that's odd
    SELF.Log('No HelpFile passed ' & _HCmd & ' ' & _Data) 
    RETURN SELF.IOriginalHelpEngine.HelpCmd(_HWnd, , _HCmd, _Data)       
  END     
    
  cChmFile=CLIP(SELF.HelpFile) !SUB(_HelpFile,1,LnStr - LEN(SELF.HelpExtension))  !  Change to  .Chm     to pass to ChmEngine
  IF _HCmd=HCMD_SHOWTOPIC           |             !Cmd is F1 Showing HLP()
      AND (_Data > 0FFFFh OR _Data < 0) THEN          !0 to 64KB is invalid pointer
    cData &= (_Data)                             !defef pointer to CSTRING
  END
  SELF.Log('HelpCmd: ' & _HelpFile & ' ' & CHOOSE(~cData, ''&_Data, cData))  
  
  If ~SELF.Tracker &= NULL 
    if _HCmd=HCMD_SHOWTOPIC AND cData <> ''
      SELF.Tracker.AddEvent('Help',SELF.HCmdName(_HCmd),cData)
    ELSE
      SELF.Tracker.AddEvent('Help',SELF.HCmdName(_HCmd))
    end!If
  end!if
  
  !#2 - Open Help Viwer by calling the CHM Engine found in .Init() and pass adjusted file name + original parms
  RetBool = SELF.IOriginalHelpEngine.HelpCmd(_HWnd, cChmFile , _HCmd, _Data)   
  
  SELF.Log('Help: ret' & RetBool)
    
  RETURN RetBool  
    
!HelpEngine interface 
TXHelpClass.HelpEngine.Match    PROCEDURE(CONST *CSTRING _HelpFileEXT)!,BOOL
    CODE
    RETURN SELF.MATCH(_HelpFileEXT) 
!------------------------------------    
TXHelpClass.HelpEngine.HelpCmd  PROCEDURE(UNSIGNED _HWnd, <CONST *CSTRING _HelpFile>, UNSIGNED _HCmd, LONG _Data=0)!,BOOL,PROC
    CODE
    RETURN SELF.HelpCmd(_HWnd,_HelpFile,_HCmd,_Data)  

!Find an Engine that Match()s on EXTension and return its SYSTEM HlpEng Index  
TXHelpClass.FindMatch   PROCEDURE(STRING HelpFileExten) !,LONG returns INDEX of Engine.Match(EXT)
cHelpEXT        CSTRING(16),AUTO
Engine2Test     &HelpEngine 
EngAddress      LONG,AUTO 
Index           LONG,AUTO 
IndexMatched    LONG
    CODE
    cHelpEXT = CLIP(UPPER(HelpFileExten))
    LOOP Index=1 TO 16                      !Look thru 16 engines
         EngAddress = SYSTEM{PROP:HelpEngine,Index} 
         IF EngAddress = 0 THEN CYCLE.                  !Index assigned Engine? 
         Engine2Test &= SYSTEM{PROP:HelpEngine,Index} 
         IF Engine2Test.Match(cHelpEXT) THEN            !Call HelpEngine.Match() 
            IndexMatched = Index
            BREAK
         END
    END
    RETURN IndexMatched
!--------------------------------------------------    
TXHelpClass.HCmdName    PROCEDURE(LONG HCmd) !,STRING
  CODE 
  RETURN CHOOSE(HCmd+1, 'HCMD_CLOSE','HCMD_SHOWTOPIC','HCMD_SHOWINDEX','HCMD_SHOWTOC','HCMD_SHOWSEARCH','HCMD_HELPONHELP','HCMD_' & HCmd)