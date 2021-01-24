  MEMBER
  INCLUDE('CbHelpEngMsgBox.INC'),ONCE 

  MAP 
HCmdName        PROCEDURE(LONG HCmd),STRING         !put in EngTlz  
  END 

!============================ IMPLEMENTS(HelpEngine) ==============================
CbHelpMsgBoxClass.HelpEngine.Match    PROCEDURE(CONST *CSTRING _HelpFileEXT)!,BOOL
    CODE
    RETURN SELF.MATCH(_HelpFileEXT) 
!------------------------------------    
CbHelpMsgBoxClass.HelpEngine.HelpCmd  PROCEDURE(UNSIGNED _HWnd, <CONST *CSTRING _HelpFile>, UNSIGNED _HCmd, LONG _Data=0)!,BOOL,PROC
    CODE
    RETURN SELF.HelpCmd(_HWnd,_HelpFile,_HCmd,_Data)
    
!=================================================================================    
!Class Methods of same name called by HelpEngine Interface to do the Real Work
CbHelpMsgBoxClass.Match PROCEDURE(CONST *CSTRING _HelpFileEXT)!,BOOL,VIRTUAL,PROTECTED
    CODE
    IF UPPER(_HelpFileEXT)='MSGBOX' THEN  
       RETURN True 
    END     
    RETURN False
!----------------    
CbHelpMsgBoxClass.HelpCmd PROCEDURE(UNSIGNED _HWnd, <CONST *CSTRING _HelpFile>, UNSIGNED _HCmd, LONG _Data=0)!,BOOL,PROC,VIRTUAL,,PROTECTED
cHelpFile       CSTRING(261)        !Either _HelpFile or Blank
cDbg            CSTRING(512)
cData           &CSTRING
cBlank          CSTRING(2)   
RetBool         BOOL 
    CODE
    cData &= cBlank
    IF ~OMITTED(_HelpFile) THEN      !No File passed, that's odd
       cHelpFile = _HelpFile
    END 
    IF _HCmd=HCMD_SHOWTOPIC           |         !F1 Showing HLP()
    AND (_Data > 0FFFFh OR _Data < 0) THEN      !0 to 64KB is invalid pointer
       cData &= (_Data)                         !defef pointer to CSTRING
    END
    
    MESSAGE(' {80}|Hnd:   ' & _HWnd & |
            '||File:     ' & cHelpFile & |
            '||Cmd:  ' & _HCmd &' '& HCmdName(_HCmd) & |
            '||Data:   ' & CHOOSE(~cData, ''&_Data, cData) & |
            '','CbHelpMsgBoxClass.HelpCmd', ICON:Help,Button:Ok,,MSGMODE:CANCOPY)
            
    RETURN True

!===================================================================
HCmdName    PROCEDURE(LONG HCmd) !,STRING  put in EngTlz
    CODE 
    RETURN CHOOSE(HCmd+1, 'HCMD_CLOSE','HCMD_SHOWTOPIC','HCMD_SHOWINDEX','HCMD_SHOWTOC','HCMD_SHOWSEARCH','HCMD_HELPONHELP','HCMD_' & HCmd)
!-------------------------------------------------------
