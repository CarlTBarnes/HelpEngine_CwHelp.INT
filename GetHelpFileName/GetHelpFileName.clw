!  HELP() sets the file name, but how do you read it back? 
!         E.g. a 3rd party may want to show its own Help file like CPCS or RPM Report Previewer, then restore the original help.
!              Currently this is done using the Template %HelpFile but that means Help must be set in every APP
!              when it should only be required in the EXE APP. 
!
!  RTL has '_WslHelp$GetHelpFile@FiPc'    it was added in C7 and 8,9,9.1,10 all have it
!    HELP() sets the file name in the RTL for all Windows
!    but ...
!    How do you Read it from the RTL. There is No Property nor BuiltIns.CLW Function;
!    however, the RTL exports  _WslHelp$GetHelpFile@FiPc
!

  PROGRAM

  MAP
GetHelpFile    FUNCTION(),STRING   
SetHelpFile    PROCEDURE(STRING _HelpFile)     !Not needed, use HELP(FN) 
ShowDiffHelp   PROCEDURE(STRING HlpFile, STRING HlpTopic) !Show Different Help File
    module('wsl')
        GetHelpFileRTL(*CSTRING HelpFileName),NAME('_WslHelp$GetHelpFile@FiPc'),DLL(dll_mode)                  !No Raw so RTL passed SIZE,POINTER
!       GetHelpFileRTL(UNSIGNED SizeHFN, *CSTRING HelpFN),name('_WslHelp$GetHelpFile@FiPc'),RAW,DLL(dll_mode)  !Raw form is same but paaases Size explicitly
        SetHelpFileRTL(CONST *CSTRING HelpFileName),NAME('_WslHelp$SetHelpFile@FPCc'),DLL(dll_mode)  !w/o CONST needs RAW 
    END     
  END
GetFN   STRING(260) 
  CODE
  !Does GetHelpFile() work to return the last HELP(File)?  Yes for me
  Help('debugview.chm')
  Message('Code did HELP(debugview.chm) ||Test GetHelpFile() returns that?||GetHelpFile()=' & GetHelpFile(),'Test 1 GetHelpFile()' )   

  IF 0 THEN      
      LOOP x# = 1000000 TO 0 by -1       !Do a lot in loop to see if it breaks
         GetFN=GetHelpFile()
         IF GetFN <> GetHelpFile() OR X#=0 THEN 
            Message('debugview.chm ?=? HelpFN=' & GetFN,'Multiple Test GetHelpFile() #' & X# )   
         END
      END 
  END 
  
  SetHelpFile('abc123.hlp') 
  Message('SetHelpFile(abc123.hlp)||Test GetHelpFile() returns that?||GetHelpFile()=' & GetHelpFile(),'Test 2 SetHelpFile()' )   !Use Help(FN) until this shows some other ability. 
                                                                            !Debuging a HELP() call shows it converts to CString can calls _WslHelp$SetHelpFile@FPCc
                                                                            
!======================================
GetHelpFile FUNCTION()!,STRING   !This gets you HELP(FileName)
CHelpFN     CSTRING(261)   
    CODE 
    GetHelpFileRTL(CHelpFN)
    RETURN CHelpFN 
    
SetHelpFile    PROCEDURE(STRING _HelpFile)     !Not needed, use HELP(FN)
CHelpFN     CSTRING(261),AUTO
    CODE
    CHelpFN=CLIP(_HelpFile) 
    SetHelpFileRTL(CHelpFN)
    RETURN 

ShowDiffHelp   PROCEDURE(STRING HlpFile, STRING HlpTopic) !Show Different Help File, then restore last HELP()
HlpNow  STRING(260),AUTO
    CODE
    HlpNow = GetHelpFile()     !Save Current Help File
    HELP(HlpFile, HlpTopic)    !Open Help in Passed File
    HELP(HlpNow)               !Restore to previous Help File
    RETURN     
    
    