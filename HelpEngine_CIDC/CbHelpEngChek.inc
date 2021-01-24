 OMIT('_EndOfInclude_',_CbHelpEngChek_)
_CbHelpEngChek_ EQUATE(1)                
!============================================================================================ 
! For DevCon 2017 Attendees
! By Carl Barnes, Barrington, Illinois 
! www.CarlBarnes.com  carl@carlbarnes.com
!-------------------------------------------------------------------------------------------
! Disclaimer
! The HelpEngine documentation is limted to:
!       LibSrc CWHELP.INT file 
!       Property.CLW new line PROP:HelpEngine         EQUATE (7D40H) ! array[integer]
!       MSDN for HtmlHelp() and WinHelp() are the target of HelpEngine.HelpCmd
! Based on the above I created the DevCon Help Engine examples.
! They work for me. What is here may not be correct, use at your own risk
!--------------------------------------------------------------------------------------------
! HelpChek HelpEngine shows HelpCmd() parms in DebugView to see what topic is being opened.
! Ctrl+Shift F1 opens a Class Window showing all the HLP() on the Under Window and allows testing each.
! 
! Implement this code and do NOT have another HELP('file.xxx') because it will disable this
! It works by making the Help('YourFile.ChmCHEK'). This Engine Matches on 'xxxCHEK'
! After OutputDebugString HelpCmd() parms the CHM topic is opened normally using the Chm Engine.
! This can work with any help engine e,g, HLP or any other you add. It "hooks" them.
!
! 1. Global Properties name your Help file: YourHelp.CHM
! 2. Global Embeds - Global Includes or Data
!       INCLUDE('CbHelpEngChek.INC'),ONCE  
!       HelpChekCls     CbHelpChekClass                  
!       HCRV            LONG              
! 3. Global Embeds - Program Setup
!       IF EXISTS('HelpChek.TXT') THEN              !Only use by Developers, Testers, Help Authors and not end users
!          HCRV = HelpChekCls.Init('YourHelp.CHM')                     !<-- CODE 
!          IF HCRV THEN Message('HelpCheck failed reason ' & HCRV )    !<-- CODE 
!       ELSE
!          HELP('YourHelp.CHM')                     !Normal users use regular CHM Engine
!       END  
!--------------------------------------------------------------------------------------------
!============================================================================================

    INCLUDE('CWHELP.INT'),ONCE

CbHelpChekClass CLASS,TYPE,IMPLEMENTS(HelpEngine),MODULE('CbHelpEngChek.clw'), LINK('CbHelpEngChek.clw',1),DLL(0)  !Ment to always be linked in

IsInited         BYTE,PROTECTED            !Init worked and not Killed 

ChmFileName     CSTRING(261)    !,PROTECTED  !File.CHM but really any File.HLP or File.XXX of your own
ChmFileEXT      CSTRING(16)     !,PROTECTED  !Real EXT probably CHM
ChmEngine       &HelpEngine     !,PROTECTED        
ChmEngIndex     LONG            !,PROTECTED   

ChekFileName    CSTRING(261)    !,PROTECTED  !MyHelp.ChmCHEK     
ChekFileEXT     CSTRING(16)     !,PROTECTED  !       CHMCHEK always UPPER
ChekEngIndex    LONG            !,PROTECTED

! Init Parameters
!      Returns:  0=Worked Out in Temp; Failed 1=Not Network  <0 is Error  -2=File not found  -3=TempPathNotFound

Init            PROCEDURE(STRING HelpFile, BOOL ShowDbgMsg=0),LONG,PROC,VIRTUAL !0=Ok else Problem
Kill            PROCEDURE()     !If IsInited sets Help(ChmFilename) and removes Index

!Called by HelpEngine Interface to do the Real Work so can inherit, and are virtual
Match           PROCEDURE(CONST *CSTRING _HelpFileEXT),BOOL,VIRTUAL,PROTECTED  !Do we match .ChekFileEXT ?
HelpCmd         PROCEDURE(UNSIGNED _HWnd, <CONST *CSTRING _HelpFile>, UNSIGNED _HCmd, LONG _Data=0),BOOL,PROC,VIRTUAL,PROTECTED

              END

_EndOfInclude_

