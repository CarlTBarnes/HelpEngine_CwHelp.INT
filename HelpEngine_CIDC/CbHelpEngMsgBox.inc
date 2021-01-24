 OMIT('_EndOfInclude_',_CbHelpEngMsgBox_)
_CbHelpEngMsgBox_ EQUATE(1)                
!============================================================================================
! MsgBox HelpEngine simply shows a Message() with the HelpCmd() Data 
! This could be modified to open WebHelp, EPub or other formats
!
! Implement this code BEFORE the first HELP() Statement to register
!   INCLUDE('CbHelpEngMsgBox.INC'),ONCE                     !<-- Global Includes or Data
!   Help2MsgBoxCls      CbHelpMsgBoxClass                   !<-- Global Includes or Data   
!   SYSTEM{PROP:HelpEngine,5} = ADDRESS(Help2MsgBoxCls)     !<-- CODE before any HELP()
!   HELP('YourHelp.CHM.MsgBox')                             !<-- msu have .MsgBox Extension
!--------------------------------------------------------------------------------------------
! For DevCon 2017 Attendees
! By Carl Barnes, Barrington, Illinois 
! www.CarlBarnes.com  carl@carlbarnes.com
!--------------------------------------------------------------------------------------------
!============================================================================================

    INCLUDE('CWHELP.INT'),ONCE

CbHelpMsgBoxClass CLASS,TYPE,IMPLEMENTS(HelpEngine),MODULE('CbHelpEngMsgBox.clw'), LINK('CbHelpEngMsgBox.clw',1),DLL(0)  

!HelpEngine Interface Methods Match and HelpCmd call the below Class Methods of same name
Match    PROCEDURE(CONST *CSTRING _HelpFileEXT),BOOL,VIRTUAL,PROTECTED  !Do we match MSGBOX FileEXT ?
HelpCmd  PROCEDURE(UNSIGNED _HWnd, <CONST *CSTRING _HelpFile>, UNSIGNED _HCmd, LONG _Data=0),BOOL,PROC,VIRTUAL,PROTECTED
                  END

_EndOfInclude_

