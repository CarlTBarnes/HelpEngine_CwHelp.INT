# HelpEngine the Clarion way to Hook Help

This is a work in progress providing my CIDC 2017 Help Presentation materials. I will expand on how to use it in the future. Please post questions on the Clarion Hub thread.

Clarion 8 implemented native support for the CHM format of help in the RTL. You can simply specify your file as HELP('Myhelp.CHM') and HLP('~MyTopic.htm') with your context string. Previously CHM was implemented using the CwHH templates and a DLL. Help opening was hooked using an ALIAS(F1Key,CtrlH) and code was inserted into every procedure by the template. This was undesirable.

What if you want some type of help other than HLP or CHM? How can you hook help? For that Clarion 8 added the HelpEngine Interface (Cwhelp.int) and SYSTEM Prop:HelpEngine. These appear only in LibSrc and are not documented or in example source.

```Clarion
! From LibSrc \ Win \ Property.CLW - Note its an "Array" so SYSTEM{PROP:HelpEngine,x}. My tests show maximum is 16.

PROP:HelpEngine         EQUATE (7D40H) ! array[integer]
```

Below is the CwHelp.INT file from LibSrc\Win\. The comments on the EQUATE's are mine:

```Clarion
                            !Clarion Sends When                   !HtmlHelp #define differs
HCMD_CLOSE       EQUATE(0)  ! Program closing                      <> 12 HH_CLOSE_ALL
HCMD_SHOWTOPIC   EQUATE(1)  ! STD(STD:Help) or F1 or HELP(,'xx')   <> 0  HH_DISPLAY_TOPIC
HCMD_SHOWINDEX   EQUATE(2)  ! STD(STD:HelpIndex)                   == 2  HH_DISPLAY_INDEX
HCMD_SHOWTOC     EQUATE(3)  ! RTL needs a STD:HelpTOC              <> 1  HH_DISPLAY_TOC
HCMD_SHOWSEARCH  EQUATE(4)  ! STD(STD:HelpSearch)                  <> 3  HH_DISPLAY_SEARCH
HCMD_HELPONHELP  EQUATE(5)  ! STD(STD:HelpOnHelp)                        Not Available

HelpEngine       INTERFACE
! Parameter:
! - extension of the help file's name
Match              PROCEDURE(CONST *CSTRING),BOOL
! Parameters:
! - Window handle returned by PROP:Handle for a WINDOW or a control
! - Name of help file
! - Help command (HCMD_xxx or extended command code)
! - Additional parameter dependent from the command
HelpCmd            PROCEDURE(UNSIGNED, <CONST *CSTRING>, UNSIGNED, LONG=0),BOOL,PROC
                 END

```

This is the Clarion Help hook, you will NOT find a Prop:HelpHook like you do for Message() with PROP:MessageHook. This is much better, it works two ways. You can Implement the Interface to "hook" up to 16 help extensions. And you can call the Interface to use Clarion's premade CHM engine. This would allow you to create an "Chm4Hlp" Engine that simply adds ".htm" to the HLP('~context string') and then call the RTL CHM engine.

This is undocumented but was not too hard to figure out. The method HelpEngine.HelpCmd() and HCMD_xxx equates match the Windows API HtmlHelpA() function documented on MSDN. One of the tricky parts is the fourth parameter a LONG "Additional parameter dependent from the command". This will vary based on the third parameter the HCMD_xxx. When it is HCMD_SHOWTOPIC the fourth parameter is the address of a CSTRING containing the HLP('help string'). This matches the way it works calling Win32 WinHelpA or HtmlHelpA().

The array property PROP:HelpEngine supports a maximum of 16 which I figured out by brute force. HelpEngine is a Claron INTERFACE which is documented that you can either IMPLEMENT() or call them one that has been implemented.

Tips: Only the old WinHelp HLP engine is loaded initially so it should be index #1 i.e. SYSTEM{PROP:HelpEngine,1}. The first call to HELP(AnyFile.CHM) will load the CHM HelpEngine into the next index, usually #2, this is because applications that require HTML Help are supposed to dynamically LoadLibrary('hhCtl.OCX').

## Dries Driessen ClarionLive Presentation using HelpEngine

Clarion Live 441 by Dries Driessen titled "On Google Analytics In Clarion".

> Dries is back! Here's what he's up to!! Dries says: Friday's presentation is about Google Analytics for Desktop applications, Web applications and Help. I'll be demonstrating how to add it to your desktop applications, your Nettalk webserver applications and even how to use Nina to get metrics on your help! And of course I will also cover the ins and outs of the Google Analytics site.

Dries uses the HelpEngine to hook help calls and log them, then he opens the help topic as normal for the user to read. This way he can see counts of what help topics are being read the most, possibly those windows need attention. His presentation covers a lot on Google Analytics. The HelpEngine appears at time 1:46:30. The slide's title is "Adding Ninja to your help" with the first bullet point as "Carl Barnes' fantastic presentation at CIDC demonstrated how to hook into Clarion's help engine."

In the Dries_ClarionLive_441 folder of this Repo I included the class Dries created and used in his presentation. This includes other classes so you probably cannot use the class as is, but you can see how he hooked help. In his code you will find a comment "!overwrite it with our address". I would not do that. I would search the 16 help engines indexes to find a Zero and use that one. You can see this in the CIDC examples function HlpEngTlz.FindFreeIndex().

## Carl Barnes Window Preview Class

My Window Preview Class allows viewing all the controls on a Window in a List. To make it available on all windows in all procedures in all DLLs the HelpEngine is used to intercept Ctrl+Shift+F1 and show the class field list window. This only requires code to implement that HelpEngine in the EXE.

The CbWndPrvHelpHookClass in the below folder uses the HelpEngine to catch
Control+Shift+F1 and show the list of fields.

Class: https://github.com/CarlTBarnes/WindowPreview/tree/master/libsrc
Example: https://github.com/CarlTBarnes/WindowPreview/tree/master/examples/School
