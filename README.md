# HelpEngine the Clarion way to Hook Help

This is a work in progress providing my CIDC 2017 help materials. I will expand on how to use it in the future. Please post questions on the Clarion Hub thread.

Clarion 8 implemented native support for the CHM format of help in the RTL. You can simply specify HELP('Myhelp.CHM') and HLP('MyTopic.htm'). Previously CHM was implemented using the CwHH templates and DLL. Help was hooked using an ALIAS(F1Key,CtrlH) and code inserted into every procedure by the template. This was undesirable.

What if you want some type of help other than HLP or CHM? How can you hook help? For that Clarion 8 added the HelpEngine Interface (Cwhelp.int) and SYSTEM Prop:HelpEngine. These appear only in LibSrc and not documented or in example source.

```Clarion
! From LibSrc \ Win \ Property.CLW - Note its an "Array" so SYSTEM{PROP:HelpEngine,x}. My tests show maximum 16.

PROP:HelpEngine         EQUATE (7D40H) ! array[integer]
```

```Clarion
! From LibSrc \ Win \ CwHelp.INT

HCMD_CLOSE       EQUATE(0)
HCMD_SHOWTOPIC   EQUATE(1)
HCMD_SHOWINDEX   EQUATE(2)
HCMD_SHOWTOC     EQUATE(3)
HCMD_SHOWSEARCH  EQUATE(4)
HCMD_HELPONHELP  EQUATE(5)

HelpEngine       INTERFACE
! Parameter:
! - extension of the help file's name
Match              PROCEDURE(CONST *CSTRING),BOOL
! Parameters:
! - window handle returned by PROP:Handle for a WINDOW or a control
! - name of help file
! - help command (HCMD_xxx or extended command code)
! - additional parameter dependent from the command
HelpCmd            PROCEDURE(UNSIGNED, <CONST *CSTRING>, UNSIGNED, LONG=0),BOOL,PROC
                 END

```

This is the Clarion Help hook, you will NOT find a Prop:HelpHook like you do for Message() with PROP:MessageHook. This is much better, it works two ways. You can Implement the Interface to "hook" up to 16 help extensions. And you can call the Interface to use Clarion's premade CHM engine. This would allow you to create an "Chm4Hlp" Engine that simply adds ".htm" to the HLP('~context string') and then call the RTL CHM engine.

This is undocumented but was not too hard to figure out. The method HelpEngine.HelpCmd() and HCMD_xxx equates match the Windows API HtmlHelpA() function documented on MSDN. The array property PROP:HelpEngine supports a maximum of 16 which I figured out by brute force. HelpEngine is a Claron INTERFACE which is documented that you can either IMPLEMENT() or call them one that has been implemented.

Tips: Only HLP engine is loaded initially so should be SYSTEM{PROP:HelpEngine,1}. The first call to HELP(AnyFile.CHM) will load the CHM HelpEngine into the next slot, usually #2, this is because application that require HTML Help are supposed to dynamically LoadLibrary('hhCtl.OCX'). 

## Dries Driessen ClarionLive Presentation using HelpEngine

Clarion Live 441 by Dries Driessen was titled "On Google Analytics In Clarion".

> Dries is back! Here's what he's up to!! Dries says: Friday's presentation is about Google Analytics for Desktop applications, Web applications and Help. I'll be demonstrating how to add it to your desktop applications, your Nettalk webserver applications and even how to use Nina to get metrics on your help! And of course I will also cover the ins and outs of the Google Analytics site.

Dries uses the HelpEngine to hook help calls and log them, then opens the help as normal. This way he can see what help topics are being read the most, possibly those windows need to be changed.

## Carl Barnes Window Preview Class

The Window Preview Class allows viewing all the controls on a Window in a List. To make it available in all procedures the HelpEngine is used to intercept Ctrl+Shift+F1 and show the window.

The CbWndPrvHelpHookClass in the below folder uses the HelpEngine to catch
Control+Shift+F1 on any window and show the list of fields.

Class: https://github.com/CarlTBarnes/WindowPreview/tree/master/libsrc
Example: https://github.com/CarlTBarnes/WindowPreview/tree/master/examples/School

I can will eventually get a better example up on Github.
