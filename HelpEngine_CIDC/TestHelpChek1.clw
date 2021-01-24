!by Carl Barnes for DevCon 2017 as an Example of the Clarion Language Help Features 
!Some Windows with HLP to tes Help Engines
  PROGRAM
  INCLUDE('KEYCODES.CLW') 

    INCLUDE('CbHelpEngMsgBox.INC'),ONCE
Help2MsgBoxCls   CbHelpMsgBoxClass       !Help Engine shows Message() with HLP() instead of Help

    INCLUDE('CbHelpEngChek.inc'),ONCE    !"Help Chek" shows help topic in DebugView
HelpChekCls     CbHelpChekClass          !Ctrl+Shift+Help opens Window showing Help and allowing test
  
  MAP
Main        PROCEDURE()
SimpleHelp1             PROCEDURE()  
SimpleHelpMenu2         PROCEDURE()  
HelpContextVsSearch     PROCEDURE()  !Diff Between HLP('~Context') and HLP('Search')  
HelpWithParents         PROCEDURE()  !Window with Tabs and GROUP               
ChmHelpMustHaveChmExtensionTest   PROCEDURE()

GetHelpTopic            FUNCTION (),STRING                                    ! HLP() for Control with Focus in Active Window
GetHelpTopic2           FUNCTION (<LONG _CtrlFeq>,<WINDOW _WndRef>),STRING    ! HLP() from any Control and Window can be prior
HexLong                 PROCEDURE(LONG ALong),STRING
  END
ChmFile4Test   EQUATE('DevCon17.CHM') 
    CODE
    !HELP('xxxx.HLP') !; HELP('xxxx.CHM')   !For testing make HLP #1, so not assuming CHM as #1 
    CASE MESSAGE('Use Which Engine?' & |
          '||CHM = Normal CHM Help' & |
          '||MsgBox = MsgBox Engine pops up Message Box showing HelpCmd parameters.' & |
          '||Help Chek = Debug View shows Topic.|<9>Ctrl+Shift+F1 opens Window showing Window''s HLPs.' & |
         '|','Select Help Engine', ICON:Exclamation, |
         'CHM|MsgBox|Help Chek')

    OF 1  ! Name: CHM   
          HELP(ChmFile4Test)

    OF 2  ! Name: MsgBox
          SYSTEM{PROP:HelpEngine,4} = ADDRESS(Help2MsgBoxCls)     !<-- CODE before any HELP()
          HELP('DevCon17.CHM.MsgBox')    
          
    OF 3  ! Name: Help Chek
          !IF EXISTS('HelpChek.TXT') THEN   !Suggest only use this Engine when run by Developers, Tester, Help Authors 
              HR# = HelpChekCls.Init(ChmFile4Test, True ) 
              IF HR# THEN Message('HelpChekCls.Init failed Result = ' & HR# &'||File='& HelpChekCls.ChmFileName ).
          !ELSE
          !   HELP(ChmFile4Test)             !End users
          !END 
    END !CASE
    Main() 
    RETURN
!----------------------------------------    
Main        PROCEDURE()

Window WINDOW('DevCon Help Chek Hook Test'),AT(,,176,172),CENTER,GRAY,SYSTEM, |
            FONT('Segoe UI',11),ICON(Icon:Help)
        BUTTON('Complex Help with Tab,Hlp() and Groups'),AT(12,13),USE(?Help4Btn)
        BUTTON('Simple Hello Help 1'),AT(47,32),USE(?Help1Btn)
        BUTTON('Help with Menu'),AT(54,52),USE(?Help2Btn)
        BUTTON('HLP ~Context vs Search'),AT(40,72),USE(?Help3Btn)
        BUTTON('Test CHM with .xCHMx extension'),AT(25,96),USE(?Help5Btn)
        BUTTON('List Engines'),AT(61,118),USE(?EngListBtn)
        BUTTON('Close'),AT(72,140),USE(?CloseBtn),STD(STD:Close)
    END
EngList     BSTRING    
    CODE  
    OPEN(Window)
    ACCEPT
        CASE ACCEPTED()
        OF ?Help1Btn    ; SimpleHelp1() 
        OF ?Help2Btn    ; SimpleHelpMenu2() 
        OF ?Help3Btn    ; HelpContextVsSearch() 
        OF ?Help4Btn    ; HelpWithParents()
        OF ?Help5Btn    ; ChmHelpMustHaveChmExtensionTest() 
        OF ?EngListBtn
            EngList ='Help Engines|' 
            LOOP e#=1 to 16 
               EngList=EngList &'|SYSTEM{{PROP:HelpEngine,' & e# &'} = '& HexLong(Address(SYSTEM{PROP:HelpEngine,e#})) 
            END
            Message(EngList)
        END 
    END

!----------------------
!Overview page:  ov_html_help_api_overview.htm
SimpleHelp1        PROCEDURE()

Window WINDOW('Hello Help 1'),AT(,,150,50),GRAY,HLP('~StartingTheDebugger.Htm'),FONT('Segoe UI',11)  
        BUTTON('Close'),AT(40,20),USE(?CloseBtn),STD(STD:Close)
        BUTTON('Help'),AT(80,20),USE(?HelpBtn),STD(STD:Help)
    END
    CODE
    !HELP('DevCon17.CHM') !<-- Set Help File name for RTL, done at start
    OPEN(Window)
    ACCEPT
    END


SimpleHelpMenu2        PROCEDURE()
Window WINDOW('Clarion Help Language - Menu'),AT(,,188,78),GRAY,SYSTEM,HLP('~overviewthedebuggingpro' & |
            'cess.htm'),FONT('Segoe UI',10,,FONT:regular)
        MENUBAR,USE(?MENUBAR1)
            ITEM('Exit!'),USE(?Exit),STD(STD:Close)
            MENU('&Help'),USE(?MENU1),MSG('Windows Help')
                ITEM('&Contents'),USE(?Helpindex),STD(STD:HelpIndex)
                ITEM('&Search for Help On...'),USE(?HelpSearch),STD(STD:HelpSearch)
                ITEM('&How to Use Help'),USE(?HelpOnHelp),STD(STD:HelpOnHelp)
            END
        END
        ENTRY(@s40),AT(2,7,181),USE(?Entry1),HLP('~cmd_hh_display_search.htm')
        BUTTON('What''s New...'),AT(2,31),USE(?WhatsNewBtn),TIP('Example of Help(,Topic)')
        BUTTON('Msg with Help'),AT(70,31),USE(?MsgBtn),TIP('Show Message with Help Button')
        BUTTON('Std:Help'),AT(141,31),USE(?StdHelpBtn),SKIP,STD(STD:Help),TIP('Displays Window Help,' & |
                ' not control')
    END
    CODE
    OPEN(Window)
    ACCEPT
        CASE ACCEPTED()
        OF ?WhatsNewBtn ; HELP(,'~NewDebuggerFeatures.htm')
        OF ?MsgBtn
                LOOP
                  CASE MESSAGE('You must start the Debugger!', |
                               'Error',,BUTTON:OK+BUTTON:Help)
                  OF BUTTON:Help
                     HELP(,'~StartingTheDebugger.htm')  
                     CYCLE !Show message again
                  END 
                UNTIL True
                    
        END 
    END


!----------------------
HelpContextVsSearch        PROCEDURE()  !Diff Between HLP('~Context') and HLP('Search') 

Window WINDOW('Hello Help 1'),AT(,,191,55),GRAY,HLP('~StartingTheDebugger.Htm'),FONT('Segoe UI',10)
        PROMPT('~Tilde Help:'),AT(1,2),USE(?PROMPT1)
        ENTRY(@s40),AT(42,2),USE(?Entry1),TIP('Push F1 Here to open ~Context')
        PROMPT('Search Help:'),AT(1,22),USE(?PROMPT2)
        ENTRY(@s40),AT(42,22),USE(?Entry2),HLP('StartingTheDebugger.Htm'),TIP('Push F1 here, opens Index tab to Searc' & |
                'h<13,10>No Tilde in HLP("StartingTheDebugger.htm")')
        BUTTON('Close'),AT(56,38),USE(?CloseBtn),STD(STD:Close)
        BUTTON('Help'),AT(96,38),USE(?HelpBtn),STD(STD:Help)
    END
    CODE
    OPEN(Window)
    ACCEPT
    END

!------------------------------------------------------------
HelpWithParents        PROCEDURE()  !Window with Tabs

Window WINDOW('Test Each Tab has Help and few Others '),AT(,,399,205),GRAY,SYSTEM, |
            HLP('~TheDebuggerWindows.htm'),FONT('Segoe UI',9,,FONT:regular), |
            ALRT(F2Key),RESIZE
        SHEET,AT(8,8,380,151),USE(?SHEET1)
            TAB('Procedures'),USE(?TAB1),HLP('~TheDebuggerWindows.htm#Procedures')
                CHECK('Focus on tab field'),AT(17,30),USE(?CHECK1)
                PROMPT('Break Point:'),AT(17,51),USE(?PROMPT1)
                ENTRY(@s20),AT(63,51,85),USE(?ENTRY1),HLP('~settingbreakpoints.htm')
                PROMPT('Press F1 with focus here and should display Break Point help'), |
                        AT(163,51),USE(?PROMPT2)
                GROUP('Has Help on RunningTheProgram'),AT(17,77,131),USE(?GROUP1), |
                        HLP('~RunningTheProgram.htm'),BOXED
                    CHECK('Focus here shows Group Help'),AT(25,91),USE(?CHECK6)
                END
                OPTION('Option - StartingTheDebugger'),AT(169,77,131,50),USE(?OPTION1), |
                        BOXED,HLP('~StartingTheDebugger.htm')
                    RADIO('Radio1'),AT(177,88),USE(?OPTION1:RADIO1)
                    RADIO('Radio2'),AT(177,100),USE(?OPTION1:RADIO2)
                    RADIO('Radio3'),AT(177,111),USE(?OPTION1:RADIO3)
                END
            END
            TAB('Globals'),USE(?TAB2),HLP('~TheDebuggerWindows.htm#Globals')
                CHECK('Focus - Keep focus on tab so TAB,HLP() is used'),AT(17,30), |
                        USE(?CHECK2)
            END
            TAB('Stack'),USE(?TAB3),HLP('~TheDebuggerWindows.htm#Stack')
                CHECK('Focus'),AT(17,30),USE(?CHECK3)
            END
            TAB('Source'),USE(?TAB4),HLP('~TheDebuggerWindows.htm#Source')
                CHECK('Focus'),AT(17,30),USE(?CHECK4)
            END
            TAB('Disassembly'),USE(?TAB5),HLP('~TheDebuggerWindows.htm#Disassembly')
                CHECK('Focus'),AT(17,30),USE(?CHECK5)
                CHECK('Bad HLP Page'),AT(17,50),USE(?CHECK5bad1),HLP('~ThisHLPdo' & |
                        'esNOTexist.htm')
                CHECK('Bad HLP Bookmark'),AT(17,70),USE(?CHECK5bad2),HLP('~TheDe' & |
                        'buggerWindows.htm#SourceBADDD')
            END
        END
        BUTTON('Close'),AT(315,165),USE(?CloseBtn),STD(STD:Close)
        BUTTON('Help'),AT(355,165),USE(?HelpBtn),STD(STD:Help),TIP('Shows window help')
        STRING('Give a Control Focus and press F1 to see Control or Parent HLP().'), |
                AT(8,163),USE(?STRING1)
        STRING('Press F2 to see GetHelpTopic() for Control with Focus'),AT(8,174), |
                USE(?STRING2)
        STRING('With Help Chek Engine be sure to press Ctrl + Shift + F1 or clic' & |
                'k Help button'),AT(8,187),USE(?STRING3),FONT(,,,FONT:bold)
    END
    CODE
    OPEN(Window)
    ACCEPT
        CASE EVENT()
        OF EVENT:AlertKey 
           CASE KEYCODE()
           OF F2Key ; Message('GetHelpTopic()=' & GetHelpTopic() &'|GetHelpTopic2()=' & GetHelpTopic2() )
           END
        END    

    END  
    
ChmHelpMustHaveChmExtensionTest   PROCEDURE()

Window WINDOW('Test CHM w/o .CHM Extension'),AT(,,150,50),GRAY,HLP('~StartingTheDebugger.Htm')
        BUTTON('Close'),AT(39,28),USE(?CloseBtn),STD(STD:Close)
        BUTTON('Help'),AT(79,28),USE(?HelpBtn),STD(STD:Help)
        STRING('CHM must have .CHM Extension to work'),AT(8,1),USE(?STRING1)
        STRING('x'),AT(8,11),USE(?STRING2)
    END
TestChmFn   STRING(32)    
    CODE
    TestChmFn = 'DevCon17.xCHMx'     !.xCHMx does NOT work
    COPY('DevCon17.CHM',TestChmFn) 
    IF ERRORCODE() OR ~EXISTS(TestChmFn) THEN STOP('Copy failed ' & ERROR() &'||'& TestChmFn) .
    HELP(TestChmFn) 
    OPEN(Window) 
    ?STRING2{PROP:Text}='Help file: '& TestChmFn
    ACCEPT
    END    
    
!==============================================================================  
GetHelpTopic      FUNCTION ()!,STRING     ! HLP() for Control with Focus in Active Window
OpenWndHlp  STRING(128)
FldFEQ      LONG,AUTO
  CODE                                                     ! Begin processed code
!Prop:Hlp is taken from the Control with focus when F1 was pressed. If that is empty then Parents (Group,Tab,Sheet,Window) are hunted up for Prop:Hlp
  
    FldFEQ=CHOOSE(~Field(),FOCUS(),FIELD())     !Field() with event, or Focus() of FEQ of last Event:Selected
    LOOP 20 TIMES                               !sanity check on infinite loop
         OpenWndHlp=FldFEQ{PROP:Hlp}
         IF OpenWndHlp OR 0=FldFEQ THEN BREAK.  !found Help, or 0=Window no more parents
         FldFEQ = FldFEQ{PROP:Parent}           !Group, Tab, Option, Window, etc may have the help
    END 
    RETURN CLIP(OpenWndHlp)

!==============================================================================  
GetHelpTopic2      FUNCTION (<LONG _CtrlFeq>,<WINDOW _WndRef>)!,STRING  ! HLP() from any Control and Window can be prior
OpenWndHlp  STRING(128)
FldFEQ      LONG,AUTO
  CODE                                                     ! Begin processed code
!Prop:Hlp is taken from the Control with focus when F1 was pressed. If that is empty then Parents (Group,Tab,Sheet,Window) are hunted up for Prop:Hlp
!CtrlFeq LONG   - omit for control with event i.e. FIELD() or FOCUS()
!WndRef  WINDOW - for when a new Window has opened so you can get Prior windows Help and assign to new window
!                 If you pass Window and Omit Ctrl you must do it before new ACCEPT loop starts
!Example
!   Open(ChildWindow)
!   ChildWindow{PROP:Hlp} = FctGetHelpTopic(,QuickWindow)  !get help from last active control 
  
    IF OMITTED(_WndRef) THEN                        !Did not pass (,Window) so work with currently open Window
        IF OMITTED(_CtrlFeq) THEN                   !Caller omitted control, so use current control with focus
           FldFEQ=CHOOSE(~Field(),FOCUS(),FIELD())  !Field() if in field event, Focus() of FEQ with last Event:Selected
        ELSE
           FldFEQ=_CtrlFEQ                          !Caller passed a Control FEQ he wants
        END                                                                    
        LOOP 20 TIMES                               !sanity check on infinite loop
             OpenWndHlp=FldFEQ{PROP:Hlp}
             IF OpenWndHlp OR 0=FldFEQ THEN BREAK.  !found Help, or 0=Window no more parents
             FldFEQ = FldFEQ{PROP:Parent}           !Group, Tab, Option, etc may have the help
        END 
    
    ELSIF ~OMITTED(_CtrlFeq) AND 0=_CtrlFEQ         !Passed (0, Window) i.e. no control
          OpenWndHlp = _WndRef{PROP:Hlp}            !so just get Window's Help

    ELSE                                            !Passed (?Control, Window) or (,Window)
        IF OMITTED(_CtrlFeq) THEN                   !Asked for current control, Only works until new window ACCEPT starts
           FldFEQ=CHOOSE(~Field(),FOCUS(),FIELD())  !Field() if in field event, Focus() of FEQ with last Event:Selected
        ELSE
           FldFEQ=_CtrlFEQ                          !Caller passed a (Control FEQ) he wants
        END                                                                    
        LOOP 20 TIMES                               !sanity check on infinite loop
             OpenWndHlp=_WndRef$FldFEQ{PROP:Hlp}
             IF OpenWndHlp OR 0=FldFEQ THEN BREAK.  !found Help, or 0=Window no more parents
             FldFEQ = _WndRef$FldFEQ{PROP:Parent}   !Group, tab, etc may have the help
        END         
    END 
    RETURN CLIP(OpenWndHlp)

!=================================================================================================
HexLong   PROCEDURE(LONG ALong)!,STRING
i       UNSIGNED,AUTO
A       UNSIGNED,AUTO
S       STRING(8),AUTO
DIGITS  STRING('0123456789ABCDEF')
  CODE
  A = ALong
  LOOP i=8 TO 1 BY -1 ; S[i] = DIGITS[BAND (A, 0Fh) + 1] ; A = BSHIFT (A, -4) ; END
  RETURN S &'h ' & ALong
 