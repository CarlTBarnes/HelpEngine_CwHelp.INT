  MEMBER
HlpEngMaxIndex  EQUATE(16)          !Max SYSTEM{PROP:HelpEngine,Index},  Undocumented found by simple tests that {,17} fails

extCHEK         EQUATE('CHEK')      !EXT dded to Chm so ChmCHEK or HlpCHEK or any XxxxCHEK

  INCLUDE('CbHelpEngChek.inc'),ONCE 
  INCLUDE('KEYCODES.CLW')

HlpEngTlz   CLASS    !Help Engine Tools, a Helper Class for dealing with Help Engines
FindMatch     PROCEDURE(STRING HelpFileExten),LONG      !Search 16 slots for Match(EXT) and return Index
FindEngine    PROCEDURE(*HelpEngine Engine2Find),LONG   !Search 16 slots for = HelpEngine returns INDEX 
FindFreeIndex PROCEDURE(LONG StartIndex=3),LONG         !Search 16 slots and return an Empty Index 
HCmdName      PROCEDURE(LONG HCmd),STRING    
FileExtOnly   PROCEDURE(string InFN),string       !Find EXT x:\xxx\xxx\flename.EXT
            END

  MAP 
HexLong             PROCEDURE(LONG ALong),STRING        !####h ###
CtrlShiftShowHelp   PROCEDURE()
    MODULE('RTL') 
ClaFieldName     PROCEDURE(SIGNED Feq),*CSTRING,NAME('Cla$FIELDNAME'),DLL(dll_mode)
    END    
    MODULE('win32')
        OutDbg(*cstring dMsg),PASCAL,RAW,NAME('OutputDebugStringA'),dll(1)
     END 
  END 

 !============================================================== 
 !Note labels say "Chm" but it can be any kind of Help File, HLP, WWW, etc
CbHelpChekClass.Init  PROCEDURE(STRING InChmFile, BOOL ShowDbgMsg=0)!,LONG,PROC,VIRTUAL 
cDbg        CSTRING(512)
    CODE
    SELF.ChmFileName = CLIP(InChmFile) 
    SELF.ChmFileEXT  = UPPER(HlpEngTlz.FileExtOnly(InChmFile))    !Might not be CHM, could be HLP or WWW or ANY    
    !IF ~EXISTS(InChmFile) THEN RETURN -2.              !This Engine can be used to hook any Engine which Web Help would not have a file on disk
    HELP(InChmFile)                                     !Be sure RTL loads CHM Engine (or whatever engine the file used e.g. HLP or WWW
    !SELF.ChmEngIndex = 1   !First thing could assume CHM will be #1, but better to Search the 16 for a Match(CHM)
    SELF.ChmEngIndex = HlpEngTlz.FindMatch(SELF.ChmFileEXT) 
    IF ~SELF.ChmEngIndex THEN  
        RETURN 1                                        !#1 problem - No Engine for CHM, or more accurately Help File EXT
    END
    SELF.ChmEngine &= SYSTEM{PROP:HelpEngine,SELF.ChmEngIndex}   !Save the Engine to open CHM Help in HelpCmd() for ChmCHEK
      
    !--Setup HELP(CHEK) as MyHelp.ChmCHEK
    SELF.ChekFileName = UPPER(SELF.ChmFileName & extCHEK)  !MyHelp .CHMCHEK
    SELF.ChekFileExt  = SELF.ChmFileEXT & extCHEK          !        CHMCHEK  
    SELF.ChekEngIndex = Hlpengtlz.FindEngine(SELF.HelpEngine)   !Was this Engine Previously Inited i.e. Indexed?
    IF ~SELF.ChekEngIndex THEN                                  !Nope, we need to find a free Index
        SELF.ChekEngIndex =Hlpengtlz.FindFreeIndex(5)           !Find a free slot (could assume 5 to 16 are free)
        IF ~SELF.ChekEngIndex THEN
           RETURN 2                                             !#2 problem - No Engine free, very unlikely
        ELSE
           SYSTEM{PROP:HelpEngine,SELF.ChekEngIndex} = ADDRESS(SELF.HelpEngine) 
        END
    END 
    HELP(SELF.ChekFileName)
    SELF.IsInited = True 

    cDbg ='CbHelpChekClass.Init( ' & InChmFile & | 
            '<13,10>.ChmFileName='    & SELF.ChmFileName & |
            '<13,10>.ChmFileExt='     & SELF.ChmFileEXT & |
            '<13,10>.ChmEngine = '    & HexLong(ADDRESS( SELF.ChmEngine)) & |
            '<13,10>.ChmEngIndex = '  & SELF.ChmEngIndex & | 
            '<13,10>' & |
            '<13,10>.ChekFileName='   & SELF.ChekFileName & |
            '<13,10>.ChekFileExt='    & SELF.ChekFileExt & |  !            '<13,10>.ChekEngine = '   & HexLong(ADDRESS( SELF.ChekEngine)) & |
            '<13,10>.ChekEngIndex = ' & SELF.ChekEngIndex & |
            '<13,10>.HelpEngine = '   & HexLong(ADDRESS( SELF.HelpEngine)) & |
            '<13,10>'
    OutDbg(cDbg) 
    IF ShowDbgMsg THEN Message(cDbg,'CbHelpChekClass.Init',,,,MSGMODE:CANCOPY ).
    RETURN 0    
!--------------------------------------------
CbHelpChekClass.Kill            PROCEDURE()     !If IsInited sets Help(ChmFilename) and removes Index
    CODE
    IF SELF.IsInited THEN
       SELF.IsInited = 0
       HELP(SELF.ChmFileName)       !Set HELP() to normal file so it might work
       IF SELF.ChekEngIndex THEN
          SYSTEM{PROP:HelpEngine,SELF.ChekEngIndex} = 0
       END 
    END
    RETURN
!=================================================================================    
CbHelpChekClass.HelpEngine.Match    PROCEDURE(CONST *CSTRING _HelpFileEXT)!,BOOL
    CODE
    RETURN SELF.MATCH(_HelpFileEXT) 
!------------------------------------    
CbHelpChekClass.HelpEngine.HelpCmd  PROCEDURE(UNSIGNED _HWnd, <CONST *CSTRING _HelpFile>, UNSIGNED _HCmd, LONG _Data=0)!,BOOL,PROC
    CODE
    RETURN SELF.HelpCmd(_HWnd,_HelpFile,_HCmd,_Data)
!=================================================================================    
!Called by HelpEngine Interface to do the Real Work
CbHelpChekClass.Match PROCEDURE(CONST *CSTRING _HelpFileEXT)!,BOOL,VIRTUAL,PROTECTED
    CODE
    IF UPPER(_HelpFileEXT)=SELF.ChekFileEXT THEN 
       RETURN True 
    END     
    RETURN False
!----------------------------------------------------------    
CbHelpChekClass.HelpCmd PROCEDURE(UNSIGNED _HWnd, <CONST *CSTRING _HelpFile>, UNSIGNED _HCmd, LONG _Data=0)!,BOOL,PROC,VIRTUAL,,PROTECTED
cChmFile        CSTRING(261)    !_HelpFile with CHEK cutoff of ChmChek
LnStr           LONG 
cDbg            CSTRING(512)
cData           &CSTRING
cBlank          CSTRING(2)   
RetBool         BOOL 
    CODE
    !#1 - Send HelpCmd() parms to DebugView so can see Topic.HTM, helps Author, Developer or Tester
    cData &= cBlank
    IF OMITTED(_HelpFile) THEN                      !No File passed, that's odd
       cDbg='ChekHelp ' & HlpEngTlz.HCmdName(_HCmd) &'  '& _Data  &'  ?.Chm?NoFilePassed?' 
       OutDbg(cDbg)
       RETURN SELF.ChmEngine.HelpCmd(_HWnd, , _HCmd, _Data)       
    END 
    LnStr=LEN(_HelpFile)                            !File passed  .ChmChek
    cChmFile=SUB(_HelpFile,1,LnStr - LEN(extCHEK))  !  Change to  .Chm     to pass to ChmEngine
    IF _HCmd=HCMD_SHOWTOPIC           |             !Cmd is F1 Showing HLP()
    AND (_Data > 0FFFFh OR _Data < 0) THEN          !0 to 64KB is invalid pointer
       cData &= (_Data)                             !defef pointer to CSTRING
    END
    cDbg='ChekHelp ' & HlpEngTlz.HCmdName(_HCmd) &' '& CHOOSE(~cData, ''&_Data, cData) &'  '&  cChmFile  !Choose(,Long,String) returns Long so ''& makes (,Strubg,String)
    OutDbg(cDbg)        

    !#2 - Open Help Viwer by calling the CHM Engine found in .Init() and pass adjusted file name + original parms
    RetBool = SELF.ChmEngine.HelpCmd(_HWnd, cChmFile , _HCmd, _Data)   
   
    !#3 - If Ctrl+Shift are down open a Window that shows all the HLP on the Under Window
    IF BAND(KeyState(),0700h)=0300h THEN
       HELP(cChmFile)
       CtrlShiftShowHelp() 
       HELP(SELF.ChekFileName)      
    END 
    RETURN RetBool

!============================= Help Engine Tools ===============================
!Find an Engine that Match()s on EXTension and return its SYSTEM HlpEng Index  
HlpEngTlz.FindMatch   PROCEDURE(STRING HelpFileExten) !,LONG returns INDEX of Engine.Match(EXT)
cHelpEXT        CSTRING(16),AUTO
Engine2Test     &HelpEngine 
EngAddress      LONG,AUTO 
Index           LONG,AUTO 
IndexMatched    LONG
    CODE
    cHelpEXT = CLIP(UPPER(HelpFileExten))
    LOOP Index=1 TO HlpEngMaxIndex                      !Look thru 16 engines
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
!Find an Engine that is same Address as Engine2Find and return its  SYSTEM HlpEng Index, to see if Engine already indexed  
HlpEngTlz.FindEngine   PROCEDURE(*HelpEngine Engine2Find) !,LONG returns INDEX of 
Index           LONG,AUTO 
IndexMatched    LONG
    CODE
    LOOP Index=1 TO HlpEngMaxIndex                      !Look thru 16 engines
         IF SYSTEM{PROP:HelpEngine,Index} = ADDRESS(Engine2Find) THEN       !Index assigned Engine? 
            IndexMatched = Index
            BREAK
         END
    END
    RETURN IndexMatched
!--------------------------------------------------
!Find a SYSTEM HE Index that is zero so I can use it    
HlpEngTlz.FindFreeIndex   PROCEDURE(LONG StartIndex=3) !,LONG Search 16 slots for an Empty
EngAddress      LONG,AUTO 
Index           LONG,AUTO 
ReturnIndex     LONG
    CODE
    LOOP Index=1 TO HlpEngMaxIndex                      !Look thru 16 engines
         EngAddress = SYSTEM{PROP:HelpEngine,Index} 
         IF EngAddress <> 0 THEN CYCLE.                 !This Index is used
         ReturnIndex = Index
         IF Index >= StartIndex THEN BREAK.             !Leave 1 and 2 open for CHM and HLP (no real need I just like to)
    END
    RETURN ReturnIndex
!--------------------------------------------------    
HlpEngTlz.HCmdName    PROCEDURE(LONG HCmd) !,STRING
    CODE 
    RETURN CHOOSE(HCmd+1, 'HCMD_CLOSE','HCMD_SHOWTOPIC','HCMD_SHOWINDEX','HCMD_SHOWTOC','HCMD_SHOWSEARCH','HCMD_HELPONHELP','HCMD_' & HCmd)
!-------------------------------------------------------
HlpEngTlz.FileExtOnly     PROCEDURE(string InFN)!,string      !Find EXT x:\xxx\xxx\flename.EXT
X   LONG,AUTO
Prd LONG 
    CODE
    LOOP X=SIZE(InFN) TO 1 BY -1  
         IF InFN[X]='\' THEN BREAK. 
         IF InFN[X]='.' ; Prd=X ; BREAK. 
    END 
    RETURN CHOOSE(Prd=0,'',CLIP(SUB(InFN,Prd+1,20)))
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


!================================== Ctrl+Shift Help Window =====================================
! Scan the Under Window Fields and Show Fields with Help
! Can double click on lines to open help
! Could adapt this to let help author assign help topics to controls. They would have to be saved to a file ...
!   ... and help topics assigned from a file. And developer cannot chnage USE strings willie-nillie

CtrlShiftShowHelp PROCEDURE() 
FieldQ    QUEUE,PRE(FldQ)
FEQ         LONG          ! FldQ:FEQ      
ParFEQ      LONG          ! FldQ:ParentFEQ
TypeName    STRING(16)    ! FldQ:TypeName 
Level       LONG          ! FldQ:Level 
FeqLabel    STRING(32)    ! FldQ:FeqLabel 
PropText    STRING(48)    ! FldQ:PropText 
Hlp         STRING(256)   ! FldQ:Hlp       !Help that shows in List
HlpProp     STRING(256)   ! FldQ:HlpRaw    !Original PROP:Hlp   
          END  
ParentG   GROUP(FieldQ),PRE(ParG)
          END 
JustTreeQ  QUEUE(FieldQ),PRE(JstTreQ)   !tree
           END
JustHelpQ  QUEUE,PRE(JstHlpQ)                      !Just Help List
TypeName    STRING(16)    ! JstHlpQ:TypeName  
FeqLabel    STRING(32)    ! JstHlpQ:FeqLabel 
PropText    STRING(48)    ! JstHlpQ:PropText 
Hlp         STRING(512)   ! JstHlpQ:Hlp      
        END  

FldCls  CLASS
IsFrame     BOOL
LoadAllQs   PROCEDURE()  !Load FieldQ then others
Add1        PROCEDURE(LONG _FEQ, STRING _Type, STRING _Label)
TakeAlert   PROCEDURE()
TakeNSML2   PROCEDURE()
TakeNSML2   PROCEDURE(*QUEUE TheQ, *STRING TheHlp)
        END
CbWndControlCls CLASS
TypeName        PROCEDURE(LONG _Type),STRING
GetHelp         PROCEDURE(LONG _FEQ),STRING
HasHelp         PROCEDURE(LONG _FEQ),BOOL
            END        

Window WINDOW('Help Chek:'),AT(,,357,185),GRAY,SYSTEM,FONT('Microsoft Sans Serif',10), |
            ALRT(CtrlC), ALRT(CtrlShiftC),RESIZE,MAX
        STRING('Double Click to Open Help. Ctrl+C to Copy 1 line, +Shift All.'), |
                AT(144,2)
        SHEET,AT(2,2),FULL,USE(?SHEET1),NOSHEET,BELOW
            TAB('Help'),USE(?TAB:Help)
                LIST,AT(2,20),FULL,USE(?LIST:JustHelpQ),VSCROLL,FROM(JustHelpQ), |
                        FORMAT('34L(2)|M~Type~@s16@60L(2)|M~Label~@s32@60L(2)|M~' & |
                        'Text~@s48@60L(2)|M~Help~@s255@')
            END
            TAB('Tree'),USE(?TAB2T)
                LIST,AT(2,20),FULL,USE(?LIST:Tree:JustTreeQ),VSCROLL,FROM(JustTreeQ), |
                        FORMAT('75L(2)|MT~Type~@s16@#3#60L(2)|M~Label~@s32@60L(2' & |
                        ')|M~Text~@s48@60L(2)|M~Help~@s255@')
            END
            TAB('Full Tree'),USE(?TAB:FullTree)
                LIST,AT(2,20),FULL,USE(?LIST:Tree:FieldQ),VSCROLL,FROM(FieldQ), |
                        FORMAT('75L(2)|MT~Type~@s16@#3#60L(2)|M~Label~@s32@60L(2' & |
                        ')|M~Text~@s48@60L(2)|M~Help~@s255@')
            END
            TAB('Debug'),USE(?TAB:DebugQ)
                LIST,AT(2,20),FULL,USE(?LIST:FieldQ),VSCROLL,FROM(FieldQ), |
                        FORMAT('20L(2)|M~FEQ~@n-7@20L(2)|M~PFeq~@n-7@40L(2)|M~Ty' & |
                        'pe~@s16@20L(2)|M~Lvl~@n-3@60L(2)|M~Label~@s32@60L(2)|M~' & |
                        'Text~@s48@60L(2)|M~Help~@s255@')
            END
        END
    END
UnderWndText    PSTRING(64),AUTO    
UnderWndHlp     STRING(256),AUTO    
    CODE
    FldCls.LoadAllQs()            
    UnderWndText = 0{PROP:Text}
    UnderWndHlp  = 0{PROP:HLP}      !Simple HLP on this window is Under Window
    SYSTEM{PROP:PropVScroll}=1 
    OPEN(Window)                    !---- OPEN the Class Window 
    0{PROP:Text} = 0{PROP:Text} &' WINDOW("' & UnderWndText &'")' 
    0{PROP:HLP} = UnderWndHlp
    ?Sheet1{PROP:TabSheetStyle}=1   
    ACCEPT 
        CASE EVENT()         
        OF EVENT:AlertKey
            FldCls.TakeAlert()
        OF EVENT:NewSelection 
           IF KEYCODE()=MouseLeft2 THEN FldCls.TakeNSML2().
        END
    END
    CLOSE(Window)
    RETURN    
!-----------------------------------
FldCls.TakeAlert   PROCEDURE()
    CODE
    message('Todo ,ALRT(CtrlC),ALRT(CtrlShiftC)')
    RETURN
!-----------------------------------
FldCls.TakeNSML2   PROCEDURE()
    CODE
    CASE FIELD()
    OF ?LIST:JustHelpQ       ; SELF.TakeNSML2(JustHelpQ, JustHelpQ.Hlp)
    OF ?LIST:Tree:JustTreeQ  ; SELF.TakeNSML2(JustTreeQ, JustTreeQ.Hlp )
    OF ?LIST:Tree:FieldQ     ; SELF.TakeNSML2(FieldQ, FieldQ.HlpProp)
    OF ?LIST:FieldQ          ; SELF.TakeNSML2(FieldQ, FieldQ.HlpProp)
    END
    RETURN
FldCls.TakeNSML2   PROCEDURE(*QUEUE TheQ, *STRING TheHlp)
TheLIST     LONG,AUTO
    CODE
    TheLIST = FIELD()
    GET(TheQ,CHOICE(TheLIST)) ; IF ERRORCODE() THEN RETURN.
    HELP(,TheHlp)
    RETURN 
!----------------------------    
FldCls.LoadAllQs  PROCEDURE() 
X           LONG,AUTO
Feq1        LONG,AUTO
cLabel      CSTRING(48) 
LevelLast   LONG(1)
WindowHLP   STRING(512),AUTO
    CODE
    IF 0{PROP:Type}=CREATE:Application THEN
        SELF.Add1(0,'APPLICATION','Application')
        SELF.IsFrame = True
        !The Tree is wrong for Frame due to negative FEQs. I'll leave it to you if you want to make it right
    ELSE 
       SELF.Add1(0,'WINDOW','Window')        !WINDOW comes first
    END
    WindowHLP = 0{PROP:HLP}
    Feq1 = 0{PROP:NextField}          
    LOOP WHILE Feq1                       !Loop all the fields on Window
      IF SELF.IsFrame |                   !Frame < 0
      OR (Feq1 > 0 AND Feq1{PROP:Parent} >=0 ) THEN    !Toolbar Buttons are > 0 but Parent Toolbar < 0
         cLabel = ClaFieldName(Feq1)
         SELF.Add1(Feq1,'todo',cLabel)    !Field added to Field Queue
      END   
      Feq1 = 0{PROP:nextfield, Feq1}
    END
    SORT(FieldQ,FldQ:Feq)

    LOOP X = 1 TO RECORDS(FieldQ)          !Pretty up FieldQ() and add to other queues
        GET(FieldQ,X)
        IF FldQ:FEQ=0 THEN CYCLE.       !Window=0 is all setup so skip it
        FldQ:FEQ = FldQ:ParFEQ
        GET(FieldQ,FldQ:FEQ)            !Get my Parent (erasing field data
        ParentG = FieldQ                !Save Parent info in Group
        IF ERRORCODE() THEN             !Should never happen that I can image
            ParentG = FieldQ            !   I guess Take myself
            ParG:Level = LevelLast - 1  !   Make level same as previous so tree not mangled
        END
        
        GET(FieldQ,X)                   !Reload the field
        FldQ:Level  = ParG:Level + 1
        IF FldQ:Hlp = ParG:Hlp OR FldQ:Hlp = WindowHLP THEN   !ITEM parent MENU doesn't have HLP so must compare to Window HLP
           CLEAR(FldQ:Hlp)
        END   
        PUT(FieldQ)
        LevelLast = FldQ:Level
    END     

    LOOP X = 1 TO RECORDS(FieldQ)        !Find those with Just Help
        GET(FieldQ,X)
        IF FldQ:FEQ <> 0 AND ~FldQ:Hlp THEN CYCLE.
        JustHelpQ :=: FieldQ
        ADD(JustHelpQ)
    END 
    LOOP X = 1 TO RECORDS(FieldQ)        !Just Help, but in Tree
        GET(FieldQ,X)
        IF FldQ:FEQ <> 0 AND ~FldQ:Hlp THEN CYCLE.
        JustTreeQ :=: FieldQ             !TODO cannot jump levels, right?
        ADD(JustTreeQ)
    END     
    RETURN 
    
FldCls.Add1 PROCEDURE(LONG _FEQ, STRING _Type, STRING _Label) 
    CODE
    FldQ:FEQ       = _FEQ         
    FldQ:ParFEQ    = _FEQ{PROP:Parent}
    FldQ:Level     = CHOOSE(0=FldQ:ParFEQ,1,0) 
    FldQ:TypeName  = UPPER(CbWndControlCls.TypeName( _FEQ{PROP:Type} ))
    FldQ:FeqLabel  = _Label
    FldQ:PropText  = _FEQ{PROP:Text}
    FldQ:Hlp       = CbWndControlCls.GetHelp(_FEQ) 
    FldQ:HlpProp   = _FEQ{PROP:Hlp}  !All have HLP() even if not in Window def
    ADD(FieldQ)
    RETURN 
!=====================================================
CbWndControlCls.TypeName PROCEDURE(LONG _Type)!,STRING
PreFix  PSTRING(24)
N       PSTRING(16)
FeqTyp  LONG,AUTO
    CODE
    FeqTyp = _Type
    IF BAND(FeqTyp,CREATE:toolbar ) THEN       !CREATE:toolbar   EQUATE (128)
       IF FeqTyp = CREATE:toolbar THEN RETURN 'ToolBar'.
       FeqTyp -= CREATE:toolbar                !I don't think this can happen
       PreFix='TLB-'
    END 
    IF BAND(FeqTyp,0100h) THEN   !CREATE:sublist EQUATE (CREATE:list + 0100H)  ! list part of a DROP or COMBO'
       IF FeqTyp = 0100h THEN RETURN 'SubList'. !I don't think this can happen
       FeqTyp -= 0100h
       PreFix='SUB-'
    END
    N=CHOOSE(FeqTyp,'SString','String','Image','Region','Line','Box','Ellipse','Entry','Button','Prompt',|          !1-10
            'Option','Check','Group','List','Combo','Spin','Text','Custom','Menu','Item',|                  !11-20
            'Radio','Menubar','Create23','Application','Window','Report','Header','Footer','Break','Form',| !21-30
            'Detail','Ole','DropList','DropCombo','Progress','Create36','Sheet','Tab','Panel','TextRTF',|   !31-40
            'Text1L','Check3','Type?' & FeqTyp)  !Assumes Type Values will not change. Not ideal but this is a programmer tool
       RETURN (PreFix & N) 
!-----------------------------------------------------------
CbWndControlCls.GetHelp PROCEDURE(LONG _FEQ)!,STRING 
    CODE
    RETURN CHOOSE(~SELF.HasHelp(_FEQ),'', _FEQ{PROP:Hlp})
!-----------------------------------------------------------    
CbWndControlCls.HasHelp PROCEDURE(LONG _FEQ)!,BOOL
FeqTyp  LONG,AUTO
RetBool BOOL !Def=False
    CODE
    FeqTyp = _FEQ{PROP:Type}    
    IF BAND(FeqTyp,CREATE:toolbar) THEN !CREATE:toolbar   EQUATE (128)
       FeqTyp -= CREATE:toolbar 
    END 
    IF BAND(FeqTyp,0100h) THEN   !CREATE:sublist EQUATE (CREATE:list + 0100H)  ! list part of a DROP or COMBO'
       FeqTyp = CREATE:Prompt !Trick to RETURN False
    END
    
    CASE FeqTyp
    OF CREATE:Prompt   !; RetBool = False
    
    OF   CREATE:Application  OROF CREATE:Group     OROF CREATE:Rtf
    OROF CREATE:Button       OROF CREATE:Item      OROF CREATE:SingleLine
    OROF CREATE:Check        OROF CREATE:List      OROF CREATE:Spin
    OROF CREATE:Combo        OROF CREATE:Ole       OROF CREATE:State3
    OROF CREATE:DropCombo    OROF CREATE:Option    OROF CREATE:Tab
    OROF CREATE:Droplist     OROF CREATE:Progress  OROF CREATE:Text
    OROF CREATE:Entry        OROF CREATE:Radio     OROF CREATE:Window
          RetBool = True
    END
    RETURN RetBool    