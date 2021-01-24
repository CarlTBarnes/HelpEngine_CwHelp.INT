# Get Help File Name

The Clarion language sets the Help file name using the HELP() command but offers no documented way of reading that value. The RTL does export the below undocumented function. I have tested it and it works fine. A small test project `GetHelpFileName.cwproj` is in this folder shows it working.

```Clarion
  MODULE('CLARUN') !C7+
     GetHelpFileRTL(*CSTRING HelpFN),NAME('_WslHelp$GetHelpFile@FiPc')
  END

GetHelpFile FUNCTION(),STRING
CHelpFN CSTRING(261)
   CODE
   GetHelpFileRTL(CHelpFN)
   RETURN CHelpFN
```

Getting the current help file is handy when you want to temporarily change the help file for one window, then change it back. This is done currently in the Report Previewers of CPCS and RPM so they can use their own help files to document the many previewer features. They do it in template code by reading the APP's Help file name and generating a HELP(AppHelpFileName) at report procedure close. That is undesirable because you must specify the help file name in every APP that uses a report. Ideally you want the help file name only specified in the EXE APP. This would interfere with hooking help using the HelpEngine because in most cases it will rely on a different file name and/or extension. This method if using the template help file would not work for a Class writer. The best way to temporarily change the help file is to get the current help file name from the RTL using _WslHelp$GetHelpFile() then set it back using HELP().
