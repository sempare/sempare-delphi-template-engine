# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2023 [Sempare Limited](http://www.sempare.ltd)

## Template Registry

The TTemplateRegistry utility class is the easiest way to maintain multiple templates in a project, spanning file and application resources.

TODO: 
- describe default behaviour
- customisation
    - naming overrides
    - live changes using auto refresh
    - loading strategies (file, resource, file else resource)
    - context

## Resource File Creation Tool

The Sempare.Template.RCGenerator.exe is available that can generate your .rc file for you quickly.

```
PS > .\Sempare.Template.RCGenerator.exe
Sempare.Template.RCGenerator.exe <rcfilename> <templatepath> [<ext>+]

<rcfilename> is the path to the filename that will be generated
<templatepath> is the path to the directory listing all the templates
<ext>+ is one one or more extensions to be included. By default: .ico, .png, .jpg, .jpeg, .webp, .tpl, .bmp, .gif, .wbmp
```