# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2021 [Sempare Limited](http://www.sempare.ltd)

## Stack Frames

Stack frames are created when a template is being evaluated. This will contain the _ value. 

'if', 'for', 'with' and 'while' statements also define stack frames to contain variables defined within their scope. 

When a template is being processed, a root stack frame is created. All variables in the _context_ are added to this stack frame. 

When you provide an input variable to _Template.Eval()_, the _ variable is also set.
