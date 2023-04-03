# WebBroker Standalone Demo

# Running the Demo

1. Open WebBrokerStandalone8080.dproj
2. In the IDE, run (F9)
3. In the console that starts up, type: start
4. In a browser, goto http://localhost:8080

# Templates directory

The templates directory has a few template files defined.

You need to pay attention to template.rc. The templates use names within the resource file, and not what is on the filesystem.

# Helper units

- DynForm.pas - example utility functionality
- TemplateRegistry.pas - utility functionality to load templates from the application resources

# WebModuleUnit1.pas

This is the web module that has been slightly modified for the example.

It uses DynForm and TemplateRegistry.

You should observe that templates are loaded using:
      TTemplateRegistry.Instance.Eval(templatename, data);
      
# Limitations

This demo shows how to use resources that are statically compiled into the application. 

Contact info@sempare.ltd if you need information or training.