![delphi compatibility](https://img.shields.io/badge/delphi%20compatability-XE%204%20or%20newer-brightgreen)
![platform compatibility](https://img.shields.io/badge/platform-Android32%20%7C%20Android64%20%7C%20Linux64%20%7C%20macOS64%20%7C%20Win32%20%7C%20Win64-lightgrey)

# ![](./images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2025 [Sempare Limited](http://www.sempare.ltd)

Contact: <info@sempare.ltd>

License: [Apache v2.0](https://www.apache.org/licenses/LICENSE-2.0) or [Sempare Limited Commercial License](./docs/commercial.license.md)

Open Source: https://github.com/sempare/sempare-delphi-template-engine

*Questionaire*: https://docs.google.com/forms/d/e/1FAIpQLScioIiDxvsWK01fMFqYr9aJ6KhCGeiw4UaU_esGuztEE7vYwA/viewform

<a name="Introduction"><h2>Introduction</h3></a>

Template engines are used often in technology where text needs to be customised by substituting variables with values from a data source. Examples where this may take place:
- web sites using template engines (for server side scripting)
- code generation
- mail merge
- notification messages

The Sempare Template Engine is a small templating (scripting) engine for [Delphi](https://www.embarcadero.com/products/delphi) (Object Pascal) that allows for templates to be created easily and efficiently by providing a simple and easy to use API.

Example usage:
```
program Example;
uses
    Sempare.Template;
type
    TInformation = record
        name: string;
        favourite_sport : string;
        count : integer;
    end;
begin
    var tpl := Template.Parse(
       'My name is <% name %>.'#13#10 +
       'My favourite sport is <% favourite_sport %>.'#13#10 +
       'Counting... <% for i := 1 to count %><% i %><% betweenitems %>, <% end %>' +
       'Counting... <% for i := 1 to count ; print(i) ; betweenitems ; print(', '); onbegin; print('[ '); onend; print('] '); end %>'
    );
    var info : TInformation;
    info.name := 'conrad';
    info.favourite_sport := 'ultimate';
    info.count := 3;
    writeln(Template.Eval(tpl, info));
end.
```

The project allows for almost any type to be dereferenced within the template script.

In the example above, you can see that the '<%' start and '%>' end the scripting statement respectively. Within a scripting statement, you can reference variables, assign variables, use conditions, for and while loops, and include other templates.

**NOTE** In examples in this documentation I may use the latest Delphi syntax, e.g. inline variable declarations. This is not backward compatible as they were introduced in Delphi 10.2 and are used to shorten the code/examples being illustrated in the documentation. The codebase will attempt to be as backward compatible as possible.

<a name="CallToAction"><h2>Call to action</h3></a>

Please 'star' the project on github.

![](./images/sempare-template-engine-start-cta.png)


## Contents
- [Introduction](#Introduction)
- [Call To Action](#CallToAction)
- [Quickstart](#Quickstart)
- [Features](#Features)
- [Objectives](#Objectives)
- [Requirements](#Requirements)
- [Installation](#Installation)
    - [GetIt](#GetIt)
    - [Boss](#Boss)
    - [Delphinus](#DelphinusSupport)
    - [Manual Install](#ManualInstall)
- [Feedback](#Feedback)
- Template Language
  - [Statements](./docs/statements.md)
  - [Expressions](./docs/expressions.md)
  - [Builtin functions](./docs/builtin-functions.md)
  - [Builtin variables](./docs/builtin-variables.md)
  - [Comments](./docs/comments.md)
- [Template Registry](./docs/template-registry.md)
- Customisation
  - [Custom functions](./docs/custom-functions.md)
  - [Configuration](./docs/configuration.md)
- [Components](./docs/components.md)
- [Tricks](./docs/tricks.md)
- [Template Patterns](./docs/template-patterns.md)
- [Whitespace Removal](./docs/whitespace-removal.md)
- [Internals](./docs/internals.md)
- [Restrictions/Limitations/Known Bugs](./docs/restrictions.md)
- [License](#License)

<a name="Quickstart"><h2>Quickstart</h3></a>

There are a few ways to get started quickly.

- Look at the examples on how to do server side scripting using some popular web frameworks:
   - [WebBroker](https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Creating_WebBroker_Applications) [Demo](https://github.com/sempare/sempare-delphi-template-engine/tree/main/demo/WebBrokerStandalone)
   - [Horse](https://github.com/hashload/horse) [Demo](https://github.com/sempare/sempare-delphi-template-engine-horse-demo)
   - [Synopse mORMot1](https://synopse.info) [Demo](https://github.com/edwinyzh/simple-sempare-template-engine-mormot1-demo)
   - [DelphiMVCFramework](https://github.com/danieleteti/delphimvcframework) [Demo](https://github.com/danieleteti/delphimvcframework/tree/master/samples/serversideviews_sempare)

[Try the demo](./demo//SempareTemplatePlayground/README.md) if you want to dive in quick and play with the template engine.

<a name="Features"><h2>Features</h3></a>
- statements
  - if, elif, else statements
  - for and while statements
  - include statement
  - extends / block statements
  - with statement
  - function/method calls
- expressions
  - simple expression evaluation (logical, numerical and string)
  - variable definition
  - functions and methods calls
  - dereference records, custom managed records, classes, interfaces, arrays, JSON objects, TDataSet descendants and dynamic arrays
  - ternary operator
- safety
  - max run-time protection
- customisation 
  - custom script token replacement
  - add custom functions
  - strip recurring spaces and new lines
- lazy template resolution
- parse time evaluation of expressions/statements
- allow use of custom encoding (UTF-8 with BOM, UTF-8 without BOM, ASCII, etc)
- extensibile RTTI interface to easily dereference classes and interfaces (current customisations for ITemplateVariables, TDictionary, TJsonObject)

<a name="Objectives"><h2>Objectives</h3></a>

The Sempare Template Engine is not intended to be a fully featured general purpose programming language such as PHP where the script itself could be a self contained programming language (but it does have most of the features).

Sempare Template Engine aims to provide just enough functionality to allow you to easily work with the 'view' aspects of a template. Any enhanced functionality required from the scripting environment should be provided by the custom functions written in Object Pascal.

<a name="Requirements"><h2>Requirements</h3></a>

The template engine works with modern versions of [Delphi](https://www.embarcadero.com/products/delphi). 

Tests currently run using the DUnitX TestFramework.

An attempt has been made not to use the latest features to ease backward compatability. The following versions have been tested:

- Delphi XE 4 to XE 8
- Delphi 10.0 Seatle
- Delphi 10.1 Berlin
- Delphi 10.2 Tokyo
- Delphi 10.3.3 Rio
- Delphi 10.4 Sydney
- Delphi 11.0 to 11.3 Alexandria
- Delphi 12 Athens

There should be no platform specific restrictions.

Have a look at Sempare.Template.Compiler.inc. The following defines can be defined if appropriate:

- SEMPARE_TEMPLATE_NO_INDY - if Indy is not present. This is used to access an html encoder if TNetEncoding is not available.

<a name="Installation"><h2>Installation</h3></a>

<a name="GetIt"><h3>GetIt</h3></a>

The Sempare Template Engine for Delphi can be installed via the [Embarcadero GetIt manager](https://getitnow.embarcadero.com/?q=sempare&product=rad-studio)

This will add the *src* folder to the search path so you can start working immediately.

<a name="Boss"><h3>Boss</h3></a>

The Sempare Template Engine for Delphi can be installed via the [Boss](https://github.com/hashload/boss/releases) package manager.

Simply run: 
```
boss install sempare/sempare-delphi-template-engine
```

<a name="DelphinusSupport"><h3>Delphinus</h3></a>

The Sempare Template Engine for Delphi can be installed via the [Delphinus](https://github.com/Memnarch/Delphinus) package manager.

This will add the *src* folder to the search path so you can start working immediately.

<a name="ManualInstall"><h3>Manual Install</h3></a>

Start by adding the *src* folder to the Delphi search path. Otherwise, there are some projects you can use:

Open __Sempare.Template.Engine.Group.groupproj__ which will include:

- __Sempare.Template.Pkg.dproj__

  The core template project. (runtime)
     
- __Sempare.Template.Tester.dproj__

   180+ unit tests

- __demo\SempareTemplatePlayground\Sempare.TemplateEngine.Playground.dproj__

   The Sempare Template Playground demo which provides a rich experience for testing the various template language features.
   
<a name="Feedback"><h2>Feedback</h3></a>

You can raise issues on [GitHub](https://github.com/sempare/sempare.template) and they will be addressed based on priority.

Most features have some basic tests in place. If a bug is been discovered, please include a basic test/scenario replicating the issue if possible as this will ease the investigation process.

<a name="Contributions"><h2>Contributions</h3></a>

Review [contibution terms and conditions](./docs/CONTRIBUTION.pdf) to contribute to the project.

Please followow the following steps when adding a feature or making an update:
- clone the repository
- create a feature branch
- create a pull request to the 'dev' branch

The deployment process is based off the dev branch. Once we decide to push a new version, we will merge 'dev' into 'main'.

<a name="License"><h2>License</h3></a>

The Sempare Template Engine is dual-licensed. You may choose to use it under the restrictions of the [Apache v2.0](https://www.apache.org/licenses/LICENSE-2.0) at
no cost to you, or you may license it for use under the [Sempare Limited Commercial License](./docs/commercial.license.md)

The dual-licensing scheme allows you to use and test the library under the appropriate license.

A commercial licence grants you the right to use Sempare Template Engine in your own applications, royalty free, and without any requirement to disclose your source code nor any modifications to
Sempare Template Engine or to any other party. A commercial license helps to ensure the project is maintained with continuous integration, patches, etc.

A support fee of $70 per developer is appreciated.

The following payment links allow you to quickly subscribe. Please note that the initial license and support links are seperate.
- [Initial License Fee](https://buy.stripe.com/aEU7t61N88pffQIdQQ)
- [Annual Support License Fee](https://buy.stripe.com/00g8xa4ZkbBr480145)

The following payment links are available for site licenses. Please note that the initial license and support links are seperate.
- [Initial Site License Fee](https://buy.stripe.com/eVa00E77s7lbfQIaEG)
- [Annual Support License Fee](https://buy.stripe.com/6oE14I4Zk6h7fQI003)

Please send an e-mail to info@sempare.ltd to request an invoice which will contain alternative payment details.

Support and enhancement requests submitted by users that pay for support will be prioritised. New developments may incur additional costs depending on time required for implementation.

