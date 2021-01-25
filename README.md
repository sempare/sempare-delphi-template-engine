[![](https://tokei.rs/b1/github/sempare/sempare-delphi-template-engine?category=lines)](https://github.com/sempare/sempare-delphi-template-engine) [![](https://tokei.rs/b1/github/sempare/sempare-delphi-template-engine?category=code)](https://github.com/sempare/sempare-delphi-template-engine) [![](https://tokei.rs/b1/github/sempare/sempare-delphi-template-engine?category=files)](https://github.com/sempare/sempare-delphi-template-engine)

# ![](./images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2021 [Sempare Limited](http://www.sempare.ltd)

Contact: <info@sempare.ltd>

License: [GPL v3.0](https://www.gnu.org/licenses/gpl-3.0.en.html) or [Sempare Limited Commercial License](./docs/commercial.license.md)

Open Source: https://github.com/sempare/sempare-delphi-template-engine

## Contents
1. [Introduction](#Introduction)
2. [Quickstart](#Quickstart)
3. [Features](#Features)
4. [Objectives](#Objectives)
5. [Requirements](#Requirements)
6. [Installation: GetIt](#Installation:_GetIt)
7. [Installation: Delphinus](#Installation:_Delphinus_Support)
8. [Feedback](#Feedback)
9. [Statements](./docs/statements.md)
10. [Expressions](./docs/expressions.md)
11. [Builtin functions](./docs/builtin-functions.md)
12. [Builtin variables](./docs/builtin-variables.md)
13. [Custom functions](./docs/custom-functions.md)
14. [Configuration](./docs/configuration.md)
15. [Components](./docs/components.md)
16. [Tricks](./docs/tricks.md)
17. [Template Patterns](./docs/template-patterns.md)
18. [Internals](./docs/internals.md)
19. [Restrictions/Limitations/Known Bugs](./docs/restrictions.md)
20. [Todo](./docs/todo.md)
21. [License](#License)

## Introduction

Template engines are used often in in the technology where text needs to be customised by substituting variables with values from a data source. Examples where this may take place:
- web sites using template engines (or languages)
- code generation
- mail merge 
- notification messages 

The Sempare Template Engine is a small templating engine for [Delphi](https://www.embarcadero.com/products/delphi) (Object Pascal) that allows for templates to be created easily and efficiently by providing a simple and easy to use interface.

Example usage:
```
program Example;
uses
    Sempare.Template;
type
    TInformation = record
        name: string;
        favourite_sport : string;
    end;
begin
    var tpl := Template.parse('My name is <% name %>. My favourite sport is <% favourite_sport %>.');
    var information : TInformation;
    information.name := 'conrad';
    information.favourite_sport := 'ultimate';
    writeln(Template.eval(tpl, information));	
end.
```

In the example above, you can see that the '<%' start and '%>' end the scripting statement respectively. Within a scripting statement, you can reference variables, assign variables, use conditions, for and while loops, and include other templates.

**NOTE** In examples in this documentation I may use the latest Delphi syntax, e.g. inline variable declarations. This is not backward compatible as they were introduced in Delphi 10.2 and are used to shorten the code/examples being illustrated in the documentation. The codebase will attempt to be as backward compatible as possible.

## Quickstart

[Try the demo](./demo/VelocityDemo/README.md) if you want to dive in quick and play with the template engine.

Quick tutorials on [You Tube](https://www.youtube.com/playlist?list=PLjjz4SuVScHreGKEInvrjPtLPMBU6l130). 
The playlist has a few videos that are very short (most less than a minute - blink and they are done). You can drag the slider in the videos if you miss something or refer to the rest of the documentation. 


## Features
- statements
  - if, elif, else statements
  - for and while statements
  - include statement
  - with statement
  - function/method calls
- expressions
  - simple expression evaluation (logical, numerical and string)
  - variable definition
  - functions and methods calls
  - dereference records, classes, arrays, JSON objects, TDataSet descendants and dynamic arrays
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

## Objectives

The Sempare Template Engine is not intended to be a fully featured general purpose programming language such as PHP where the script itself could be a self contained programming language (but it does have most of the features).

Sempare Template Engine aims to provide just enough functionality to allow you to easily work with the 'view' aspects of a template. Any enhanced functionality required from the scripting environment should be provided by the custom functions written in Pascal.

## Requirements

This should work with most modern versions of [Delphi](https://www.embarcadero.com/products/delphi). 

Tests currently run using the DUnitX TestFramework.

An attempt has been made not to use the latest features to ease backward compatability.
Although the development was done on Delphi 10.3.3, build and tests have been run on the following versions:
- Delphi XE 4
- Delphi XE 8
- Delphi 10.0 Seatle
- Delphi 10.1 Berlin
- Delphi 10.2 Tokyo
- Delphi 10.3.3 Rio
- Delphi 10.4 Sydney
- Delphi 10.4.1 Sydney

There should be no platform specific restrictions.

Have a look at Sempare.Template.Compiler.inc. The following defines can be defined if appropriate:

- SEMPARE_TEMPLATE_FIREDAC - to support tests for TDataSet
- SEMPARE_TEMPLATE_NO_INDY - if Indy is not present. This is used to access an html encoder if TNetEncoding is not available.

## Installation: GetIt

The Sempare Template Engine for Delphi can be installed via the [Embarcadero GetIt manager](https://getitnow.embarcadero.com/?q=sempare&product=rad-studio)

This will add the *src* folder to the search path so you can start working immediately.

## Installation: Delphinus-Support

The Sempare Template Engine for Delphi can be installed via the [Delphinus](https://github.com/Memnarch/Delphinus) package manager.

This will add the *src* folder to the search path so you can start working immediately.

### Using the Sempare Template Engine in your Delphi project

Start by adding the *src* folder to the Delphi search path. Otherwise, there are some projects you can use:

Open __Sempare.Template.Engine.Group.groupproj__ which will include:

- __Sempare.Template.Pkg.dproj__

  The core template project. (runtime)
     
- __Sempare.Template.Tester.dproj__

   100+ unit tests

- __demo\VelocityDemo\Sempare.Template.Demo.dproj__

   The velocity real-time demo.   
   
   
## Help needed

I have not been able to test on XE5-XE7 as I don't have access to those compilers. If anyone in the community can help check the output of the tests, that will be extremely helpful.

## Feedback

You can raise issues on [GitHub](https://github.com/sempare/sempare.template) and they will be addressed based on priority.

Most features have some basic tests in place. If a bug is been discovered, please include a basic test/scenario replicating the issue if possible as this will ease the investigation process.

# Contributions

Please see the [contibution terms and conditions](./docs/CONTRIBUTION.pdf)

# License

The Sempare Template Engien is dual-licensed. You may choose to use it under the restrictions of the [GPL v3.0](https://www.gnu.org/licenses/gpl-3.0.en.html) at
no cost to you, or you may purchase for user under the [Sempare Limited Commercial License](./docs/commercial.license.md)

The dual-licensing scheme allows you to test the library with no restrictions, but subject to the terms of the GPL. A nominal fee is requested to support the maintenance of the library if the product is to be used in commercial products. This support fee binds you to the commercial license, removing any of the GPL restrictions, and allowing you to use the library in your products as you will.

A commercial licence grants you the right to use Sempare Template in your own applications, royalty free, and without any requirement to disclose your source code nor any modifications to
Sempare Templte to any other party. A commercial licence lasts into perpetuity, and entitles you to all future updates - free of charge.

A commercial licence is provided per developer developing applications that use the Sempare Template Engine. The initial cost is $70 per developer and includes first year of support.
For support thereafter, at your discretion, a support fee of $30 per developer per year would be appreciated (the cost of a few cups of coffee). Please contact us for site license pricing.

Please send an e-mail to info@sempare.ltd to request an invoice which will contain the bank details.

Support and enhancement requests submitted by users that pay for support will be prioritised. New developments may incur additional costs depending on time required for implementation.
