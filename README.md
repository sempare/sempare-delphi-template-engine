# ![](./images/sempare-logo-45px.png) Sempare Boot Velocity Template Engine

Copyright (c) 2019 [Sempare Limited](http://www.sempare.ltd), [Conrad Vermeulen](mailto:conrad.vermeulen@gmail.com)

Contact: <info@sempare.ltd>

License: [Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0.txt)

Open Source: https://github.com/sempare/sempare.boot.velocity.oss

Tutorial: [You Tube](https://www.youtube.com/watch?v=iotzeJv77cY&list=PLjjz4SuVScHreGKEInvrjPtLPMBU6l130)

## Introduction

Template engines are used often in in the technology where text needs to be customised by substituting variables with values from a data source. Examples where this may take place:
- web sites using template engines (or languages)
- code generation
- mail merge 
- notification messages 

The Sempare Boot Velocity Template Engine is small templating engine for [Delphi](https://www.embarcadero.com/products/delphi) (Object Pascal) that allows for templates to be created easily and efficiently by providing a simple and easy to use interface.

Example usage:
```
program Example;
uses
	Sempare.Boot.Template.Velocity;
type
    TInformation = record
        name: string;
        favourite_sport : string;
    end;
begin
    var template := Velocity.parse('My name is <% name %>. My favourite sport is <% favourite_sport %>.');
    var information : TInformation;
    information.name := 'conrad';
    information.favourite_sport := 'ultimate';
    writeln(Velocity.eval(template, information));	
end.
```

In the example above, you can see that the '<%' start and '%>' end the scripting statement respectively. Within a scripting statement, you can reference variables, assign variables, use conditions, for and while loops, and include other templates.

**NOTE** In examples in this documentation I may use the latest Delphi syntax, such as inline variable declarations. These may are note backward compatible as they were introduced in Delphi 10.2 and are used to shorten the code/examples being illustrated. The codebase will attempt to be as backward compatible as possible, but was developed with Delphi 10.3.

## Quickstart

[Try the demo](./demo/VelocityDemo/README.md) if you want to dive in quick and play with the template engine.

Quick tutorials on [You Tube](https://www.youtube.com/playlist?list=PLjjz4SuVScHreGKEInvrjPtLPMBU6l130). 
The playlist has a few videos that are very short (most less than a minute - blink and they are done). You can drag the slider in the videos if you miss something or refer to the rest of the documentation. 

## Contents
1. [Introduction](#Introduction)
2. [Features](#Features)
3. [Objectives](#Objectives)
4. [Requirements](#Requirements)
5. [Feedback](#Feedback)
6. [Components](./docs/components.md)
7. [Statements](./docs/statements.md)
8. [Expressions](./docs/expressions.md)
9. [Builtin functions](./docs/builtin-functions.md)
10. [Custom functions](./docs/custom-functions.md)
11. [Stack Frames](./docs/stack-frames.md)
12. [Tricks](./docs/tricks.md)
13. [Configuration](./docs/configuration.md)
14. [Debugging the script behaviour](./docs/debugging.md)
15. [Simplified Grammar definition](./docs/simplified-grammar.md)
16. [Design considerations](./docs/design-considerations.md)
18. [Internals](./docs/internals.md)
17. [Restrictions](./docs/restrictions.md)
18. [Todo](./docs/todo.md)
19. [FreePascal](./docs/fpc.md)

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
  - dereference records, classes, arrays, JSON objects and dynamic arrays
  - ternary operator
- safety
  - max runtime protection
- customisation 
  - custom script token replacement
  - add custom functions
  - strip recurring spaces and new lines
- lazy template resolution
- parse time evaluation of expressions/statements
- allow use of custom encoding (UTF-8 with BOM, UTF-8 without BOM, ASCII, etc)
- extensibile rtti interface to easily dereference classes and interfaces (current customisations for IVelocityVariables, TDictionary, TJsonObject) 

## Objectives

Sempare Boot Velocity is not intended to be a fully featured general purpose programming language such as PHP where the script itself could be a self contained programming language.

Sempare Boot Velocity aims to provide just enough functionality to allow you to easily work with the 'view' aspects of a template. Any enhanced functionality required from the scripting environment should be provided by the custom functions written in Pascal.

## Requirements

This should work with most modern versions of [Delphi](https://www.embarcadero.com/products/delphi). 

Tests currently run using the DUnitX TestFramework.

An attempt has been made not to use the latest features to ease backward compatability.
Although the development was done on Dlephi 10.3.3, I have done a quick build/test on:
- Delphi XE 8
- Delphi 10.0 Seatle
- Delphi 10.1 Berlin
- Delphi 10.2 Tokyo
- Delphi 10.3.3 Rio.

There should be no platform specific restrictions.

### Using Sempare Boot Velocity in your Delphi project

Open __Sempare.Boot.Template.Velocity.groupproj__ which will include:

- __Sempare.Boot.Template.Velocity.Pkg.dprog__

  The core velocity project. (runtime)

- __Sempare.Boot.Template.Velocity.Components.Pkg.dprog__

  Contains runtime components used at design-time.
  
- __Sempare.Boot.Template.Velocity.Design.Pkg.dprog__

   The design time components.
   
- __Sempare.Boot.Template.Velocity.Tester.dprog__

   70+ unit tests

- __demo\VelocityDemo\Sempare.Boot.Velocity.Demo.dprog__

   The velocity real-time demo.   

<!--

Sempare Boot Velocity depends on the other Sempare Boot projects:
- [Sempare Boot Test](https://github.com/sempare/sempare.boot.test.oss)

 # Related work
The Sempare Boot Velocity template engine relates to the following projects:
- [Sempare Boot Common](https://github.com/sempare/sempare.boot.common.oss)
- [Sempare Boot Rtti](https://github.com/sempare/sempare.boot.rtti.oss)
- [Sempare Boot Http](https://github.com/sempare/sempare.boot.http.oss)
- [Sempare Boot Build](https://github.com/sempare/sempare.boot.build.oss)
- [Sempare Boot CLI](https://github.com/sempare/sempare.boot.oss)


To see a full list of Sempare Boot projects visit https://www.sempare.ltd/sempare.boot (coming soon 2020)-->

## Feedback

You can raise issues on [GitHub](https://github.com/sempare/sempare.boot.velocity.oss) and they will be addressed based on priority.

Most features have some basic tests in place. If a bug is been discovered, please include a basic test/scenario replicating the issue if possible as this will ease the investigation process.

## Financial Support

As you know - software takes some time to develop, polish and support. Although this project is provided as open source, if you would like to support the development 
and enhancement of the framework, please get in touch via info@sempare.ltd. 

We have a subscription service in place which will allow sponsors to make a small contribution and will receive priority updates.

## Promoting the library

The license does not force you to mention the library used in your project, but it is nice if you can provide a reference to help promoting it.