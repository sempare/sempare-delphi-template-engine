# ![](../images/sempare-logo-45px.png) Sempare Boot Velocity Template Engine

Copyright (c) 2019 [Sempare Limited](http://www.sempare.ltd), [Conrad Vermeulen](mailto:conrad.vermeulen@gmail.com)

## Todo

- validation
  - use 'require' to validate the data passed in
  - identify required variables ahead of time where possible.
- newline to custom newline encoding. e.g. nl -> &lt;br>nl
- interactive script debugger
- create bindings to data sources like TDataSource, TFDMemTable, TFDQuery, etc...
- create design time components for those that like them
		- TemplateComponent (wrapper around TVelocityTemplate)
			- TemplateVariable (to allow for live bindings)
- review free pascal and support for older versions of Delphi