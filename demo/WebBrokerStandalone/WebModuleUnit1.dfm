object WebModule1: TWebModule1
  OnCreate = WebModuleCreate
  Actions = <
    item
      Default = True
      Name = 'ErrorHandler'
      PathInfo = '/404'
      OnAction = WebModule1ErrorHandlerAction
    end
    item
      MethodType = mtGet
      Name = 'IndexHandler'
      PathInfo = '/'
      OnAction = WebModule1IndexHandlerAction
    end
    item
      MethodType = mtGet
      Name = 'FormInput'
      PathInfo = '/form'
      OnAction = WebModule1FormInputAction
    end
    item
      MethodType = mtPost
      Name = 'FormInputHandler'
      PathInfo = '/form'
      OnAction = WebModule1FormInputHandlerAction
    end>
  Height = 230
  Width = 415
end
