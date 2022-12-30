module RunTime.Interpreter where

execute :: RunOptions -> SiteDefinition -> Template -> ExecContext -> ContentGen -> IO (Either GenError ExecContext)
execute rtOpts siteDef template execCtxt contentGen =
  -- run the interpreter on the code in template for contentGen page, within execCtxt.
