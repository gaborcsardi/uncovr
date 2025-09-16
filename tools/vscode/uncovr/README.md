# uncovr README

Add commands for the various functions of the [uncovr](
  https://github.com/gaborcsardi/uncovr) package.

## Features

Commands: 

| Command           | Description                                                       |
|:------------------|:------------------------------------------------------------------|
|`uncovr.reload`    | Calls `uncovr::reload()` to (re)load the R package                |
|`uncovr.test`      | Calls `uncovr::test()` to run package tests.                      |
|`uncovr.document`  | Calls `uncovr::document()` to generage the manual.                |
|`uncovr.retest`    | Calls `uncovr::retest()` to re-run the failing tests.             |
|`uncovr.report`    | Calls `uncovr::report()` to create the HTML coverage report.      |
|`uncovr.builds`    | Calls `uncovr::builds()` to list all package builds.              |
|`uncovr.last`      | Calls `uncovr::last()` to show the last coverage and test results.|
|`uncovr.testActive`| Calls `uncovr::test_active()` to run tests for the active file.   |
|`uncovr.lcov`      | Calls `uncovr::lcov` to generate an LCOV coverage report.         |

## Requirements

* The [uncovr](https://github.com/gaborcsardi/uncovr) R package.
* The [R Extension for Visual Studio Code](
  https://marketplace.visualstudio.com/items?itemName=REditorSupport.r).


## Release Notes

No releases as of yet.
