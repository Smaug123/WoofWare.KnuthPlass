# Project-specific instructions for Claude

## Before committing
Always run `dotnet fantomas .` to format the code before creating commits.

# NUnit bugs

NUnit's filtering is pretty borked.
You can't apply filters that contain special characters in the test name (like a space character).
You have to do e.g. `FullyQualifiedName~singleword` rather than `FullyQualifiedName~single word test`, but this only works on tests whose names are single words to begin with.

Instead of running `dotnet test`, you can perform a build (`dotnet build`) and then run `dotnet woofware.nunittestrunner WoofWare.Zoomies.Test/bin/Debug/net9.0/WoofWare.Zoomies.Test.dll`.
This is an NUnit test runner which accepts a `--filter` arg that takes the same filter syntax as `dotnet test`, but actually parses it correctly: test names can contain spaces.
(The most foolproof way to provide test names to WoofWare.NUnitTestRunner is by XML-encoding: e.g. `FullyQualifiedName="MyNamespace.MyTestsClass&lt;ParameterType1%2CParameterType2&gt;.MyTestMethod"`. The `~` query operator is also supported.)

## FsCheck properties

Use unit-returning `actual |> shouldEqual expected` (with an `FsUnitTyped` assertion) rather than `actual = expected` as the conclusion of an FsCheck property.
When that property fails, the version which throws gives you much better output than the one with an equality check.

## Constructing FsCheck properties

Generally try and avoid filtering unless it's really hard to avoid.
You can often construct what you need instead of filtering.
For example, instead of filtering to a nonempty list, you can pass an `'a` and an `'a list`, and as the first line of the function, prepend the element to the list. Now it's nonempty!

## WoofWare.Expect snapshot testing

The usual workflow for updating snapshots using the WoofWare.Expect snapshot testing library is:

* Enter bulk-update mode by setting a `[<OneTimeSetUp>]` function (from NUnit.Framework) to `GlobalBuilderConfig.enterBulkUpdateMode ()`
* Run the tests. They will fail in the process of updating snapshots (this is so that you can't accidentally commit a test in update mode).
* Undo bulk-update mode by commenting out the `enterBulkUpdateMode ()`.
* Rerun the tests, if you like, to observe that the snapshots are now working.

