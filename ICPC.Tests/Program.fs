module Program =
    open NUnit.Framework

    [<Parallelizable(ParallelScope.Children)>]
    do ()

    let [<EntryPoint>] main _ = 0

