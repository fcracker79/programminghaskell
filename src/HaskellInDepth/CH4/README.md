Relevants commands for Chapter 4
================================

Invoking `ghc-pkg` using Stack
--------------------------------

`find-module` usage:

```
$ stack exec -- ghc-pkg find-module Data.Monoid
/home/mirko/.stack/programs/x86_64-linux/ghc-tinfo6-8.10.4/lib/ghc-8.10.4/package.conf.d
    base-4.14.1.0
/home/mirko/.stack/snapshots/x86_64-linux-tinfo6/e1f75c1179e3884f5a87713d5e59b59d49d910337dc94d34850bf085e6ee673d/8.10.4/pkgdb
    (no packages)
/home/mirko/dev/haskell/programminghaskell/.stack-work/install/x86_64-linux-tinfo6/e1f75c1179e3884f5a87713d5e59b59d49d910337dc94d34850bf085e6ee673d/8.10.4/pkgdb
    (no packages)
```

Here we can see the following three locations where packages are searched:

1. Global database
2. User database from Stack (snapshot)
3. Project-specific database

In the above example, `Data.Monoid` comes from global database.

We might search modules from the current project, as follows:

```
$ stack exec -- ghc-pkg find-module BookOfMonads.Chapter10.Swappiness
/home/mirko/.stack/programs/x86_64-linux/ghc-tinfo6-8.10.4/lib/ghc-8.10.4/package.conf.d
    (no packages)
/home/mirko/.stack/snapshots/x86_64-linux-tinfo6/e1f75c1179e3884f5a87713d5e59b59d49d910337dc94d34850bf085e6ee673d/8.10.4/pkgdb
    (no packages)
/home/mirko/dev/haskell/programminghaskell/.stack-work/install/x86_64-linux-tinfo6/e1f75c1179e3884f5a87713d5e59b59d49d910337dc94d34850bf085e6ee673d/8.10.4/pkgdb
    programminghaskell-0.1.0.0
```

or we might search module in the dependencies from the Stack snapshot:

```
$ stack exec -- ghc-pkg find-module Data.Csv
/home/mirko/.stack/programs/x86_64-linux/ghc-tinfo6-8.10.4/lib/ghc-8.10.4/package.conf.d
    (no packages)
/home/mirko/.stack/snapshots/x86_64-linux-tinfo6/e1f75c1179e3884f5a87713d5e59b59d49d910337dc94d34850bf085e6ee673d/8.10.4/pkgdb
    cassava-0.5.2.0
/home/mirko/dev/haskell/programminghaskell/.stack-work/install/x86_64-linux-tinfo6/e1f75c1179e3884f5a87713d5e59b59d49d910337dc94d34850bf085e6ee673d/8.10.4/pkgdb
    (no packages)
```

In this case we just see what we decided to install from the Stack snapshot, *not* the what the whole snapshot includes.
e.g.

```
$ stack exec -- ghc-pkg find-module System.TimeIt
/home/mirko/.stack/programs/x86_64-linux/ghc-tinfo6-8.10.4/lib/ghc-8.10.4/package.conf.d
    (no packages)
/home/mirko/.stack/snapshots/x86_64-linux-tinfo6/e1f75c1179e3884f5a87713d5e59b59d49d910337dc94d34850bf085e6ee673d/8.10.4/pkgdb
    (no packages)
/home/mirko/dev/haskell/programminghaskell/.stack-work/install/x86_64-linux-tinfo6/e1f75c1179e3884f5a87713d5e59b59d49d910337dc94d34850bf085e6ee673d/8.10.4/pkgdb
    (no packages)
```

`describe` usage:

```
$ stack exec -- ghc-pkg describe cassava
name:                 cassava
version:              0.5.2.0
visibility:           public

...

```
