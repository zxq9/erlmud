erlmud
======

Evolutionary demonstration of Erlang/OTP development. The example project is a MUD system.

Stages
------

In general, the evolutionary stages involved can be viewed as
* Concept
* Psuedo-code (or what I call "wish-code", as in "I wish I could make it work this way)
* Pure Erlang
* Psychobitch rewrite
* Idiomatic OTP
* Deployabile
* Maintenance

Different kinds of projects may follow different schedules, and different elements of the same project may be at different stages of evolution at any given time. The goal of this project is to develop related elements of functionality together in a coherent way, and tag each stage of the evolution. The central idea is that most newcomers to Erlang read through LYSE, Programming Erlang, and even OTP in Action without ever quite getting a full picture of how each point discussed fits into a project, or at what stage in a project's life a certain element or idea may become useful.

Every project, development situation, customer requirement, business need, and developer is different. It is totally unreasonable to characterize every project as following the stages of evolution listed above, or even prescribing such a path as a good practice. Some clients want custom systems, installed early and dirty, then iteratively improved throughout their life. Some software is written for the mass market where you can't afford to let the world see what versions 0 or 1 looked like, and only release a relatively polished (but perhaps elementary) version 2 as your public beta. Sometimes (well, oftentimes) the actual system requirements require extensive discovery during development, and those discoveries can occasionally bring project-shaking changes to the system (ouch!).

With that in mind, the stages of evolution above are somewhat arbitrary points along a continuous road of developmental evolution that I am calling attention to because I find them to be useful designations. Nobody gets a complex system right the first time (well, almost never, when you do its actually sort of scary because you're sitting there, waiting for stuff to crash, freaked out that it hasn't), and each source file, blob of conceptual functionality, and even each function will pass through these stages, even if sometimes just in the mind of the programmer immediately before writing a version that is somewhat more polished than the initial idea was in the mind.

This codebase is accompanied by a set of commentary pages which are referenced from within the code and links back from those discussions into relevant parts of the source at certain moments in git time. The commentary is written as a set of HTML pages stored within the root project directory in *html/* and permanently hosted at http://zxq9.com/erlmud/html/.

Legal Mumbojumbo
----------------

Program sources and other content copyright is held by Craig Everett (aka "zxq9", <zxq9@zxq9.com>).

Program source is released under the GPL 3.0, which should be inclded in the LICENSE file in the project root directory.

Other textual materials included in the *html/* subdirectory are released under the terms of the [Creative Commons Attribution Non-Commercial No Derivative License](http://creativecommons.org/licenses/by-nc-nd/3.0/), as noted in the *html/LICENSE* stub file.
