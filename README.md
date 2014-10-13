erlmud
======

Evolutionary demonstration of Erlang/OTP development. The example project is a MUD system.

Stages
------

The stages of change are arbitrarily designated:
* Concept
* Psuedo-code / wish-code
* Raw Erlang
* Psychobitch port
* Idiomatic OTP
* Deployabile OTP
* OTP that others wouldn't hate to maintain
* Cyclic maintenance

The above definitions are entirely arbitrary, but give a general idea of the stages most code moves through in its life. The need to port code to Psychobitch and then to idiomatic OTP is not something that every project requires, but I've included a "Raw Erlang" phase to represent the sort of non-OTP stage that most newcomers to Erlang pass through on their way to eventually figuring out why OTP is a good idea.

Its common for different elements of a project to be developed as their own little mini-projects, so the "current stage" of a particular piece of code within a real project could be anywhere along its development life. This happens in ErlMUD to some degree, but generally speaking the tagged points in the source references represent general adherence to a certain stage's style (though in the early stages this does not mean that everything works well, or even at all).

This codebase is accompanied by a set of commentary pages which are referenced from within the code and links back from those discussions into relevant parts of the source at certain moments in git time. The commentary is written as a set of HTML pages stored within the root project directory in *html/* and permanently hosted at http://zxq9.com/erlmud/.

Legal Mumbojumbo
----------------

Program sources and other content copyright is held by Craig Everett (aka "zxq9", <zxq9@zxq9.com>).

Program source is released under the GPL 3.0, which should be included in the LICENSE file in the project root directory.

Other textual materials included in the *html/* subdirectory are released under the terms of the [Creative Commons Attribution Non-Commercial No Derivative License](http://creativecommons.org/licenses/by-nc-nd/3.0/), as noted in the *html/LICENSE* stub file.
