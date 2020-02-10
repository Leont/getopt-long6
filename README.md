[![Build Status](https://travis-ci.org/Leont/getopt-long6.svg?branch=master)](https://travis-ci.org/Leont/getopt-long6)

NAME
====

Getopt::Long

SYNOPSIS
========

    use Getopt::Long;
    get-options("length=i" => my $length, # numeric
                "file=s"   => my $file    # string
                "verbose"  => my $verbose); # flag

or

    use Getopt::Long;
    sub MAIN(Int :$length, Str :$file, Bool :$verbose) { ... }

DESCRIPTION
===========

The Getopt::Long module implements extended getopt functions called `get-options()` and `get-options-from`, as well as automatic argument parsing for a `MAIN` sub.

This function adheres to the POSIX syntax for command line options, with GNU extensions. In general, this means that options have long names instead of single letters, and are introduced with a double dash "--". Support for bundling of command line options, as was the case with the more traditional single-letter approach, is also provided.

Command Line Options, an Introduction
=====================================

Command line operated programs traditionally take their arguments from the command line, for example filenames or other information that the program needs to know. Besides arguments, these programs often take command line *options* as well. Options are not necessary for the program to work, hence the name 'option', but are used to modify its default behaviour. For example, a program could do its job quietly, but with a suitable option it could provide verbose information about what it did.

Command line options come in several flavours. Historically, they are preceded by a single dash `-`, and consist of a single letter.

    -l -a -c

Usually, these single-character options can be bundled:

    -lac

Options can have values, the value is placed after the option character. Sometimes with whitespace in between, sometimes not:

    -s 24 -s24

Due to the very cryptic nature of these options, another style was developed that used long names. So instead of a cryptic `-l` one could use the more descriptive `--long`. To distinguish between a bundle of single-character options and a long one, two dashes are used to precede the option name. Also, option values could be specified either like

    --size=24

or

    --size 24

Getting Started with Getopt::Long
=================================

To use Getopt::Long from a Raku program, you must include the following line in your program:

    use Getopt::Long;

This will load the core of the Getopt::Long module and prepare your program for using it.

Simple options
--------------

The most simple options are the ones that take no values. Their mere presence on the command line enables the option. Popular examples are:

    --all --verbose --quiet --debug

Handling simple options is straightforward:

    sub MAIN(Bool :$verbose, Bool :$all) { ... }

or:

    get-options('verbose' => my $verbose, 'all' => my $all);

The call to `get-options()` parses the command line arguments that are present in `@*ARGS` and sets the option variable to the value `True` if the option did occur on the command line. Otherwise, the option variable is not touched. Setting the option value to true is often called *enabling* the option.

The option name as specified to the `get-options()` function is called the option *specification*. Later we'll see that this specification can contain more than just the option name.

`get-options()` will return a `Capture` if the command line could be processed successfully. Otherwise, it will throw an error using die().

A little bit less simple options
--------------------------------

Getopt::Long supports two useful variants of simple options: *negatable* options and *incremental* options.

A negatable option is specified with an exclamation mark `!` after the option name or a default value for `MAIN` argument:

    sub MAIN(Bool :$verbose = False) { ... }

or:

    get-options('verbose!' => my $verbose);

or:

    my $options = get-options('verbose!');

Now, using `--verbose` on the command line will enable `$verbose`, as expected. But it is also allowed to use `--noverbose` or `--no-verbose`, which will disable `$options<verbose> ` by setting its value to `False`.

An incremental option is specified with a plus `+` after the option name:

    sub MAIN(Int :$verbose is getopt('+')) { ... }

or:

    get-options('verbose+' => my $verbose);

or

    my $options = get-options('verbose+');

Using `--verbose` on the command line will increment the value of `$verbose`. This way the program can keep track of how many times the option occurred on the command line. For example, each occurrence of `--verbose` could increase the verbosity level of the program.

Mixing command line option with other arguments
-----------------------------------------------

Usually programs take command line options as well as other arguments, for example, file names. It is good practice to always specify the options first, and the other arguments last. Getopt::Long will, however, allow the options and arguments to be mixed and 'filter out' all the options before passing the rest of the arguments to the program. To stop Getopt::Long from processing further arguments, insert a double dash `--` on the command line:

    --size 24 -- --all

In this example, `--all` will *not* be treated as an option, but passed to the program unharmed, in `@ARGV`.

Options with values
-------------------

For options that take values it must be specified whether the option value is required or not, and what kind of value the option expects.

Three kinds of values are supported: integer numbers, floating point numbers, and strings.

If the option value is required, Getopt::Long will take the command line argument that follows the option and assign this to the option variable. If, however, the option value is specified as optional, this will only be done if that value does not look like a valid command line option itself.

    sub MAIN(Str :$tag) { ... }

or

    get-options('tag=s' => my $tag);

or my %options = get-options('tag=s');

In the option specification, the option name is followed by an equals sign `=` and the letter `s`. The equals sign indicates that this option requires a value. The letter `s` indicates that this value is an arbitrary string. Other possible value types are `i` for integer values, and `f` for floating point values. Using a colon `:` instead of the equals sign indicates that the option value is optional. In this case, if no suitable value is supplied, string valued options get an empty string `''` assigned, while numeric options are set to `0`.

Options with multiple values
----------------------------

Options sometimes take several values. For example, a program could use multiple directories to search for library files:

    --library lib/stdlib --library lib/extlib

You can specify that the option can have multiple values by adding a "@" to the format, or declare the argument as positional:

    sub MAIN(Str :@library) { ... }

or

    get-options('library=s@' => my @libraries);

or

    my $options = get-options('library=s@');

Used with the example above, `@libraries`/`$options<library>` would contain two strings upon completion: `"lib/stdlib"` and `"lib/extlib"`, in that order. It is also possible to specify that only integer or floating point numbers are acceptable values.

Warning: What follows is an experimental feature.

Options can take multiple values at once, for example

    --coordinates 52.2 16.4 --rgbcolor 255 255 149

This can be accomplished by adding a repeat specifier to the option specification. Repeat specifiers are very similar to the `{...}` repeat specifiers that can be used with regular expression patterns. For example, the above command line would be handled as follows:

    my $options = get-options('coordinates=f{2}', 'rgbcolor=i{3}');

or

    sub MAIN(Rat :@coordinates is getopt('f{2}'),
      Int :@rgbcolor is getopt('i{3}'))


    get-options('coordinates=f{2}' => my @coordinates,
      'rgbcolor=i{3}' => my @rgbcolor);

It is also possible to specify the minimal and maximal number of arguments an option takes. `foo=s{2,4}` indicates an option that takes at least two and at most 4 arguments. `foo=s{1,}` indicates one or more values; `foo:s{,}` indicates zero or more option values.

Options with hash values
------------------------

If you specify that the option can have multiple named values by adding a "%":

    sub MAIN(Str :%define) { ... }

or

    get-options("define=s%" => my %define);

or

    my $options = get-options("define=s%");

When used with command line options:

    --define os=linux --define vendor=redhat

the hash `%defines` or `$options<define> ` will contain two keys, `"os"` with value `"linux"` and `"vendor"` with value `"redhat"`. It is also possible to specify that only integer or floating point numbers are acceptable values. The keys are always taken to be strings.

Options with multiple names
---------------------------

Often it is user friendly to supply alternate mnemonic names for options. For example `--height` could be an alternate name for `--length`. Alternate names can be included in the option specification, separated by vertical bar `|` characters. To implement the above example:

    sub MAIN(:height(:$length)) { ... }

or

    get-options('length|height=f' => my $length);

or

    $options = get-options('length|height=f');

The first name is called the *primary* name, the other names are called *aliases*. When using a hash to store options, the key will always be the primary name.

Multiple alternate names are possible.

Summary of Option Specifications
--------------------------------

Each option specifier consists of two parts: the name specification and the argument specification.

The name specification contains the name of the option, optionally followed by a list of alternative names separated by vertical bar characters.

    length            option name is "length"
    length|size|l     name is "length", aliases are "size" and "l"

The argument specification is optional. If omitted, the option is considered boolean, a value of `True` will be assigned when the option is used on the command line.

The argument specification can be

  * !

    The option does not take an argument and may be negated by prefixing it with "no" or "no-". E.g. `"foo!"` will allow `--foo` (a value of 1 will be assigned) as well as `--nofoo` and `--no-foo` (a value of 0 will be assigned). If the option has aliases, this applies to the aliases as well.

    Using negation on a single letter option when bundling is in effect is pointless and will result in a warning.

  * +

    The option does not take an argument and will be incremented by 1 every time it appears on the command line. E.g. `"more+"`, when used with `--more --more --more`, will increment the value three times, resulting in a value of 3 (provided it was 0 or undefined at firs).

    The `+` specifier is ignored if the option destination is not a scalar.

  * = *type* [ *desttype* ] [ *repeat* ]

    The option requires an argument of the given type. Supported types are:

        * s

          String. An arbitrary sequence of characters. It is valid for the argument to start with `-` or `--`.

        * i

          Integer. This can be either an optional leading plus or minus sign, followed by a sequence of digits, or an octal string (`0o`, optionally followed by '0', '1', .. '7'), or a hexadecimal string (`0x` followed by '0' .. '9', 'a' .. 'f', case insensitive), or a binary string (`0b` followed by a series of '0' and '1').

        * r

          Rational number. For example `3.14`.

        * f

          Floating-pointer number. For example `3.14`, `-6.23E24` and so on.

        * c

          Complex number. For example `1+2i`.

    The *desttype* can be `@` or `%` to specify that the option is list or a hash valued.

    The *repeat* specifies the number of values this option takes per occurrence on the command line. It has the format `{` [ *min* ] [ `,` [ *max* ] ] `}`.

    *min* denotes the minimal number of arguments. It defaults to `0`.

    *max* denotes the maximum number of arguments. It must be at least *min*. If *max* is omitted, *but the comma is not*, there is no upper bound to the number of argument values taken.

  * : *type* [ *desttype* ]

    Like `=`, but designates the argument as optional. If omitted, an empty string will be assigned to string values options, and the value zero to numeric options.

    Note that if a string argument starts with `-` or `--`, it will be considered an option on itself.

  * : *number* [ *desttype* ]

    Like `:i`, but if the value is omitted, the *number* will be assigned.

  * : + [ *desttype* ]

    Like `:i`, but if the value is omitted, the current value for the option will be incremented.

Advanced Possibilities
======================

Object oriented interface
-------------------------

Getopt::Long can be used in an object oriented way as well:

    use Getopt::Long;
    my $p = Getopt::Long.new-from-patterns(@options);
    my $o = $p.get-options(@args) ...

Configuration options can be passed to the constructor as named arguments:

    $p = Getopt::Long.new-from-patterns(@options, :!permute);

Parsing options from an arbitrary array
---------------------------------------

By default, get-options parses the options that are present in the global array `@*ARGV`. A special entry `get-options-from` can be used to parse options from an arbitrary array.

    use Getopt::Long;
    $ret = get-options-from(@myargs, ...);

The following two calls behave identically:

    $ret = get-options( ... );
    $ret = get-options-from(@*ARGS, :overwrite, ... );

Bundling
--------

With bundling it is possible to set several single-character options at once. For example if `a`, `v` and `x` are all valid options,

    -vax

will set all three.

Getopt::Long supports three styles of bundling. To enable bundling, a call to Getopt::Long::Configure is required.

Configured this way, single-character options can be bundled but long options **must** always start with a double dash `--` to avoid ambiguity. For example, when `vax`, `a`, `v` and `x` are all valid options,

    -vax

will set `a`, `v` and `x`, but

    --vax

will set `vax`.

Configuring Getopt::Long
========================

`get-options` and `get-options-from` take the following named options to configure.

  * permute (default:disabled)

    Whether command line arguments are allowed to be mixed with options. Default is disabled.

    If `permute` is enabled, this means that

        --foo arg1 --bar arg2 arg3

    is equivalent to

        --foo --bar arg1 arg2 arg3

  * bundling (default: enabled)

    Enabling this option will allow single-character options to be bundled. To distinguish bundles from long option names, long options *must* be introduced with `--` and bundles with `-`.

    Note that, if you have options `a`, `l` and `all`, , possible arguments and option settings are:

        using argument   sets option(s)
        -------------------------------
        -a, --a          a
        -l, --l          l
        -all             a, l
        --all            all

  * compat-singles (default: disabled)

    Enabling this will allow single letter arguments with an `=` between the letter and its argument. E.g. `-j=2` instead of `-j2`. This is for compatibility with raku's built-in argument parsing.

Return values and Errors
========================

`get-options` returns a capture to indicate success. Configuration errors and errors in the option definitions are signalled using die() and will terminate the calling program unless caught by exception handling.

Troubleshooting
===============

`get-options` does not fail when an option is not supplied
----------------------------------------------------------

That's why they're called 'options'.

`get-options` does not split the command line correctly
-------------------------------------------------------

The command line is not split by get-options, but by the command line interpreter (CLI). On Unix, this is the shell. On Windows, it is CMD.EXE. Other operating systems have other CLIs.

It is important to know that these CLIs may behave different when the command line contains special characters, in particular quotes or backslashes. For example, with Unix shells you can use single quotes (`'`) and double quotes (`"`) to group words together. The following alternatives are equivalent on Unix:

    "two words"
    'two words'
    two\ words

In case of doubt, insert the following statement in front of your Perl program:

    note @*ARGS.join('|');

to verify how your CLI passes the arguments to the program.

AUTHOR
======

Leon Timmermans <fawaka@gmail.com>

