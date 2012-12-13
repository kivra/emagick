# Wrapper in Erlang for Image/GraphicsMagick

Simple wrapper for GraphicsMagick or ImageMagick using an Erlang port.


## Dependencies

Either GraphicsMagick or ImageMagick commandline tools are of course
required. On a Debian based system install either `graphicsmagick` or
`imagemagick` and then configure `magick_prefix` in your app.config
accordingly.

Furthermore `rebar` and `git` are necessary to use the automatic build
system.

The Erlang package erlang-uuid is another dependency

    http://gitorious.org/avtobiff/erlang-uuid.git


## Installation

Build and install with the supplied Makefile or use rebar. Simply typing
`make` will build emagick.


## Configuration

It is possible to choose which \*magick command to run and what path to
use as working directory (default `/tmp/emagick` if omitted) by
configuring `magick_prefix` and `working_directory` in your app.config.

    {emagick, [
        {magick_prefix, ""},       %% ImageMagick
        %{magick_prefix, "gm"},    %% GraphicsMagick
        {working_directory, "/tmp/emagick"}
    ]}.


## Example usage

    1> {ok, Pdf} = file:read_file("something.pdf").
    2> {ok, Png} = emagick:convert(Pdf, pdf, png, [{density, 200}]).
    3> ok = file:write_file("something.png", Png).


## License

emagick is released under an MIT license. See LICENSE for the full
license text.


 vim:ft=markdown:tw=72
