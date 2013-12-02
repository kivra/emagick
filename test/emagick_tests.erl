%%------------------------------------------------------------------------------
%%
%% emagick: Wrapper for Graphics/ImageMagick command line tool.
%%
%% Copyright (c) 2012 KIVRA
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.
%%------------------------------------------------------------------------------
-module(emagick_tests).
-author('Per Andersson').

-include_lib("eunit/include/eunit.hrl").


-define(PNG_MAGIC, <<16#89, 16#50, 16#4e, 16#47, 16#0d, 16#0a, 16#1a, 16#0a>>).


%% @doc Test that converting PDF to PNG results in a PNG image.
image_info_test_() ->
    {setup,
        fun() -> os:cmd("convert logo: ../test/logo.gif"), ok end,
        fun(_) -> ok end,
        fun(_) ->
            fun() ->
                {ok, GifBin} = file:read_file("../test/logo.gif"),
                {ok, Info} = emagick:imageinfo(GifBin),
                Expected = [{format, <<"GIF">>}, {dimensions, {640, 480}}],
                ?assertEqual(Info, Expected)
            end
        end}.
convert_test_() ->
    {setup,
     fun() -> os:cmd("convert logo: ../test/logo.gif"), ok end,
     fun(_) -> ok end,
     fun(_) ->
        fun() ->
            {ok, Bin} = file:read_file("../test/logo.gif"),
            {ok, [<<Magic:64, _/binary>>]} =
                emagick:convert(Bin, gif, png, [{density, 200}]),
            ?assertEqual(?PNG_MAGIC, <<Magic:64>>)
        end
    end}.

