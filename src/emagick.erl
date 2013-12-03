%% -----------------------------------------------------------------------------
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
%% -----------------------------------------------------------------------------
-module(emagick).
-author('Per Andersson').

-export([convert/3, convert/4, convert/5]).
-export ([imageinfo/1, imageinfo/2, imageinfo/3]).
-export ([with/3, with/4, with_imageinfo/1, with_imageinfo/2, with_convert/2, with_convert/3]).

-define (DEFAULT_WORKDIR, "/tmp/emagick").
-define (WORKDIR (AppEnv), proplists:get_value(working_directory, AppEnv, ?DEFAULT_WORKDIR)).
-define (MAGICK_PFX (AppEnv), proplists:get_value(magick_prefix, AppEnv, "")).
%% -----------------------------------------------------------------------------

%%
%% @doc
%%      Call functions in a session with *Magick
%% @end
%% -----------------------------------------------------------------------------
with(InData, From, Funs) -> with(InData, From, Funs, []).
with(InData, From, Funs, AppEnv) ->
    WorkDir = ?WORKDIR(AppEnv),
    ok = filelib:ensure_dir(WorkDir ++ "/"),
    Filename = uuid:uuid_to_string(uuid:get_v4()),
    InFile = WorkDir ++ "/" ++ Filename ++ "." ++ atom_to_list(From),
    ok = file:write_file(InFile, InData),
    % Res = call_funs(Funs, {InFile, AppEnv}),
    try lists:foldl(fun (Fun, Arg) -> Fun(Arg) end, {InFile, [{filename, Filename} | AppEnv]}, Funs) of
        Res -> Res
    catch Err -> {error, Err}
    after
        file:delete(InFile)
    end.

with_imageinfo(Args) -> with_imageinfo(Args, []).
with_imageinfo({InFile, AppEnv}, Opts) ->
    {ok, Res} = run_with(imageinfo, [{infile, InFile},
                                     {opts, Opts},
                                     {app, AppEnv}]),
    {{InFile, AppEnv}, Res}.

with_convert(Args, To) -> with_convert(Args, To, []).
with_convert({InFile, AppEnv}, To, Opts) ->
    {ok, Res} = run_with(convert, [{infile, InFile},
                                   {to, To},
                                   {opts, Opts},
                                   {app, AppEnv}]),
    {{InFile, AppEnv}, Res}.

-spec convert(InData, From, To) -> {ok, OutData}
    when InData  :: binary(),
         From    :: atom(), %% pdf | png | jpg | gif | ...
         To      :: atom(), %% same as To
         OutData :: binary().
-spec convert(InData, From, To, Opts) -> {ok, OutData}
    when InData  :: binary(),
         From    :: atom(),
         To      :: atom(),
         Opts    :: proplists:proplist(),
         OutData :: binary().
-spec convert(InData, From, To, Opts, AppEnv) -> {ok, OutData}
    when InData  :: binary(),
         From    :: atom(),
         To      :: atom(),
         Opts    :: proplists:proplist(),
         AppEnv  :: proplists:proplist(),
         OutData :: binary().
%%
%% @doc
%%      Convert indata with *Magick.
%% @end
%% -----------------------------------------------------------------------------
convert(InData, From, To) -> convert(InData, From, To, []).
convert(InData, From, To, Opts) -> convert(InData, From, To, Opts, []).
convert(InData, From, To, Opts, AppEnv) ->
    run(convert, [{indata, InData},
                  {from, From},
                  {to, To},
                  {opts, Opts},
                  {app, AppEnv}]).

%%
%% @doc
%%      Get indata info with *Magick.
%% @end
%% -----------------------------------------------------------------------------
-spec imageinfo(InData) -> {ok, proplists:proplist()}
    when InData :: binary().
-spec imageinfo(InData, Opts) -> {ok, proplists:proplist()}
    when InData :: binary(),
         Opts   :: proplists:proplist().
-spec imageinfo(InData, Opts, AppEnv) -> {ok, proplists:proplist()}
    when InData :: binary(),
         Opts   :: proplists:proplist(),
         AppEnv :: proplists:proplist().
imageinfo(InData) -> imageinfo(InData, []).
imageinfo(InData, Opts) -> imageinfo(InData, Opts, []).
imageinfo(InData, Opts, AppEnv) ->
    run(imageinfo, [{indata, InData},
                    {opts, Opts},
                    {app, AppEnv}]).

%% =============================================================================
%%
%% INTERNAL FUNCTIONS
%%
%% =============================================================================

%% -----------------------------------------------------------------------------
run_with(imageinfo, Opts) ->
    InFile = proplists:get_value(infile, Opts),
    CmdOpts = proplists:get_value(opts, Opts, ""),
    AppEnv = proplists:get_value(app, Opts, []),

    MagickPrefix = ?MAGICK_PFX(AppEnv),

    PortCommand = string:join([MagickPrefix, "convert", format_opts(CmdOpts), InFile, "info:"], " "),
    PortOpts = [stream, use_stdio, exit_status, binary],
    Port = erlang:open_port({spawn, PortCommand}, PortOpts),

    {ok, Data, 0} = receive_until_exit(Port, []),
    case erlang:port_info(Port) of
        undefined -> ok;
        _         -> true = erlang:port_close(Port)
    end,

    [_, Fmt, Dims | _] = binary:split(Data, <<" ">>, [trim, global]),
    [W,H] = lists:map(fun(X) -> list_to_integer(binary_to_list(X)) end, binary:split(Dims, <<"x">>)),
    {ok, [{format, Fmt},
          {dimensions, {W, H}}]};
run_with(convert, Opts) ->
    InFile = proplists:get_value(infile, Opts),
    To = proplists:get_value(to, Opts),
    CmdOpts = proplists:get_value(opts, Opts, ""),
    AppEnv = proplists:get_value(app, Opts, []),

    Filename = proplists:get_value(filename, AppEnv),
    Workdir = ?WORKDIR(AppEnv),

    MagickPrefix = ?MAGICK_PFX(AppEnv),
    OutFile = Workdir ++ "/" ++ Filename ++ "_%06d" ++ "." ++ atom_to_list(To),
    PortCommand = string:join([MagickPrefix, "convert",
                                   format_opts(CmdOpts), InFile, OutFile], " "),

    %% execute as port
    PortOpts = [stream, use_stdio, exit_status, binary],
    Port = erlang:open_port({spawn, PortCommand}, PortOpts),

    %% crash upon non-zero exit status
    {ok, _Data, 0} = receive_until_exit(Port, []),
    case erlang:port_info(Port) of
        undefined -> ok;
        _ ->         true = erlang:port_close(Port)
    end,

    %% return converted file(s)
    {ok, _} = read_converted_files(Workdir, Filename, To, InFile).

-spec run(Command, Opts) -> {ok, Result}
    when Command :: atom(),
         Opts    :: proplists:proplist(),
         Result  :: list(binary()).
%%
%% @doc
%%      Format and execute the supplied *magick command.
%% @end
%% -----------------------------------------------------------------------------
run(imageinfo, Opts) ->
    InData  = proplists:get_value(indata, Opts),
    CmdOpts = proplists:get_value(opts, Opts, ""),
    AppEnv  = proplists:get_value(app, Opts, []),

    Workdir = proplists:get_value(working_directory, AppEnv, "/tmp/emagick"),

    ok = filelib:ensure_dir(Workdir ++ "/"),

    Filename = uuid:uuid_to_string(uuid:get_v4()),
    InFile = Workdir ++ "/" ++ Filename,

    ok = file:write_file(InFile, InData),

    try
        MagickPrefix = proplists:get_value(magick_prefix, AppEnv, ""),

        PortCommand = string:join([MagickPrefix, "convert", format_opts(CmdOpts), InFile, "info:"], " "),

        PortOpts = [stream, use_stdio, exit_status, binary],
        Port = erlang:open_port({spawn, PortCommand}, PortOpts),

        {ok, Data, 0} = receive_until_exit(Port, []),
        case erlang:port_info(Port) of
            undefined -> ok;
            _ ->         true = erlang:port_close(Port)
        end,

        %test.jpg JPEG 1500x1000 1500x1000+0+0 8-bit sRGB 573KB 0.040u 0:00.039
        [_, Fmt, Dims | _] = binary:split(Data, <<" ">>, [trim, global]),
        [W,H] = lists:map(fun(X) -> list_to_integer(binary_to_list(X)) end, binary:split(Dims, <<"x">>)),
        {ok, [{format, Fmt}, 
              {dimensions, {W, H}}]}
    catch
        Err -> {error, Err}
    after
        file:delete(InFile)
    end;
run(convert=Command, Opts) ->
    %% get opts
    InData  = proplists:get_value(indata, Opts),
    From    = proplists:get_value(from, Opts),
    To      = proplists:get_value(to, Opts),
    CmdOpts = proplists:get_value(opts, Opts, ""),
    AppEnv  = proplists:get_value(app, Opts, []),


    %% create working directory if it does not exist already
    Workdir = proplists:get_value(working_directory, AppEnv, "/tmp/emagick"),

    %% add trailing slash to ensure path is dir
    ok = filelib:ensure_dir(Workdir ++ "/"),

    %% write input file to temporary location
    Filename = uuid:uuid_to_string(uuid:get_v4()),
    InFile  = Workdir ++ "/" ++ Filename ++ "." ++ atom_to_list(From),
    %% TODO template _%06d should be configurable
    OutFile = Workdir ++ "/" ++ Filename ++ "_%06d" ++ "." ++ atom_to_list(To),

    %% dump indata to file to be consumed by command
    ok = file:write_file(InFile, InData),

    try
        %% convert magick
        MagickPrefix = proplists:get_value(magick_prefix, AppEnv, ""),

        PortCommand = string:join([MagickPrefix, atom_to_list(Command),
                                   format_opts(CmdOpts), InFile, OutFile], " "),

        %% execute as port
        PortOpts = [stream, use_stdio, exit_status, binary],
        Port = erlang:open_port({spawn, PortCommand}, PortOpts),

        %% crash upon non-zero exit status
        {ok, _Data, 0} = receive_until_exit(Port, []),
        case erlang:port_info(Port) of
            undefined -> ok;
            _ ->         true = erlang:port_close(Port)
        end,

        %% return converted file(s)
        {ok, _} = read_converted_files(Workdir, Filename, To, InFile)
    catch
        Err -> {error, Err}
    after
        %% cleanup
        file:delete(InFile)
    end.


%% -----------------------------------------------------------------------------
-spec read_converted_files(Workdir, Filename, Suffix, Except) -> {ok, Result}
    when Workdir  :: string(),
         Filename :: string(),
         Suffix :: atom(),
         Except :: string(),
         Result   :: list(binary()).
%% @doc
%%      Read converted files, delete them, and return file data.
%% @end
%% -----------------------------------------------------------------------------
read_converted_files(Workdir, Filename, Suffix, Except) ->
    Files0 = filelib:wildcard(Workdir ++ "/" ++ Filename ++ "*." ++
                                 atom_to_list(Suffix)),
    Files = Files0 -- [Except],
    io:format("AllFile: ~p~nToRead: ~p~n", [Files0, Files]),
    do_read_converted_files(lists:sort(Files), []).

do_read_converted_files([], Acc) ->
    {ok, lists:reverse(Acc)};
do_read_converted_files([File|Files], Acc) ->
    {ok, FileBin} = file:read_file(File),
    file:delete(File),
    do_read_converted_files(Files, [FileBin|Acc]).


%% -----------------------------------------------------------------------------
-spec format_opts(Opts) -> Result
    when Opts   :: proplists:proplist(),
         Result :: string().
%%
%% @doc
%%      Format option proplist as string.
%% @end
%% -----------------------------------------------------------------------------
format_opts(Opts) -> format_opts(Opts, []).
format_opts([], Res) -> string:join(Res, " ");
format_opts([Opt|Opts], Res) ->
    ArgStr =
        "-" ++ string:join([to_string(Arg) || Arg <- tuple_to_list(Opt)], " "),
    format_opts(Opts, Res ++ [ArgStr]).

-spec to_string(term()) -> string().
to_string(E) when is_atom(E) ->    atom_to_list(E);
to_string(E) when is_binary(E) ->  binary_to_list(E);
to_string(E) when is_integer(E) -> integer_to_list(E);
to_string(E) when is_list(E) ->    E.


%% -----------------------------------------------------------------------------
-spec receive_until_exit(Port, ReverseBuffer) -> {ok, Data, Status}
    when Port          :: port(),
         ReverseBuffer :: list(iolist()),
         Data          :: binary(),
         Status        :: pos_integer().
%%
%% @doc
%%      Receives until the given port exits.
%% @end
%% -----------------------------------------------------------------------------
receive_until_exit(Port, ReverseBuffer) ->
    receive
        {Port, {exit_status, Status}} ->
            Data = iolist_to_binary(lists:reverse(ReverseBuffer)),
            {ok, Data, Status};
        {Port, {data, Data}} ->
            receive_until_exit(Port, [Data | ReverseBuffer])
    end.
