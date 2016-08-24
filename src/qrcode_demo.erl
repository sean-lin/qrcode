%% Copyright 2011 Steve Davis <steve@simulacity.com>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

-module(qrcode_demo).

%% Shows how to achieve HOTP/SHA1 with a mobile phone using Google Authenticator.
%%
%% This module is a rag-bag of supporting functions, many of which are simplified
%% extracts from the core libs (?_common, ?_crypto, ?_math, ?_image). This is to
%% allow a full-cycle demo without requiring open-sourcing of the entire platform.
%%
%% @ref QR Code: ISO/IEC 18004 (2000, 1st Edition)

%% Google Authenticator Phone App
%% iPhone:  <http://itunes.apple.com/us/app/google-authenticator/id388497605?mt=8>
%% Android: <https://market.android.com/details?id=com.google.android.apps.authenticator>

%% Google Authenticator URL Specification
% @ref <http://code.google.com/p/google-authenticator/wiki/KeyUriFormat>
%  otpauth://TYPE/LABEL?PARAMETERS
%  TYPE: hotp | totp
%  LABEL: string() (usually email address)
%  PARAMETERS:
%    digits = 6 | 8 (default 6)
%    counter = integer() (hotp only, default 0?)
%    period = integer() (in seconds, totp only, default 30)
%    secret = binary() qrbase32 encoded
%    algorithm = MD5 | SHA1 | SHA256 | SHA512 (default SHA1)


-include("qrcode.hrl").

-compile(export_all).

-define(TTY(Term), io:format(user, "[~p] ~p~n", [?MODULE, Term])).

run() ->
	Passcode = crypto:sha(<<"password">>),
	run(<<"demo@mydomain.com">>, Passcode, 60).

run(Domain, Passcode, Seconds) ->
	PasscodeBase32 = qrbase32:encode(Passcode),
	Period = list_to_binary(integer_to_list(Seconds)),
	Token = <<"otpauth://totp/", Domain/binary, "?period=", Period/binary, "&secret=", PasscodeBase32/binary>>,
	?TTY({token, Token}),
	QRCode = qrcode:encode(Token),
	Image = qrcode_png:simple_png_encode(QRCode),
	Filename = "qrcode.png",
	ok = file:write_file(Filename, Image),
	?TTY({image, filename:absname(Filename)}),
	QRCode.


%%
totp() ->
	Key = crypto:sha(<<"password">>),
	totp(Key, 60).
totp(Key, Period) ->
	T = unow() div Period,
	{hotp(Key, T - 1), hotp(Key, T), hotp(Key, T + 1)}.
%% RFC-4226 "HOTP: An HMAC-Based One-Time Password Algorithm"
%% @ref <http://tools.ietf.org/html/rfc4226>
hotp(Key, Count) when is_binary(Key), is_integer(Count) ->
	HS = crypto:sha_mac(Key, <<Count:64>>),
	<<_:19/binary, _:4, Offset:4>> = HS,
	<<_:Offset/binary, _:1, P:31, _/binary>> = HS,
	HOTP = integer_to_list(P rem 1000000),
	Pad = lists:duplicate(6 - length(HOTP), $0),
	list_to_binary([Pad, HOTP]).


-define(UNIX_TIME_ZERO, 62167219200).

unow() ->
	calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - ?UNIX_TIME_ZERO.
