
-module(qrcode_png).

-include("qrcode.hrl").

-export([simple_png_encode/1]).


%% Very simple PNG encoder for demo purposes
simple_png_encode(#qrcode{dimension = Dim, data = Data}) ->
	MAGIC = <<137, 80, 78, 71, 13, 10, 26, 10>>,
	Size = Dim * 8,
	IHDR = png_chunk(<<"IHDR">>, <<Size:32, Size:32, 8:8, 2:8, 0:24>>),
	PixelData = get_pixel_data(Dim, Data),
	IDAT = png_chunk(<<"IDAT">>, PixelData),
	IEND = png_chunk(<<"IEND">>, <<>>),
	<<MAGIC/binary, IHDR/binary, IDAT/binary, IEND/binary>>.

png_chunk(Type, Bin) ->
	Length = byte_size(Bin),
	CRC = erlang:crc32(<<Type/binary, Bin/binary>>),
	<<Length:32, Type/binary, Bin/binary, CRC:32>>.

get_pixel_data(Dim, Data) ->
	Pixels = get_pixels(Data, 0, Dim, <<>>),
	zlib:compress(Pixels).

get_pixels(<<>>, Dim, Dim, Acc) ->
	Acc;
get_pixels(Bin, Count, Dim, Acc) ->
	<<RowBits:Dim/bits, Bits/bits>> = Bin,
	Row = get_pixels0(RowBits, <<0>>), % row filter byte
	FullRow = binary:copy(Row, 8),
	get_pixels(Bits, Count + 1, Dim, <<Acc/binary, FullRow/binary>>).

get_pixels0(<<1:1, Bits/bits>>, Acc) ->
	Black = binary:copy(<<0>>, 24),
	get_pixels0(Bits, <<Acc/binary, Black/binary>>);
get_pixels0(<<0:1, Bits/bits>>, Acc) ->
	White = binary:copy(<<255>>, 24),
	get_pixels0(Bits, <<Acc/binary, White/binary>>);
get_pixels0(<<>>, Acc) ->
	Acc.
