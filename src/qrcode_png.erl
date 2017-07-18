
-module(qrcode_png).

-include("qrcode.hrl").

-export([simple_png_encode/2]).


%% Very simple PNG encoder for demo purposes
simple_png_encode(#qrcode{dimension = Dim, data = Data}, BoxSize) ->
    Chunk = {BoxSize, bits:duplicate(<<1:1>>, BoxSize), bits:duplicate(<<0:1>>, BoxSize)},
	MAGIC = <<137, 80, 78, 71, 13, 10, 26, 10>>,
	Size = Dim * BoxSize,
	IHDR = png_chunk(<<"IHDR">>, <<Size:32, Size:32, 1:8, 0:8, 0:24>>),
	PixelData = get_pixel_data(Dim, Data, Chunk),
	IDAT = png_chunk(<<"IDAT">>, PixelData),
	IEND = png_chunk(<<"IEND">>, <<>>),
	<<MAGIC/binary, IHDR/binary, IDAT/binary, IEND/binary>>.

png_chunk(Type, Bin) ->
	Length = byte_size(Bin),
	CRC = erlang:crc32(<<Type/binary, Bin/binary>>),
	<<Length:32, Type/binary, Bin/binary, CRC:32>>.

get_pixel_data(Dim, Data, Chunk) ->
	Pixels = get_pixels(Data, 0, Dim, <<>>, Chunk),
	zlib:compress(Pixels).

get_pixels(<<>>, Dim, Dim, Acc, _BoxSize) ->
	Acc;
get_pixels(Bin, Count, Dim, Acc, {BoxSize, _, _}=Chunk) ->
	<<RowBits:Dim/bits, Bits/bits>> = Bin,
	Row = get_pixels0(RowBits, <<0>>, Chunk), % row filter byte
    Rest = 8 - bit_size(Row) rem 8,
    RestBits = rest(Rest),
    NewRow = <<Row/bits, RestBits/bits>>,
	FullRow = bits:duplicate(NewRow, BoxSize),
	get_pixels(Bits, Count + 1, Dim, <<Acc/bits, FullRow/bits>>, Chunk).

get_pixels0(<<1:1, Bits/bits>>, Acc, {_, _, BlackBits}=Chunk) ->
	get_pixels0(Bits, <<Acc/bits, BlackBits/bits>>, Chunk);
get_pixels0(<<0:1, Bits/bits>>, Acc, {_, WhiteBits, _} = Chunk) ->
	get_pixels0(Bits, <<Acc/bits, WhiteBits/bits>>, Chunk);
get_pixels0(<<>>, Acc, _BoxSize) ->
	Acc.

rest(8) ->
    <<>>;
rest(N) ->
    bits:duplicate(<<0:1>>, N).
