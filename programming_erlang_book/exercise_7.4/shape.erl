-module(shape).
-export([area/1,perimeter/1]).
-include("Shapes.hrl").


area(#circle{radius=R}) ->
  R*R*3.14;
area(#rectangle{length=L,width=W}) ->
  L*W;
area(_) ->
  invalid_shape.

perimeter(#circle{radius=R}) ->
  2*R*3.14;
perimeter(#rectangle{length=L,width=W}) ->
  2*(L+W);
perimeter(_) ->
  invalid_shape.
