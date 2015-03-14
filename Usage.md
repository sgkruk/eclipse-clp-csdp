#How to call the code

# Introduction #

Here is a small example of the main calls.

# Details #
%
% Test of the csdp interface.
% This example is from the CSDP documentation (User guide)
% The objective value is 2.75.  The optimal Y is [0.75 1]
%
:-use\_module(csdp).

adata([0,1,1,1,2.0]).
adata([0,1,1,2,1.0]).
adata([0,1,2,2,2.0]).
adata([0,2,1,1,3.0]).
adata([0,2,1,3,1.0]).
adata([0,2,2,2,2.0]).
adata([0,2,3,3,3.0]).

adata([1,1,1,1,3.0]).
adata([1,1,1,2,1.0]).
adata([1,1,2,2,3.0]).
adata([1,3,1,1,1.0]).



adata([2,2,1,1,3.0]).
adata([2,2,1,3,1.0]).
adata([2,2,2,2,4.0]).
adata([2,2,3,3,5.0]).
adata([2,3,2,2,1.0]).

bdata([1,1.0]).
bdata([2,2.0]).

trial(ObjValue,Y) :-
> csdp\_setup(Handle),
> % Setup the matrices in a list
> findall(L,adata(L),AllaData),
> (foreach(M,AllaData),foreach(ME,Ais) do
> > M=[Id,Block,I,J,D],
> > csdp\_spmatrix\_entry(Id,Block,I,J,D,ME)

> ),
> % Now the right hand side
> findall(L,bdata(L),AllbData),
> (foreach(B,AllbData),foreach(BE,Bis) do
> > B=[Id,D],
> > csdp\_spvector\_entry(Id,D,BE)

> ),
> csdp\_exec(Handle,Ais,Bis,ObjValue,Y,XZ,Log),
> writeln(Log),
> csdp\_cleanup(Handle).

