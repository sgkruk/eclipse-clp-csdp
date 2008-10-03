%
% Interface to CSDP from within ECLiSPe
% %	$Id$	
%
:- module(csdp).
:- export(csdp_setup/1).
:- export(csdp_spvector_entry/3).
:- export(csdp_spmatrix_entry/6).
:- export(csdp_add_spmatrix/7).
:- export(csdp_add_spvector/4).
:- export(csdp_exec/7).
:- export(csdp_cleanup/1).

:-comment(summary, "Low-level interface to the CSDP solver.").
:-comment(desc, "Low-level interface to the CSDP solver.\
The intent is to use this to build higher-level interfaces,
 specialized to different semidefinite problems.").
:-comment(author, "Serge G. Kruk").

:- local struct(spmatrix(id,block,i,j,d)).
:- local struct(spvector(id,d)).

:-comment(csdp_setup/1, 
          [summary: "Returns a handle to the CSDP process.",
           amode: csdp_setup(-),
           args: "Handle":"Handle to be used in csdp_exec and csdp_cleanup"]).
csdp_setup(Handle) :-
        open('csdp.data',write,Handle),
        writeln(Handle,'*comment').

csdp_spmatrix_entry(Id,Block,I,J,D,ME):-
        ME=spmatrix{id:Id,block:Block,i:I,j:J,d:D}.

csdp_add_spmatrix(Id,Block,I,J,D,L,NL):-
        csdp_spmatrix_entry(Id,Block,I,J,D,ME),
        NL=[ME | L].

csdp_spvector_entry(Id,D,VE) :-
        VE=spvector{id:Id,d:D}.

csdp_add_spvector(Id,D,L,NL) :-
        csdp_spvector_entry(Id,D,VE),
        NL=[VE | L].

blocksize(N,L,Sizes) :-
        blocksize(N,L,[],Sizes).
blocksize(0,_,A,A):-!,true.
blocksize(N,L,A,Sizes) :-
        findall(J,member(spmatrix(_,N,_,J,_),L),LJ),
        max(LJ,Size),
        N1 is N-1,
        A1=[Size|A],
        blocksize(N1,L,A1,Sizes).

csdp_exec(Handle,LA,LB,ObjValue,Y,XZ,Log) :-
        findall(ID,member(spmatrix(ID,_,_,_,_),LA),IDs),
        max(IDs,M),writeln(Handle,M), % Number of spmatrices
        findall(Block,member(spmatrix(_,Block,_,_,_),LA),Bs),
        max(Bs,N),writeln(Handle,N), % Number of blocks
        blocksize(N,LA,Sizes),
        (foreach(Size,Sizes),param(Handle) do
            printf(Handle,"%d ",Size)
        ),nl(Handle),
        sort(LB,SortedLB),
        fillin(SortedLB,SLB),
        length(SLB,Mlen),
        (foreach(D,SLB), param(Handle) do
            printf(Handle,"%f ",D)
        ),nl(Handle),
        sort(LA,SortedLA),
        (foreach(C,SortedLA),param(Handle) do
            C=spmatrix{id:ID,block:Block,i:I,j:J,d:D},
            printf(Handle,"%d %d %d %d %f \n",[ID,Block,I,J,D])
            ),nl(Handle),
        close(Handle),
%        system('csdp csdp.data csdp.output |fgrep 'Success:' >csdp.log'),
        exec(['csdp','csdp.data','csdp.output'],[null,out]),
        read_log(out,Log),close(out),
        read_solution(Mlen,Y,XZ),
        inner(SLB,Y,ObjValue).

inner(V1,V2,R) :-
        inner(V1,V2,0,R).
inner([],[],A,A).
inner([H1|V1], [H2|V2], A, R) :-
        A1 is A+H1*H2,
        inner(V1,V2,A1,R).

read_log(out,Log):-
        read_string(out,end_of_file,_,Log).

fillin(L,FL) :-
        (foreach(B,L),count(I,1,_),fromto(FL,Out,In,[]) do
            B=spvector{id:ID,d:D},
            I=ID -> Out=[D|In] ; Out=[0.0 | In]
        ).
        
read_solution(Mlen,Y,XZ) :-
        open('csdp.output',read,Istream),
        read_y(Istream,Mlen,Y),
        read_xz(Istream,XZ),
        close(Istream).

read_y(File, Mlen, Y) :-
        (for(_,1,Mlen), fromto(Y,Out,In,[]), param(File) do
            read_string(File," ",_,Yelem),
            number_string(YelemN,Yelem),
            Out=[YelemN | In]
        ).

read_xz(File, XZ) :-
        ( (read_string(File, " ", _, XZn),
           read_string(File, " ", _, Block),
           read_string(File, " ", _, I),
           read_string(File, " ", _, J),
           read_string(File, " ", _, D),
           (number(D) -> Number_D = D ; number_string(Number_D,D)),
           V=spmatrix{id:XZn,block:Block,i:I,j:J,d:Number_D})
          ->
            XZ = [V|Vs],
            read_xz(File, Vs)
        ;
            XZ = []
        ).
csdp_cleanup(_).


