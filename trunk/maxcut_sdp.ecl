%
% Interface to CSDP to solve the SDP relaxation of maxcut
%
:-module(maxcut_sdp).
:-export(maxcut_sdp/2).
:-comment(summary,"Interface built on top of csdp to obtain the SDP ralaxation of"
                  " Maxcut").
:-comment(author, "Serge G. Kruk").

:-use_module(csdp).
:-lib(graph_algorithms).

:-comment(maxcut_sdp/2,
          [summary: "Given a graph, returns the maxcut SDP relaxation"
                    " value",
           amode: maxcut_sdp(+,-),
           args: "Graph":"An undirected graph of the structure defined by"
                         " graph_algorithms", 
           "ObjValue":"The SDP objective value"]).
maxcut_sdp(Graph,ObjValue) :-
        csdp_setup(Handle),
        laplacian(Graph,Cs0),
        diag_constraint(Graph,CsA,Bs), 
        append(Cs0,CsA,Cs),
        csdp_exec(Handle,Cs,Bs,ObjValue,Y,XZ,Log),
        csdp_cleanup(Handle).


laplacian(Graph,Cs) :-
        graph_get_maxnode(Graph,N),
        (for(I,1,N),fromto([],In,Out,Css), param(Graph) do
            add_weights(Graph,I,CsI),
            Out=[CsI | In]
        ),
        flatten(Css,Cs).

add_weights(Graph,I,Cs) :-
        graph_get_adjacent_edges(Graph,I,Edges),
        (foreach(Edge,Edges), fromto([],In,Out,L), fromto([],Win,Wout,Wl) do
            Edge=e(I,J,W),
            (J>I ->
                We is -W/4.0,
                csdp_spmatrix_entry(0,1,I,J,We,ME),
                Out=[ME | In],
                Wout=[W | Win]
            ;
                Out=In,
                Wout=[W | Win]
            )
        ),
        length(Wl,NbEdges),
        (NbEdges>0 ->
            sum(Wl,A0),A is A0/4.0,
            csdp_add_spmatrix(0,1,I,I,A,L,Cs)
        ; 
            Cs=[]
        ).
        
diag_constraint(Graph,Cs,Bs) :-
        graph_get_maxnode(Graph,N),
        (for(I,1,N), fromto([],In,Out,Cs), fromto([],Bin,Bout,Bs) do
            csdp_spmatrix_entry(I,1,I,I,1.0,ME),
            Out=[ME | In],
            csdp_spvector_entry(I,1.0,VE),
            Bout=[VE | Bin]
        ).
