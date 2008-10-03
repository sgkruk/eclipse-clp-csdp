:-use_module(maxcut_sdp).
:-lib(graph_algorithms).

trial(ObjValue):-
        make_graph( 13,
                    [ e(1,6,1),e(1,2,1),e(1,7,1),e(1,3,1),e(4,6,1),e(4,5,1),
                      e(5,6,1),e(5,7,1),e(7,10,1),e(3,7,1),e(7,8,1),e(8,9,1),
                      e(10,11,1),e(10,12,1),e(10,13,1),e(7,12,1),e(12,13,1) ],
                    Graph),
        make_undirected_graph(Graph,UGraph),
        maxcut_sdp(UGraph,ObjValue).
