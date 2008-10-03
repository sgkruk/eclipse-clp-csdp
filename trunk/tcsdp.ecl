%
% Test of the csdp interface
%
:-use_module(csdp).

% Should return 1 (within accuracy)
trial(ObjValue):-
        csdp_setup(Handle),
        csdp_add_spmatrix(0,1,1,1,1.0,[],C0),
        csdp_add_spmatrix(0,1,2,2,2.0,C0,C1),
        csdp_add_spmatrix(0,2,1,1,3.0,C1,C2),
        csdp_add_spmatrix(0,2,2,2,4.0,C2,C3),

        csdp_add_spmatrix(1,1,1,1,1.0,C3,C4),
        csdp_add_spmatrix(1,1,2,2,1.0,C4,C5),
        csdp_add_spvector(1,10.0,[],B0),
        
        csdp_add_spmatrix(2,1,2,2,1.0,C5,C6),
        csdp_add_spmatrix(2,2,1,1,5.0,C6,C7),
        csdp_add_spmatrix(2,2,1,2,2.0,C7,C8),
        csdp_add_spmatrix(2,2,2,2,6.0,C8,Cs),
        csdp_add_spvector(2,20.0,B0,Bs),
        csdp_exec(Handle,Cs,Bs,ObjValue,Y,XZ,Log),
        writeln(Log),
        csdp_cleanup(Handle).
        