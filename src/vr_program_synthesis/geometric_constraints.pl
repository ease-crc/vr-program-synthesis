:- module(geometric_constraints, [
    constrains_dof/2
]).

constrains_dof(O, NumDof) :-
    kb_call(instance_of(O, vrps:'Peg')) ->    NumDof = 2;
    NumDof = 0.
