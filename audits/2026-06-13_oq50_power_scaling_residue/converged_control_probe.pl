% OQ-128 — validate the false-summit machinery against ACTUALLY-CONVERGED controls.
% Scoped (one_seat_audited §The cap): this tests the SEAT-COMPUTATION (does the engine's
% computed consensus-correspondence place a converged claim as Mountain), NOT metaphysical
% settledness — the seat here is the story's authored metrics + the engine's sheaf/temporal verdict.
%
% Hypothesis (one_seat_audited / OQ-128 resolution): a claim authored as CONVERGED (no competing
% mechanism, "impossible alternative", consensus — e.g. thermodynamics_entropy) should GLUE as
% mountain (descends(mountain), no false_summit), whereas the CONTESTED controls (radiative/actinide,
% which name rival mechanisms with no consensus) correctly do NOT glue. If a converged control ALSO
% false-summits, that is a genuine residual bug. If it glues, the machinery correctly tracks the
% consensus line and radiative/actinide were correctly placed below it.
:- initialization(main).
:- [stack].
:- use_module(cache_registry).
:- use_module(boltzmann_compliance).

targets([thermodynamics_entropy, weak_law_large_numbers, well_ordering_theorem,
         maxwell_demon_impossibility, axiom_of_choice_consequence, busy_beaver_noncomputability,
         finite_group_classification, topological_invariant_universality, planar_graph_embeddability,
         newtons_method_convergence, lagrange_multipliers, maximum_entropy_principle,
         thermodynamic_arrow, conservation_of_energy, second_law_thermodynamics, speed_of_light_invariance]).

seats(C, Pairs) :-
    findall(Ctx-T, (drl_core:standard_context(Ctx), catch(drl_core:dr_type(C,Ctx,T),_,T=err)), Pairs).
sig(C,S) :- ( catch(signature_detection:constraint_signature(C,S),_,fail) -> true ; S = none ).
desc(C,D) :- ( catch(grothendieck_cohomology:descent_status(C,D),_,fail) -> true ; D = no_descent ).
bz(C,V) :- ( catch(boltzmann_compliance:boltzmann_compliant(C,R),_,R=err)->true;R=fail ),
    ( R=compliant(_)->V=compliant ; R=non_compliant(_,_)->V=non_compliant ; R=inconclusive(_)->V=inconclusive ; V=R ).
glues_mtn(Pairs) :- forall(member(_-T,Pairs), T==mountain).

main :-
    getenv('CORPUS_DIR', Dir),
    retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path, Dir)),
    corpus_loader:load_all_testsets, cache_registry:clear_all_caches,
    targets(Ts),
    include([C]>>(corpus_loader:corpus_constraint(C)), Ts, Present),
    format("~n==== ~w : converged-control validation (~w/~w targets present) ====~n", [Dir, _, _]),
    forall(member(C, Present),
        ( ( narrative_ontology:constraint_claim(C, Claim) -> true ; Claim = '(no-claim)' ),
          seats(C, Pairs), sig(C,S), desc(C,D), bz(C,B),
          ( glues_mtn(Pairs) -> G = 'GLUES-MOUNTAIN' ; G = 'varies' ),
          findall(T, member(_-T,Pairs), Types),
          format("  ~w~n     claim=~w  seats=~w  ~w~n     sig=~w  descent=~w  boltzmann=~w~n",
                 [C, Claim, Types, G, S, D, B]) )),
    subtract(Ts, Present, Absent),
    format("  (absent from this corpus: ~w)~n", [Absent]),
    halt.
main :- format("CONVERGED CONTROL PROBE FAILED~n"), halt(1).
