% Investigate the "redundant" claim: characterize the strip discriminant, then
% diff each affects_constraint consumer old-vs-new under a REVERSIBLE load-time
% strip of same-kernel typed-sibling affects_constraint edges.
:- initialization((catch(run,E,(print_message(error,E),halt(2))),halt(0))).

:- use_module(drl_composition).
:- use_module(drl_counterfactual).
:- use_module(signature_detection).
:- use_module(drl_purity_network).

ctx(Ctx) :- constraint_indexing:default_context(Ctx).

% --- the strip discriminant ---
typed_sibling(A, B) :-
    (   narrative_ontology:cs_story_uid(A, UA), narrative_ontology:cs_reading_relation(UA, B, _)
    ;   narrative_ontology:cs_story_uid(B, UB), narrative_ontology:cs_reading_relation(UB, A, _)
    ), !.

strip_edge(A, B) :-
    narrative_ontology:affects_constraint(A, B),
    narrative_ontology:cs_kernel_id(A, K),
    narrative_ontology:cs_kernel_id(B, K),
    typed_sibling(A, B).

run :-
    ( getenv('CORPUS', CDir), CDir \== '' ->
        retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path, CDir)) ; true ),
    corpus_loader:ensure_corpus_loaded,
    ctx(Ctx),

    % ===== PART 1: discriminant characterization =====
    aggregate_all(count, narrative_ontology:affects_constraint(_,_), NTotal),
    findall(A-B, (narrative_ontology:affects_constraint(A,B),
                  narrative_ontology:cs_kernel_id(A,K), narrative_ontology:cs_kernel_id(B,K)), SameK),
    length(SameK, NSameK),
    findall(A-B, strip_edge(A,B), StripEdges0), sort(StripEdges0, StripEdges),
    length(StripEdges, NStrip),
    findall(A-B, (member(A-B,SameK), \+ strip_edge(A,B)), SameKUntyped0), sort(SameKUntyped0, SameKUntyped),
    length(SameKUntyped, NSameKUntyped),
    NCross is NTotal - NSameK,
    % directionality of strip edges: bidirectional vs one-way
    findall(A-B, (member(A-B,StripEdges), narrative_ontology:affects_constraint(B,A)), Bidir),
    length(Bidir, NBidir),
    format("~n===== DISCRIMINANT CHARACTERIZATION =====~n"),
    format("  total affects_constraint edges        : ~w~n", [NTotal]),
    format("  same-kernel edges                     : ~w~n", [NSameK]),
    format("    of which TYPED-sibling (STRIP set)  : ~w~n", [NStrip]),
    format("    of which same-kernel UNTYPED        : ~w~n", [NSameKUntyped]),
    format("  cross-kernel edges (UKE dep graph)    : ~w~n", [NCross]),
    format("  strip edges that are BIDIRECTIONAL    : ~w / ~w~n", [NBidir, NStrip]),
    ( NSameKUntyped > 0
    -> format("  [!] same-kernel-untyped present — discriminant would MISS these (or they are non-sibling)~n")
    ;  format("  [ok] every same-kernel edge is typed-sibling (discriminant == same-kernel here)~n") ),

    % endpoints incident to strip edges = the only constraints whose consumer output can change
    findall(X, (member(A-B,StripEdges), (X=A;X=B)), Eps0), sort(Eps0, Endpoints),
    length(Endpoints, NEp),
    format("  strip-incident endpoints (affected set): ~w~n", [NEp]),

    % ===== PART 2: per-consumer baseline snapshots =====
    snapshot(Endpoints, Ctx, Base),

    % ===== apply reversible strip =====
    forall(member(A-B, StripEdges), retract(narrative_ontology:affects_constraint(A,B))),
    cache_registry:clear_all_caches,

    snapshot(Endpoints, Ctx, Stripped),

    % restore (reversible)
    forall(member(A-B, StripEdges), assertz(narrative_ontology:affects_constraint(A,B))),
    cache_registry:clear_all_caches,

    % ===== diff per consumer =====
    Base = snap(FpnB, ExtB, ViaB, DepB, CoupB),
    Stripped = snap(FpnS, ExtS, ViaS, DepS, CoupS),
    format("~n===== PER-CONSUMER OLD-vs-NEW DIFF (affected endpoints only) =====~n"),
    diff_report("FPN effective_purity (C-EP)        ", FpnB, FpnS),
    diff_report("composition detect_extraction_dom  ", ExtB, ExtS),
    diff_report("signature has_viable_alternatives  ", ViaB, ViaS),
    diff_report("counterfactual dependency_chain    ", DepB, DepS),
    diff_report("inferred_coupling baseline edges   ", CoupB, CoupS),
    format("~n  (constraint_bridge + uke_dr_bridge are recommendation-source-gated;~n"),
    format("   sibling edges have constraint sources, so they are structurally unreachable — not diffed)~n").

snapshot(Endpoints, Ctx, snap(Fpn, Ext, Via, Dep, Coup)) :-
    % FPN: effective purity of each affected endpoint (rounded to avoid float noise)
    findall(C-EPr, (member(C,Endpoints),
                    catch(drl_purity_network:effective_purity(C,Ctx,EP,_),_,fail),
                    EPr is round(EP*10000)/10000), Fpn0), sort(Fpn0, Fpn),
    % composition: extraction-dominance evidence with affected composite
    findall(C-Ev, (member(C,Endpoints),
                   catch(drl_composition:detect_extraction_dominance(C,Ev),_,fail)), Ext0), sort(Ext0, Ext),
    % signature: viable-alternatives among affected
    findall(C, (member(C,Endpoints),
                catch(signature_detection:has_viable_alternatives(C,true),_,fail)), Via0), sort(Via0, Via),
    % counterfactual: dependency edges sourced at an affected endpoint
    findall(S-T, (member(S,Endpoints),
                  catch(drl_counterfactual:dependency_chain(S,T,_,_,Ctx),_,fail)), Dep0), sort(Dep0, Dep),
    % inferred coupling: explicit baseline edges incident to an affected endpoint
    findall(A-B, (member(A,Endpoints), narrative_ontology:affects_constraint(A,B)), Coup0), sort(Coup0, Coup).

diff_report(Label, Base, Stripped) :-
    ord_subtract(Base, Stripped, Removed),
    ord_subtract(Stripped, Base, Added),
    length(Removed, NR), length(Added, NA),
    ( NR =:= 0, NA =:= 0
    ->  format("  ~w : NO DIFF (witnessed-redundant)~n", [Label])
    ;   format("  ~w : CHANGED  -~w +~w~n", [Label, NR, NA]),
        ( Removed = [Ex|_] -> format("        e.g. removed: ~w~n", [Ex]) ; true ) ).
