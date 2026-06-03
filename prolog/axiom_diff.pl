% ============================================================================
% AXIOM_DIFF — OQ-59 #4 : the cyclopean disparity operator at the AXIOM layer
% ============================================================================
% The reading_diff partition (agreement = situation-fixed / disparity = depth /
% blind = coverage gap), lifted from (P,T,E,S) type-cells to a reading's AXIOMS.
% A reading's authored axioms are cs_axiom(UID, Tier, Name) reached via
% cs_story_uid(Reading, UID); the per-axiom value compared is its GROUNDING
% (cs_axiom_grounding(UID, Name, G)) — chosen because grounding is per-story and
% therefore can vary across readings, whereas cs_axiom_status/2 is keyed on the
% axiom NAME (global) and so cannot vary for a shared name.
%
% THE SEAT IS SHARPER HERE. At the type-cell layer `exact` (P,T,E,S) is a real
% mechanical key (readings reuse the canonical context tuples). At the axiom
% layer there is NO mechanical cross-reading identity: corpus-wide, 0 of 935
% within-kernel reading-pairs share even one axiom NAME (every reading authors
% bespoke names). So `exact_name` is structurally all-blind across readings, and
% any non-trivial axiom alignment requires a DECLARED semantic equivalence — the
% caller's seat, never baked into the operator (cf. reading_diff throwing on a
% defaulted `weighted`). The declared seat is the (initially empty) multifile
% predicate axiom_concept/2: axioms mapped to the same Concept align.
%
% Authored-facts-only (cs_axiom*, cs_story_uid). Asserts nothing. kernel-agnostic.
%
% Run (from prolog/, after [stack] + corpus load):
%   ?- axiom_diff:report_axiom_pair(R_a, R_b, all_keys).
%   % declare a seat first for the concept key, e.g.:
%   ?- assertz(axiom_diff:axiom_concept(territorial_sovereignty_categorically_inviolable, sovereignty_absolute)).
% ============================================================================

:- module(axiom_diff, [
    axiom_diff/6,            % axiom_diff(+RA,+RB,+Key, -Agree,-Disparity,-Blind)
    axioms_of/2,             % axioms_of(+Reading, -Axioms)  Axioms = [ax(Name,Tier,Grounding)]
    axiom_aligned/3,         % axiom_aligned(+Key, +NameA, +NameB)
    has_ax_partition/1,
    ax_per_key_regime/4,
    ax_stability_verdict/3,
    ax_stability_verdict/4,
    report_axiom_pair/3
]).

:- use_module(narrative_ontology).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(apply)).
:- use_module(library(yall)).

% The DECLARED alignment seat for the `concept` key: empty by default, populated
% by the caller (the human's ruling on which axioms are "the same axiom"). Never
% baked here.
:- multifile axiom_concept/2.
:- dynamic   axiom_concept/2.

% ----------------------------------------------------------------------------
% Axiom cells.  ax(Name, Tier, Grounding).
% ----------------------------------------------------------------------------

%% axioms_of(+Reading, -Axioms) is det.
axioms_of(R, Axs) :-
    findall(ax(Name, Tier, G),
        ( narrative_ontology:cs_story_uid(R, U),
          narrative_ontology:cs_axiom(U, Tier, Name),
          ( narrative_ontology:cs_axiom_grounding(U, Name, G) -> true ; G = unknown )
        ),
        Axs0),
    sort(Axs0, Axs).

% ----------------------------------------------------------------------------
% Alignment relations over axiom NAMES (the declared seats).
% ----------------------------------------------------------------------------

axiom_aligned(exact_name, A, B) :- A == B.
axiom_aligned(concept, A, B) :- axiom_concept(A, C), axiom_concept(B, C).

% Both are equivalence relations -> vantage partition is well-defined.
has_ax_partition(exact_name).
has_ax_partition(concept).

% vantage key of an axiom under a partition key.  Under `concept`, an axiom with
% no declared concept gets a UNIQUE vantage (unmapped(Name)) so it reads as blind
% rather than silently collapsing all unmapped axioms together.
ax_vantage(exact_name, ax(Name,_,_), Name).
ax_vantage(concept,    ax(Name,_,_), V) :-
    ( axiom_concept(Name, C) -> V = C ; V = unmapped(Name) ).

% ----------------------------------------------------------------------------
% Vantage-level partition (mirrors reading_diff; value compared = GROUNDING).
% ----------------------------------------------------------------------------

%% axiom_diff(+RA, +RB, +Key, -Agreement, -Disparity, -Blind) is det.
%  Agreement = [agree(VKey, Groundings)]
%  Disparity = [disparity(VKey, GroundingsA, GroundingsB)]
%  Blind     = [blind(VKey, Side, Groundings)], Side in {a,b}
axiom_diff(RA, RB, Key, Agreement, Disparity, Blind) :-
    ( has_ax_partition(Key) -> true
    ; throw(error(domain_error(ax_partition_key, Key), context(axiom_diff/6, _)))
    ),
    axioms_of(RA, AxA),
    axioms_of(RB, AxB),
    ax_typemap(Key, AxA, MA),
    ax_typemap(Key, AxB, MB),
    pairs_keys(MA, KA), pairs_keys(MB, KB),
    append(KA, KB, K0), sort(K0, AllKeys),
    findall(R, ( member(V, AllKeys), ax_classify(V, MA, MB, R) ), Results),
    include([X]>>(X = agree(_,_)), Results, Agreement),
    include([X]>>(X = disparity(_,_,_)), Results, Disparity),
    include([X]>>(X = blind(_,_,_)), Results, Blind).

%% ax_typemap(+Key, +Axs, -Map) : Map = ordered VKey-SortedGroundingSet
ax_typemap(Key, Axs, Map) :-
    findall(V-G,
        ( member(ax(Name, Tier, G), Axs), ax_vantage(Key, ax(Name, Tier, G), V) ),
        Ps),
    keysort(Ps, S),
    group_pairs_by_key(S, Grp),
    maplist([K-Gs, K-Set]>>sort(Gs, Set), Grp, Map).

ax_classify(V, MA, MB, Result) :-
    ( memberchk(V-Ga, MA) -> HasA = true ; HasA = false ),
    ( memberchk(V-Gb, MB) -> HasB = true ; HasB = false ),
    ( HasA == true, HasB == true ->
        ( Ga == Gb -> Result = agree(V, Ga) ; Result = disparity(V, Ga, Gb) )
    ; HasA == true -> Result = blind(V, a, Ga)
    ; Result = blind(V, b, Gb)
    ).

% ----------------------------------------------------------------------------
% Regime + stability verdict (mirrors reading_diff; order-independent).
% ----------------------------------------------------------------------------

ax_per_key_regime(RA, RB, Key, Regime) :-
    axiom_diff(RA, RB, Key, _, Disp, _),
    ( Disp == [] -> Regime = undersampled ; Regime = binocular ).

ax_stability_verdict(RA, RB, Verdict) :-
    ax_stability_verdict(RA, RB, [exact_name, concept], Verdict).

ax_stability_verdict(RA, RB, Keys, Verdict) :-
    findall(N, ( member(K, Keys), axiom_diff(RA, RB, K, _, D, _), length(D, N) ), Counts),
    ( forall(member(C, Counts), C >= 1) -> Verdict = robustly_binocular
    ; forall(member(C, Counts), C =:= 0) -> Verdict = robustly_undersampled
    ; Verdict = key_fragile
    ).

% ----------------------------------------------------------------------------
% Reporting.
% ----------------------------------------------------------------------------

report_axiom_pair(RA, RB, all_keys) :- !,
    report_ax_header(RA, RB),
    report_ax_key(RA, RB, exact_name),
    report_ax_key(RA, RB, concept),
    ax_stability_verdict(RA, RB, [exact_name, concept], V),
    format("~n>>> AXIOM STABILITY VERDICT over [exact_name, concept] : ~w~n", [V]),
    ( \+ axiom_concept(_, _)
    -> format("    (note: axiom_concept/2 is EMPTY — the `concept` seat is undeclared, so concept~n    alignment is all-blind by construction. Declare axiom_concept/2 to use it.)~n")
    ;  true ).
report_axiom_pair(RA, RB, Key) :-
    report_ax_header(RA, RB),
    report_ax_key(RA, RB, Key).

report_ax_header(RA, RB) :-
    format("~n================================================================~n"),
    format("AXIOM DIFF~n  A = ~w~n  B = ~w~n", [RA, RB]),
    ( narrative_ontology:cs_kernel_id(RA, KA) -> true ; KA = '(none)' ),
    ( narrative_ontology:cs_kernel_id(RB, KB) -> true ; KB = '(none)' ),
    ( KA == KB -> format("  kernel: ~w (within-kernel)~n", [KA])
    ;            format("  kernels: ~w vs ~w (CROSS-KERNEL)~n", [KA, KB]) ),
    format("================================================================~n").

report_ax_key(RA, RB, Key) :-
    axiom_diff(RA, RB, Key, Ag, Disp, Blind),
    length(Ag, NA), length(Disp, ND), length(Blind, NB),
    ( ND >= 1 -> Regime = binocular ; Regime = undersampled ),
    format("~n[key=~w]  axiom-vantage partition (value compared = grounding):~n", [Key]),
    format("    agree=~w  disparity=~w  blind=~w   regime(@~w)=~w~n", [NA, ND, NB, Key, Regime]),
    ( Disp == [] -> true
    ; format("    disparity vantages (grounding mismatch):~n"),
      forall(member(disparity(V, Ga, Gb), Disp),
             format("      ~w :  A=~w  vs  B=~w~n", [V, Ga, Gb]))
    ),
    ( Blind == [] -> true
    ; format("    blind vantages:~n"),
      forall(member(blind(V, Side, Gs), Blind),
             ( ( Side == a -> Who = RA ; Who = RB ),
               format("      ~w :  ~w-only ~w~n", [V, Who, Gs]) ))
    ).
