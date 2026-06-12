/* OQ-114 archive divergence probe — runs under the PINNED criterion
   (ISSUES.md OQ-114, operator 2026-06-12 incl. the pre-freeze zero-divergence
   amendment; the pin FREEZES at this probe's first commit).

   Per archive (one swipl process each — v6 ID reuse makes cross-archive
   loading in one process a module-collision risk):
   - Population: corpus stories with constraint_claim=mountain AND >=1 authored
     classification cell. Mountain-claimed perspectives-free stories are
     BUCKETED OUT with a count (vacuous-forall trap — pinned).
   - Comparator U: old authored-cell unanimity (>=1 cell, all mountain).
   - C: nl_certification_chain semantics under LIVE config params (pinned:
     the question is the live guard's extension, not era-faithful
     reconstruction; params printed).
   - Comparator positive control BEFORE divergences count: must find >=1
     unanimity-true story AND >=1 mountain-claimed story whose cells DISAGREE
     (unanimity false via differing types, not absence). Control
     unsatisfiable => HALT (wrongly-specified clause; never waived).
   - Output: four-cell counts with denominators + per-story lists for the
     divergence cells (C-only = fail-open direction).

   Run from prolog/ (one archive per invocation):
     swipl -g "consult('../audits/2026-06-12_oq114_archive_probe/archive_probe.pl'), run('archives/datasets/kernel_v1', kernel_v1), halt" -t "halt(1)"
     swipl -g "consult('../audits/2026-06-12_oq114_archive_probe/archive_probe.pl'), run('archives/datasets/original_v6', original_v6), halt" -t "halt(1)"
*/

:- [stack].

chain_c(C) :-
    narrative_ontology:constraint_claim(C, mountain),
    drl_core:emerges_naturally(C),
    narrative_ontology:constraint_metric(C, accessibility_collapse, AC),
    number(AC),
    config:param(natural_law_collapse_min, CollapseMin),
    AC >= CollapseMin,
    narrative_ontology:constraint_metric(C, resistance, R),
    number(R),
    config:param(natural_law_resistance_max, ResMax),
    R =< ResMax.

unanimity_u(C) :-
    constraint_indexing:constraint_classification(C, _, _),
    \+ (constraint_indexing:constraint_classification(C, Type, _), Type \= mountain).

has_cells(C) :- constraint_indexing:constraint_classification(C, _, _), !.

% cells disagree = at least two cells with different types (not absence)
cells_disagree(C) :-
    constraint_indexing:constraint_classification(C, T1, _),
    constraint_indexing:constraint_classification(C, T2, _),
    T1 \= T2, !.

run(ArchivePath, Label) :-
    retractall(config:param(corpus_path, _)),
    assertz(config:param(corpus_path, ArchivePath)),
    corpus_loader:load_all_testsets,
    findall(X, corpus_loader:corpus_constraint(X), Cs0), sort(Cs0, Cs),
    length(Cs, NLoaded),
    config:param(natural_law_collapse_min, Min),
    config:param(natural_law_resistance_max, Max),
    format("ARCHIVE ~w: ~w loaded constraint ids (LIVE params: collapse_min=~w resistance_max=~w)~n",
           [Label, NLoaded, Min, Max]),

    % population split
    findall(C, ( member(C, Cs), narrative_ontology:constraint_claim(C, mountain) ), MClaim0),
    sort(MClaim0, MClaim),   % dedupe: archives carry duplicate claim facts
    length(MClaim, NMClaim),
    include(has_cells, MClaim, WithCells),
    exclude(has_cells, MClaim, NoCells),
    length(WithCells, NPop), length(NoCells, NNoCells),
    format("mountain-claimed: ~w | population (cells present): ~w | BUCKETED OUT perspectives-free: ~w~n",
           [NMClaim, NPop, NNoCells]),

    % comparator positive control — BEFORE any divergence is counted
    include(unanimity_u, WithCells, UTrue),
    include(cells_disagree, WithCells, Disagree),
    length(UTrue, NUTrue), length(Disagree, NDis),
    format("comparator control: unanimity-true stories found = ~w; seat-DISAGREEING mountain-claimed stories found = ~w~n",
           [NUTrue, NDis]),
    (   NUTrue >= 1, NDis >= 1
    ->  format("COMPARATOR POSITIVE CONTROL: PASS (probe can find both agreement and divergence)~n")
    ;   format("COMPARATOR POSITIVE CONTROL: UNSATISFIABLE — HALT under the wrongly-specified clause~n"),
        throw(oq114_comparator_control_unsatisfiable(Label, NUTrue, NDis))
    ),

    % four-cell table — include/3 semantics (call-once per element): archived
    % stories carry DUPLICATE claim/metric facts, so bare findall over
    % multi-solution goals multiplies rows (witnessed: both=104 over a
    % 41-story denominator on the first kernel_v1 run).
    include(unanimity_u, WithCells, UT),
    exclude(unanimity_u, WithCells, UF),
    include(chain_c, UT, Both),
    exclude(chain_c, UT, UOnly),
    include(chain_c, UF, COnly),
    exclude(chain_c, UF, Neither),
    length(Both, NB), length(UOnly, NU), length(COnly, NC), length(Neither, NN),
    format("FOUR-CELL (denominator ~w): both=~w | unanimity-only(fail-closed)=~w | C-only(FAIL-OPEN)=~w | neither=~w~n",
           [NPop, NB, NU, NC, NN]),
    format("C-only stories (the fail-open cell): ~w~n", [COnly]),
    format("unanimity-only stories: ~w~n", [UOnly]),
    format("both (sanity — old-6 analogues live here): ~w~n", [Both]).
