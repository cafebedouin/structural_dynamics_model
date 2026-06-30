% ============================================================================
% CANARY: coexists_with exclusion from the FPN — checked invariant, not absence
% ============================================================================
%
% OQ-23 (Ω_C, operator ruling a+, 2026-06-29). The architecture claims a
% `coexists_with` reading-relation carries "zero contamination weight by
% definition" in the purity-contamination network (FPN). That is design intent,
% not a mathematical property: the FPN is label-blind. Case 2 of
% test_forecloses_fpn_injection.pl already proves a coexists_with edge injected
% as an `affects_constraint` fact produces Contam > 0 identical to any scalar.
%
% The ruling ratifies documented-only (no engine behavior change) but converts
% "holds by absence" into "holds by a CHECKED invariant" via this canary, sited
% at the construction point (constraint_neighbors/3), positive-controlled so an
% empty result means "looked and found none," not "didn't look" (build_discipline
% Pattern 5 — absence satisfies the gate).
%
% THE REAL RISK is NOT the direct typed-edge channel (no constructor routes
% cs_reading_relation into constraint_neighbors/3 today). It is the SIDE CHANNEL:
% the generation template authors an `affects_constraint` edge between sibling
% readings "to enable contamination propagation across readings," and the
% intra-kernel filter at drl_purity_network.pl:105 applies ONLY to shared-agent
% edges — NOT to explicit affects_constraint. So a coexists_with sibling pair
% WOULD contaminate via its parallel affects_constraint edge IF both siblings are
% co-present AND purity >= 0 (non-sentinel) AND differ. This canary measures
% exactly that, across the legs where siblings co-exist.
%
% On testsets/ the exclusion holds by sparsity + sentinel, NOT by a label filter
% (singleton working set: coexists siblings are ungenerated/phantom, and
% purity_score returns the -1.0 sentinel without factorization metrics). Run only
% there, the canary is GREEN-by-sparsity — a false green. It MUST run where
% siblings co-exist: testsets_haiku/, testsets_flash/, archives/datasets/kernel_v1.
% The census (run_coexists_census/0) is the measurement that tells the three
% greens apart: sparsity (denom 0), sentinel-suppressed (eligible 0), genuine
% structural (eligible N / leaked 0).
%
% MODULE QUALIFICATION: compute_edge_contamination/7 is NOT exported from
% drl_purity_network — always use the drl_purity_network: qualifier. Synthetic
% fixtures must author ontology presence (a claim) and corpus_constraint/1
% membership to participate (constraint_neighbors/3 is fail-closed on phantoms,
% OQ-95; the census enumerates corpus_constraint/1).
% ============================================================================

:- module(test_coexists_fpn_canary, [
    coexists_census/5,
    forecloses_census/5,
    run_coexists_census/0,
    run_forecloses_census/0,
    coexists_contamination_leak/2
]).

% Engine modules (idempotent if [stack] already loaded them, per the standard
% invocation `swipl -g "[stack],[tests/test_coexists_fpn_canary],run_tests"`).
:- use_module(drl_purity_network).
:- use_module(purity_scoring).
:- use_module(narrative_ontology).
:- use_module(constraint_indexing).
:- use_module(corpus_loader).
:- use_module(cache_registry).
:- use_module(config).
:- use_module(library(plunit)).
:- use_module(library(apply)).
:- use_module(library(yall)).

% ----------------------------------------------------------------------------
% CONTEXT
% ----------------------------------------------------------------------------
ctx(Ctx) :- constraint_indexing:default_context(Ctx).

sort_pair(X, Y, A, B) :- ( X @< Y -> A = X, B = Y ; A = Y, B = X ).

% ----------------------------------------------------------------------------
% PAIR ENUMERATION (parameterised by relation type)
%
% Edge keying (cs_corpus_analysis.pl:131): source is UID-keyed, target is
% sibling NAME-keyed. cs_reading_relation(UID, SiblingName, Rel) with
% cs_story_uid(C1, UID) resolves the source reading C1; SiblingName is C2.
% A pair is CO-PRESENT iff BOTH endpoints are loaded (corpus_constraint/1) —
% dangling/phantom siblings drop out of the denominator by construction.
% ----------------------------------------------------------------------------
copresent_pairs(Rel, Pairs) :-
    findall(A-B,
            ( narrative_ontology:cs_reading_relation(UID, C2, Rel),
              narrative_ontology:cs_story_uid(C1, UID),
              C1 \== C2,
              corpus_loader:corpus_constraint(C1),
              corpus_loader:corpus_constraint(C2),
              sort_pair(C1, C2, A, B) ),
            Raw),
    sort(Raw, Pairs).            % dedup unordered co-present pairs

% A pair is ELIGIBLE for contamination iff both purities are non-sentinel
% (>= 0) and they differ (delta > 0). A sentinel (-1.0) or zero-delta pair is
% short-circuited by compute_edge_contamination / effective_purity.
pair_eligible(A-B) :-
    purity_scoring:purity_score(A, PA), PA >= 0.0,
    purity_scoring:purity_score(B, PB), PB >= 0.0,
    abs(PA - PB) > 0.0.

% A pair is COUPLED iff an authored affects_constraint edge (the side channel)
% connects them, in either direction. Read narrative_ontology directly rather
% than the post-dedup neighbor source label: deduplicate_neighbors/2 keeps the
% strongest edge per neighbor and can relabel a 1.0 explicit edge's source to a
% shared-agent type when both exist (witnessed on a no-kernel synthetic pair).
% Reading affects_constraint is exactly the plan's "side channel is wired for
% them" and is immune to that relabel. (Note: for REAL same-kernel siblings the
% intra-kernel filter at drl_purity_network.pl:105 strips shared-agent edges, so
% the surviving contaminating edge IS the explicit affects_constraint one.)
pair_coupled(A-B, _Ctx) :-
    (   narrative_ontology:affects_constraint(A, B)
    ;   narrative_ontology:affects_constraint(B, A)
    ), !.

% A pair LEAKS iff the real consumer (effective_purity) attributes non-zero
% contamination from one sibling to the other. This is faithful to the engine:
% effective_purity short-circuits on a sentinel receiver, computes the donor's
% dr_type contamination strength, and applies the downward-only delta. Checking
% both directions covers whichever sibling is the higher-purity receiver.
pair_leak(A-B, Ctx, Contam) :-
    (   leak_dir(A, B, Ctx, Contam)
    ;   leak_dir(B, A, Ctx, Contam)
    ), !.

leak_dir(Recv, Donor, Ctx, Contam) :-
    drl_purity_network:effective_purity(Recv, Ctx, _EP,
        purity_components(_Intrinsic, _Total, contamination_detail(Edges))),
    member(edge(Donor, _Delta, Contam), Edges),
    Contam > 0.0.

% Public leak iterator (census numerator surface).
coexists_contamination_leak(A-B, Contam) :-
    ctx(Ctx),
    copresent_pairs(coexists_with, Pairs),
    member(A-B, Pairs),
    pair_leak(A-B, Ctx, Contam).

% ----------------------------------------------------------------------------
% CENSUS — pinned schema (Step 2). Per relation Rel, emit:
%   Denom    — co-present pairs (both corpus_constraint-present)
%   Eligible — of those, purity >= 0 on both AND delta > 0
%   Coupled  — of those, an affects_constraint edge is present (side channel)
%   Leaked   — actually leaked (effective_purity contam > 0)
%   CoupIneligibleN — coupled pairs that are purity-INELIGIBLE (funnel guard #5)
% ----------------------------------------------------------------------------
census(Rel, Denom, EligibleN, CoupledN, LeakedN, CoupIneligibleN) :-
    ctx(Ctx),
    copresent_pairs(Rel, Pairs),
    length(Pairs, Denom),
    include(pair_eligible, Pairs, Eligible),
    length(Eligible, EligibleN),
    include([P]>>pair_coupled(P, Ctx), Pairs, Coupled),
    length(Coupled, CoupledN),
    include([P]>>pair_leak(P, Ctx, _), Pairs, Leaked),
    length(Leaked, LeakedN),
    include([P]>>(pair_coupled(P, Ctx), \+ pair_eligible(P)), Pairs, CoupIneligible),
    length(CoupIneligible, CoupIneligibleN).

coexists_census(Denom, EligibleN, CoupledN, LeakedN, CoupIneligibleN) :-
    census(coexists_with, Denom, EligibleN, CoupledN, LeakedN, CoupIneligibleN).

forecloses_census(Denom, EligibleN, CoupledN, LeakedN, CoupIneligibleN) :-
    census(forecloses, Denom, EligibleN, CoupledN, LeakedN, CoupIneligibleN).

% ----------------------------------------------------------------------------
% STANDALONE CENSUS RUNNERS (invoked per-leg via corpus_path overlay)
% ----------------------------------------------------------------------------
print_census(Rel, Denom, EligibleN, CoupledN, LeakedN, CoupIneligibleN) :-
    Suppressed is Denom - EligibleN,
    ( config:param(corpus_path, CPath) -> true ; CPath = '<default>' ),
    format("~n== ~w census (corpus=~w) ==~n", [Rel, CPath]),
    format("  1. denominator  (co-present pairs)      = ~w~n", [Denom]),
    format("  2. eligible     (purity>=0 both, dlt>0) = ~w~n", [EligibleN]),
    format("  3. coupled      (affects_constraint)    = ~w~n", [CoupledN]),
    format("  4. leaked       (effective_purity>0)    = ~w~n", [LeakedN]),
    format("  5. coupled-but-INELIGIBLE (funnel grd)  = ~w~n", [CoupIneligibleN]),
    format("  -- suppressed (denom - eligible)        = ~w~n", [Suppressed]).

run_coexists_census :-
    corpus_loader:ensure_corpus_loaded,
    coexists_census(D, E, C, L, CI),
    print_census(coexists_with, D, E, C, L, CI),
    report_leaked_pairs(coexists_with),
    ( D > 0 -> true
    ; format("  [NOTE] denominator 0 — sparsity green (no co-present coexists siblings)~n") ).

run_forecloses_census :-
    corpus_loader:ensure_corpus_loaded,
    forecloses_census(D, E, C, L, CI),
    print_census(forecloses, D, E, C, L, CI),
    report_leaked_pairs(forecloses).

% Single-pass: compute Contam once per pair. Display is capped at 25 lines
% (the count is the witness; the full list bloats logs on the big legs).
report_leaked_pairs(Rel) :-
    ctx(Ctx),
    copresent_pairs(Rel, Pairs),
    findall(Cm-(A-B), ( member(A-B, Pairs), pair_leak(A-B, Ctx, Cm) ), LeakedRaw),
    ( LeakedRaw == []
    ->  format("  leaked pairs: none~n")
    ;   length(LeakedRaw, NLeak),
        sort(0, @>=, LeakedRaw, Sorted),   % highest Contam first
        format("  LEAKED PAIRS (Contam>0), ~w total — showing up to 25 by Contam:~n", [NLeak]),
        forall(nth1(I, Sorted, Cm-(A-B)),
               ( I =< 25
               ->  format("    ~w <-> ~w  Contam=~6f~n", [A, B, Cm])
               ;   true )) ).

% ============================================================================
% PLUNIT — positive + negative controls (validate the INSTRUMENT).
%
% The controls check their OWN injected pair via the pair-level predicates
% (pair_eligible/pair_coupled/pair_leak) — NOT a global census count. Keying a
% control on the corpus-wide LeakedN would conflate the synthetic pair with any
% real leaks already in the loaded corpus (a Pattern-6 aggregate conflation —
% witnessed: with testsets/ loaded the real leaks made the equal-purity control's
% global "leaked==0" assertion fail). Pair-specific assertions are immune.
%
% The CORPUS-LEVEL measurement (the actual OQ-23 finding) is NOT a plunit test:
% it is run_coexists_census/0 under a corpus_path overlay, with raw logs saved to
% audits/2026-06-29_oq23_coexists_fpn_canary/census_*.log. While the exclusion is
% RED there is no green corpus invariant to assert; once the operator rules
% option 1 (filter), add a plunit test asserting coexists_census leaked == 0.
% ============================================================================

% Synthetic fixture: register two readings as corpus members with authored
% ontology presence. epistemic_access_check needs >= 3 classifications (config
% boltzmann_min_classifications) to lift purity off the -1.0 sentinel.
assert_reading(C, Ext, Supp, Type) :-
    assertz(narrative_ontology:constraint_claim(C, coexists_canary_fixture)),
    assertz(narrative_ontology:constraint_metric(C, extractiveness, Ext)),
    assertz(narrative_ontology:constraint_metric(C, suppression_requirement, Supp)),
    forall(member(X, [c_analytical, c_national, c_institutional]),
           assertz(constraint_indexing:constraint_classification(C, Type, X))),
    assertz(corpus_loader:corpus_constraint(C)).

% A high-extraction reading needs victim/beneficiary for d to resolve to a
% contaminating type (snare). Low-extraction stays rope.
assert_extractive_agents(C) :-
    assertz(narrative_ontology:constraint_victim(C, canary_public)),
    assertz(narrative_ontology:constraint_beneficiary(C, canary_elite)).

assert_coexists_pair(C1, UID1, C2) :-
    assertz(narrative_ontology:cs_story_uid(C1, UID1)),
    assertz(narrative_ontology:cs_reading_relation(UID1, C2, coexists_with)).

canary_teardown :-
    forall(member(C, [cx_high, cx_low, eq_a, eq_b, snt_a, snt_b, tw_x, tw_y]),
           ( retractall(narrative_ontology:constraint_claim(C, _)),
             retractall(narrative_ontology:constraint_metric(C, _, _)),
             retractall(constraint_indexing:constraint_classification(C, _, _)),
             retractall(corpus_loader:corpus_constraint(C)),
             retractall(narrative_ontology:constraint_victim(C, _)),
             retractall(narrative_ontology:constraint_beneficiary(C, _)),
             retractall(narrative_ontology:affects_constraint(C, _)),
             retractall(narrative_ontology:affects_constraint(_, C)),
             retractall(narrative_ontology:cs_story_uid(C, _)) )),
    retractall(narrative_ontology:cs_reading_relation(_, cx_low, _)),
    retractall(narrative_ontology:cs_reading_relation(_, eq_b, _)),
    retractall(narrative_ontology:cs_reading_relation(_, snt_b, _)),
    retractall(narrative_ontology:cs_reading_relation(_, tw_y, _)),
    cache_registry:clear_all_caches.

:- begin_tests(coexists_fpn_canary).

% --- POSITIVE CONTROL --------------------------------------------------------
% Fact-level injection through the recon-identified construction path: a
% coexists_with sibling pair, both co-present with differing non-sentinel
% purity, the low member a contaminating snare, joined by an affects_constraint
% side-channel edge. The canary MUST flag it (Contam > 0). Proves the probe SEES
% a leak — discharges Pattern 5. NOT a direct neighbor/edge assertion (that would
% prove only the arithmetic); the engine builds the edge from the facts.
test(positive_control_fact_level_leak,
     [setup(canary_teardown), cleanup(canary_teardown)]) :-
    assert_reading(cx_high, 0.15, 0.10, rope),
    assert_reading(cx_low,  0.90, 0.85, snare),
    assert_extractive_agents(cx_low),
    assertz(narrative_ontology:affects_constraint(cx_high, cx_low)),
    assert_coexists_pair(cx_high, uid_cx_high, cx_low),
    cache_registry:clear_all_caches,
    ctx(Ctx),

    % the census ENUMERATES this pair (sort_pair: cx_high @< cx_low)
    copresent_pairs(coexists_with, Pairs),
    assertion(memberchk(cx_high-cx_low, Pairs)),
    % ...it is eligible, coupled via the affects_constraint side channel...
    assertion(pair_eligible(cx_high-cx_low)),
    assertion(pair_coupled(cx_high-cx_low, Ctx)),
    % ...and it LEAKS (the probe SEES it — discharges Pattern 5)
    pair_leak(cx_high-cx_low, Ctx, Contam),
    assertion(Contam > 0.0),
    format("~n[positive control] leak detected on injected pair: Contam=~6f~n", [Contam]).

% --- NEGATIVE CONTROL: equal purity, coupled --------------------------------
% Same metrics → identical purity → delta 0 → Contam 0. Coupled but INELIGIBLE.
% Witnesses that a coupled pair with no purity delta does NOT leak (and feeds
% the funnel-guard #5 count its complement case).
test(negative_control_equal_purity,
     [setup(canary_teardown), cleanup(canary_teardown)]) :-
    assert_reading(eq_a, 0.90, 0.85, snare),
    assert_reading(eq_b, 0.90, 0.85, snare),
    assert_extractive_agents(eq_a),
    assert_extractive_agents(eq_b),
    assertz(narrative_ontology:affects_constraint(eq_a, eq_b)),
    assert_coexists_pair(eq_a, uid_eq_a, eq_b),
    cache_registry:clear_all_caches,
    ctx(Ctx),

    assertion(pair_coupled(eq_a-eq_b, Ctx)),       % side channel IS present
    assertion(\+ pair_eligible(eq_a-eq_b)),        % equal purity → Δ=0 → ineligible
    assertion(\+ pair_leak(eq_a-eq_b, Ctx, _)),    % so it does NOT leak
    format("~n[negative control: equal purity] coupled, ineligible (Δ=0), no leak~n").

% --- NEGATIVE CONTROL: sentinel donor (funnel-guard #5 direct witness) -------
% snt_b has NO classifications → purity_score -1.0 sentinel → ineligible. Even
% coupled, effective_purity short-circuits (Intrinsic<0 path / OtherPurity<0
% catch-all) → Contam 0. Directly witnesses the assumption funnel-count 5 banks:
% a coupled-but-sentinel-ineligible pair computes 0, never silently leaks.
test(negative_control_sentinel_donor,
     [setup(canary_teardown), cleanup(canary_teardown)]) :-
    assert_reading(snt_a, 0.15, 0.10, rope),
    % snt_b: claim + metrics + membership but NO classifications → sentinel purity
    assertz(narrative_ontology:constraint_claim(snt_b, coexists_canary_fixture)),
    assertz(narrative_ontology:constraint_metric(snt_b, extractiveness, 0.90)),
    assertz(narrative_ontology:constraint_metric(snt_b, suppression_requirement, 0.85)),
    assertz(corpus_loader:corpus_constraint(snt_b)),
    assertz(narrative_ontology:affects_constraint(snt_a, snt_b)),
    assert_coexists_pair(snt_a, uid_snt_a, snt_b),
    cache_registry:clear_all_caches,
    ctx(Ctx),

    purity_scoring:purity_score(snt_b, PB),
    assertion(PB < 0.0),                           % confirm sentinel
    assertion(pair_coupled(snt_a-snt_b, Ctx)),     % side channel IS present
    assertion(\+ pair_eligible(snt_a-snt_b)),      % sentinel donor → ineligible
    assertion(\+ pair_leak(snt_a-snt_b, Ctx, _)),  % short-circuits → no leak
    format("~n[negative control: sentinel donor] purity(snt_b)=~w → coupled, ineligible, no leak~n", [PB]).

% --- TRIPWIRE: typed relation alone creates NO edge (belt, not load-bearing) -
% A cs_reading_relation between two co-present, edge-FREE readings must NOT
% produce a contamination neighbor: constraint_neighbors_existing/2 reads no
% cs_reading_relation label. Covers the DIRECT typed-edge channel (data-
% independent). NOT the live risk (the affects_constraint side channel is what
% the canary proper covers) — kept as the belt.
test(tripwire_typed_relation_no_edge,
     [setup(canary_teardown), cleanup(canary_teardown)]) :-
    assert_reading(tw_x, 0.15, 0.10, rope),
    assert_reading(tw_y, 0.90, 0.85, snare),
    assert_extractive_agents(tw_y),
    % typed relation ONLY — no affects_constraint edge
    assert_coexists_pair(tw_x, uid_tw_x, tw_y),
    cache_registry:clear_all_caches,
    ctx(Ctx),
    drl_purity_network:constraint_neighbors(tw_x, Ctx, Nx),
    drl_purity_network:constraint_neighbors(tw_y, Ctx, Ny),
    assertion(\+ member(neighbor(tw_y, _, _), Nx)),
    assertion(\+ member(neighbor(tw_x, _, _), Ny)),
    \+ coexists_contamination_leak(tw_x-tw_y, _),
    format("~n[tripwire] typed coexists_with relation alone produced no contamination edge~n").

% NOTE: there is intentionally NO plunit test asserting a corpus-wide
% "leaked == 0" invariant. As measured 2026-06-29 the exclusion is RED on every
% populated leg (testsets/ 2 leaks, haiku 178, flash 361, kernel_v1 662 — see
% audits/2026-06-29_oq23_coexists_fpn_canary/census_*.log). The corpus-level
% measurement is run_coexists_census/0 under a corpus_path overlay, not a plunit
% assertion (there is no green to assert while RED, and a global-count assertion
% mixed with the controls would conflate synthetic and real leaks). When the
% operator rules option 1 (filter the side channel), add a plunit test here:
%   test(no_coexists_leak, [setup(load_corpus_nonempty)]) :-
%       coexists_census(_,_,_,Leaked,_), assertion(Leaked =:= 0).

:- end_tests(coexists_fpn_canary).
