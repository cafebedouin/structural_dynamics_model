% ============================================================================
% TEST: FPN Convergence-Proof Precondition Check under cs_reading_relation Injection
% ============================================================================
%
% Settles whether v7's architecture note (docs/two_axis_architecture_v7.md:33-47)
% makes a true claim when it asserts that feeding cs_reading_relation edges
% (forecloses/coexists_with) into the purity-contamination network "would
% invalidate the fixed-point convergence proof the network relies on."
%
% Three preconditions from the proof (drl_fpn.pl:44-62):
%   P1 — Monotone endofunctor: EP_k non-increasing; types invariant across iterations
%   P2 — Compact/complete lattice: domain [0,1]^2 — not a check target
%   P3 — Downward-only flow: Delta = max(0, MyPurity - OtherEP) >= 0 always
%
% Test corpus: tw_deterrence (rope, IP=0.75) and tw_doctrine (snare, IP=0.20)
%   from test_contradiction_signatures.pl:68-80.
%   cs_reading_relation(tw_deterrence, tw_doctrine, forecloses): det makes doc impossible.
%   Causal arrow for forecloses: det -> doc.
%
% MODULE QUALIFICATION NOTE: fpn_intrinsic/2, fpn_ep/3, fpn_iteration_info/4 are
% exported from drl_fpn — assertz without qualifier correctly targets drl_fpn.
% fpn_type_cache/3 and fpn_neighbors_cache/3 are NOT exported — always use the
% drl_fpn: qualifier, or the fact lands in the test module and is invisible to
% fpn_compute_ep inside drl_fpn.
%
% Branch outcomes (pre-registered):
%   A — Any precondition genuinely violated by a static injection
%   B — No precondition violated by any injection
%   C — Split (one edge violates, other does not)
%   D — Malformed-but-convergent (proof holds, semantics corrupt)
%   E — Gradient-orthogonality: correct direction inert, reversed causation-inverting
% ============================================================================

:- module(test_forecloses_fpn_injection, []).

:- use_module(drl_fpn).
:- use_module(drl_purity_network).
:- use_module(narrative_ontology).
:- use_module(constraint_indexing).
:- use_module(config).
:- use_module(library(plunit)).

% ---------------------------------------------------------------------------
% HELPERS
% ---------------------------------------------------------------------------

%% fpn_test_setup(+Ctx)
%  Clears FPN state for Ctx, then asserts intrinsic purities, initial EPs,
%  and type cache. fpn_intrinsic/2 and fpn_ep/3 are exported from drl_fpn,
%  so plain assertz targets drl_fpn correctly. fpn_type_cache/3 is NOT exported;
%  must use drl_fpn: qualifier.
fpn_test_setup(Ctx) :-
    fpn_cleanup(Ctx),
    % OQ-95: constraint_neighbors/3 is fail-closed on zero-fact atoms
    % (phantom_subject/1) — synthetic constraints must author ontology
    % presence (a claim suffices) to participate in the network.
    assertz(narrative_ontology:constraint_claim(tw_deterrence, fpn_injection_test_fixture)),
    assertz(narrative_ontology:constraint_claim(tw_doctrine,   fpn_injection_test_fixture)),
    assertz(fpn_intrinsic(tw_deterrence, 0.75)),
    assertz(fpn_intrinsic(tw_doctrine,   0.20)),
    assertz(drl_fpn:fpn_type_cache(tw_deterrence, Ctx, rope)),
    assertz(drl_fpn:fpn_type_cache(tw_doctrine,   Ctx, snare)),
    assertz(fpn_ep(tw_deterrence, Ctx, 0.75)),
    assertz(fpn_ep(tw_doctrine,   Ctx, 0.20)).

%% fpn_test_teardown(+Ctx)
fpn_test_teardown(Ctx) :-
    fpn_cleanup(Ctx),
    retractall(narrative_ontology:affects_constraint(tw_deterrence, tw_doctrine)),
    retractall(narrative_ontology:affects_constraint(tw_doctrine, tw_deterrence)),
    retractall(narrative_ontology:constraint_claim(tw_deterrence, _)),
    retractall(narrative_ontology:constraint_claim(tw_doctrine, _)).

%% cache_computed_neighbors(+Cs, +Ctx)
%  Calls constraint_neighbors/3 live for each C and caches the result into
%  drl_fpn:fpn_neighbors_cache (module-qualified — not exported from drl_fpn).
cache_computed_neighbors([], _).
cache_computed_neighbors([C|Rest], Ctx) :-
    constraint_neighbors(C, Ctx, Ns),
    assertz(drl_fpn:fpn_neighbors_cache(C, Ctx, Ns)),
    cache_computed_neighbors(Rest, Ctx).

%% fpn_one_jacobi_step(+Cs, +Ctx, -MaxDelta)
%  Executes one Jacobi update without the convergence check.
fpn_one_jacobi_step(Cs, Ctx, MaxDelta) :-
    findall(C-NewEP,
            ( member(C, Cs), drl_fpn:fpn_compute_ep(C, Ctx, NewEP) ),
            NewValues),
    drl_fpn:fpn_jacobi_update(NewValues, Ctx, MaxDelta).

ctx(Ctx) :- constraint_indexing:default_context(Ctx).

% ---------------------------------------------------------------------------
:- begin_tests(fpn_injection).

% ---------------------------------------------------------------------------
% A1 — BASELINE: No injection between tw_deterrence and tw_doctrine
% ---------------------------------------------------------------------------
test(a1_baseline_no_injection, [
    setup(ctx(Ctx)),
    cleanup(fpn_test_teardown(Ctx))
]) :-
    ctx(Ctx),
    fpn_test_setup(Ctx),
    cache_computed_neighbors([tw_deterrence, tw_doctrine], Ctx),

    config:param(fpn_epsilon, Eps),
    config:param(fpn_max_iterations, MaxIter),
    drl_fpn:fpn_iterate([tw_deterrence, tw_doctrine], Ctx, 1, MaxIter, Eps),
    fpn_iteration_info(Ctx, Iters, MaxDelta, Converged),

    fpn_ep(tw_deterrence, Ctx, EP_det),
    fpn_ep(tw_doctrine,   Ctx, EP_doc),

    assertion(Converged == true),
    assertion(EP_det =:= 0.75),
    assertion(EP_doc =:= 0.20),
    !,

    format("~n=== A1 BASELINE (no injection) ===~n"),
    format("  Converged=~w  iterations=~w  MaxDelta=~6f~n", [Converged, Iters, MaxDelta]),
    format("  EP_det=~4f (IP=0.75)  EP_doc=~4f (IP=0.20)~n", [EP_det, EP_doc]),
    format("  P1: HOLDS (converged, EP=IP — isolated, no cross-edges)~n"),
    format("  P3: HOLDS trivially (no cross-edges, Delta=0 everywhere)~n"),
    format("  Baseline confirmed.~n~n").

% ---------------------------------------------------------------------------
% CASE 1a — forecloses in CORRECT SEMANTIC DIRECTION (det -> doc)
%
% CRITICAL WITNESS: confirm tw_deterrence is present in tw_doctrine's COMPUTED
% neighbor list AND Delta is computed by fpn_edge_contamination, not asserted.
% EP_doc = IP_doc is ambiguous (same for absent edge); inert-with-edge requires
% all three: edge present, Delta=0 computed, Contam=0 computed.
% ---------------------------------------------------------------------------
test(forecloses_1a_correct_direction, [
    setup((
        ctx(Ctx),
        assertz(narrative_ontology:affects_constraint(tw_deterrence, tw_doctrine))
    )),
    cleanup(fpn_test_teardown(Ctx))
]) :-
    ctx(Ctx),
    fpn_test_setup(Ctx),

    % --- STEP 1: Compute tw_doctrine's neighbor list live ---
    constraint_neighbors(tw_doctrine, Ctx, DocNeighbors),
    format("~n=== CASE 1a: forecloses in correct direction (det->doc) ===~n"),
    format("  tw_doctrine's computed neighbor list: ~w~n", [DocNeighbors]),

    % Edge-presence check (once/1 prevents choicepoint from member/2)
    (   once(member(neighbor(tw_deterrence, EdgeStr, explicit), DocNeighbors))
    ->  format("  EDGE PRESENT: neighbor(tw_deterrence, ~w, explicit) confirmed~n", [EdgeStr])
    ;   format("  EDGE ABSENT: tw_deterrence not in list — cannot support Branch E~n"),
        fail
    ),

    % --- STEP 2: Delta and contamination via the actual predicate ---
    % fpn_edge_contamination reads fpn_ep(tw_deterrence, Ctx, OtherEP) = 0.75.
    % Delta = max(0, 0.20 - 0.75) = 0.
    fpn_ep(tw_deterrence, Ctx, DetEP),
    DocIP = 0.20,
    Delta is max(0.0, DocIP - DetEP),
    drl_fpn:fpn_edge_contamination(DocIP, tw_deterrence, EdgeStr, Ctx, Contam),

    format("  DetEP (from fpn_ep)   = ~4f~n", [DetEP]),
    format("  DocIP                 = ~4f~n", [DocIP]),
    format("  Delta = max(0, ~4f - ~4f) = ~4f~n", [DocIP, DetEP, Delta]),
    format("  fpn_edge_contamination: Contam = ~4f~n", [Contam]),

    assertion(Delta  =:= 0.0),
    assertion(Contam =:= 0.0),

    % --- STEP 3: FPN run with edge present (using computed neighbor lists) ---
    assertz(drl_fpn:fpn_neighbors_cache(tw_doctrine, Ctx, DocNeighbors)),
    constraint_neighbors(tw_deterrence, Ctx, DetNeighbors),
    assertz(drl_fpn:fpn_neighbors_cache(tw_deterrence, Ctx, DetNeighbors)),

    config:param(fpn_epsilon, Eps),
    config:param(fpn_max_iterations, MaxIter),
    drl_fpn:fpn_iterate([tw_deterrence, tw_doctrine], Ctx, 1, MaxIter, Eps),
    fpn_iteration_info(Ctx, Iters1a, _MaxDelta1a, Converged1a),
    fpn_ep(tw_doctrine, Ctx, EP_doc_final),

    assertion(Converged1a == true),
    assertion(EP_doc_final =:= DocIP),

    format("  FPN: converged=~w  iterations=~w~n", [Converged1a, Iters1a]),
    format("  EP_doc_final=~4f = IP_doc=~4f  (edge PRESENT-AND-INERT, not absent)~n",
           [EP_doc_final, DocIP]),
    format("~n  FINDING: Correctly-oriented forecloses is gradient-orthogonal.~n"),
    format("    forecloses points UP the gradient (high-purity det -> low-purity doc)~n"),
    format("    but contamination flows DOWN. Delta=max(0,0.20-0.75)=0.~n"),
    format("    Inert in its correct direction. Not a P3 violation (Delta >= 0),~n"),
    format("    but the edge cannot produce non-zero contamination without~n"),
    format("    reversing its causal arrow (see 1b).~n"),
    format("  BRANCH E CANDIDATE: inert-with-edge confirmed.~n~n").

% ---------------------------------------------------------------------------
% CASE 1b — Scalar reversed injection (doc -> det)
% Reverses causal direction; produces non-zero contamination. Confirms the
% scalar proof survives P1+P3. Records that causation is inverted.
% ---------------------------------------------------------------------------
test(forecloses_1b_reversed_scalar, [
    setup((
        ctx(Ctx),
        assertz(narrative_ontology:affects_constraint(tw_doctrine, tw_deterrence))
    )),
    cleanup(fpn_test_teardown(Ctx))
]) :-
    ctx(Ctx),
    fpn_test_setup(Ctx),

    constraint_neighbors(tw_deterrence, Ctx, DetNeighbors),
    format("~n=== CASE 1b: forecloses reversed (doc->det, causation inverted) ===~n"),
    format("  tw_deterrence's computed neighbor list: ~w~n", [DetNeighbors]),

    (   once(member(neighbor(tw_doctrine, EdgeStr, explicit), DetNeighbors))
    ->  format("  EDGE PRESENT: neighbor(tw_doctrine, ~w, explicit)~n", [EdgeStr])
    ;   fail
    ),

    fpn_ep(tw_doctrine, Ctx, DocEP),
    DetIP = 0.75,
    Delta is max(0.0, DetIP - DocEP),
    drl_fpn:fpn_edge_contamination(DetIP, tw_doctrine, EdgeStr, Ctx, Contam),

    format("  DocEP (from fpn_ep)   = ~4f~n", [DocEP]),
    format("  DetIP                 = ~4f~n", [DetIP]),
    format("  Delta = max(0, ~4f - ~4f) = ~4f~n", [DetIP, DocEP, Delta]),
    format("  fpn_edge_contamination: Contam = ~4f~n", [Contam]),

    % P3: Delta >= 0 (downward flow holds)
    assertion(Delta  >= 0.0),
    % Contamination must be non-zero — confirms reversed injection actually flows
    assertion(Contam >  0.0),

    assertz(drl_fpn:fpn_neighbors_cache(tw_deterrence, Ctx, DetNeighbors)),
    constraint_neighbors(tw_doctrine, Ctx, DocNeighbors),
    assertz(drl_fpn:fpn_neighbors_cache(tw_doctrine, Ctx, DocNeighbors)),

    config:param(fpn_epsilon, Eps),
    config:param(fpn_max_iterations, MaxIter),
    drl_fpn:fpn_iterate([tw_deterrence, tw_doctrine], Ctx, 1, MaxIter, Eps),
    fpn_iteration_info(Ctx, Iters1b, MaxDelta1b, Converged1b),
    fpn_ep(tw_deterrence, Ctx, EP_det_final),

    assertion(Converged1b == true),
    assertion(EP_det_final < 0.75),

    format("  FPN: converged=~w  iterations=~w  MaxDelta=~6f~n",
           [Converged1b, Iters1b, MaxDelta1b]),
    format("  EP_det_final=~4f < IP_det=0.75 (contamination applied)~n", [EP_det_final]),
    format("~n  P1: HOLDS (converged, EP_det strictly decreased)~n"),
    format("  P3: HOLDS (Delta=~4f >= 0)~n", [Delta]),
    format("  FINDING: Scalar reversed injection satisfies all proof preconditions.~n"),
    format("    But causal direction is inverted: output says doc's impurity corrodes det;~n"),
    format("    forecloses says det makes doc structurally impossible.~n"),
    format("    Convergent. Causation inverted. Branch D/E territory.~n~n").

% ---------------------------------------------------------------------------
% CASE 1c — Negative control on rigging
%
% The ONLY way to get non-zero activity from forecloses in its correct direction
% is to inject a mechanism the edge does not natively have: a mid-iteration type
% mutation. This DOES break P1, but the violation is BUILT, not FOUND.
% The forecloses edge does not require type mutation; its correct-direction
% representation is static and inert (1a). 1c demonstrates how the test could
% have rigged itself; it cannot count as Branch A.
% ---------------------------------------------------------------------------
test(forecloses_1c_typeflip_rigging_control, [
    setup((
        ctx(Ctx),
        assertz(narrative_ontology:affects_constraint(tw_doctrine, tw_deterrence))
    )),
    cleanup(fpn_test_teardown(Ctx))
]) :-
    ctx(Ctx),
    fpn_test_setup(Ctx),
    Cs = [tw_deterrence, tw_doctrine],

    constraint_neighbors(tw_deterrence, Ctx, DetNeighbors),
    constraint_neighbors(tw_doctrine,   Ctx, DocNeighbors),
    assertz(drl_fpn:fpn_neighbors_cache(tw_deterrence, Ctx, DetNeighbors)),
    assertz(drl_fpn:fpn_neighbors_cache(tw_doctrine,   Ctx, DocNeighbors)),

    format("~n=== CASE 1c: type-flip rigging control ===~n"),
    format("  Setup: doc->det (reversed); tw_doctrine type=snare~n"),

    % --- Jacobi step 1: doc is snare (contamination_strength 1.0) ---
    fpn_one_jacobi_step(Cs, Ctx, MaxDelta1),
    fpn_ep(tw_deterrence, Ctx, EP_det_1),
    format("  Iteration 1 (doc=snare, strength=1.0): EP_det=~4f  MaxDelta=~6f~n",
           [EP_det_1, MaxDelta1]),

    % --- Apply forecloses categorical override: snare -> foreclosed ---
    % type_contamination_strength(foreclosed, 0.0) via catch-all.
    % type_immunity(foreclosed, 0.5) via catch-all — same as snare.
    % Net effect: doc stops emitting contamination.
    retract(drl_fpn:fpn_type_cache(tw_doctrine, Ctx, snare)),
    assertz(drl_fpn:fpn_type_cache(tw_doctrine, Ctx, foreclosed)),
    format("  Type flip: tw_doctrine snare -> foreclosed~n"),
    format("    contamination_strength: 1.0 -> 0.0 (catch-all)~n"),

    % --- Jacobi step 2: doc is foreclosed (contamination_strength 0.0) ---
    fpn_one_jacobi_step(Cs, Ctx, MaxDelta2),
    fpn_ep(tw_deterrence, Ctx, EP_det_2),
    format("  Iteration 2 (doc=foreclosed, strength=0.0): EP_det=~4f  MaxDelta=~6f~n",
           [EP_det_2, MaxDelta2]),

    format("~n  P1 monotonicity check: EP_det iter1=~4f  iter2=~4f~n", [EP_det_1, EP_det_2]),
    (   EP_det_2 > EP_det_1
    ->  format("  P1 VIOLATED: EP_det INCREASED (~4f -> ~4f) after type flip~n",
               [EP_det_1, EP_det_2])
    ;   format("  P1 holds: EP_det did not increase (~4f -> ~4f)~n", [EP_det_1, EP_det_2])
    ),

    format("~n  VERDICT: BUILT, NOT FOUND.~n"),
    format("    forecloses does not require mid-iteration type mutation.~n"),
    format("    Correct-direction static representation is inert (1a, Delta=0).~n"),
    format("    Type flip was invented to force activity the edge natively lacks.~n"),
    format("    P1 breaks because type-invariance invariant (drl_fpn.pl:60-62) was~n"),
    format("    violated by hand. Tests 'type mutations break type-invariance proofs'~n"),
    format("    — true by inspection, not a property of forecloses.~n"),
    format("  1c is a NEGATIVE CONTROL ON RIGGING. Cannot count as Branch A.~n~n").

% ---------------------------------------------------------------------------
% CASE 2 — coexists_with: label-blindness check
%
% v7: coexists_with is "zero-flow by definition." Tests whether this is a
% mathematical property or an unimplemented design intent. Same scalar injection
% as 1b — FPN cannot read the semantic label.
% ---------------------------------------------------------------------------
test(coexists_with_label_blindness, [
    setup((
        ctx(Ctx),
        assertz(narrative_ontology:affects_constraint(tw_doctrine, tw_deterrence))
    )),
    cleanup(fpn_test_teardown(Ctx))
]) :-
    ctx(Ctx),
    fpn_test_setup(Ctx),

    constraint_neighbors(tw_deterrence, Ctx, DetNeighbors),
    format("~n=== CASE 2: coexists_with label-blindness check ===~n"),
    format("  tw_deterrence's computed neighbor list: ~w~n", [DetNeighbors]),

    (   once(member(neighbor(tw_doctrine, EdgeStr, explicit), DetNeighbors))
    ->  format("  EDGE PRESENT: neighbor(tw_doctrine, ~w, explicit)~n", [EdgeStr])
    ;   fail
    ),

    fpn_ep(tw_doctrine, Ctx, DocEP),
    DetIP = 0.75,
    Delta is max(0.0, DetIP - DocEP),
    drl_fpn:fpn_edge_contamination(DetIP, tw_doctrine, EdgeStr, Ctx, Contam),

    format("  Delta = max(0, ~4f - ~4f) = ~4f~n", [DetIP, DocEP, Delta]),
    format("  fpn_edge_contamination: Contam = ~4f~n", [Contam]),

    % If "zero by definition" were a mathematical property, Contam would be 0.
    % Contam > 0 confirms label-blindness: FPN computes identical to 1b.
    assertion(Contam > 0.0),

    assertz(drl_fpn:fpn_neighbors_cache(tw_deterrence, Ctx, DetNeighbors)),
    constraint_neighbors(tw_doctrine, Ctx, DocNeighbors),
    assertz(drl_fpn:fpn_neighbors_cache(tw_doctrine, Ctx, DocNeighbors)),

    config:param(fpn_epsilon, Eps),
    config:param(fpn_max_iterations, MaxIter),
    drl_fpn:fpn_iterate([tw_deterrence, tw_doctrine], Ctx, 1, MaxIter, Eps),
    fpn_iteration_info(Ctx, Iters2, MaxDelta2, Converged2),
    fpn_ep(tw_deterrence, Ctx, EP_det_final),

    assertion(Converged2 == true),
    assertion(EP_det_final < 0.75),

    format("  FPN: converged=~w  iterations=~w  MaxDelta=~6f~n",
           [Converged2, Iters2, MaxDelta2]),
    format("  EP_det_final=~4f (FPN displaced det despite coexists_with semantics)~n",
           [EP_det_final]),
    format("~n  P1: HOLDS  P3: HOLDS~n"),
    format("  FINDING: 'zero by definition' is unimplemented design intent, not math property.~n"),
    format("    No code path in drl_purity_network.pl or drl_fpn.pl returns zero contamination~n"),
    format("    for coexists_with edges. FPN is label-blind: Contam=~4f, same as 1b.~n~n",
           [Contam]).

% ---------------------------------------------------------------------------
% CASE 3 — influences: control (the intentional bridge)
%
% influences already crosses the seam (detect_necessity_inheritance, drl_composition.pl).
% Must satisfy all preconditions. Calibration: if 1a is inert while 3 converges,
% the inertness is directional (gradient-orthogonality), not label-specific.
% ---------------------------------------------------------------------------
test(influences_control, [
    setup((
        ctx(Ctx),
        assertz(narrative_ontology:affects_constraint(tw_doctrine, tw_deterrence))
    )),
    cleanup(fpn_test_teardown(Ctx))
]) :-
    ctx(Ctx),
    fpn_test_setup(Ctx),

    constraint_neighbors(tw_deterrence, Ctx, DetNeighbors),
    format("~n=== CASE 3: influences control (intentional bridge) ===~n"),
    format("  tw_deterrence's computed neighbor list: ~w~n", [DetNeighbors]),

    (   once(member(neighbor(tw_doctrine, EdgeStr, explicit), DetNeighbors))
    ->  format("  EDGE PRESENT: neighbor(tw_doctrine, ~w, explicit)~n", [EdgeStr])
    ;   fail
    ),

    fpn_ep(tw_doctrine, Ctx, DocEP),
    DetIP = 0.75,
    Delta is max(0.0, DetIP - DocEP),
    drl_fpn:fpn_edge_contamination(DetIP, tw_doctrine, EdgeStr, Ctx, Contam),

    format("  Delta=~4f  Contam=~4f~n", [Delta, Contam]),
    assertion(Delta  >= 0.0),
    assertion(Contam >  0.0),

    assertz(drl_fpn:fpn_neighbors_cache(tw_deterrence, Ctx, DetNeighbors)),
    constraint_neighbors(tw_doctrine, Ctx, DocNeighbors),
    assertz(drl_fpn:fpn_neighbors_cache(tw_doctrine, Ctx, DocNeighbors)),

    config:param(fpn_epsilon, Eps),
    config:param(fpn_max_iterations, MaxIter),
    drl_fpn:fpn_iterate([tw_deterrence, tw_doctrine], Ctx, 1, MaxIter, Eps),
    fpn_iteration_info(Ctx, Iters3, MaxDelta3, Converged3),
    fpn_ep(tw_deterrence, Ctx, EP_det_final),

    assertion(Converged3 == true),
    assertion(EP_det_final < 0.75),

    format("  FPN: converged=~w  iterations=~w  MaxDelta=~6f~n",
           [Converged3, Iters3, MaxDelta3]),
    format("  EP_det_final=~4f~n", [EP_det_final]),
    format("~n  P1: HOLDS  P3: HOLDS~n"),
    format("  CONTROL CONFIRMED: influences (scalar injection) satisfies all preconditions.~n"),
    format("    FPN is label-blind: influences produces same result as 1b and 2.~n"),
    format("    Calibration: 1a's inertness is directional (gradient-orthogonal),~n"),
    format("    not a label-specific rejection by the FPN.~n~n").

:- end_tests(fpn_injection).
