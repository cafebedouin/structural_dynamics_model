:- module(signature_detection, [
    constraint_signature/2,
    signature_confidence/3,
    explain_signature/3,
    integrate_signature_with_modal/3,
    false_natural_law/2,
    false_summit_mountain/2,
    coupling_invariant_rope/2,
    false_ci_rope/2,
    structural_purity/2,
    resolve_modal_signature_conflict/3,
    get_constraint_profile/2,
    has_viable_alternatives/2,
    has_metric_perspectival_variance/1,
    signature_grade/2,              % OQ-98: correction | commentary
    signature_severity/2,           % OQ-98: correction-grade -> moderate
    level_gradient_divergence/2,    % OQ-93 Stage D: the level-gradient crossing
    residual_signature_firing/1     % OQ-138 (2026-07-14): residual-clause fire monitor
]).

:- use_module(library(lists)).
:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(boltzmann_compliance).
:- use_module(coercion_projection).   % OQ-93 Stage D: level gradients from the authored grid

/* ================================================================
   STRUCTURAL SIGNATURE DETECTION v3.2

   Problem Statement (from Copilot's analysis):
   "Your classifier sees MAGNITUDE but not TYPE."

   The current DRL classifier uses only metric thresholds:
   - suppression > 0.1 → not a mountain
   - extractiveness > 0.7 → snare

   This causes misclassification of conceptual constraints:
   - Chaitin's Ω: collapse=1.0, suppression=0.0 → classified as mountain
   - Special Relativity: collapse=1.0, suppression=0.0 → classified as mountain
   - Arrow's Theorem: collapse=0.9, suppression=0.1 → fraud detection fires

   But these are STRUCTURALLY DIFFERENT:
   - Chaitin's Ω = NATURAL LAW (inherent impossibility)
   - Special Relativity = COORDINATION SCAFFOLD (successful standard)
   - Arrow's Theorem = NATURAL LAW (mathematical impossibility)

   Solution: Add STRUCTURAL SIGNATURES that detect constraint ORIGIN
   rather than just constraint METRICS.

   Three Core Signatures:
   1. Natural Law - empirical invariant, no alternatives possible
   2. Coordination Scaffold - voluntary equilibrium, alternatives existed
   3. Constructed Constraint - enforced rule, power asymmetries
   ================================================================ */

%% constraint_signature(+ConstraintID, -Signature)
%  Main entry point: classifies structural signature
%  Returns: false_natural_law | false_ci_rope | coupling_invariant_rope
%         | natural_law | coordination_scaffold | piton_signature
%         | constructed_low_extraction | constructed_high_extraction
%         | constructed_constraint | ambiguous
%
%  Priority order:
%    1. Boltzmann-derived signatures (v5.1) — checked first, most specific
%       a. FNL — catches false mountains (physics-washed)
%       b. FCR — catches false ropes (coordination-washed)
%       c. CI_Rope — certifies true coordination
%    2. Profile-based signatures (v3.2) — fallback classification

% Boltzmann-derived: False Natural Law (v5.1)
% Intercepts constraints that claim naturality but fail Boltzmann independence.
% Checked BEFORE natural_law to catch "physics-washed" constraints.
constraint_signature(C, false_natural_law) :-
    false_natural_law(C, _), !.

% Boltzmann-derived: False CI_Rope (v5.1)
% Intercepts constraints that appear to be ropes from metrics but fail
% Boltzmann structural tests. The "coordination-washed" analogue of FNL.
% Checked BEFORE CI_Rope to catch constraints that would falsely certify.
constraint_signature(C, false_ci_rope) :-
    false_ci_rope(C, _), !.

% Metric-derived: False Summit Mountain (v6.9; agency gate June 2026)
% Intercepts constraints that meet all mountain metric thresholds but have
% identifiable AGENT beneficiaries. Genuine natural laws have zero agent
% beneficiaries; a mountain with agent beneficiaries indicates a naturalized
% constructed constraint. Proposition-kind beneficiaries (doctrines/hypotheses
% the constraint vindicates) are filtered by narrative_ontology:agent_beneficiary/2
% and do NOT trip FSM. Checked BEFORE natural_law so agent-beneficiary-bearing
% constraints are not certified as natural law. NOTE (2026-05-31 gap check):
% FSM is NOT belt-and-suspenders backup for natural_law_signature's
% BeneficiaryCount==0 gate — it is the ONLY live beneficiary screen. By cascade
% construction FSM catches every mountain-metric + emerges_naturally constraint
% that carries an agent_beneficiary/2 solution, so the constraints that fall
% through to the natural_law clause below are exactly the agent-beneficiary-blind
% residue. Measured on testsets_3000 (pre-agency-gate): 0/404 NL-signature
% constraints carry a constraint_beneficiary fact (corpus held 6739 such facts,
% none on the 404). NL's own BeneficiaryCount gate read intent_power_change,
% which is empty corpus-wide (0 facts) — that gate was dormant-over-empty-table,
% satisfied by absence; it now counts agent beneficiaries (see
% count_power_beneficiaries). See ISSUES.md OQ on the satisfy-on-absence class.
constraint_signature(C, false_summit_mountain) :-
    false_summit_mountain(C, _), !.

% Boltzmann-derived: Natural Law via Emergence (v6.1)
% Intercepts before CI_Rope for natural laws with incidental beneficiaries.
% A constraint that emerges naturally AND passes the full NL profile check
% is certified as natural_law regardless of coordination-function status.
% Without this, constraints like P!=NP (which have constraint_beneficiary
% declarations for perspectival analysis) would be intercepted by CI_Rope
% and misclassified as rope.
constraint_signature(C, natural_law) :-
    domain_priors:emerges_naturally(C),
    get_constraint_profile(C, Profile),
    natural_law_signature(Profile), !.

% Boltzmann-derived: Coupling-Invariant Rope (v5.1)
% Certifies true coordination mechanisms with full Boltzmann invariance.
% Checked before profile-based classification for positive certification.
constraint_signature(C, coupling_invariant_rope) :-
    coupling_invariant_rope(C, _), !.

% Honest abstain on absent authored metrics (2026-06-08, audit
% 2026-06-08_coordination_washing_clean_pass). Missing metric vectors used to
% default to 0.5 in get_metric_average; 0.5 exceeds snare_epsilon_floor (0.46),
% so a constraint with no authored extraction vector fabricated a
% constructed_high_extraction / false_ci_rope verdict from no data. The
% authored-claim signatures above (FNL/FCR/FSM/NL via emergence/CI_Rope) read
% constraint_claim / constraint_metric / base_extractiveness / agent_beneficiary
% directly and fail (not throw) on absence, so they still fire when their
% authored inputs exist. Only the metric-derived fallback path reaches here; when
% any metric it consumes is the `unknown` absence sentinel, emit the honest
% `unknown` signature instead of a default-fabricated verdict. The cut guards the
% fallback clause below so classify_by_signature never sees `unknown`.
constraint_signature(C, unknown) :-
    \+ profile_metrics_authored(C), !.

% Profile-based classification (v3.2 original pipeline)
constraint_signature(C, Signature) :-
    get_constraint_profile(C, Profile),
    config:param(extractiveness_metric_name, ExtMetricName),
    get_metric_average(C, ExtMetricName, Extraction),
    classify_by_signature(Profile, Extraction, Signature).

%% profile_metrics_authored(+C)
%  True iff every metric the metric-derived signature fallback path consumes is
%  an authored number (not the `unknown` absence sentinel returned by
%  get_metric_average/3 on a missing vector). Gate for the fail-closed abstain
%  above. Uses number/1, never arithmetic, so it cannot throw on `unknown`.
profile_metrics_authored(C) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    get_metric_average(C, ExtMetricName, Ext),           number(Ext),
    get_metric_average(C, accessibility_collapse, AC),   number(AC),
    get_metric_average(C, SuppMetricName, Sup),          number(Sup),
    get_metric_average(C, resistance, Res),              number(Res).

/* ================================================================
   PROFILE EXTRACTION

   Extracts 7 key features for signature classification:
   1. Accessibility Collapse (avg across time)
   2. Suppression Requirement (avg across time)
   3. Resistance Level (avg across time)
   4. Beneficiary Count (how many asymmetric winners)
   5. Has Viable Alternatives (were there choices?)
   6. Temporal Stability (does it evolve or remain constant?)
   7. Coordination Success (high access + low enforcement)
   ================================================================ */

get_constraint_profile(C,
                      profile(AccessCollapse, Suppression, Resistance,
                             BeneficiaryCount, HasAlternatives,
                             TemporalStability, CoordinationSuccess)) :-

    config:param(suppression_metric_name, SuppMetricName),

    % Get averaged metrics across all levels
    get_metric_average(C, accessibility_collapse, AccessCollapse),
    get_metric_average(C, SuppMetricName, Suppression),
    get_metric_average(C, resistance, Resistance),

    % Count asymmetric beneficiaries
    count_power_beneficiaries(C, BeneficiaryCount),

    % Check for viable alternatives
    has_viable_alternatives(C, HasAlternatives),

    % Compute temporal stability
    compute_temporal_stability(C, SuppMetricName, TemporalStability),

    % Check coordination success pattern
    CoordinationSuccess = (AccessCollapse > 0.8, Suppression < 0.2).

%% get_metric_average(+Constraint, +MetricType, -Average)
%  Averages a metric across all levels (structural, organizational, class, individual)
get_metric_average(C, MetricType, Average) :-
    findall(Val, narrative_ontology:constraint_metric(C, MetricType, Val), Vals),
    (   Vals \= []
    ->  sum_list(Vals, Sum),
        length(Vals, N),
        Average is Sum / N
    ;   Average = unknown  % Honest abstain on missing data (was 0.5, which
                           % exceeded snare_epsilon_floor 0.46 and fabricated a
                           % constructed_high_extraction verdict from no data;
                           % audit 2026-06-08_coordination_washing_clean_pass).
                           % Consumers must guard with number/1 — see
                           % profile_metrics_authored/1 and the unknown-abstain
                           % clause of constraint_signature/2.
    ).

%% count_power_beneficiaries(+Constraint, -Count)
%  Counts distinct classes with positive power changes
count_power_beneficiaries(C, Count) :-
    % D3 / OQ-43 fail-close: read the AUTHORED, populated constraint_beneficiary table
    % (1237 facts live), NOT the join against intent_power_change — which is empty corpus-wide
    % (0 facts in BOTH corpora) and so made `BeneficiaryCount == 0` a VACUOUS pass for every
    % constraint (passing by data-absence, never by a checked condition). Absence of a
    % constraint_beneficiary/2 fact is authored-zero (generation emits one when a beneficiary is
    % declared), so counting authored beneficiaries makes the natural_law gate's
    % `BeneficiaryCount == 0` an honest check over a NON-empty table — consistent with
    % false_summit_mountain (:1217) and drl_core:287, which already read constraint_beneficiary.
    % This does NOT populate intent_* (deliberately); whether any of the 404 NL certifications
    % hide an authored winner is a separate corpus-content audit (OQ-45). Success criterion is
    % "the gate stops passing on absence," NOT "the NL/mountain count stayed fixed" — a count
    % shift here is a possibly-correct outcome (the gate declining to certify on absence),
    % recorded as a finding, not reverted as a regression.
    % June 2026 agency gate: count AGENT beneficiaries only (a proposition the
    % constraint vindicates is not an "asymmetric winner"), so natural_law's
    % BeneficiaryCount==0 stops being blocked by proposition-kind values.
    % Registry + two-gate principle: narrative_ontology:non_agent_beneficiary/1.
    findall(B, narrative_ontology:agent_beneficiary(C, B), Beneficiaries),
    sort(Beneficiaries, UniqueBeneficiaries),
    length(UniqueBeneficiaries, Count).

%% has_viable_alternatives(+Constraint, -HasAlternatives)
%  Checks if viable alternatives were considered (indicates choice vs necessity).
%  OQ-43 fifth instance / OQ-44 policy fail-close (operator ruling 2026-06-11):
%  the default used to be `false`, which the empty corpus-wide
%  intent_viable_alternative/3 table (GAP-08) satisfied by ABSENCE — and
%  natural_law_signature requires HasAlternatives == false, so the absence
%  SUPPORTED every NL certification (pass-open). Default is now `unknown`:
%  `false` requires authored evidence (none exists — no authoring surface yet),
%  so NL's == false check fails until the intent layer or an authored
%  alternatives table exists. The resulting un-certification of
%  thermal_dissipation_constraint was ACCEPTED in the ruling (a known-vacuous
%  certification is tolerated rather than undetected). The == true consumers
%  (coordination_scaffold/successful_coordination) are unchanged: they never
%  fired on the empty table and still don't.
%
%  OQ-113 (closed 2026-06-18, fork (b) — evidence-forced, not chosen): the
%  RANGE of this builder is exactly {true, unknown}; `false` is
%  BUILDER-UNREACHABLE (not merely unauthored — there is NO clause that can
%  emit it). So natural_law_signature/1's `HasAlternatives == false` leg is
%  DEAD-BY-RANGE on every corpus (0 firings, live-corpus witnessed
%  has_viable_alternatives never returns false). Powering it is NOT "extend the
%  builder to emit false" — that collapses into building the GAP-08 §7
%  author-independent immovability signal (a structural fact written by neither
%  the author nor the degradation that reads "immovable"), which does not yet
%  exist. The remaining `false`-sources are falsified: prose/omega-`false` is
%  the contaminated source GAP-08 rejects (370/627 engine-mountains carry
%  "impossible alternative" prose AND a contested reading). Refs: OQ-113,
%  docs/design/design_gaps.md GAP-08 (the §7 author-independent immovability
%  residual), audits/2026-06-17_mountain_authoring_sweep/ROUTING_SINK_DESIGN.md
%  §9a(i)/§7. The natural_law detector survives as a wired-but-dark router
%  socket (OQ-128 retired its override; the gate leg stays documented, not
%  revived).
has_viable_alternatives(C, true) :-
    narrative_ontology:affects_constraint(I, C),
    narrative_ontology:intent_viable_alternative(I, _, _), !.
has_viable_alternatives(_, unknown).

%% compute_temporal_stability(+Constraint, -Stability)
%  Measures whether constraint metrics remain stable over time
%  Returns: stable | evolving
compute_temporal_stability(C, MetricName, Stability) :-
    % Get suppression values at different time points for this constraint
    findall(Val,
            narrative_ontology:constraint_metric(C, MetricName, Val),
            Vals),
    (   Vals = []
    ->  Stability = unknown
    ;   Vals = [_SingleVal]
    ->  Stability = stable  % Only one measurement
    ;   compute_variance(Vals, Variance),
        (   Variance < 0.05
        ->  Stability = stable
        ;   Stability = evolving
        )
    ).

compute_variance(Vals, Variance) :-
    length(Vals, N),
    N > 0,
    sum_list(Vals, Sum),
    Mean is Sum / N,
    findall(SqDiff, (member(V, Vals), SqDiff is (V - Mean) * (V - Mean)), SqDiffs),
    sum_list(SqDiffs, SumSqDiffs),
    Variance is SumSqDiffs / N.

/* ================================================================
   SIGNATURE CLASSIFICATION LOGIC

   Decision Tree:

   1. Check Natural Law signature FIRST (most specific)
      - Extreme collapse + minimal enforcement + no alternatives
      - Examples: Chaitin's Ω, Heisenberg, Arrow's Theorem

   2. Check Coordination Scaffold SECOND
      - Extreme collapse + minimal enforcement + HAS alternatives
      - Examples: Special Relativity, SI Units, ISO Standards

   3. Check Constructed Constraint LAST (most general)
      - Positive enforcement OR beneficiary asymmetries
      - Examples: 26 USC §469, GS1 Barcodes, Hammurabi's Code

   4. Otherwise: ambiguous
   ================================================================ */

classify_by_signature(Profile, _, natural_law) :-
    natural_law_signature(Profile), !.

classify_by_signature(Profile, _, coordination_scaffold) :-
    coordination_scaffold_signature(Profile), !.

% OQ-90 (2026-06-11): the profile-path piton_signature dispatch is RETIRED. piton is
% now an FCR-branch refinement keyed on computed capture (narrative_ontology:piton_candidate/1
% via resolve_with_perspectival_check/4), not on the falsified Supp<=0.2 proxy. The old gate
% was corpus-dark (it never fired on the live corpus) and keyed on metrics witnessed wrong
% (2026-06-10 controls). Retirement scope is ONLY this clause + the piton_signature/1 helper
% (operator ruling); the drl_core theater-based piton clauses are superseded-pending, not removed.
% Two-sided witness: audits/2026-06-11_oq90_piton_refinement/phase4_witness.md.

% Constructed constraint sub-signatures (extraction-aware):
% Low extraction (ε ≤ rope_chi_ceiling): enforcement exists but extraction is low → rope-like
classify_by_signature(Profile, Extraction, constructed_low_extraction) :-
    constructed_constraint_signature(Profile),
    config:param(rope_chi_ceiling, RopeChi),
    Extraction =< RopeChi, !.

% High extraction (ε ≥ snare_epsilon_floor): high extraction construct → snare-like
classify_by_signature(Profile, Extraction, constructed_high_extraction) :-
    constructed_constraint_signature(Profile),
    config:param(snare_epsilon_floor, SnareEps),
    Extraction >= SnareEps, !.

% Mid extraction (between rope_chi_ceiling and snare_epsilon_floor): genuinely tangled
classify_by_signature(Profile, _, constructed_constraint) :-
    constructed_constraint_signature(Profile), !.

classify_by_signature(_, _, ambiguous).

/* ================================================================
   SIGNATURE 1: NATURAL LAW

   Diagnostic Pattern:
   ✓ Extreme accessibility collapse (≥ 0.85)
   ✓ Minimal suppression (≤ 0.15)
   ✓ Minimal resistance (≤ 0.15)
   ✓ Zero beneficiaries (no asymmetric winners)
   ✓ No viable alternatives (not a choice)
   ✓ Temporally stable (doesn't evolve)

   Interpretation:
   This is an INHERENT PROPERTY of the system, not a human choice.
   It cannot be changed by coordination or enforcement.

   Examples:
   - Chaitin's Omega (algorithmic information limit)
   - Heisenberg Uncertainty (quantum measurement limit)
   - Arrow's Impossibility (social choice limit)
   - Second Law of Thermodynamics (entropy increase)
   ================================================================ */

natural_law_signature(profile(AccessCollapse, Suppression, Resistance,
                             BeneficiaryCount, HasAlternatives,
                             TemporalStability, _CoordinationSuccess)) :-

    % Fail-closed on the `unknown` absence sentinel (2026-06-09): an unauthored
    % metric arrives as `unknown` from get_metric_average/3; guard with number/1
    % so the comparisons below FAIL rather than throw type_error. LOAD-BEARING:
    % reached pre-abstain via the natural_law clause (:112) and the FNL
    % claimed_natural path and boltzmann_compliance.pl:580.
    number(AccessCollapse), number(Suppression), number(Resistance),

    % Metric conditions
    config:param(natural_law_collapse_min, CollapseMin),
    AccessCollapse >= CollapseMin,  % Default: 0.85

    config:param(natural_law_suppression_max, SuppMax),
    Suppression =< SuppMax,  % Default: 0.15

    config:param(natural_law_resistance_max, ResMax),
    Resistance =< ResMax,  % Default: 0.15

    % Structural conditions (CRITICAL for distinguishing from coordination)
    BeneficiaryCount == 0,  % No asymmetric winners
    % DEAD-BY-RANGE (OQ-113): has_viable_alternatives/2's range is {true, unknown};
    % `false` is builder-unreachable, so this leg makes the whole signature
    % unsatisfiable on every corpus. Powering it = GAP-08 §7 author-independent
    % immovability signal (unsolved). See has_viable_alternatives/2 header.
    HasAlternatives == false,  % Not a choice
    TemporalStability == stable.  % Doesn't evolve

/* ================================================================
   SIGNATURE 2: COORDINATION SCAFFOLD

   Diagnostic Pattern:
   ✓ Extreme accessibility collapse (≥ 0.85)
   ✓ Minimal suppression (≤ 0.15)
   ✓ Minimal resistance (≤ 0.15)
   ✓ Zero or few beneficiaries (symmetric benefits)
   ✓ HAS viable alternatives (WAS a choice)
   ✓ May be stable or evolving (can be refined)

   KEY DISTINCTION from Natural Law:
   - Alternatives EXISTED → this was a COORDINATION CHOICE
   - Success is CONTINGENT → depends on continued acceptance
   - Could theoretically be replaced → not inherent to reality

   Interpretation:
   This is a SUCCESSFUL VOLUNTARY STANDARD that everyone adopted
   because it provides symmetric coordination benefits.

   Examples:
   - Special Relativity (replaced Newtonian mechanics)
   - SI Metric System (replaced imperial units)
   - UTC Time Standard (replaced local solar time)
   - IP Protocol (replaced other network protocols)
   ================================================================ */

coordination_scaffold_signature(profile(AccessCollapse, Suppression, Resistance,
                                       BeneficiaryCount, HasAlternatives,
                                       _TemporalStability, _CoordinationSuccess)) :-

    % Fail-closed on `unknown` (2026-06-09) — see natural_law_signature. Backstop:
    % reached only post-abstain via classify_by_signature (:140), but guarded
    % uniformly (cost-asymmetry ruling: a free guard beats a future throw).
    number(AccessCollapse), number(Suppression), number(Resistance),

    % Metric conditions (same as natural law)
    config:param(coordination_collapse_min, CollapseMin),
    AccessCollapse >= CollapseMin,  % Default: 0.85

    config:param(coordination_suppression_max, SuppMax),
    Suppression =< SuppMax,  % Default: 0.15

    config:param(coordination_resistance_max, ResMax),
    Resistance =< ResMax,  % Default: 0.15

    % Structural conditions (DIFFERENT from natural law)
    BeneficiaryCount =< 1,  % Symmetric or near-symmetric
    HasAlternatives == true.  % KEY: This WAS a choice

/* ================================================================
   SIGNATURE 3: PITON

   Diagnostic Pattern:
   ✓ Variable accessibility collapse
   ✓ Low suppression (≤ 0.2) - persists through inertia, not force
   ✓ Positive resistance (> 0.2) - it's now causing friction
   ✓ Had viable alternatives - it was originally a choice
   ✓ Temporally evolving - it got worse over time

   Interpretation:
   This was once a useful coordination scaffold (a Rope), but has since
   ossified and now creates more problems than it solves. It persists
   due to high switching costs and institutional inertia. It is a Piton
   stuck in the mountain.

   Examples:
   - QWERTY Keyboard Layout
   - Legacy software monoliths
   ================================================================ */

% piton_signature/1 RETIRED (OQ-90, 2026-06-11) together with its dispatch clause above.
% It keyed on Supp<=0.2 + Resistance>0.2 + alternatives + evolving — proxies witnessed wrong
% (2026-06-10 controls) and corpus-dark. piton is now an FCR-branch refinement on computed
% capture (narrative_ontology:piton_candidate/1). Two-sided witness:
% audits/2026-06-11_oq90_piton_refinement/phase4_witness.md (positive control fires-before /
% falls-through-after; 0-row corpus diff).

/* ================================================================
   SIGNATURE 4: CONSTRUCTED CONSTRAINT

   Diagnostic Pattern:
   ✓ Variable accessibility collapse
   ✓ Positive suppression (> 0.2) OR
   ✓ Positive resistance (> 0.2) OR
   ✓ Multiple beneficiaries (asymmetric gains)

   Interpretation:
   This is an INSTITUTIONALLY ENFORCED RULE that requires
   active maintenance and produces asymmetric outcomes.

   Examples:
   - 26 USC §469 (passive loss limitation)
   - GS1 Barcode System (licensing monopoly)
   - Hammurabi's Code (benefice system)
   - Lehman's Repo 105 (accounting fiction)
   ================================================================ */

constructed_constraint_signature(profile(_AccessCollapse, Suppression, Resistance,
                                        BeneficiaryCount, _HasAlternatives,
                                        _TemporalStability, _CoordinationSuccess)) :-

    % Fail-closed on `unknown` (2026-06-09): Suppression is authored corpus-wide so
    % the disjunction usually short-circuits before Resistance, but guard both so an
    % unknown Resistance with Suppression =< 0.2 fails rather than throws.
    number(Suppression), number(Resistance),
    % At least one indicator of constructed constraint
    (   Suppression > 0.2        % Requires enforcement
    ;   Resistance > 0.2         % Faces opposition
    ;   BeneficiaryCount > 1     % Asymmetric benefits
    ).

/* ================================================================
   CONFIDENCE SCORING

   Returns confidence level based on how strongly the signature
   pattern matches the classification.
   ================================================================ */

%% signature_confidence(+ConstraintID, +Signature, -Confidence)
%  Returns: high | medium | low

% Boltzmann-derived signature confidence (v5.1)
% These require the constraint ID for Boltzmann tests, so they're
% handled before the profile-based compute_signature_confidence.
signature_confidence(C, false_natural_law, Confidence) :-
    (   cross_index_coupling(C, CouplingScore)
    ->  (   CouplingScore > 0.50 -> Confidence = high
        ;   CouplingScore > 0.25 -> Confidence = medium
        ;   Confidence = low
        )
    ;   Confidence = low
    ), !.

signature_confidence(C, false_ci_rope, Confidence) :-
    (   false_ci_rope(C, fcr_evidence(_, FailedTests, _, _, _, _, _))
    ->  length(FailedTests, NF),
        (   NF >= 3 -> Confidence = high
        ;   NF >= 2 -> Confidence = medium
        ;   Confidence = low
        )
    ;   Confidence = low
    ), !.

signature_confidence(C, coupling_invariant_rope, Confidence) :-
    (   structural_purity(C, PurityClass)
    ->  (   PurityClass = pure_coordination -> Confidence = high
        ;   PurityClass = pure_unclassified -> Confidence = medium
        ;   Confidence = medium
        )
    ;   Confidence = medium
    ), !.

% False Summit Mountain confidence: scales with beneficiary count and coupling score.
% Zero coupling (common for Mountains) → medium; high coupling → high.
% OQ-93 Stage D: a witnessed level-gradient divergence raises confidence ONE
% rung (consumed positively); LevelDiv = open leaves the pre-wiring value
% untouched (absence never lowers, never blocks).
signature_confidence(C, false_summit_mountain, Confidence) :-
    (   false_summit_mountain(C, fsm_evidence(BCount, CScore, LevelDiv))
    ->  (   CScore > 0.25 -> Base = high
        ;   BCount > 1   -> Base = medium
        ;                   Base = low
        ),
        (   LevelDiv = divergence(_, _)
        ->  confidence_rung_up(Base, Confidence)
        ;   Confidence = Base
        )
    ;   Confidence = low
    ), !.

% Profile-based confidence (v3.2 original pipeline)
signature_confidence(C, Signature, Confidence) :-
    get_constraint_profile(C, Profile),
    % Fail-closed on `unknown` (2026-06-09): compute_signature_confidence/3 compares
    % AccessCollapse/Suppression/Resistance inside findall/3, which does NOT catch the
    % type_error an `unknown` would raise. If the profile is not fully numeric the
    % constraint has abstained (signature `unknown`) and has no scoreable confidence —
    % return low rather than throw. Separate entry point from the signature cascade,
    % so this guard is load-bearing, not a backstop.
    (   profile_numeric(Profile)
    ->  compute_signature_confidence(Profile, Signature, Confidence)
    ;   Confidence = low
    ).

% OQ-93 Stage D: one-rung confidence bump for a witnessed level-gradient
% divergence (placed AFTER the last signature_confidence/3 clause to keep
% that predicate contiguous — the load-warning gate flags interleaving).
confidence_rung_up(low, medium).
confidence_rung_up(medium, high).
confidence_rung_up(high, high).

%% profile_numeric(+Profile)
%  True iff the three arithmetic-compared profile metrics are authored numbers.
%  (BeneficiaryCount is always a count; HasAlternatives/TemporalStability are atoms
%  compared with ==, which do not throw on `unknown`.)
profile_numeric(profile(AccessCollapse, Suppression, Resistance, _, _, _, _)) :-
    number(AccessCollapse), number(Suppression), number(Resistance).

compute_signature_confidence(Profile, natural_law, Confidence) :-
    Profile = profile(AccessCollapse, Suppression, Resistance, _, _, _, _),

    % Count strong indicators
    findall(1, (
        (AccessCollapse > 0.95);
        (Suppression < 0.05);
        (Resistance < 0.05)
    ), Indicators),
    length(Indicators, Count),

    (   Count >= 3 -> Confidence = high
    ;   Count >= 2 -> Confidence = medium
    ;   Confidence = low
    ).

compute_signature_confidence(Profile, coordination_scaffold, Confidence) :-
    Profile = profile(AccessCollapse, Suppression, _, _, HasAlternatives, _, _),
    % Strong indicators
    findall(1, (
        (AccessCollapse > 0.95);
        (Suppression < 0.05);
        (HasAlternatives == true)  % Critical for coordination
    ), Indicators),
    length(Indicators, Count),
    (   Count >= 3 -> Confidence = high
    ;   Count >= 2 -> Confidence = medium
    ;   Confidence = low
    ).

% superseded by OQ-90 FCR refinement; unreachable from profile path (the piton_signature
% dispatch was retired 2026-06-11). Left in place — removal exceeds the ruled scope.
compute_signature_confidence(Profile, piton_signature, Confidence) :-
    Profile = profile(_, Suppression, Resistance, _, HasAlternatives, TemporalStability, _),
    % Count strong indicators for a piton
    findall(1, (
        (Suppression =< 0.2);
        (Resistance > 0.5);
        (HasAlternatives == true);
        (TemporalStability == evolving)
    ), Indicators),
    length(Indicators, Count),
    (   Count >= 3 -> Confidence = high
    ;   Count >= 2 -> Confidence = medium
    ;   Confidence = low
    ).

compute_signature_confidence(Profile, constructed_constraint, Confidence) :-
    Profile = profile(_, Suppression, Resistance, BeneficiaryCount, _, _, _),
    findall(1, (
        (Suppression > 0.5);
        (Resistance > 0.5);
        (BeneficiaryCount > 2)
    ), Indicators),
    length(Indicators, Count),
    (   Count >= 2 -> Confidence = high
    ;   Count >= 1 -> Confidence = medium
    ;   Confidence = low
    ).

% Sub-signature confidence delegates to constructed_constraint base
compute_signature_confidence(Profile, constructed_low_extraction, Confidence) :-
    compute_signature_confidence(Profile, constructed_constraint, Confidence).
compute_signature_confidence(Profile, constructed_high_extraction, Confidence) :-
    compute_signature_confidence(Profile, constructed_constraint, Confidence).

compute_signature_confidence(_, ambiguous, low).

/* ================================================================
   EXPLANATION GENERATION
   ================================================================ */

%% explain_signature(+ConstraintID, +Signature, -Explanation)
explain_signature(C, natural_law, Explanation) :-
    get_constraint_profile(C, Profile),
    Profile = profile(AC, S, R, _, _, _, _),
    format(atom(Explanation),
           'NATURAL LAW signature for ~w: Extreme inaccessibility (collapse=~2f) with minimal enforcement (suppression=~2f, resistance=~2f). No viable alternatives exist. This represents an inherent property of the system, not a coordination choice. Cannot be changed by policy.',
           [C, AC, S, R]).

explain_signature(C, coordination_scaffold, Explanation) :-
    get_constraint_profile(C, Profile),
    Profile = profile(AC, S, _, _, _, _, _),
    format(atom(Explanation),
           'COORDINATION SCAFFOLD signature for ~w: Extreme accessibility (collapse=~2f) with minimal enforcement (suppression=~2f). Viable alternatives existed historically, indicating this is a successful coordination standard rather than a natural law. Maintains adoption through symmetric benefits.',
           [C, AC, S]).

% superseded by OQ-90 FCR refinement; unreachable from profile path (dispatch retired 2026-06-11).
explain_signature(C, piton_signature, Explanation) :-
    get_constraint_profile(C, Profile),
    Profile = profile(_, S, R, _, _, _, _),
    format(atom(Explanation),
           'PITON signature for ~w: Persists through inertia (suppression=~2f) but faces user friction (resistance=~2f). Was once a choice, but has now become an ossified liability. This is a Piton.',
           [C, S, R]).

explain_signature(C, constructed_constraint, Explanation) :-
    get_constraint_profile(C, Profile),
    Profile = profile(_, S, R, BC, _, _, _),
    format(atom(Explanation),
           'CONSTRUCTED CONSTRAINT signature for ~w: Active enforcement detected (suppression=~2f, resistance=~2f) with ~d asymmetric beneficiaries. Mid-extraction range: genuinely tangled coordination/extraction mix.',
           [C, S, R, BC]).

explain_signature(C, constructed_low_extraction, Explanation) :-
    get_constraint_profile(C, Profile),
    Profile = profile(_, S, R, _, _, _, _),
    config:param(extractiveness_metric_name, ExtMetricName),
    get_metric_average(C, ExtMetricName, Ext),
    format(atom(Explanation),
           'CONSTRUCTED LOW-EXTRACTION signature for ~w: Enforcement present (suppression=~2f, resistance=~2f) but extraction is low (~2f). This is a rule-based coordination structure, not an extraction mechanism.',
           [C, S, R, Ext]).

% NOTE (OQ-98 post-close check, 2026-06-11): the "metrics failed to classify as
% snare" prose below is UNCONDITIONAL — it prints whenever the signature fires,
% without checking what the metric layer actually returned (witnessed:
% agenda_conditioning, metric == dr_type == snare at every standard context, yet
% this line printed and was quoted into OQ-98 as if it reported a rewire). Do not
% read it as a rewire claim; signature_grade/2 is the checked rewire fact.
explain_signature(C, constructed_high_extraction, Explanation) :-
    get_constraint_profile(C, Profile),
    Profile = profile(_, S, R, _, _, _, _),
    config:param(extractiveness_metric_name, ExtMetricName),
    get_metric_average(C, ExtMetricName, Ext),
    format(atom(Explanation),
           'CONSTRUCTED HIGH-EXTRACTION signature for ~w: Enforcement present (suppression=~2f, resistance=~2f) with high extraction (~2f). This is an extraction mechanism that metrics failed to classify as snare.',
           [C, S, R, Ext]).

explain_signature(C, false_summit_mountain, Explanation) :-
    (   false_summit_mountain(C, fsm_evidence(BCount, CScore, _LevelDiv))
    ->  format(atom(Explanation),
               'FALSE SUMMIT MOUNTAIN signature for ~w: Meets all mountain metric thresholds (low extractiveness, low suppression, emerges naturally) but has ~d identifiable beneficiaries. Genuine natural laws have zero beneficiaries. This constraint has been naturalized — its constructed origin has become invisible. Coupling score=~3f.',
               [C, BCount, CScore])
    ;   format(atom(Explanation),
               'FALSE SUMMIT MOUNTAIN signature for ~w: Mountain metrics with identifiable beneficiaries. Use false_summit_mountain/2 for detailed evidence.',
               [C])
    ).

explain_signature(C, false_natural_law, Explanation) :-
    (   false_natural_law(C, fnl_evidence(Claim, _BoltzResult, CouplingScore,
                                           CoupledPairs, ExcessExtraction))
    ->  length(CoupledPairs, NPairs),
        format(atom(Explanation),
               'FALSE NATURAL LAW signature for ~w: Claims naturality (~w) but fails Boltzmann independence test. Coupling score=~3f with ~d coupled dimension pairs. Excess extraction=~w. This constraint is "physics-washed" — it appears natural but its coupling topology reveals structural construction.',
               [C, Claim, CouplingScore, NPairs, ExcessExtraction])
    ;   format(atom(Explanation),
               'FALSE NATURAL LAW signature for ~w: Claims naturality but fails Boltzmann independence. Use false_natural_law/2 for detailed evidence.',
               [C])
    ).

explain_signature(C, false_ci_rope, Explanation) :-
    (   false_ci_rope(C, fcr_evidence(AppType, FailedTests, CouplingScore, _, _, _, _))
    ->  length(FailedTests, NF),
        format(atom(Explanation),
               'FALSE CI_ROPE signature for ~w: Appears to be rope (~w) but fails ~d Boltzmann structural test(s): ~w. Coupling score=~w. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.',
               [C, AppType, NF, FailedTests, CouplingScore])
    ;   format(atom(Explanation),
               'FALSE CI_ROPE signature for ~w: Appears to be rope from metrics but fails Boltzmann structural tests. Use false_ci_rope/2 for detailed evidence.',
               [C])
    ).

explain_signature(C, coupling_invariant_rope, Explanation) :-
    (   coupling_invariant_rope(C, ci_rope_evidence(Compliance, ScopeResult,
                                                     ExcessEps, _))
    ->  format(atom(Explanation),
               'COUPLING-INVARIANT ROPE signature for ~w: coupling-clean coordination (snapshot). Boltzmann compliance=~w, scope invariance=~w, excess extraction=~3f. Passes the coupling/scope-invariance tests (NOT an excess-extraction or drift gate) — reads as genuine coordination rather than coordination-washed construction; check lifecycle drift and excess extraction for trajectory.',
               [C, Compliance, ScopeResult, ExcessEps])
    ;   format(atom(Explanation),
               'COUPLING-INVARIANT ROPE signature for ~w: coupling-clean coordination (snapshot; coupling/scope tests only, not excess/drift). Use coupling_invariant_rope/2 for detailed evidence.',
               [C])
    ).

explain_signature(C, ambiguous, Explanation) :-
    format(atom(Explanation),
           'AMBIGUOUS signature for ~w: Insufficient structural differentiation to classify. Consider gathering more data on alternatives, beneficiaries, and temporal evolution.',
           [C]).

% OQ-137 fix (2026-07-02): `unknown` had NO explanation clause, so
% explain_signature/3 FAILED on its own domain (C, its computed signature) for
% every honest-abstain constraint — and report_constraint_signature chained it
% unguarded, so one claim-authoring unknown-signature constraint silently
% truncated the whole [STRUCTURAL SIGNATURE ANALYSIS] section (planted-fixture
% witness: 0/110 lines printed; audits/2026-07-02_oq137_reading_totality/).
% The honest abstain carries its provenance, mirroring constraint_signature's
% own unknown clause (:136): absence of authored metrics, not a verdict.
explain_signature(C, unknown, Explanation) :-
    format(atom(Explanation),
           'UNKNOWN signature for ~w: honest abstain — the profile metrics are unauthored (absence sentinel), so no signature verdict is fabricated. Author the metric vector to obtain a signature (see constraint_signature/2 unknown clause).',
           [C]).

/* ================================================================
   INTEGRATION WITH MODAL CLASSIFICATION

   This is the key integration point: structural signatures
   OVERRIDE modal classification when there's a mismatch.

   Example: Special Relativity
   - Modal classifier says: "mountain" (suppression=0, snapback=0)
   - Signature detector says: "coordination_scaffold"
   - Integrated result: "rope" with note about coordination success
   ================================================================ */

%% integrate_signature_with_modal(+Constraint, +ModalType, -AdjustedType)
%  Adjusts modal classification based on structural signature.
%  For FCR signatures, checks perspectival variance first: if the metric
%  layer produces different classifications across power positions, the
%  indexical system is working and the FCR override should defer.
integrate_signature_with_modal(C, ModalType, AdjustedType) :-
    constraint_signature(C, Signature),
    resolve_with_perspectival_check(C, ModalType, Signature, AdjustedType).

%% resolve_with_perspectival_check(+C, +ModalType, +Signature, -AdjustedType)
%  Gate on FCR override: if the constraint shows perspectival variance
%  at the metric layer, preserve the metric-based classification.
%  Uniform classification despite varying χ is genuinely suspicious.
%  Perspectival differentiation is evidence the system is working.
%  Honest "unknown" (an absence of metric classification, not a valid
%  perspectival result) is now SURFACED as unknown rather than overridden to
%  tangled_rope (OQ-37, 2026-06-01): a band-gap / authored-gap / swallowed-error
%  reading must stay visible, not be masked by the FCR override.
%  (Was: "never preserve unknown" — that usability-era behavior is removed.)
%  Dead-coordination pitons classify uniformly — this is correct structural
%  behavior, not FCR evidence.  The piton pre-check (drl_core.pl) fires
%  context-independently because dead coordination is a structural fact,
%  not a perspectival one.  Uniform piton classification is expected, not
%  suspicious.  Exempt from the perspectival variance gate.
resolve_with_perspectival_check(C, piton, false_ci_rope, piton) :-
    drl_core:coordination_dead(C), !.
% OQ-90: capture-keyed piton refinement. A constraint that appears as rope, fails
% the Boltzmann tests (=> false_ci_rope), and whose authored receipt surface says
% the extraction is uncaptured AND prohibitive to remove is a piton (a structural
% pin nobody profits from removing). Like the dead-coordination clause above, this
% is exempt from the perspectival-variance gate: piton_candidate/1 is an authored
% structural fact, not a perspectival result, so uniform classification is expected,
% not suspicious. The cut commits before the generic FCR clause below — placement is
% position-encoded cascade priority. Fires even when fcr_override_enabled=0 (separate
% axis; kill-switch is piton_refinement_enabled). Reads piton_candidate directly, not
% the fcr_evidence disposition field (which is the evidence trail, populated upstream).
resolve_with_perspectival_check(C, _ModalType, false_ci_rope, piton) :-
    config:param(piton_refinement_enabled, 1),
    narrative_ontology:piton_candidate(C), !.
resolve_with_perspectival_check(C, ModalType, false_ci_rope, AdjustedType) :-
    !,
    (   config:param(fcr_override_enabled, 1)
    ->  (   ModalType \= unknown,
            has_metric_perspectival_variance(C)
        ->  AdjustedType = ModalType    % Preserve: indexical differentiation detected
        ;   ModalType == unknown
        ->  AdjustedType = unknown       % Surface: honest unknown is not a metric result (OQ-37, 2026-06-01)
        ;   AdjustedType = ModalType    % OQ-138 (2026-06-21): ROUTE — was `tangled_rope`. FCR no longer
                                        % overwrites; the diagnostic rides the victim-discriminated severity
                                        % on fcr_routed/1 seats (the "FCR-9"). Piton (clause 2 above) and
                                        % inert seats (unknown / perspectival-preserved) are unaffected.
                                        % Restore the legacy override by reverting this branch to tangled_rope.
        )
    ;   AdjustedType = ModalType        % Ablation: preserve metric-based type
    ).
resolve_with_perspectival_check(_C, ModalType, Signature, AdjustedType) :-
    resolve_modal_signature_conflict(ModalType, Signature, AdjustedType).

%% has_metric_perspectival_variance(+C)
%  True if the constraint classifies differently at the metric layer
%  across at least two standard power positions (scope held constant).
%  Uses the coupling test classifier to avoid circular dependency with
%  drl_core. If even the simplified classifier shows variance, the full
%  pipeline certainly would.
has_metric_perspectival_variance(C) :-
    coupling_test_powers(Powers),
    findall(
        Type,
        (   member(P, Powers),
            coupling_test_context(P, national, Ctx),
            classify_at_context(C, Ctx, Type)
        ),
        Types
    ),
    sort(Types, UniqueTypes),
    length(UniqueTypes, N),
    N > 1.

% -----------------------------------------------------------------------
% SIGNATURE OVERRIDE RULE (logic.md §III-A, Rule NL):  [RETIRED 2026-06-17, OQ-128]
%   FORMERLY: NL(C) → Mountain (▪) regardless of metric-based classification.
%   RETIRED under the routing-sink architecture (the engine ROUTES, it does not
%   RECLASSIFY — only review reclassifies). This clause was the natural_law
%   OVERWRITE (rope→mountain); it manufactured a verdict rather than flagging a
%   disagreement. The DETECTOR (`natural_law_signature`/`constraint_signature(C,
%   natural_law)`) LIVES — demoted from override-trigger to a router input
%   (currently unpowered: HasAlternatives==false is builder-unreachable, see
%   ROUTING_SINK_DESIGN.md §9a(i)). Retirement is WITNESSED behavior-neutral: the
%   clause fired 0/3843 across six corpora (the detector that gated it never
%   succeeds), so removing it changes no dr_type — see §9b.2 and the OQ-128 record.
%   The retired clause was:
%       resolve_modal_signature_conflict(_, natural_law, Result) :- !, Result = mountain.
% -----------------------------------------------------------------------
% -----------------------------------------------------------------------
% BINDING-SAFE OVERRIDE RULES
% All override clauses use body unification (Result = X) rather than head
% unification for the output argument. This prevents a pre-bound third
% argument from bypassing overrides via head unification failure and
% falling through to the identity fallback.
%
% The cut fires BEFORE the output unification, so:
% - With unbound Result: cut commits, unification succeeds → correct type
% - With pre-bound Result: cut commits, unification may fail → query
%   correctly returns false (the constraint is NOT that type)
% -----------------------------------------------------------------------

% Categorical: natural_law resolver RETIRED 2026-06-17 (OQ-128) — see the retirement
% tombstone above. The detector survives; the overwrite does not. The other
% resolve_modal_signature_conflict clauses (false_natural_law, ...) are UNTOUCHED.

% FNL RULE (v5.1 §III-A; OQ-138 conversion 2026-07-03):  [ACTIVE, routed]
%   The honest `unknown` abstain is UNCHANGED (2026-06-01, OQ-37 ruling): an
%   `unknown` modal type is an *absence* of metric classification, not a metric
%   result to override — surfacing it keeps a band-gap / authored-gap /
%   swallowed-error reading VISIBLE instead of masked.
%   Otherwise FNL now ROUTES (OQ-138, 2026-07-03): the metric type stands and
%   the FNL diagnostic rides the victim-discriminated severity
%   (signature_diagnostic_severity/3 → verdict_join), mirroring FSM and FCR-9.
%   Set false_natural_law_override_enabled=1 to RESTORE the legacy v5.1
%   tangled_rope overwrite (ablation lever). Severe-escalation alternative
%   (vic>0 → severe, treating naturalization-concealment as categorically more
%   serious) is a config-shaped lever NOT taken — the ruling is moderate.
resolve_modal_signature_conflict(unknown, false_natural_law, Result) :- !, Result = unknown.
resolve_modal_signature_conflict(ModalType, false_natural_law, Result) :-
    !,
    (   config:param(false_natural_law_override_enabled, 1)
    ->  Result = tangled_rope           % legacy overwrite (ablation lever)
    ;   Result = ModalType              % OQ-138: ROUTE (default) — was tangled_rope
    ).

% CI_ROPE OVERRIDE RULE (v5.1, §III-A extension):  [ACTIVE, unconditional]
%   CI_Rope(C) → rope regardless of metric-based classification.
resolve_modal_signature_conflict(_, coupling_invariant_rope, Result) :- !, Result = rope.

% FCR OVERRIDE RULE (v5.1, §III-A extension, perspectival gate v5.3):  [ACTIVE, gated]
%   NOTE: This rule is now only reached as fallback from
%   resolve_with_perspectival_check/4 when has_metric_perspectival_variance
%   fails. Direct callers of resolve_modal_signature_conflict still see
%   the unconditional override for backward compatibility.
resolve_modal_signature_conflict(ModalType, false_ci_rope, Result) :-
    !,
    (   config:param(fcr_override_enabled, 1)
    ->  Result = tangled_rope
    ;   Result = ModalType              % Ablation: preserve metric-based type
    ).

% Coordination scaffolds should be ROPES not mountains
% OQ-138 (2026-07-14): RESIDUAL — routes to abstain via residual_route/2 (was `rope`).
resolve_modal_signature_conflict(mountain, coordination_scaffold, Result) :- !, residual_route(rope, Result).

% False Summit Mountain — OQ-138 (2026-06-21): CONVERTED from RECLASSIFY to ROUTE/COMMENT.
% FSM no longer overwrites dr_type; it reverts to the metric type (mountain — the
% authored claim) and its diagnostic rides a victim-discriminated severity
% (signature_diagnostic_severity/3) that floors verdict_join only on the
% concealment case (vic>0). The committed DEFAULT config param is `mountain`, so
% Result = Target = mountain = ModalType (no overwrite). The hook stays a live
% ablation lever: set false_summit_override_target=tangled_rope to RESTORE the
% legacy v6.9 overwrite. FSM was also removed from abductive_helpers
% known_override_signature/1 + override_target/2 (it no longer overrides) so the
% probe_signature / P1 / P7 override-artifact consumers go cleanly vacuous.
resolve_modal_signature_conflict(mountain, false_summit_mountain, Result) :-
    !,
    (   config:param(false_summit_override_target, Target)
    ->  Result = Target
    ;   Result = mountain
    ).
% OQ-138: unknown-input FSM surfaces the honest abstain rather than laundering an
% absent metric classification into tangled_rope (OQ-37 precedent, cf. the FNL
% unknown clause above). NOTE: zero live fires — this arm rides the OQ-37 ruling,
% not a diff; unverified-in-commit.
resolve_modal_signature_conflict(unknown, false_summit_mountain, Result) :- !, Result = unknown.

% Constructed constraints override mountain classification
% OQ-138 (2026-07-14): RESIDUAL (mountain-input) — route to abstain via residual_route/2. The
% unknown-input constructed_high clause below is a SEPARATE, already-converted case (constructed-3).
resolve_modal_signature_conflict(mountain, constructed_low_extraction, Result) :- !, residual_route(rope, Result).
resolve_modal_signature_conflict(mountain, constructed_high_extraction, Result) :- !, residual_route(tangled_rope, Result).
resolve_modal_signature_conflict(mountain, constructed_constraint, Result) :- !, residual_route(tangled_rope, Result).

% When metrics fail (unknown), signature provides extraction-aware classification
% OQ-138 (2026-07-14): RESIDUAL (unknown-input stragglers) — route to abstain via residual_route/2
% (was `rope`), completing the OQ-37 honest-unknown arc these two clauses had escaped.
resolve_modal_signature_conflict(unknown, coordination_scaffold, Result) :- !, residual_route(rope, Result).
resolve_modal_signature_conflict(unknown, constructed_low_extraction, Result) :- !, residual_route(rope, Result).
% OQ-138 (2026-06-21): constructed_high_extraction unknown-input CONVERTED RECLASSIFY→ROUTE.
% Was `snare` (manufactured a type from unknown metrics, against which type_1_false_summit then
% fired severe on a mountain-claim). Now routes to the honest abstain (unknown); the diagnostic
% rides the CLAIM-discriminated severity (signature_diagnostic_severity/3: mountain-claim => severe
% floor, preserving the concealment flag that the manufactured snare used to carry via type_1).
% Seat-aware via constructed_routed/1. Mountain-input constructed + constructed_low/constraint are
% NOT converted here (0 live changers — separate sub-item). Restore by reverting to `snare`.
resolve_modal_signature_conflict(unknown, constructed_high_extraction, Result) :- !, Result = unknown.
% OQ-138 (2026-07-14): RESIDUAL — route to abstain via residual_route/2 (was `tangled_rope`).
resolve_modal_signature_conflict(unknown, constructed_constraint, Result) :- !, residual_route(tangled_rope, Result).
% superseded by OQ-90 FCR refinement; unreachable from profile path (dispatch retired 2026-06-11).
resolve_modal_signature_conflict(unknown, piton_signature, Result) :- !, Result = piton.
resolve_modal_signature_conflict(unknown, ambiguous, Result) :- !, Result = unknown.

% No conflict - keep original classification
resolve_modal_signature_conflict(ModalType, _, ModalType).

%% residual_route(+LegacyTarget, -Result)  — OQ-138 (2026-07-14)
%  The seven RESIDUAL overwrite clauses route through this. Default (lever 0): abstain to `unknown`
%  — the honest "a residual signature matched, the engine declines to manufacture a type." Legacy
%  (lever 1): the historical overwrite target (ablation). The abstain target is `unknown`, NEVER
%  `untyped`: is_real_type/1 tests `\== unknown`, so `unknown` is filtered out of the H¹ real-seat
%  set (an abstained seat is not a real typed seat) while `untyped` would count as a real disagreeing
%  type and silently INFLATE H¹ (the exact pathology OQ-138 exists to kill). See the footgun control
%  in tests/test_residual_signature_guard.pl.
residual_route(Legacy, Result) :-
    (   config:param(residual_signature_override_enabled, 1)
    ->  Result = Legacy
    ;   Result = unknown
    ).

%% residual_signature_firing(+C)  — OQ-138 (2026-07-14) MONITOR
%  A seat where one of the seven residual (metric-type, signature) patterns is met — i.e. a residual
%  clause fires. Under the guard the fire routes to `unknown` (no manufacture); this predicate is the
%  MONITORED surface (a run_pipeline gate RED on count>0 auto-reopens the successor OQ). Detects a
%  fire regardless of the guard's output token, so it also catches the unknown-input no-op cases.
%  Corpus-inert on all four legs at build time (0 fires) — a nonzero count is the reopen witness.
residual_signature_firing(C) :-
    constraint_indexing:default_context(Ctx),
    constraint_signature(C, Sig),
    drl_core:metric_based_type_indexed(C, Ctx, MT),
    residual_signature_pattern(MT, Sig).

residual_signature_pattern(mountain, coordination_scaffold).
residual_signature_pattern(mountain, constructed_low_extraction).
residual_signature_pattern(mountain, constructed_high_extraction).
residual_signature_pattern(mountain, constructed_constraint).
residual_signature_pattern(unknown,  coordination_scaffold).
residual_signature_pattern(unknown,  constructed_low_extraction).
residual_signature_pattern(unknown,  constructed_constraint).

/* ================================================================
   BOLTZMANN-DERIVED SIGNATURES v5.1

   Three new signatures derived from the Boltzmann compliance engine:

   1. False Natural Law (FNL)
      Detects "physics-washed" constraints: claimed as natural
      but fail Boltzmann independence. The natural-law analogue
      of False Mountain (FM).

   2. Coupling-Invariant Rope (CI_Rope)
      Detects "true coordination mechanisms": Boltzmann-compliant,
      scope-invariant, zero excess extraction, has coordination
      function. The coordination analogue of a natural law.

   3. Structural Purity (meta-invariant)
      Classifies constraint purity based on all four Boltzmann
      tests. Pure constraints are either "pure_natural_law",
      "pure_coordination", or "pure_scaffold". Impure constraints
      carry extractive or coupling contamination.

   These signatures integrate with the existing classification
   pipeline via the override rules in resolve_modal_signature_conflict/3.
   ================================================================ */

/* ----------------------------------------------------------------
   SIGNATURE: FALSE NATURAL LAW (FNL)
   ----------------------------------------------------------------
   FNL(C) :-
       claimed_natural(C),
       boltzmann_compliant(C, non_compliant).

   Detects constraints that CLAIM to be natural laws (Mountains)
   but fail the Boltzmann independence test. This captures:

   - "Physics-washed" constraints: extraction mechanisms dressed
     up as immutable facts ("humans are naturally hierarchical")
   - Naturalized extraction: constraints so old that their
     constructed origin has been forgotten
   - Ideological inevitability claims: "there is no alternative"
     when alternatives exist but are suppressed

   Unlike False Mountain (FM), which detects metric-level fraud
   (high ε claimed as Mountain), FNL detects STRUCTURAL fraud:
   the constraint's coupling topology reveals construction even
   when its metrics look natural.

   ACTIVE: FNL detection triggers the tangled_rope override via
   constraint_signature/2 → resolve_modal_signature_conflict/3
   (line 709: FNL → tangled_rope). Operational since FNL unification
   fix (2026-02).
   ---------------------------------------------------------------- */

% Categorical: Naturality failure witness [STRICT] — detects non-commutativity for constraints claiming naturality
%% false_natural_law(+Constraint, -Evidence)
%  Detects constraints that claim naturality but fail Boltzmann
%  compliance. Returns structured evidence for diagnostics.
%
%  Evidence = fnl_evidence(Claim, BoltzmannResult, CouplingScore,
%                          CoupledPairs, ExcessExtraction)

false_natural_law(C, fnl_evidence(Claim, BoltzmannResult, CouplingScore,
                                   CoupledPairs, ExcessExtraction)) :-
    % Must claim to be natural/mountain
    claimed_natural(C, Claim),

    % Must fail Boltzmann compliance
    boltzmann_compliant(C, BoltzmannResult),
    BoltzmannResult = non_compliant(_, _),

    % Gather diagnostic evidence
    cross_index_coupling(C, CouplingScore),
    (   detect_nonsensical_coupling(C, CoupledPairs, _)
    ->  true
    ;   CoupledPairs = []
    ),
    (   excess_extraction(C, ExcessExtraction)
    ->  true
    ;   ExcessExtraction = unknown
    ).

%% claimed_natural(+C, -ClaimType)
%  Checks if a constraint claims natural/immutable status.
%  ClaimType records the form of the claim for evidence trail.
%
%  Two sources of naturality claims:
%  1. Explicit mountain constraint_claim in testset data
%  2. Profile matches natural_law_signature pattern
%
%  RULED OUT (operator ruling 2026-06-05, OQ-70): the former middle source —
%  `constraint_indexing:constraint_classification(C, mountain, _)`, i.e. ANY single
%  authored perspective classifying as mountain — read indexical PERCEPTION as a
%  story-level CLAIM. An authored perspective is a seat's view ("this seat perceives
%  immutability"), which the two-axis design wants authored; a naturality CLAIM is
%  the story-level constraint_claim. Reading one as the other made FNL prevalence
%  measure authoring convention, not detection (827/1106 pre-reset, all via that
%  source; bait-fungible with FCR's sibling clause — counterfactual witnessed
%  2026-06-04). CLASS RULE: no signature may read a single authored perspective as
%  a story-level claim. Claim-vs-computed divergence is the story-level diff
%  machinery's job (dr_claim_mismatch over constraint_claim), which covers it.
claimed_natural(C, explicit_mountain_claim) :-
    narrative_ontology:constraint_claim(C, mountain), !.
claimed_natural(C, natural_law_signature_match) :-
    get_constraint_profile(C, Profile),
    natural_law_signature(Profile).

/* ----------------------------------------------------------------
   SIGNATURE: COUPLING-INVARIANT ROPE (CI_Rope)
   ----------------------------------------------------------------
   CI_Rope(C) :-
       boltzmann_compliant(C, compliant),
       scope_invariant(C),
       excess_extraction(C, ≈ 0),
       Coord(C).

   Detects "true coordination mechanisms" — constraints that:
   - Are Boltzmann-compliant (independent dimensions)
   - Classify the same way at all scope levels
   - Have no extraction above the Boltzmann floor
   - Have a genuine coordination function

   This distinguishes:
   - "True coordination" from "low-extraction constructs that
     happen to pass threshold gates"
   - Stable Ropes from Ropes that are merely pre-Tangled

   CI_Rope is the positive signature: it certifies that a Rope
   is structurally sound, not just metrically passing.
   ---------------------------------------------------------------- */

% Categorical: Naturality certificate [STRICT] — passes all four naturality conditions
%% coupling_invariant_rope(+Constraint, -Evidence)
%  Detects coupling-invariant coordination mechanisms.
%  Returns structured evidence for diagnostics.
%
%  Evidence = ci_rope_evidence(Compliance, ScopeResult,
%                              ExcessEps, HasCoordination)

coupling_invariant_rope(C, ci_rope_evidence(Compliance, ScopeResult,
                                             ExcessEps, true)) :-
    % Must be Boltzmann-compliant
    boltzmann_compliant(C, Compliance),
    Compliance = compliant(_),

    % Must be scope-invariant
    scope_invariance_test(C, ScopeResult),
    ScopeResult = invariant,

    % Must have a coordination function
    narrative_ontology:has_coordination_function(C),

    % OQ-94 row-2 gate (ruled 2026-06-10, after the reachability control —
    % 7/7 of the live CI_Rope set is beneficiary-bearing): a constraint whose
    % gains demonstrably accrue to a seat must not certify as "structurally
    % sound true coordination". Computed capture only; absent surface changes
    % nothing (the 7 corpus certifications carry no gain_flow and still pass).
    \+ narrative_ontology:constraint_captured(C),

    % Collect excess extraction as diagnostic evidence (not a gate).
    % The floor override in boltzmann_floor_for/2 is editorial data,
    % not a classification input — gating here allowed overrides to
    % suppress CI_Rope certification on genuinely coordinating constraints.
    (   excess_extraction(C, ExcessEps)
    ->  true
    ;   ExcessEps = 0.0
    ).

/* ----------------------------------------------------------------
   META-INVARIANT: STRUCTURAL PURITY
   ----------------------------------------------------------------
   A constraint is "structurally pure" if it passes all four
   Boltzmann tests:
     1. Boltzmann-compliant (factorization)
     2. Scope-invariant
     3. No nonsensical coupling
     4. No excess extraction

   Purity classes:
     pure_natural_law     — NL signature + all four tests pass.
                            UNREACHABLE pending GAP-08 §7 (OQ-113 limb 2):
                            the NL signature is dead-by-range, so this subtype
                            is never emitted; determine_pure_subtype throws if
                            it ever becomes reachable (the §9b.2 KILL tripwire).
     pure_coordination    — CI_Rope signature + all four tests pass
     pure_scaffold        — has sunset clause + all four tests pass
     contaminated(Reasons) — one or more tests fail
     inconclusive         — insufficient data for reliable test

   Structural purity does not determine classification — it is
   a diagnostic meta-property that indicates how "clean" a
   constraint's structure is. A contaminated constraint may still
   be correctly classified as a Rope, but the contamination
   signals future drift risk.
   ---------------------------------------------------------------- */

%% structural_purity(+Constraint, -PurityClass)
%  Computes the structural purity classification.

structural_purity(C, inconclusive) :-
    % Bound-probe fix (2026-06-03 purity audit): calling epistemic_access_check(C, false)
    % with `false` bound always succeeded via the catch-all clause (clause 1's head can't
    % unify with false, so its guard+cut never ran), making this clause fire for EVERY
    % constraint and the four purity tests below unreachable. Call with an unbound
    % variable so clause order dispatches, then test the result.
    % Witness: audits/2026-06-03_purity/purity_audit_20260603.md §2.
    epistemic_access_check(C, Access),
    Access == false, !.

structural_purity(C, PurityClass) :-
    % Run all four tests
    purity_test_factorization(C, T1),
    purity_test_scope_invariance(C, T2),
    purity_test_coupling(C, T3),
    purity_test_excess(C, T4),

    Tests = [T1, T2, T3, T4],
    include(boltzmann_compliance:is_failure, Tests, Failures),

    (   Failures = []
    ->  % All tests pass — determine purity subtype
        determine_pure_subtype(C, PurityClass)
    ;   PurityClass = contaminated(Failures)
    ).

%% purity_test_factorization(+C, -Result)
purity_test_factorization(C, Result) :-
    boltzmann_compliant(C, Comp),
    (   Comp = compliant(_) -> Result = pass(factorization)
    ;   Comp = inconclusive(_) -> Result = pass(factorization_inconclusive)
    ;   Result = fail(factorization, Comp)
    ).

%% purity_test_scope_invariance(+C, -Result)
purity_test_scope_invariance(C, Result) :-
    scope_invariance_test(C, ScopeResult),
    (   ScopeResult = invariant -> Result = pass(scope_invariance)
    ;   Result = fail(scope_invariance, ScopeResult)
    ).

%% purity_test_coupling(+C, -Result)
purity_test_coupling(C, Result) :-
    (   detect_nonsensical_coupling(C, Pairs, Strength),
        Pairs \= []
    ->  Result = fail(nonsensical_coupling, strength(Strength))
    ;   Result = pass(no_nonsensical_coupling)
    ).

%% purity_test_excess(+C, -Result)
purity_test_excess(C, Result) :-
    (   excess_extraction(C, Excess)
    ->  (   Excess =< 0.05
        ->  Result = pass(no_excess_extraction)
        ;   Result = fail(excess_extraction, Excess)
        )
    ;   Result = pass(no_extraction_data)
    ).

%% determine_pure_subtype(+C, -Subtype)
%  Given that all purity tests pass, determines which "pure" class.
%
%  OQ-113 limb 2 (closed 2026-06-18): the pure_natural_law branch gates on
%  natural_law_signature/1, which is DEAD-BY-RANGE (its HasAlternatives==false
%  leg is builder-unreachable — see has_viable_alternatives/2 header). So this
%  branch is provably 0-firing on every corpus: a documentation/ghost-behavior
%  hazard, not a live one. Rather than a silent dead clause, it now THROWS — a
%  tripwire that doubles as the §9b.2 KILL: if a future corpus or schema change
%  ever powers the detector, this fires LOUD and forces re-derivation rather
%  than silently emitting pure_natural_law off the burned (non-discriminating)
%  accessibility/suppression/resistance metrics. Witness it stays unreached:
%  the OQ-113 probe shows natural_law_signature 0-firing on the live corpus.
determine_pure_subtype(C, _) :-
    get_constraint_profile(C, Profile),
    natural_law_signature(Profile),
    !,
    throw(unreachable_pure_natural_law(C)).
determine_pure_subtype(C, pure_coordination) :-
    narrative_ontology:has_coordination_function(C),
    % OQ-94 row-3 gate (ruled 2026-06-10, riding with row 1): captured
    % constraints don't label pure_coordination (commentary-grade).
    \+ narrative_ontology:constraint_captured(C), !.
determine_pure_subtype(C, pure_scaffold) :-
    narrative_ontology:has_sunset_clause(C), !.
determine_pure_subtype(_, pure_unclassified).

/* ================================================================
   SIGNATURE: FALSE CI_ROPE (FCR) — v5.1
   ================================================================
   FCR(C) :-
       appears_as_rope(C),
       fails_boltzmann_test(C).

   The "coordination-washed" analogue of FNL. Detects constraints
   that LOOK like ropes from metrics but fail structural Boltzmann
   tests, revealing hidden extraction or coupling.

   This catches:
   - "Nudges" that steer choice while claiming neutrality
   - "Soft paternalism" with distributed enforcement
   - "Behavioral defaults" that extract via inertia
   - Metric manipulation: ε and χ kept low while coupling
     reveals cross-dimensional extraction

   Unlike a true CI_Rope (which passes all four tests), FCR
   identifies constraints that pass the metric gates but fail
   the structural gates. It answers: "Is this coordination
   real or performed?"

   ACTIVE: FCR detection triggers the tangled_rope override via
   constraint_signature/2 → resolve_with_perspectival_check/4,
   gated by has_metric_perspectival_variance/1. When perspectival
   variance exists, the metric classification is preserved; when
   absent, FCR overrides to tangled_rope (v5.1).
   ================================================================ */

%% false_ci_rope(+Constraint, -Evidence)
%  Detects constraints that appear to be ropes from metrics but fail
%  Boltzmann structural tests.
%
%  Evidence = fcr_evidence(AppearanceType, FailedTests, CouplingScore,
%                           ExcessExtraction, ScopeResult, ZeroExcessFlag,
%                           CaptureDisposition)
%
%  ZeroExcessFlag = zero_excess_exemption_present | none
%    Records whether the zero-excess exemption condition was met.
%    Diagnostic only — does not gate the certificate.
%
%  CaptureDisposition = captured | piton_candidate | transient_neglect | absent
%    (OQ-90) The cut's verdict over the authored receipt surface, recorded as
%    evidence. Populated here at the constructor; the FCR-branch refinement
%    (resolve_with_perspectival_check, Phase 3) reads piton_candidate from
%    narrative_ontology directly, NOT from this field — the field is the
%    evidence trail / per-diffuse-story trace for ruling 4, not a classification
%    input. 'absent' = the receipt surface was not authored enough to decide
%    (fail-closed: never promotes to piton).

false_ci_rope(C, fcr_evidence(AppearanceType, FailedTests, CouplingScore,
                               ExcessExtraction, ScopeResult, ZeroExcessFlag,
                               CaptureDisposition)) :-
    % Must appear to be a rope from metrics
    appears_as_rope(C, AppearanceType),

    % Must fail at least one Boltzmann structural test
    collect_fcr_failures(C, FailedTests),
    FailedTests \= [],

    % Gather diagnostic data
    (   cross_index_coupling(C, CouplingScore)
    ->  true
    ;   CouplingScore = unknown
    ),
    (   excess_extraction(C, ExcessExtraction)
    ->  true
    ;   ExcessExtraction = unknown
    ),
    (   scope_invariance_test(C, ScopeResult)
    ->  true
    ;   ScopeResult = unknown
    ),

    % Collect zero-excess exemption status as diagnostic evidence (not a gate).
    % Floor overrides must not suppress FCR certification. See audits/2026-04-14_blocking_gate/blocking_gate_audit_20260414.md.
    % The exemption condition is preserved as a flag for the MaxEnt classifier.
    (   zero_excess_coupling_only(ExcessExtraction, FailedTests)
    ->  ZeroExcessFlag = zero_excess_exemption_present
    ;   ZeroExcessFlag = none
    ),

    % OQ-90: record the capture-cut verdict as evidence (does NOT gate the
    % certificate; classification reads narrative_ontology:piton_candidate/1
    % directly in resolve_with_perspectival_check, Phase 3).
    capture_disposition(C, CaptureDisposition).

%% capture_disposition(+C, -Disposition)
%  OQ-90: the authored receipt-surface verdict, recorded into fcr_evidence/7.
%  'captured' first (the fail-safe danger reading wins any malformed double-
%  authoring); piton_candidate/transient_neglect are mutually exclusive uncaptured
%  cases; 'absent' = surface not authored enough to decide (fail-closed).
capture_disposition(C, captured) :-
    narrative_ontology:constraint_captured(C), !.
capture_disposition(C, piton_candidate) :-
    narrative_ontology:piton_candidate(C), !.
capture_disposition(C, transient_neglect) :-
    narrative_ontology:transient_neglect(C), !.
capture_disposition(_, absent).

%% zero_excess_coupling_only(+Excess, +FailedTests)
%  True when the ONLY FCR evidence is Boltzmann coupling and
%  excess extraction is at or below the noise floor.
%  In this case, scope-sensitive classification is indexical
%  differentiation, not coordination washing.
zero_excess_coupling_only(Excess, FailedTests) :-
    number(Excess),
    Excess =< 0.05,
    % Every failure must be coupling-based (boltzmann or nonsensical)
    FailedTests \= [],
    forall(
        member(F, FailedTests),
        coupling_based_failure(F)
    ).

coupling_based_failure(boltzmann_non_compliant(_, _)).
coupling_based_failure(nonsensical_coupling(_)).

%% appears_as_rope(+C, -AppearanceType)
%  Checks if constraint's metrics look like rope/coordination.
%  AppearanceType records the form of the appearance for evidence trail.
%
%  IMPORTANT: Low extraction alone is NOT sufficient — Mountains also
%  have low ε. The low_extraction_profile check requires that the
%  constraint is NOT exclusively classified as Mountain from all
%  indexed perspectives. This prevents natural laws from being
%  misidentified as "coordination-washed."
appears_as_rope(C, explicit_rope_claim) :-
    narrative_ontology:constraint_claim(C, rope), !.
% RULED OUT (operator ruling 2026-06-05, OQ-70 class rule — same defect as
% claimed_natural's removed source): `constraint_classification(C, rope, _)` read ANY
% single authored rope perspective as a story-level "appears as coordination" claim.
% A snare's beneficiary seat CORRECTLY perceives rope — that is the perspectival gap
% working, not a disguise. Witnessed on the live-20: FCR fired on snare-claimed
% stories at eps~0.7 (far above the rope ceiling), only this clause could have fired;
% pre-reset the bait was fungible between FNL and this clause (2026-06-04
% counterfactual). No signature may read a single authored perspective as a
% story-level claim.
appears_as_rope(C, low_extraction_profile) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(C, ExtMetricName, Eps),
    config:param(rope_epsilon_ceiling, EpsCeil),
    Eps =< EpsCeil,
    % Exclude constraints that are mountains from ALL perspectives.
    % Mountains have low ε by nature — that's not "appearing as rope."
    \+ only_mountain_classifications(C).

%% only_mountain_classifications(+C)
%  FCR's mountain-protection guard: identifies natural-law stories that should
%  not be considered "rope-appearing."
%  BRIDGE (operator ruling 2026-06-12, OQ-109 B3; full adjudication:
%  audits/2026-06-11_oq109_phase_b/UNANIMITY_ADJUDICATION.md):
%   - Arm 1 (legacy): authored-cell unanimity — decides through Phase B; DIES
%     AT PHASE C with the perspectives[] retirement (named retirement point;
%     re-witness the guard there).
%   - Arm 2 (surviving): the authored NL-certification chain (candidate C) —
%     claim=mountain + emerges_naturally + NL collapse/resistance profile.
%     Story-level inputs only; signature-layer-safe (no dr_type, no
%     classification-table read); fail-closed on absence (unauthored metric
%     arrives non-numeric and the guard FAILS). Closes census seam A1: a
%     perspectives-free NL story is protected via this arm.
%  Both named criterion candidates FAILED the pinned gauntlet (computed-seat
%  unanimity splits mountain/rope on genuine NL profiles; natural_law_signature
%  is unsatisfiable — OQ-113). Option 4 (C + no-beneficiary) witnessed
%  under-restrictive (retains 1/6). Extension today = old 6 + 3 NL-certified
%  mountains with non-unanimous authored seats (OQ-114 adjudicates the 3;
%  they all declare beneficiaries, so FSM scrutiny is untouched).
%  SPEC CORRECTION (operator, 2026-06-12): the first bridge landed as a
%  DISJUNCTION (old ∨ C) — but C ⊇ old, so the union just IS C's extension:
%  clause order controls which arm succeeds, not which stories pass. That
%  executed the extension change OQ-114 was filed to defer. Converted to
%  CONDITIONAL DISPATCH: authored table present → old semantics VERBATIM;
%  the C arm fires only on perspectives-free stories. Through Phase B every
%  live story has authored cells, so the corpus-wide extension is exactly the
%  old guard's; at Phase C the dispatch collapses to the C arm (named
%  re-witnessing point), by which time OQ-114 has ruled the extension.
only_mountain_classifications(C) :-
    (   constraint_indexing:constraint_classification(C, _, _)
    ->  % authored-cell arm: old unanimity semantics, verbatim
        \+ (constraint_indexing:constraint_classification(C, Type, _), Type \= mountain)
    ;   % perspectives-free arm: the surviving NL-certification chain, guarded
        % by the OQ-114 per-story exclusion list (guard_exclusions.pl) —
        % FAIL-CLOSED: if the list module is absent/unreadable the
        % current_predicate check fails and the C arm DISABLES (old pre-C
        % behavior: no protection, everything examined) — never silent
        % protection. Listed stories (institutional_trust_erosion, OQ-114
        % substantive dissent) get old-guard semantics: no cells -> no
        % protection -> FCR examines.
        current_predicate(guard_exclusions:nl_chain_exclusion/2),
        \+ guard_exclusions:nl_chain_exclusion(C, _),
        nl_certification_chain(C)
    ).

%% nl_certification_chain(+C)
%  The authored natural-law certification chain (adjudication candidate C).
nl_certification_chain(C) :-
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

%% collect_fcr_failures(+C, -FailedTests)
%  Collects which Boltzmann structural tests fail for a
%  rope-appearing constraint.
collect_fcr_failures(C, FailedTests) :-
    findall(Failure, fcr_test_failure(C, Failure), FailedTests).

% Individual FCR failure tests:

% Test 1: Boltzmann non-compliance (dimension coupling)
fcr_test_failure(C, boltzmann_non_compliant(Score, Threshold)) :-
    boltzmann_compliant(C, non_compliant(Score, Threshold)).

% Test 2: Scope variance (classification changes across scopes)
fcr_test_failure(C, scope_variant(UniqueTypes)) :-
    scope_invariance_test(C, variant(UniqueTypes)).

% Test 3: Excess extraction above Boltzmann floor
fcr_test_failure(C, excess_above_floor(Excess)) :-
    excess_extraction(C, Excess),
    config:param(fcr_excess_floor, NoiseFloor),
    Excess > NoiseFloor.

% Test 4: Nonsensical coupling (coupling without functional justification)
fcr_test_failure(C, nonsensical_coupling(Strength)) :-
    detect_nonsensical_coupling(C, Pairs, Strength),
    Pairs \= [].

% Test 5 (OQ-93 Stage D, ruling (b) 2026-06-11): the level-gradient crossing.
% Rising structural coercion while individual-level coercion falls is the
% WITNESSED-PROCESS form of coordination-washing — falling individual
% coercion IS the camouflage, rising structural coercion IS the extraction.
% Consumed POSITIVELY with named-level requirements: both gradients must be
% PRESENT from the authored grid (level_gradient_divergence/2 fails on an
% absent/partial grid), so the live grid-absent corpus is untouched and
% absence never blocks FCR's other tests. OQ-94 read-site sort: FCR is in
% the SOUND mountain-likeness family; the CI_Rope benignity-family gates
% (:1019, :1122) are NOT touched by this wiring.
fcr_test_failure(C, level_gradient_divergence(GS, GI)) :-
    level_gradient_divergence(C, divergence(GS, GI)).

%% level_gradient_divergence(+C, -divergence(GStructural, GIndividual))
%  The grid-derived divergence signal (OQ-93's unique product: the level
%  axis). Requires its two needles — structural AND individual gradients —
%  computable from authored grid data at the interval's first gradient
%  point; succeeds only when structural rises and individual falls past the
%  system gradient threshold. Absent/partial grid: FAILS (the signal is
%  OPEN; consumers fall back to their non-grid evidence — preregistration
%  absence-semantics pin, audits/2026-06-11_oq93_grid_migration/).
level_gradient_divergence(C, divergence(GS, GI)) :-
    narrative_ontology:interval(C, T0, _),
    catch(coercion_projection:coercion_gradient(structural, C, T0, GS), _, fail),
    catch(coercion_projection:coercion_gradient(individual, C, T0, GI), _, fail),
    config:param(system_gradient_threshold, Thr),
    GS > Thr,
    GI < -Thr.

/* ================================================================
   SIGNATURE: FALSE SUMMIT MOUNTAIN (FSM) — v6.9
   ================================================================
   FSM(C) :-
       mountain_metric_profile(C),       % low ε, low suppression, emerges naturally
       agent_beneficiary(C, _).          % has identifiable AGENT beneficiaries

   Unlike FNL (which requires Boltzmann non-compliance) and FCR
   (which requires rope-appearing metrics), FSM targets the specific
   case where a Mountain meets ALL metric thresholds but has
   beneficiaries that reveal its constructed origin.

   Genuine natural laws have zero AGENT beneficiaries: they benefit no
   particular agent because they are structural features of reality.
   A "natural law" with agent beneficiaries is a naturalized construct —
   a constraint whose constructed origin has been made invisible
   through historical accumulation. A PROPOSITION-kind beneficiary (a
   doctrine/hypothesis the constraint vindicates, e.g. maxwell_demon's
   entropic_universe_hypothesis) is not an agent and must not trip FSM
   (June 2026 agency gate — registry and two-gate principle in
   narrative_ontology.pl at non_agent_beneficiary/1).

   Key design decision: coupling is NOT a hard gate. Mountains are
   immune to contamination (type_contamination_strength = 0.0,
   type_immunity = 0.0), so their coupling scores are typically zero
   even when the underlying extractive structure is present. Requiring
   non-zero coupling would cause FSM to miss the very constraints it
   is designed to catch.

   ACTIVE: FSM detection triggers the tangled_rope override via
   constraint_signature/2 → resolve_modal_signature_conflict/3
   (mountain + false_summit_mountain → tangled_rope). Operational
   since v6.9.
   ================================================================ */

%% false_summit_mountain(+C, -Evidence)
%  Detects mountain-classified constraints with identifiable beneficiaries.
%  Primary gate: beneficiary presence (not coupling score).
%  Coupling collected as diagnostic evidence for abductive T17 trigger.
%
%  Evidence = fsm_evidence(BeneficiaryCount, CouplingScore, LevelDiv)
%    LevelDiv (OQ-93 Stage D) = divergence(GS, GI) when the authored grid
%    witnesses rising-structural/falling-individual coercion (the watchable
%    form of beneficiary naturalization), or the atom `open` when the grid
%    is absent/partial — consumed POSITIVELY (confidence rung below), never
%    a gate: an open signal leaves the verdict exactly as before wiring.
false_summit_mountain(C, fsm_evidence(BeneficiaryCount, CouplingScore, LevelDiv)) :-
    % Metric profile must be consistent with mountain classification.
    % Replicate mountain conditions from classify_from_metrics/6 without
    % the context-dependent immutability check (which varies by observer).
    drl_core:base_extractiveness(C, BaseEps),
    config:param(mountain_extractiveness_max, MaxX),
    BaseEps =< MaxX,
    drl_core:get_raw_suppression(C, Supp),
    % OQ-44 statute (ruled 2026-06-11, merged alongside this Stage-D edit):
    % get_raw_suppression now returns the `unknown` sentinel on absence —
    % fail closed here (no authored suppression scalar => FSM abstains),
    % never compare the sentinel arithmetically.
    number(Supp),
    config:param(mountain_suppression_ceiling, SuppCeil),
    Supp =< SuppCeil,
    domain_priors:emerges_naturally(C),

    % Primary gate: must have at least one identifiable AGENT beneficiary.
    % Genuine natural laws have none — agent-beneficiary presence is the
    % structural signal of constructedness. Proposition-kind values
    % (doctrines/hypotheses the constraint vindicates) are filtered out via
    % narrative_ontology:agent_beneficiary/2 (June 2026 agency gate; ruling
    % and two-gate registry principle documented at the registry).
    findall(B, narrative_ontology:agent_beneficiary(C, B), Beneficiaries),
    Beneficiaries \= [],
    length(Beneficiaries, BeneficiaryCount),

    % Coupling as diagnostic evidence only — not a hard gate.
    % Many false summits have zero coupling because Mountain immunity
    % prevents contamination network from registering the structure.
    (   catch(cross_index_coupling(C, CS), _, CS = 0.0)
    ->  CouplingScore = CS
    ;   CouplingScore = 0.0
    ),

    % OQ-93 Stage D: level-gradient divergence as additional positive
    % evidence; `open` on absent/partial grid (never blocks).
    (   level_gradient_divergence(C, Div)
    ->  LevelDiv = Div
    ;   LevelDiv = open
    ).

/* ================================================================
   SIGNATURE GRADE (OQ-98)

   Grade-determines-wiring (verdict grade distinction): a signature
   finding is CORRECTION-grade iff it is an override signature
   (abductive_helpers:known_override_signature/1) AND it actually
   rewired the type at the default context — i.e. the post-signature
   dr_type/3 departs from the pre-signature metric classification.
   Everything else with a detected signature is COMMENTARY-grade
   (annotates, never alerts).

   Severity = moderate for correction grade — RULED (operator,
   2026-06-11) as the working value: a fired override is one
   subsystem disagreeing with metrics, the same weight
   compute_verdict gives a tension (yellow); the false-claim
   coincidence case already floors red via a severe dr_mismatch.
   Confirmed by the pre-Commit-2 corpus histogram
   (audits/2026-06-11_oq98_verdict_join/).

   Load requirement: drl_core and abductive_helpers are called
   module-qualified at runtime (no use_module here — a static import
   would cycle: abductive_helpers -> maxent_classifier ->
   signature_detection:constraint_signature/2 (maxent_classifier.pl:60)
   back into this module, plus the grothendieck_cohomology -> drl_core
   arm). stack.pl now side-loads abductive_helpers (OQ-115), so
   [stack]-only consumers are covered as well; on the run_pipeline
   chain it also arrives via diagnostic_summary:verdict_join/3, whose
   module imports abductive_helpers. Load-path witnessed on the
   run_pipeline chain: audits/2026-06-11_oq98_verdict_join/p2.
   ================================================================ */

%% converted_signature(?Signature)
%  OQ-138 (2026-06-21): signatures converted from RECLASSIFY (overwrite dr_type)
%  to ROUTE/COMMENT. After conversion dr_type reverts to the metric type, so the
%  legacy `MetricType \= FinalType` grade test is ALWAYS false for these — they
%  would silently fall to commentary and drop their diagnostic (the trap). Instead
%  they grade on their OWN discriminant (signature_diagnostic_severity/3), mirroring
%  drl_core:dr_claim_mismatch/4 which grades on the metric outcome, never on whether
%  a type was overwritten. constructed / coupling_invariant_rope are NOT here — they
%  still overwrite and still grade via the legacy type-delta path (unchanged).
%  false_ci_rope is SEAT-split (converted only at fcr_routed/1 seats — the FCR-9), so it
%  is keyed via converted_at_seat/2, not here (a signature-level entry would wrongly
%  convert the inert + piton false_ci_rope seats). See OQ-138 deferred-clause ruling.
converted_signature(false_summit_mountain).

%% fcr_routed(+C)
%  OQ-138 (2026-06-21): the false_ci_rope seats actually ROUTED by the conversion
%  (the "FCR-9") — those whose post-conversion dr_type is NOT an FCR override target
%  (tangled_rope), NOT the honest-abstain (unknown), and NOT piton. Keyed on the dispatch
%  OUTCOME (dr_type) rather than mirroring clause 3's guard conditions, so it cannot drift
%  from the dispatch (an earlier metric_based_type_indexed proxy diverged from the live
%  ModalType on 2 haiku + 4 flash seats — caught by the cross-corpus generality sweep).
%  NON-CIRCULAR: dr_type/3 is the TYPE dispatch (resolve_with_perspectival_check), which
%  does not consult converted_at_seat/2 or the severity machinery; only the grade/severity
%  side reads fcr_routed. Piton/inert/no-op (metric==tangled_rope) seats are excluded.
fcr_routed(C) :-
    constraint_signature(C, Sig0), Sig0 == false_ci_rope,  % TRUE cascade winner (unbound; a bound-arg
                                                            % query trips on the detector even when FNL/FCR
                                                            % shadows it upstream — §1 wiring gotcha)
    \+ narrative_ontology:piton_candidate(C),       % clause 2 piton refinement (OQ-90)   } stable dispatch
    \+ drl_core:coordination_dead(C),               % clause 1 dead-coordination piton    } GATE predicates
    \+ has_metric_perspectival_variance(C),         % indexical-preserved (override defers)} (no proxy divergence)
    constraint_indexing:default_context(Ctx),
    drl_core:dr_type(C, Ctx, DT),                   % OUTCOME (robust; replaces the metric_based_type_indexed
    DT \== tangled_rope,                            % proxy that diverged from ModalType): routed away from the
    DT \== unknown.                                 % override target, and not the honest-abstain inert case

%% constructed_routed(+C)
%  OQ-138 (2026-06-21): the constructed_high_extraction seats ROUTED by the conversion
%  (the "constructed-3" on live) — the unknown-input seats the override used to lift to snare,
%  now reverted to the honest abstain `unknown`. Outcome-keyed (dr_type == unknown), the same
%  robust pattern as fcr_routed/1: a constructed_high cascade-winner whose post-conversion dr_type
%  is `unknown` is exactly one whose ModalType was unknown (the override fired the unknown-input
%  clause). The 47 inert seats (metric already snare) keep dr_type=snare and are excluded.
constructed_routed(C) :-
    constraint_signature(C, Sig0), Sig0 == constructed_high_extraction,  % TRUE cascade winner (unbound;
                                                            % a bound-arg query trips on the constructed_high
                                                            % DETECTOR even when false_ci_rope shadows it —
                                                            % e.g. superheavy_decay, an FCR inert seat — §1 gotcha)
    constraint_indexing:default_context(Ctx),
    drl_core:dr_type(C, Ctx, unknown).

%% fnl_routed(+C)
%  OQ-138 (2026-07-03): the false_natural_law seats actually ROUTED by the conversion —
%  those whose post-conversion dr_type is NOT the override target (tangled_rope) and NOT
%  the honest-abstain (unknown, the :924 OQ-37 clause — inert seats). Outcome-keyed on
%  dr_type like fcr_routed/1 so it cannot drift from the dispatch. NO piton/coordination-dead/
%  perspectival-variance gates: FNL has no such refinements (it is handled ONLY in
%  resolve_modal_signature_conflict/3, no resolve_with_perspectival_check clause) — the
%  generality sweep's routed∩piton=0 invariant is the standing positive control for this claim.
%  NON-CIRCULAR: dr_type/3's reachable call set does not consult converted_at_seat/2 or the
%  severity machinery — witnessed at HEAD 823b6789, not assumed
%  (audits/2026-07-02_oq138_fnl_evidence/fnl_noncircularity_trace.log).
fnl_routed(C) :-
    constraint_signature(C, Sig0), Sig0 == false_natural_law,  % TRUE cascade winner (unbound;
                                                            % a bound-arg query trips on the
                                                            % detector even when shadowed — §1 gotcha)
    constraint_indexing:default_context(Ctx),
    drl_core:dr_type(C, Ctx, DT),
    DT \== tangled_rope,        % routed away from the override target
    DT \== unknown.             % not the honest-abstain inert case

%% converted_at_seat(+C, +Signature)
%  Seat-level "this seat is converted to route". Signature-level for false_summit_mountain
%  (all its cascade-winners are genuinely overridden); seat-level for false_ci_rope
%  (fcr_routed/1) and constructed_high_extraction (constructed_routed/1). The grade/severity
%  dispatch uses THIS, not converted_signature/1, so a seat-split signature converts only its
%  routed seats.
converted_at_seat(_, false_summit_mountain).
converted_at_seat(C, false_ci_rope) :- fcr_routed(C).
converted_at_seat(C, constructed_high_extraction) :- constructed_routed(C).
converted_at_seat(C, false_natural_law) :- fnl_routed(C).

%% signature_diagnostic_severity(+C, +Signature, -Severity)
%  Discriminated severity for a converted signature, decoupled from the type delta.
%  false_summit_mountain (OQ-122 discriminant): an authored agent victim means
%  concealment is possible => moderate (floors verdict_join yellow). No victim
%  (vic=0) means nothing to conceal (the no-victim exemption) => informational,
%  which routes (the alert is present and VISIBLE in verdict_join Alerts) but raises
%  NO floor (severity_floor/2 is closed on severe->red, moderate->yellow). The
%  informational alert is what keeps "routed" distinguishable from "dropped".
signature_diagnostic_severity(C, false_summit_mountain, moderate) :-
    once(narrative_ontology:constraint_victim(C, _)), !.
signature_diagnostic_severity(_, false_summit_mountain, informational).
% false_ci_rope (OQ-138 FCR-9): same victim discriminant as FSM — an authored victim
% => concealment possible => moderate (floor); none => informational (route, no floor).
signature_diagnostic_severity(C, false_ci_rope, moderate) :-
    once(narrative_ontology:constraint_victim(C, _)), !.
signature_diagnostic_severity(_, false_ci_rope, informational).
% false_natural_law (OQ-138 FNL, 2026-07-03): same victim discriminant as FSM/FCR — an
% authored victim => concealment possible => moderate (yellow floor); none => informational
% (route, no floor); base tensions still render red honestly if warranted (FSM Position-A).
% The CLAIM discriminant (constructed-3's) is degenerate here: every census firing is
% mountain-claimed (FNL fires definitionally on claimed naturality), so it would floor
% every routed seat identically; victim varies (census vic 0–4) and is robust to future
% source-2 profile-match FNL seats where constraint_claim(mountain) is false.
signature_diagnostic_severity(C, false_natural_law, moderate) :-
    once(narrative_ontology:constraint_victim(C, _)), !.
signature_diagnostic_severity(_, false_natural_law, informational).
% constructed_high_extraction (OQ-138 constructed-3): CLAIM discriminant, not victim (all 3 routed
% seats are vic>0, so victim does not distinguish; the authored claim does). A MOUNTAIN claim over a
% high-extraction finding is the concealment (a false-summit shape) — keep its floor at `severe`,
% replacing the floor the manufactured snare used to carry via type_1_false_summit (which now reads
% informational at dr_type=unknown). Non-mountain claims already admit structure => informational
% (route; their headline, if red, comes from the honest base unmask, not the signature).
signature_diagnostic_severity(C, constructed_high_extraction, severe) :-
    narrative_ontology:constraint_claim(C, mountain), !.
signature_diagnostic_severity(_, constructed_high_extraction, informational).

alerting_severity(moderate).
alerting_severity(severe).

%% signature_grade(+Constraint, -Grade)
%  Grade in {correction, commentary}. Fails if no signature detected.
%  Converted signatures (OQ-138) project from their discriminated severity
%  (alerting -> correction, informational -> commentary) so the serialized SigGrade
%  field stays meaningful after dr_type reverts. Legacy override signatures grade on
%  the type delta, unchanged.
signature_grade(C, Grade) :-
    constraint_signature(C, Sig),
    converted_at_seat(C, Sig),
    !,
    signature_diagnostic_severity(C, Sig, Sev),
    ( alerting_severity(Sev) -> Grade = correction ; Grade = commentary ).
signature_grade(C, correction) :-
    constraint_signature(C, Sig),
    abductive_helpers:known_override_signature(Sig),
    constraint_indexing:default_context(Ctx),
    drl_core:metric_based_type_indexed(C, Ctx, MetricType),
    drl_core:dr_type(C, Ctx, FinalType),
    MetricType \= FinalType,
    !.
signature_grade(C, commentary) :-
    constraint_signature(C, _), !.

%% signature_severity(+Constraint, -Severity)
%  Converted signatures (OQ-138) carry their discriminated severity directly —
%  including `informational`, which IS emitted as an alert (the visible route) but
%  raises no floor. Legacy (still-overwriting) signatures keep the historical
%  correction => moderate mapping; commentary-grade legacy gets NO alert.
signature_severity(C, Sev) :-
    constraint_signature(C, Sig),
    converted_at_seat(C, Sig),
    !,
    signature_diagnostic_severity(C, Sig, Sev).
signature_severity(C, moderate) :-
    signature_grade(C, correction).
