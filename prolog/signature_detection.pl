:- module(signature_detection, [
    constraint_signature/2,
    signature_confidence/3,
    explain_signature/3,
    integrate_signature_with_modal/3,
    false_natural_law/2,
    coupling_invariant_rope/2,
    false_ci_rope/2,
    structural_purity/2,
    resolve_modal_signature_conflict/3,
    get_constraint_profile/2,
    has_viable_alternatives/2
]).

:- use_module(library(lists)).
:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(boltzmann_compliance).

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

% Profile-based classification (v3.2 original pipeline)
constraint_signature(C, Signature) :-
    get_constraint_profile(C, Profile),
    config:param(extractiveness_metric_name, ExtMetricName),
    get_metric_average(C, ExtMetricName, Extraction),
    classify_by_signature(Profile, Extraction, Signature).

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
    ;   Average = 0.5  % Default if no data
    ).

%% count_power_beneficiaries(+Constraint, -Count)
%  Counts distinct classes with positive power changes
count_power_beneficiaries(C, Count) :-
    % Find intervals affecting this constraint
    findall(Class, (
        narrative_ontology:affects_constraint(I, C),
        narrative_ontology:intent_power_change(I, Class, Delta),
        Delta > 0.1  % Meaningful gain threshold
    ), Beneficiaries),
    sort(Beneficiaries, UniqueBeneficiaries),
    length(UniqueBeneficiaries, Count).

%% has_viable_alternatives(+Constraint, -HasAlternatives)
%  Checks if viable alternatives were considered (indicates choice vs necessity)
has_viable_alternatives(C, true) :-
    narrative_ontology:affects_constraint(I, C),
    narrative_ontology:intent_viable_alternative(I, _, _), !.
has_viable_alternatives(_, false).

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

classify_by_signature(Profile, _, piton_signature) :-
    piton_signature(Profile), !.

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

    % Metric conditions
    config:param(natural_law_collapse_min, CollapseMin),
    AccessCollapse >= CollapseMin,  % Default: 0.85

    config:param(natural_law_suppression_max, SuppMax),
    Suppression =< SuppMax,  % Default: 0.15

    config:param(natural_law_resistance_max, ResMax),
    Resistance =< ResMax,  % Default: 0.15

    % Structural conditions (CRITICAL for distinguishing from coordination)
    BeneficiaryCount == 0,  % No asymmetric winners
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

piton_signature(profile(_AccessCollapse, Suppression, Resistance,
                                   _BeneficiaryCount, HasAlternatives,
                                   TemporalStability, _CoordinationSuccess)) :-
    Suppression =< 0.2,       % Low active enforcement
    Resistance > 0.2,         % But people are pushing back
    HasAlternatives == true,  % It was a choice
    TemporalStability == evolving. % It has decayed or gotten worse

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
    (   false_ci_rope(C, fcr_evidence(_, FailedTests, _, _, _))
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

% Profile-based confidence (v3.2 original pipeline)
signature_confidence(C, Signature, Confidence) :-
    get_constraint_profile(C, Profile),
    compute_signature_confidence(Profile, Signature, Confidence).

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

explain_signature(C, constructed_high_extraction, Explanation) :-
    get_constraint_profile(C, Profile),
    Profile = profile(_, S, R, _, _, _, _),
    config:param(extractiveness_metric_name, ExtMetricName),
    get_metric_average(C, ExtMetricName, Ext),
    format(atom(Explanation),
           'CONSTRUCTED HIGH-EXTRACTION signature for ~w: Enforcement present (suppression=~2f, resistance=~2f) with high extraction (~2f). This is an extraction mechanism that metrics failed to classify as snare.',
           [C, S, R, Ext]).

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
    (   false_ci_rope(C, fcr_evidence(AppType, FailedTests, CouplingScore, _, _))
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
               'COUPLING-INVARIANT ROPE signature for ~w: Certified true coordination mechanism. Boltzmann compliance=~w, scope invariance=~w, excess extraction=~3f. Passes all structural purity tests — this is genuine coordination, not low-extraction construction.',
               [C, Compliance, ScopeResult, ExcessEps])
    ;   format(atom(Explanation),
               'COUPLING-INVARIANT ROPE signature for ~w: Certified true coordination mechanism. Use coupling_invariant_rope/2 for detailed evidence.',
               [C])
    ).

explain_signature(C, ambiguous, Explanation) :-
    format(atom(Explanation),
           'AMBIGUOUS signature for ~w: Insufficient structural differentiation to classify. Consider gathering more data on alternatives, beneficiaries, and temporal evolution.',
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
resolve_with_perspectival_check(C, ModalType, false_ci_rope, AdjustedType) :-
    !,
    (   has_metric_perspectival_variance(C)
    ->  AdjustedType = ModalType    % Preserve: indexical differentiation detected
    ;   AdjustedType = tangled_rope % Override: uniform classification is suspicious
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
% SIGNATURE OVERRIDE RULE (logic.md §III-A, Rule NL):  [ACTIVE, unconditional]
%   NL(C) → Mountain (▪) regardless of metric-based classification.
%   Natural Law is the strongest structural signal. If a constraint
%   passes the NL signature test (extreme collapse, zero enforcement,
%   no alternatives, no beneficiaries, temporally stable), it IS a
%   Mountain no matter what the metric classifier says.
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

% Categorical: Priority resolution on type space — structural signal overrides metric classification
resolve_modal_signature_conflict(_, natural_law, Result) :- !, Result = mountain.

% FNL OVERRIDE RULE (v5.1, §III-A extension):  [ACTIVE, unconditional]
%   FNL(C) → tangled_rope regardless of metric-based classification.
resolve_modal_signature_conflict(_, false_natural_law, Result) :- !, Result = tangled_rope.

% CI_ROPE OVERRIDE RULE (v5.1, §III-A extension):  [ACTIVE, unconditional]
%   CI_Rope(C) → rope regardless of metric-based classification.
resolve_modal_signature_conflict(_, coupling_invariant_rope, Result) :- !, Result = rope.

% FCR OVERRIDE RULE (v5.1, §III-A extension, perspectival gate v5.3):  [ACTIVE, gated]
%   NOTE: This rule is now only reached as fallback from
%   resolve_with_perspectival_check/4 when has_metric_perspectival_variance
%   fails. Direct callers of resolve_modal_signature_conflict still see
%   the unconditional override for backward compatibility.
resolve_modal_signature_conflict(_, false_ci_rope, Result) :- !, Result = tangled_rope.

% Coordination scaffolds should be ROPES not mountains
resolve_modal_signature_conflict(mountain, coordination_scaffold, Result) :- !, Result = rope.

% Constructed constraints override mountain classification
resolve_modal_signature_conflict(mountain, constructed_low_extraction, Result) :- !, Result = rope.
resolve_modal_signature_conflict(mountain, constructed_high_extraction, Result) :- !, Result = tangled_rope.
resolve_modal_signature_conflict(mountain, constructed_constraint, Result) :- !, Result = tangled_rope.

% When metrics fail (unknown), signature provides extraction-aware classification
resolve_modal_signature_conflict(unknown, coordination_scaffold, Result) :- !, Result = rope.
resolve_modal_signature_conflict(unknown, constructed_low_extraction, Result) :- !, Result = rope.
resolve_modal_signature_conflict(unknown, constructed_high_extraction, Result) :- !, Result = snare.
resolve_modal_signature_conflict(unknown, constructed_constraint, Result) :- !, Result = tangled_rope.
resolve_modal_signature_conflict(unknown, piton_signature, Result) :- !, Result = piton.
resolve_modal_signature_conflict(unknown, ambiguous, Result) :- !, Result = unknown.

% No conflict - keep original classification
resolve_modal_signature_conflict(ModalType, _, ModalType).

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
%  Three sources of naturality claims:
%  1. Explicit mountain constraint_claim in testset data
%  2. Indexed classification as mountain from any perspective
%  3. Profile matches natural_law_signature pattern
claimed_natural(C, explicit_mountain_claim) :-
    narrative_ontology:constraint_claim(C, mountain), !.
claimed_natural(C, indexed_mountain_classification) :-
    constraint_indexing:constraint_classification(C, mountain, _), !.
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

    % Must have no excess extraction (or very close to zero)
    (   excess_extraction(C, ExcessEps)
    ->  ExcessEps =< 0.05  % Within noise floor of Boltzmann floor
    ;   ExcessEps = 0.0    % No extraction data = no excess
    ),

    % Must have a coordination function
    narrative_ontology:has_coordination_function(C).

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
     pure_natural_law     — NL signature + all four tests pass
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
    epistemic_access_check(C, false), !.

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
determine_pure_subtype(C, pure_natural_law) :-
    get_constraint_profile(C, Profile),
    natural_law_signature(Profile), !.
determine_pure_subtype(C, pure_coordination) :-
    narrative_ontology:has_coordination_function(C), !.
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
%                           ExcessExtraction, ScopeResult)

false_ci_rope(C, fcr_evidence(AppearanceType, FailedTests, CouplingScore,
                               ExcessExtraction, ScopeResult)) :-
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

    % Zero-excess exemption: if a constraint has no extractive overhead,
    % coupling alone is insufficient evidence of coordination washing.
    % Scope-sensitive classification with zero excess is the indexical
    % system working correctly, not a sign of hidden extraction.
    % Requires at least one non-coupling failure to flag as FCR.
    \+ zero_excess_coupling_only(ExcessExtraction, FailedTests).

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
appears_as_rope(C, indexed_rope_classification) :-
    constraint_indexing:constraint_classification(C, rope, _), !.
appears_as_rope(C, low_extraction_profile) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(C, ExtMetricName, Eps),
    config:param(rope_epsilon_ceiling, EpsCeil),
    Eps =< EpsCeil,
    % Exclude constraints that are mountains from ALL perspectives.
    % Mountains have low ε by nature — that's not "appearing as rope."
    \+ only_mountain_classifications(C).

%% only_mountain_classifications(+C)
%  True if the constraint has at least one indexed classification
%  AND all of them are mountain. This identifies pure natural laws
%  that should not be considered "rope-appearing."
only_mountain_classifications(C) :-
    constraint_indexing:constraint_classification(C, _, _),  % At least one exists
    \+ (constraint_indexing:constraint_classification(C, Type, _), Type \= mountain).

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
    Excess > 0.05.  % Above noise floor

% Test 4: Nonsensical coupling (coupling without functional justification)
fcr_test_failure(C, nonsensical_coupling(Strength)) :-
    detect_nonsensical_coupling(C, Pairs, Strength),
    Pairs \= [].
