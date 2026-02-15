:- module(structural_signatures, [
    constraint_signature/2,
    signature_confidence/3,
    explain_signature/3,
    integrate_signature_with_modal/3,

    % Boltzmann Compliance Engine (v5.0)
    boltzmann_compliant/2,              % boltzmann_compliant(C, Result)
    boltzmann_shadow_audit/2,           % boltzmann_shadow_audit(C, AuditReport)
    cross_index_coupling/2,             % cross_index_coupling(C, CouplingScore)
    detect_nonsensical_coupling/3,      % detect_nonsensical_coupling(C, CoupledPairs, Strength)
    complexity_adjusted_threshold/2,    % complexity_adjusted_threshold(C, Threshold)
    epistemic_access_check/2,           % epistemic_access_check(C, Result)
    excess_extraction/2,                % excess_extraction(C, ExcessEps)
    boltzmann_floor_for/2,              % boltzmann_floor_for(C, Floor)
    boltzmann_invariant_mountain/2,     % boltzmann_invariant_mountain(C, Result)

    % Boltzmann-Derived Signatures (v5.1)
    false_natural_law/2,                % false_natural_law(C, Evidence)
    coupling_invariant_rope/2,          % coupling_invariant_rope(C, Evidence)
    false_ci_rope/2,                    % false_ci_rope(C, Evidence)
    structural_purity/2,                % structural_purity(C, PurityClass)
    purity_score/2,                     % purity_score(C, Score) ∈ [0,1] | -1.0

    % Purity subscores (v5.1 — exposed for reform recommendations)
    factorization_subscore/2,           % factorization_subscore(C, Score)
    scope_invariance_subscore/2,        % scope_invariance_subscore(C, Score)
    coupling_cleanliness_subscore/2,    % coupling_cleanliness_subscore(C, Score)
    excess_extraction_subscore/2,       % excess_extraction_subscore(C, Score)

    % Cache management
    clear_classification_cache/0        % clear_classification_cache
]).

:- use_module(library(lists)).
:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(constraint_indexing).

% --- Classification memoization cache ---
% Avoids redundant classify_at_context/3 and cross_index_coupling/2
% calls. classify_at_context is called 12 times per constraint
% (4 powers × 3 scopes), and the same constraint may be classified
% 100+ times per test run across Boltzmann, purity, FNL, FCR, etc.
:- dynamic cached_classification/3.   % cached_classification(C, Context, Type)
:- dynamic cached_coupling/2.         % cached_coupling(C, CouplingScore)

%% clear_classification_cache/0
%  Invalidates all memoized classifications and coupling scores.
%  Called by scenario_manager:clear_kb/0 between test intervals.
clear_classification_cache :-
    retractall(cached_classification(_, _, _)),
    retractall(cached_coupling(_, _)).

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
   BOLTZMANN COMPLIANCE ENGINE v5.0

   Based on Tamuz & Sandomirskiy (2025), "On the origin of the
   Boltzmann distribution," Mathematische Annalen.

   Core theorem: The Boltzmann distribution is the ONLY distribution
   that correctly describes unrelated (uncoupled) systems.

   Application to DR: A Natural Law (Mountain) must show Boltzmann-
   compliant independence across index dimensions. Any constraint
   that couples independent dimensions is necessarily Constructed,
   not Natural.

   ACTIVE: Boltzmann compliance drives classification overrides via
   integrate_signature_with_modal/3 (called from drl_core:dr_type/3).
   Override rules NL, FNL, CI_Rope, and FCR are all live in the
   classification pipeline. The original shadow-mode calibration
   period ended when the override rules were wired into
   resolve_modal_signature_conflict (v5.1).

   Edge Cases Handled:
   1. Complexity Offset — high-complexity coordination types have
      inherently higher coupling (global infrastructure vs naming)
   2. Epistemic Access — insufficient indexed classifications make
      the test inconclusive rather than rejecting
   3. Moving Boltzmann Floor — the minimum necessary extraction
      can increase with system complexity over time
   ================================================================ */

/* ----------------------------------------------------------------
   BOLTZMANN COMPLIANCE TEST
   ---------------------------------------------------------------- */

% Categorical: Naturality condition [STRICT] — tests factorizability of classification across index dimensions
%% boltzmann_compliant(+Constraint, -Result)
%  Tests whether a constraint's classification across index
%  dimensions is consistent with Boltzmann independence.
%
%  Result is one of:
%    compliant(CouplingScore)
%    non_compliant(CouplingScore, Threshold)
%    inconclusive(Reason)
%
%  ACTIVE: This predicate feeds constraint_signature/2, which feeds
%  integrate_signature_with_modal/3, which overrides dr_type/3.
%  Use boltzmann_shadow_audit/2 for full diagnostic output.

boltzmann_compliant(C, Result) :-
    (   epistemic_access_check(C, true)
    ->  cross_index_coupling(C, CouplingScore),
        complexity_adjusted_threshold(C, Threshold),
        (   CouplingScore =< Threshold
        ->  Result = compliant(CouplingScore)
        ;   Result = non_compliant(CouplingScore, Threshold)
        )
    ;   Result = inconclusive(insufficient_classifications)
    ).

%% boltzmann_shadow_audit(+Constraint, -AuditReport)
%  Full diagnostic report for Boltzmann compliance.
%  Designed for logging in test_harness.pl without triggering
%  classification changes.
%
%  AuditReport = boltzmann_audit(
%      Constraint,
%      ComplianceResult,
%      CouplingScore,
%      Threshold,
%      CoupledPairs,
%      ExcessExtraction,
%      InvariantResult
%  )

boltzmann_shadow_audit(C, boltzmann_audit(C, Compliance, Coupling, Threshold,
                                          CoupledPairs, Excess, Invariant)) :-
    boltzmann_compliant(C, Compliance),
    (   cross_index_coupling(C, Coupling)
    ->  true
    ;   Coupling = unknown
    ),
    (   complexity_adjusted_threshold(C, Threshold)
    ->  true
    ;   Threshold = unknown
    ),
    (   detect_nonsensical_coupling(C, CoupledPairs, _)
    ->  true
    ;   CoupledPairs = []
    ),
    (   excess_extraction(C, Excess)
    ->  true
    ;   Excess = unknown
    ),
    (   boltzmann_invariant_mountain(C, Invariant)
    ->  true
    ;   Invariant = unknown
    ).

/* ----------------------------------------------------------------
   CROSS-INDEX COUPLING DETECTION
   ----------------------------------------------------------------
   The "Sicherman Dice" test.

   For each constraint, compute classification across a grid of
   (Power, Scope) combinations. If the classification map factorizes
   — i.e., changing Power has the same effect at all Scope levels
   and vice versa — the constraint is Boltzmann-compliant.

   If a scope change flips classification at ONE power level but
   not another, there's a coupling that violates independence.
   ---------------------------------------------------------------- */

% Categorical: Naturality square test [STRICT] — checks commutativity of classification on Power x Scope grid
%% cross_index_coupling(+Constraint, -CouplingScore)
%  Computes coupling score from 0.0 (fully independent) to 1.0
%  (maximally coupled) by testing classification factorizability
%  across Power × Scope grid.

cross_index_coupling(C, CouplingScore) :-
    (   cached_coupling(C, CachedScore)
    ->  CouplingScore = CachedScore
    ;   compute_cross_index_coupling(C, ComputedScore),
        assertz(cached_coupling(C, ComputedScore)),
        CouplingScore = ComputedScore
    ).

%% compute_cross_index_coupling(+Constraint, -CouplingScore)
%  Implementation body for cross_index_coupling/2.
compute_cross_index_coupling(C, CouplingScore) :-
    coupling_test_powers(Powers),
    coupling_test_scopes(Scopes),
    findall(
        classified(P, S, Type),
        (   member(P, Powers),
            member(S, Scopes),
            coupling_test_context(P, S, Ctx),
            classify_at_context(C, Ctx, Type)
        ),
        Grid
    ),
    length(Grid, GridSize),
    (   GridSize < 2
    ->  CouplingScore = 0.0  % Not enough data points
    ;   count_coupling_violations(Grid, Powers, Scopes, Violations),
        length(Powers, NP),
        length(Scopes, NS),
        MaxViolations is NP * (NS - 1),
        (   MaxViolations > 0
        ->  CouplingScore is min(1.0, Violations / MaxViolations)
        ;   CouplingScore = 0.0
        )
    ).

%% coupling_test_powers(-Powers)
%  The power levels used for coupling grid test.
coupling_test_powers([powerless, moderate, institutional, analytical]).

%% coupling_test_scopes(-Scopes)
%  The scope levels used for coupling grid test.
coupling_test_scopes([local, national, global]).

%% coupling_test_context(+Power, +Scope, -Context)
%  Builds a canonical context for coupling grid test.
%  Uses standard time horizon and exit options per power level.
coupling_test_context(powerless, Scope, context(
    agent_power(powerless), time_horizon(biographical),
    exit_options(trapped), spatial_scope(Scope))).
coupling_test_context(moderate, Scope, context(
    agent_power(moderate), time_horizon(biographical),
    exit_options(mobile), spatial_scope(Scope))).
coupling_test_context(institutional, Scope, context(
    agent_power(institutional), time_horizon(generational),
    exit_options(arbitrage), spatial_scope(Scope))).
coupling_test_context(analytical, Scope, context(
    agent_power(analytical), time_horizon(civilizational),
    exit_options(analytical), spatial_scope(Scope))).

%% classify_at_context(+C, +Context, -Type)
%  Memoizing wrapper around classify_at_context_impl/3.
%  Checks cached_classification/3 first; on miss, delegates to impl
%  and caches the result. This avoids redundant metric lookups and
%  classification calls — classify_at_context is invoked 12 times per
%  constraint per Boltzmann test, and the same constraint participates
%  in multiple tests (coupling, purity, FNL, FCR, scope invariance).
classify_at_context(C, Context, Type) :-
    (   cached_classification(C, Context, CachedType)
    ->  Type = CachedType
    ;   classify_at_context_impl(C, Context, ComputedType),
        assertz(cached_classification(C, Context, ComputedType)),
        Type = ComputedType
    ).

%% classify_at_context_impl(+C, +Context, -Type)
%  Computes metrics and delegates to drl_core:classify_from_metrics/6
%  (the single source of truth for threshold classification).
%  Uses explicit module qualification to avoid circular use_module
%  dependency — drl_core imports structural_signatures, so we cannot
%  import drl_core, but runtime-qualified calls work fine since both
%  modules are loaded by the time any coupling test runs.
%  Also uses extractiveness_for_agent (v6.0 directionality chain)
%  instead of the legacy power_modifier * scope_modifier calculation.
classify_at_context_impl(C, Context, Type) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    (   narrative_ontology:constraint_metric(C, ExtMetricName, BaseEps)
    ->  true
    ;   BaseEps = 0.5
    ),
    constraint_indexing:extractiveness_for_agent(C, Context, Chi),
    config:param(suppression_metric_name, SuppMetricName),
    (   narrative_ontology:constraint_metric(C, SuppMetricName, Supp)
    ->  true
    ;   Supp = 0
    ),
    drl_core:classify_from_metrics(C, BaseEps, Chi, Supp, Context, Type).

%% count_coupling_violations(+Grid, +Powers, +Scopes, -Violations)
%  Counts how many (Power, Scope) pairs show classification that
%  doesn't factorize. For each power level, checks if the type
%  is invariant across scopes. Each scope-level change in type
%  counts as a violation.
count_coupling_violations(Grid, Powers, Scopes, Violations) :-
    findall(1,
        (   member(P, Powers),
            member(S1, Scopes),
            member(S2, Scopes),
            S1 @< S2,
            member(classified(P, S1, T1), Grid),
            member(classified(P, S2, T2), Grid),
            T1 \= T2
        ),
        ViolationList
    ),
    length(ViolationList, ScopeViolations),
    % Also check power invariance at each scope
    findall(1,
        (   member(S, Scopes),
            member(P1, Powers),
            member(P2, Powers),
            P1 @< P2,
            member(classified(P1, S, T1), Grid),
            member(classified(P2, S, T2), Grid),
            T1 \= T2,
            % Power-driven variance is EXPECTED (indexical relativity).
            % Only count as violation if the PATTERN of power-variance
            % differs across scopes (i.e., power shifts type at one scope
            % but not another in a way that isn't explained by σ scaling).
            \+ expected_power_divergence(P1, P2, T1, T2)
        ),
        PowerViolationList
    ),
    length(PowerViolationList, PowerViolations),
    Violations is ScopeViolations + PowerViolations.

%% expected_power_divergence(+P1, +P2, +T1, +T2)
%  Power-driven classification divergence is EXPECTED in DR.
%  A powerless agent seeing snare while institutional sees rope
%  is not a coupling violation — it's indexical relativity working
%  correctly. This predicate identifies expected divergence patterns.
expected_power_divergence(powerless, institutional, _, _) :- !.
expected_power_divergence(institutional, powerless, _, _) :- !.
expected_power_divergence(powerless, analytical, _, _) :- !.
expected_power_divergence(analytical, powerless, _, _) :- !.
% Moderate-analytical divergence is expected (π = 1.0 vs 1.15)
expected_power_divergence(moderate, analytical, _, _) :- !.
expected_power_divergence(analytical, moderate, _, _) :- !.
% Mountain-rope divergence at moderate/powerless and analytical/institutional
% is expected: the immutability gate returns rope for (biographical, mobile)
% and (generational, arbitrage) contexts. A natural constraint doesn't become
% changeable because the observer has mobile exit options.
% Type-conditioned: only suppresses divergence when one side is mountain.
expected_power_divergence(moderate, powerless, _, mountain) :- !.
expected_power_divergence(powerless, moderate, mountain, _) :- !.
expected_power_divergence(analytical, institutional, mountain, _) :- !.
expected_power_divergence(institutional, analytical, _, mountain) :- !.
% Indexically opaque transitions: legitimate waypoint on rope → {tangled_rope, snare} path
% As d increases from institutional, extraction becomes visible before consent activates.
expected_power_divergence(_, _, rope, indexically_opaque) :- !.
expected_power_divergence(_, _, indexically_opaque, rope) :- !.
expected_power_divergence(_, _, indexically_opaque, tangled_rope) :- !.
expected_power_divergence(_, _, tangled_rope, indexically_opaque) :- !.
expected_power_divergence(_, _, indexically_opaque, snare) :- !.
expected_power_divergence(_, _, snare, indexically_opaque) :- !.

/* ----------------------------------------------------------------
   NONSENSICAL COUPLING DETECTION
   ----------------------------------------------------------------
   Identifies WHICH specific dimension pairs show coupling that
   violates Boltzmann independence. This is the "Sicherman Dice"
   diagnostic — it tells you exactly which "dice" are "crazy."
   ---------------------------------------------------------------- */

%% detect_nonsensical_coupling(+Constraint, -CoupledPairs, -Strength)
%  Returns list of coupled dimension pairs and overall coupling strength.
%  CoupledPairs = [coupled(Dim1, Dim2, Score), ...]
%  Strength = aggregate coupling strength in [0, 1]

detect_nonsensical_coupling(C, CoupledPairs, Strength) :-
    coupling_test_powers(Powers),
    coupling_test_scopes(Scopes),
    findall(
        classified(P, S, Type),
        (   member(P, Powers), member(S, Scopes),
            coupling_test_context(P, S, Ctx),
            classify_at_context(C, Ctx, Type)
        ),
        Grid
    ),
    findall(
        coupled(power_scope, P, ScopePair, Score),
        (   member(P, Powers),
            member(S1, Scopes), member(S2, Scopes),
            S1 @< S2,
            member(classified(P, S1, T1), Grid),
            member(classified(P, S2, T2), Grid),
            T1 \= T2,
            ScopePair = S1-S2,
            Score = 1.0
        ),
        CoupledPairs
    ),
    (   CoupledPairs = []
    ->  Strength = 0.0
    ;   length(CoupledPairs, N),
        length(Powers, NP), length(Scopes, NS),
        MaxPairs is NP * (NS * (NS - 1)) // 2,
        (MaxPairs > 0 -> Strength is min(1.0, N / MaxPairs) ; Strength = 0.0)
    ).

/* ----------------------------------------------------------------
   COMPLEXITY-ADJUSTED THRESHOLD
   ----------------------------------------------------------------
   Edge Case #1: A global power grid MUST couple dimensions that
   a simple naming convention does not. The Boltzmann coupling
   threshold should be higher for inherently complex coordination.
   ---------------------------------------------------------------- */

%% complexity_adjusted_threshold(+Constraint, -Threshold)
%  Returns the effective Boltzmann coupling threshold after applying
%  the complexity offset for the constraint's coordination type.

complexity_adjusted_threshold(C, Threshold) :-
    config:param(boltzmann_coupling_threshold, BaseThreshold),
    coordination_type_offset(C, Offset),
    Threshold is BaseThreshold + Offset.

%% coordination_type_offset(+Constraint, -Offset)
%  Looks up the complexity offset for a constraint's coordination type.
%  Falls back to default if no coordination type is declared.
coordination_type_offset(C, Offset) :-
    narrative_ontology:coordination_type(C, Type),
    coordination_type_to_offset_param(Type, ParamName),
    config:param(ParamName, Offset), !.
coordination_type_offset(_, Offset) :-
    config:param(complexity_offset_default, Offset).

coordination_type_to_offset_param(information_standard,  complexity_offset_information_standard).
coordination_type_to_offset_param(resource_allocation,   complexity_offset_resource_allocation).
coordination_type_to_offset_param(enforcement_mechanism, complexity_offset_enforcement_mechanism).
coordination_type_to_offset_param(global_infrastructure, complexity_offset_global_infrastructure).

/* ----------------------------------------------------------------
   EPISTEMIC ACCESS CHECK
   ----------------------------------------------------------------
   Edge Case #2: If an agent's Markov Blanket prevents them from
   seeing enough of the constraint, the Boltzmann test is
   inconclusive rather than rejecting.
   ---------------------------------------------------------------- */

%% epistemic_access_check(+Constraint, -Sufficient)
%  Returns true if enough indexed classifications exist for a
%  reliable Boltzmann compliance test, false otherwise.
epistemic_access_check(C, true) :-
    config:param(boltzmann_min_classifications, MinN),
    findall(Ctx,
        constraint_indexing:constraint_classification(C, _, Ctx),
        Ctxs
    ),
    length(Ctxs, N),
    N >= MinN, !.
epistemic_access_check(_, false).

/* ----------------------------------------------------------------
   PRICE OF ANARCHY / EXCESS EXTRACTION
   ----------------------------------------------------------------
   The Boltzmann floor is the minimum extraction inherent to the
   coordination type. Extraction above the floor is "extractive
   overhead" — the Price of Anarchy excess.

   Edge Case #3: The floor can move. Technology changes can lower
   the floor (reform pressure) or raise it (necessary complexity
   increase). Testsets can override via boltzmann_floor_override/2.
   ---------------------------------------------------------------- */

%% excess_extraction(+Constraint, -ExcessEps)
%  Computes how much extraction exceeds the Boltzmann floor.
%  ExcessEps = max(0, ε(C) - floor(coordination_type(C)))
%  This is the "extractive overhead" — the PoA excess.
excess_extraction(C, ExcessEps) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(C, ExtMetricName, Eps),
    boltzmann_floor_for(C, Floor),
    ExcessEps is max(0.0, Eps - Floor).

%% boltzmann_floor_for(+Constraint, -Floor)
%  Returns the Boltzmann floor for a constraint.
%  Priority: per-constraint override > coordination type > default
boltzmann_floor_for(C, Floor) :-
    narrative_ontology:boltzmann_floor_override(C, Floor), !.
boltzmann_floor_for(C, Floor) :-
    narrative_ontology:coordination_type(C, Type),
    coordination_type_to_floor_param(Type, ParamName),
    config:param(ParamName, Floor), !.
boltzmann_floor_for(_, Floor) :-
    config:param(boltzmann_floor_default, Floor).

coordination_type_to_floor_param(information_standard,  boltzmann_floor_information_standard).
coordination_type_to_floor_param(resource_allocation,   boltzmann_floor_resource_allocation).
coordination_type_to_floor_param(enforcement_mechanism, boltzmann_floor_enforcement_mechanism).
coordination_type_to_floor_param(global_infrastructure, boltzmann_floor_global_infrastructure).

/* ----------------------------------------------------------------
   BOLTZMANN-INVARIANT MOUNTAIN TEST
   ----------------------------------------------------------------
   Axiom: Mountains must be Boltzmann-invariant across all indices.

   A constraint passes the Boltzmann-invariant mountain test iff:
   1. χ(C, P, S) factorizes (cross_index_coupling ≤ threshold)
   2. Classification is scope-invariant (same type at all scopes)
   3. No coupling drift (coupling topology is static)
   4. No excess extraction above Boltzmann floor

   This is the mathematically crisp definition of "natural law."

   SHADOW MODE: Results logged, not enforced.
   ---------------------------------------------------------------- */

%% boltzmann_invariant_mountain(+Constraint, -Result)
%  Tests whether a constraint satisfies all four Boltzmann
%  invariance conditions for Mountain classification.
%
%  Result is one of:
%    invariant(Details)        — passes all four tests
%    variant(FailedTests)      — fails one or more tests
%    inconclusive(Reason)      — insufficient data

boltzmann_invariant_mountain(C, inconclusive(insufficient_data)) :-
    epistemic_access_check(C, false), !.

boltzmann_invariant_mountain(C, Result) :-
    % Test 1: Factorization (Boltzmann compliance)
    boltzmann_compliant(C, CompResult),
    (   CompResult = compliant(_)
    ->  T1 = pass(factorization)
    ;   T1 = fail(factorization, CompResult)
    ),

    % Test 2: Scope invariance
    scope_invariance_test(C, ScopeResult),
    (   ScopeResult = invariant
    ->  T2 = pass(scope_invariance)
    ;   T2 = fail(scope_invariance, ScopeResult)
    ),

    % Test 3: No excess extraction above Boltzmann floor
    (   excess_extraction(C, Excess)
    ->  (   Excess =< 0.01
        ->  T3 = pass(no_excess_extraction)
        ;   T3 = fail(excess_extraction, Excess)
        )
    ;   T3 = pass(no_extraction_data)  % Mountains often have ε ≈ 0
    ),

    % Test 4: Natural law signature (existing check)
    get_constraint_profile(C, Profile),
    (   natural_law_signature(Profile)
    ->  T4 = pass(natural_law_signature)
    ;   T4 = fail(natural_law_signature)
    ),

    % Aggregate results
    Tests = [T1, T2, T3, T4],
    include(is_failure, Tests, Failures),
    (   Failures = []
    ->  Result = invariant(Tests)
    ;   Result = variant(Failures)
    ).

%% scope_invariance_test(+Constraint, -Result)
%  Tests whether classification is stable across all scope levels
%  while holding power fixed at analytical (the most sensitive).
scope_invariance_test(C, Result) :-
    coupling_test_scopes(Scopes),
    findall(
        Type,
        (   member(S, Scopes),
            coupling_test_context(analytical, S, Ctx),
            classify_at_context(C, Ctx, Type)
        ),
        Types
    ),
    sort(Types, UniqueTypes),
    (   length(UniqueTypes, 1)
    ->  Result = invariant
    ;   Result = variant(UniqueTypes)
    ).

%% is_failure(+TestResult)
is_failure(fail(_)).
is_failure(fail(_, _)).

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
    include(is_failure, Tests, Failures),

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

/* ----------------------------------------------------------------
   IB-AWARE COMPLEXITY THRESHOLD REFINEMENT
   ----------------------------------------------------------------
   Refines complexity_adjusted_threshold/2 for constraints that
   have theater ratio data. High theater ratio (high NoiseRatio
   in IB terms) suggests the coupling is extractive rather than
   functional, even in high-complexity coordination types.

   This prevents global_infrastructure constraints from getting
   a free pass on coupling if their theater ratio reveals the
   coupling is performance rather than function.
   ---------------------------------------------------------------- */

%% ib_adjusted_threshold(+Constraint, -Threshold)
%  Like complexity_adjusted_threshold but reduces the offset
%  when theater ratio is high (IB signal loss).
ib_adjusted_threshold(C, Threshold) :-
    config:param(boltzmann_coupling_threshold, BaseThreshold),
    coordination_type_offset(C, RawOffset),
    % If theater ratio is available, scale the offset down
    % proportionally. High theater = coupling is likely extractive
    % not functional, so don't give it the full complexity benefit.
    (   config:param(theater_metric_name, TM),
        narrative_ontology:constraint_metric(C, TM, TR),
        TR > 0.0
    ->  % IB scaling: offset × (1 - TheaterRatio)
        % At TR=0: full offset. At TR=0.7: only 30% of offset.
        SignalRetention is max(0.0, 1.0 - TR),
        AdjustedOffset is RawOffset * SignalRetention
    ;   AdjustedOffset = RawOffset
    ),
    Threshold is BaseThreshold + AdjustedOffset.

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

/* ================================================================
   PURITY SCORE — v5.1
   ================================================================
   Combines the four Boltzmann structural tests into a single
   scalar in [0, 1]:

     purity_score(C) = 0.30 × factorization
                     + 0.25 × scope_invariance
                     + 0.25 × coupling_cleanliness
                     + 0.20 × (1 - excess_extraction)

   Interpretation:
     1.0  = perfectly pure (all tests pass, no contamination)
     >0.8 = structurally sound (minor contamination tolerable)
     0.5  = borderline (significant contamination, drift risk)
     <0.3 = contaminated (multiple structural failures)
     -1.0 = inconclusive (insufficient epistemic data)

   Use cases:
     - Rank coordination mechanisms by structural soundness
     - Detect drift toward impurity (purity_score decreasing)
     - Compare constraints across domains
     - Identify fragile ropes: purity ∈ [0.5, 0.7] →
       one drift event from tangled_rope
     - Integrate into fingerprint coupling dimension
   ================================================================ */

% Categorical: Naturality health scalar — weighted composite of four naturality test subscores
%% purity_score(+Constraint, -Score)
%  Computes scalar purity score. Returns -1.0 for insufficient data.
purity_score(C, Score) :-
    epistemic_access_check(C, true),
    !,
    factorization_subscore(C, F),
    scope_invariance_subscore(C, SI),
    coupling_cleanliness_subscore(C, CC),
    excess_extraction_subscore(C, EX),
    RawScore is 0.30 * F + 0.25 * SI + 0.25 * CC + 0.20 * EX,
    Score is min(1.0, max(0.0, RawScore)).
purity_score(_, -1.0).  % Sentinel for insufficient epistemic data

%% factorization_subscore(+C, -F)
%  1.0 if Boltzmann-compliant. Decays with coupling score.
factorization_subscore(C, F) :-
    (   cross_index_coupling(C, CouplingScore)
    ->  F is max(0.0, 1.0 - CouplingScore)
    ;   F = 0.5  % Neutral if no data
    ).

%% scope_invariance_subscore(+C, -SI)
%  1.0 if scope-invariant. Penalized per extra classification type.
scope_invariance_subscore(C, SI) :-
    scope_invariance_test(C, Result),
    (   Result = invariant
    ->  SI = 1.0
    ;   Result = variant(Types)
    ->  length(Types, N),
        % Penalize 0.25 per extra type beyond unity
        SI is max(0.0, 1.0 - (N - 1) * 0.25)
    ;   SI = 0.5
    ).

%% coupling_cleanliness_subscore(+C, -CC)
%  1.0 if no nonsensical coupling. Decays with coupling strength.
coupling_cleanliness_subscore(C, CC) :-
    (   detect_nonsensical_coupling(C, Pairs, Strength),
        Pairs \= []
    ->  CC is max(0.0, 1.0 - Strength)
    ;   CC = 1.0  % No coupling = clean
    ).

%% excess_extraction_subscore(+C, -EX)
%  1.0 if no excess extraction. Decays: excess of 0.5 → score 0.0.
excess_extraction_subscore(C, EX) :-
    (   excess_extraction(C, Excess)
    ->  EX is max(0.0, 1.0 - min(1.0, Excess * 2.0))
    ;   EX = 1.0  % No extraction data = clean
    ).
