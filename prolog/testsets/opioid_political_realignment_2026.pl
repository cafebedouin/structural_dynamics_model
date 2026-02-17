% ============================================================================
% CONSTRAINT STORY: opioid_political_realignment_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_opioid_realignment, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: opioid_political_realignment_2026
 * human_readable: Opioid-Induced Political Capture
 * domain: political/economic/social
 * * SUMMARY:
 * This constraint tracks the causal link between pharmaceutical extraction (opioid
 * marketing) and subsequent political realignment in the United States.
 * Quasi-exogenous exposure to the epidemic created economic hardship and dependency
 * on public transfers, which crystallized into a durable Republican vote share
 * increase (approx. 4.5 percentage points per std-dev increase by 2022).
 * * KEY AGENTS:
 * - [Affected Communities]: Subject (Powerless) - Trapped in cycles of mortality and economic decline.
 * - [Political Incumbents]: Beneficiary (Institutional) - Gaining additional House seats and vote share.
 * - [Litigants/Researchers]: Auditor (Analytical) - Uncovering the Purdue Pharma records and causal links.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.68) due to the "quasi-exogenous" predatory marketing documented in records.
domain_priors:base_extractiveness(opioid_political_realignment_2026, 0.68).

% High suppression (0.72) of alternatives; economic hardship restricts exit options for affected populations.
domain_priors:suppression_score(opioid_political_realignment_2026, 0.72).

% Moderate theater (0.35) initially, as the "marketing" masqueraded as medicine (functional coordination).
domain_priors:theater_ratio(opioid_political_realignment_2026, 0.35).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(opioid_political_realignment_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(opioid_political_realignment_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(opioid_political_realignment_2026, theater_ratio, 0.35).

% Constraint self-claim: Beneficiaries frame the political shift as a natural
% coordination around shared values, masking the extractive origins.
narrative_ontology:constraint_claim(opioid_political_realignment_2026, tangled_rope).
narrative_ontology:human_readable(opioid_political_realignment_2026, "Opioid-Induced Political Capture").

% Binary flags & Structural properties for Tangled Rope classification
% Enforcement is political/structural: the machinery that translates votes into
% power and maintains the resulting status quo constitutes active enforcement.
domain_priors:requires_active_enforcement(opioid_political_realignment_2026).

% Structural property derivation hooks (required for Tangled Rope):
% has_coordination_function/1 is derived from constraint_beneficiary/2
% has_asymmetric_extraction/1 is derived from constraint_victim/2
narrative_ontology:constraint_beneficiary(opioid_political_realignment_2026, political_incumbents).
narrative_ontology:constraint_victim(opioid_political_realignment_2026, affected_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The epidemic is a predatory trap. Withdrawal and economic ruin remove exit options.
constraint_indexing:constraint_classification(opioid_political_realignment_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Political parties view the realignment as a "natural" coordination around shared
% grievances or "common sense" platform shifts, facilitating electoral success.
constraint_indexing:constraint_classification(opioid_political_realignment_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid nature: a coordination around public transfers (Rope)
% entangled with predatory pharmaceutical extraction (Snare).
constraint_indexing:constraint_classification(opioid_political_realignment_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(opioid_realignment_tests).

test(perspectival_gap) :-
    % Verify the Subject sees a Snare while the Beneficiary sees a Rope.
    constraint_indexing:constraint_classification(opioid_political_realignment_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(opioid_political_realignment_2026, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(opioid_political_realignment_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties_present) :-
    % Verify all three required properties for Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(opioid_political_realignment_2026, _),
    narrative_ontology:constraint_victim(opioid_political_realignment_2026, _),
    domain_priors:requires_active_enforcement(opioid_political_realignment_2026).

:- end_tests(opioid_realignment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.68) is anchored in the "quasi-exogenous" nature of the marketing—
 * it was not a response to existing pain, but a manufactured demand. The suppression (0.72)
 * reflects the combined chemical dependency and economic collapse that traps victims.
 * The Perspectival Gap is extreme: what the institutional agent views as a "realigned
 * electorate" (coordination), the subject experiences as a "mortality trap" (extraction).
 *
 * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is necessary because the realignment isn't just
 * random death; it's the creation of a new political feedback loop involving
 * public transfer dependency. This creates a "forced coordination" function that
 * benefits a political group, which is a classic Tangled Rope signature. Without
 * this classification, the system would misread the political coordination as
 * either benign (Rope) or the entire system as pure predation (Snare), missing
 * the hybrid, self-reinforcing nature of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_causality_depth,
    'To what extent is the realignment "sticky" if the pharmaceutical extraction is removed?',
    'Analysis of 2024-2030 voting patterns in post-settlement litigation zones.',
    'If vote share persists, it indicates a permanent cultural shift (Mountain); if it reverts, it was primarily a Snare-driven effect.',
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(opioid_political_realignment_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Required for high-extraction (0.68 > 0.46).
% Interval spans from early 2000s (T=0) to 2022 (T=10).

% Theater ratio: Increases as the medical justification for marketing
% (functional) is replaced by the "theatrical" persistence of the political divide.
narrative_ontology:measurement(opioid_tr_t0, opioid_political_realignment_2026, theater_ratio, 0, 0.15).
narrative_ontology:measurement(opioid_tr_t5, opioid_political_realignment_2026, theater_ratio, 5, 0.25).
narrative_ontology:measurement(opioid_tr_t10, opioid_political_realignment_2026, theater_ratio, 10, 0.35).

% Extraction: Increases as the opioid exposure accumulates and
% translates into "additional House seats" (political rent-seeking).
narrative_ontology:measurement(opioid_ex_t0, opioid_political_realignment_2026, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(opioid_ex_t5, opioid_political_realignment_2026, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(opioid_ex_t10, opioid_political_realignment_2026, base_extractiveness, 10, 0.68).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The coordination type is the political system itself, which acts as an
% enforcement mechanism for the new electoral alignment.
narrative_ontology:coordination_type(opioid_political_realignment_2026, enforcement_mechanism).

% This constraint is structurally coupled with the general economic decline
% of the affected regions, creating a feedback loop.
narrative_ontology:affects_constraint(opioid_political_realignment_2026, rural_economic_decline).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */