% ============================================================================
% CONSTRAINT STORY: france_2027_presidential_election
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_france_2027_presidential_election, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: france_2027_presidential_election
 * human_readable: 2027 French Presidential Election Framework
 * domain: political
 * * SUMMARY:
 * As of early 2026, the 2027 French presidential election is defined by a
 * "Republican Barrier" that is structurally thinning. With President Macron
 * ineligible and the far-right polling strongly, the constitutional
 * 500-endorsement requirement and the two-round voting system act as a
 * coordination mechanism for the political establishment. However, high fiscal
 * deficits and political gridlock create significant extraction felt by voters.
 * * KEY AGENTS:
 * - The French Voter: Subject (Powerless), experiences gridlock and limited choice.
 * - Centrist Institutional Parties: Beneficiary (Institutional), use the system to block extremes.
 * - The Political Scientist: Auditor (Analytical), observes the hybrid nature of the system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Base extractiveness: High (0.48)
% Rationale: High fiscal pressure (112% debt-to-GDP) and the "cost of gridlock"
% where government struggles to pass budgets, extracting value from the populace.
domain_priors:base_extractiveness(france_2027_presidential_election, 0.48).

% Suppression score: Moderate (0.38)
% Rationale: The 500-mayoral endorsement rule acts as a "soft" suppression
% for non-institutional candidates, limiting the field of viable contenders.
domain_priors:suppression_score(france_2027_presidential_election, 0.38).

% Theater Ratio: Low-Moderate (0.40)
% Rationale: While political discourse is performative, the electoral rules and
% legal challenges (e.g., candidate eligibility) are functional and actively enforced.
domain_priors:theater_ratio(france_2027_presidential_election, 0.40).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(france_2027_presidential_election, extractiveness, 0.48).
narrative_ontology:constraint_metric(france_2027_presidential_election, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(france_2027_presidential_election, theater_ratio, 0.40).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(france_2027_presidential_election, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(france_2027_presidential_election).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(france_2027_presidential_election, centrist_institutional_parties).
narrative_ontology:constraint_victim(france_2027_presidential_election, non_institutional_political_movements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE DISENCHANTED VOTER (SNARE)
% High extraction from gridlock and high suppression of choice, felt as a trap.
constraint_indexing:constraint_classification(france_2027_presidential_election, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE INSTITUTIONAL CENTRIST (ROPE)
% Viewed as an essential coordination tool to maintain the "Republic" against extremes.
constraint_indexing:constraint_classification(france_2027_presidential_election, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE POLITICAL SCIENTIST (TANGLED ROPE)
% Detects the hybrid nature: a genuine coordination function (voter choice,
% republican front) tangled with asymmetric extraction (fiscal gridlock,
% suppression of outsiders).
constraint_indexing:constraint_classification(france_2027_presidential_election, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(france_2027_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(france_2027_presidential_election, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(france_2027_presidential_election, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    % The analytical view must resolve the conflict as a Tangled Rope.
    constraint_indexing:constraint_classification(france_2027_presidential_election, tangled_rope, context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    narrative_ontology:constraint_metric(france_2027_presidential_election, extractiveness, E),
    E >= 0.46. % Correctly flags for Tangled Rope / Snare logic.

:- end_tests(france_2027_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness of 0.48 is high, driven by the economic consequences
 * of political gridlock. This high extraction, combined with moderate suppression
 * (0.38 from the 500-signature rule), creates a significant perspectival gap.
 * For the powerless voter, the effective extraction is amplified (χ = 0.48 * 1.5 * 1.0 = 0.72),
 * making the system feel like a Snare. For the institutional beneficiary, the
 * extraction is negated (χ = 0.48 * -0.2 * 1.0 = -0.096), appearing as a pure
 * coordination Rope. The analytical observer correctly identifies this as a
 * Tangled Rope, as it possesses both a genuine coordination function (beneficiary
 * exists) and asymmetric extraction (victim exists), and requires active
* enforcement.
 *
 * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The Tangled Rope classification is critical here. A simpler model might
 * label the system a Snare due to high extraction. However, the existence of
 * a clear beneficiary (centrist parties) who relies on its coordination function
 * (the 'Republican Front') proves it is not pure extraction. This prevents
 * mandatrophy by acknowledging the system's dual nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_france_le_pen_eligibility,
    'Will the appeal court overturn Marine Le Pen\'s 5-year eligibility ban?',
    'Court ruling expected before Summer 2026.',
    'If Upheld: Bardella becomes the sole RN standard-bearer, potentially consolidating the far-right vote. If Overturned: Creates potential for internal rivalry and splits the vote.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(france_2027_presidential_election, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the increasing political gridlock and polarization
% over the interval leading up to the election. Extraction accumulates as
% the government's ability to manage fiscal policy weakens.

% Theater ratio over time (slight increase in political posturing):
narrative_ontology:measurement(france_2027_tr_t0, france_2027_presidential_election, theater_ratio, 0, 0.30).
narrative_ontology:measurement(france_2027_tr_t5, france_2027_presidential_election, theater_ratio, 5, 0.35).
narrative_ontology:measurement(france_2027_tr_t10, france_2027_presidential_election, theater_ratio, 10, 0.40).

% Extraction over time (extraction_accumulation due to gridlock):
narrative_ontology:measurement(france_2027_ex_t0, france_2027_presidential_election, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(france_2027_ex_t5, france_2027_presidential_election, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(france_2027_ex_t10, france_2027_presidential_election, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The electoral system is a mechanism for enforcing a particular political order.
narrative_ontology:coordination_type(france_2027_presidential_election, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */