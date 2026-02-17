% ============================================================================
% CONSTRAINT STORY: dead_sea_effect
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_dead_sea_effect, []).

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
 * * constraint_id: dead_sea_effect
 * human_readable: The Dead Sea Effect (Talent Evaporation)
 * domain: social/economic
 * * SUMMARY:
 * The Dead Sea Effect occurs in organizations when highly talented and mobile individuals "evaporate" (leave) because they have the best exit options, while less talented or mobile individuals stay behind. This process increases the "salinity" (concentration of mediocrity) of the remaining talent pool, leading to systemic decay.
 * * KEY AGENTS:
 * - High-Performer: The mobile agent who leaves the decaying system.
 * - Remaining Employee: The less-mobile agent trapped in the decaying system.
 * - Oblivious Management: The institutional agent who misinterprets the symptoms.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(dead_sea_effect, 0.50). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(dead_sea_effect, 0.40).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(dead_sea_effect, 0.10).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(dead_sea_effect, extractiveness, 0.50).
narrative_ontology:constraint_metric(dead_sea_effect, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(dead_sea_effect, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
% Management often frames this as a natural, unavoidable process.
narrative_ontology:constraint_claim(dead_sea_effect, snare).
narrative_ontology:human_readable(dead_sea_effect, "The Dead Sea Effect (Talent Evaporation)").

% Structural property derivation hooks:
% The effect extracts value from the institution and its remaining employees,
% which indirectly benefits competitors who hire the evaporated talent.
narrative_ontology:constraint_beneficiary(dead_sea_effect, competitor_organizations).
narrative_ontology:constraint_victim(dead_sea_effect, the_stagnant_institution).
narrative_ontology:constraint_victim(dead_sea_effect, remaining_employees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE HIGH-PERFORMER (ROPE)
% For the mobile, high-performer, the decaying environment is a signal to
% leave. The constraint doesn't bind them; it's a 'Rope' they use to climb
% out to a better opportunity. Their high power and mobility negate the extraction.
constraint_indexing:constraint_classification(dead_sea_effect, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: THE REMAINING EMPLOYEE (SNARE)
% For those who cannot leave, the increasing "salinity" is a 'Snare'.
% As talent leaves, workload and institutional decay accelerate. They are
% trapped in a failing system with a declining career path.
constraint_indexing:constraint_classification(dead_sea_effect, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: OBLIVIOUS MANAGEMENT (MOUNTAIN)
% To management that misinterprets the data (e.g., seeing high retention of
% the remaining pool as a sign of loyalty), the effect is a 'Mountain'. They
% perceive talent attrition as a natural, unchangeable law of business,
% failing to see it as a symptom of a preventable systemic failure.
constraint_indexing:constraint_classification(dead_sea_effect, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (SNARE)
% The analyst sees a system that asymmetrically extracts value from its
% trapped members and the institution itself, benefiting external competitors.
% It lacks a true coordination function and is not temporary, classifying it
% as a constructed Snare resulting from poor policy.
constraint_indexing:constraint_classification(dead_sea_effect, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dead_sea_effect_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between the key agents.
    constraint_indexing:constraint_classification(dead_sea_effect, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dead_sea_effect, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(dead_sea_effect, TypePowerful, context(agent_power(powerful), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == mountain,
    TypePowerful == rope,
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    % Verify this is correctly identified as a high-extraction constraint.
    narrative_ontology:constraint_metric(dead_sea_effect, extractiveness, E),
    E >= 0.46.

:- end_tests(dead_sea_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a system failure masked as a natural process.
 * - Extractiveness (0.50): The organization extracts the stability of its less-mobile workforce at the cost of its high-performers' potential, leading to a net loss of institutional value that benefits competitors.
 * - Suppression (0.40): The true nature of the problem is suppressed by misleading metrics like overall retention, which management misinterprets as positive.
 * - Theater Ratio (0.10): The effect is a real, not performative, decay.
 *
 * The Perspectival Gap is extreme:
 * - The mobile (powerful) see a Rope to a better job.
 * - The trapped (powerless) see a Snare that degrades their career.
 * - The institutionally blind see a Mountain, an unchangeable fact of life.
 * This divergence is characteristic of constructed Snares that mimic natural laws.
 *
 * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] This constraint is not a Tangled Rope because it lacks a genuine, internal coordination function. The "benefit" to competitors is a side effect of internal decay, not a designed coordination mechanism. Classifying it as a Snare from the analytical perspective correctly identifies it as a value-destructive process, preventing the system from misinterpreting the talent filtering as a valid (if tangled) coordination effort.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_dead_sea_effect,
    'Is the effect a result of active policy (e.g., uniform compensation that penalizes high-performers) or passive neglect (failure to implement retention strategies)?',
    'Internal audit of HR policies, compensation structures, and management training programs compared to industry best practices.',
    'If active policy, it is a pure Snare. If passive neglect, it has characteristics of a Piton (institutional inertia causing decay).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(dead_sea_effect, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the progression of the Dead Sea Effect. Initially, the
% organization is healthier (low extraction), but as talent evaporates, the
% concentration of mediocrity rises, increasing the extractive pressure on
% those who remain and on the institution's overall vitality.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(dse_tr_t0, dead_sea_effect, theater_ratio, 0, 0.05).
narrative_ontology:measurement(dse_tr_t5, dead_sea_effect, theater_ratio, 5, 0.08).
narrative_ontology:measurement(dse_tr_t10, dead_sea_effect, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(dse_ex_t0, dead_sea_effect, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(dse_ex_t5, dead_sea_effect, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(dse_ex_t10, dead_sea_effect, base_extractiveness, 10, 0.50).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% This constraint is a failure mode of a system, not a coordination mechanism
% in itself, so no coordination_type is declared. No network relationships
% are specified in this context.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */