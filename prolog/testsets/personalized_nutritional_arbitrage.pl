% ============================================================================
% CONSTRAINT STORY: personalized_nutritional_arbitrage
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_personalized_nutritional_arbitrage, []).

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
 * * constraint_id: pna
 * human_readable: Personalized Nutritional Arbitrage
 * domain: economic/technological/healthcare
 * * SUMMARY:
 * This constraint describes the system of personalized nutrition platforms (e.g., Zoe)
 * that use microbiome and blood sugar data to provide tailored dietary advice.
 * It creates a two-tiered system: those with access can optimize their health
 * (a coordination benefit), while the platforms extract valuable biological data
 * for research and product development. Those without access remain subject to
 * generic, often suboptimal, public health guidelines.
 * * KEY AGENTS:
 * - Generic Diet Follower: Subject (Powerless), follows one-size-fits-all advice.
 * - Nutrition Platform: Beneficiary (Institutional), monetizes data and insights.
 * - Health Systems Analyst: Auditor (Analytical), evaluates the system's net effect.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(pna, 0.65). % High extraction via data monetization and subscription fees.
domain_priors:suppression_score(pna, 0.40).   % Moderate suppression of alternatives (generic advice is still dominant but less effective).
domain_priors:theater_ratio(pna, 0.15).       % Low theater; the service is primarily functional.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(pna, extractiveness, 0.65).
narrative_ontology:constraint_metric(pna, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(pna, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
% Platforms market themselves as a pure coordination tool for health.
narrative_ontology:constraint_claim(pna, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(pna). % Requires active user data logging and sample submission.

% Structural property derivation hooks:
% has_coordination_function/1 is DERIVED from constraint_beneficiary/2
% has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(pna, nutrition_tech_platforms).
narrative_ontology:constraint_victim(pna, generic_diet_followers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the user without access, the existence of a superior, personalized system
% renders generic public health advice a trap that extracts health and agency.
% χ = 0.65 * π(powerless, 1.5) * σ(national, 1.0) = 0.975
constraint_indexing:constraint_classification(pna, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the platform, the system is a pure coordination mechanism, generating
% revenue and research data while providing a valuable service.
% χ = 0.65 * π(institutional, -0.2) * σ(global, 1.2) = -0.156
constraint_indexing:constraint_classification(pna, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the genuine coordination function and the asymmetric
% data extraction, classifying it as a Tangled Rope.
% χ = 0.65 * π(analytical, 1.15) * σ(global, 1.2) = 0.897
constraint_indexing:constraint_classification(pna, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pna_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(pna, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pna, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(threshold_validation_high_extraction) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(pna, ExtMetricName, E),
    E >= 0.46.

test(tangled_rope_conditions_met) :-
    % Verify the analytical observer correctly identifies a Tangled Rope.
    constraint_indexing:constraint_classification(pna, tangled_rope, context(agent_power(analytical), _, _, _)),
    domain_priors:requires_active_enforcement(pna),
    narrative_ontology:constraint_beneficiary(pna, _),
    narrative_ontology:constraint_victim(pna, _).

:- end_tests(pna_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness of 0.65 reflects the high value of proprietary biological
 * data sets and the subscription-based business model. The suppression score of 0.40
 * acknowledges that while generic advice is the default, it is not coercively
 * enforced; the suppression comes from its demonstrated inferiority.
 *
 * The Perspectival Gap is stark:
 * - The `powerless` user, unable to afford or access the platform, is trapped in an
 *   inferior system. The existence of a better, inaccessible alternative turns their
 *   situation into a Snare, where their health outcomes are extracted relative to
 *   the "optimized" cohort.
 * - The `institutional` platform experiences negative extraction (χ = -0.156), viewing
 *   the system as a highly efficient Rope for coordinating user health and data assets.
 * - The `analytical` observer, seeing both sides, correctly identifies the structure
 *   as a Tangled Rope: a system with a real coordination function (the Rope part)
 *   that is inseparable from its high, asymmetric extraction (the Snare part).
 *
 * * MANDATROPHY ANALYSIS:
 * This classification prevents mislabeling the system as a pure Snare. While highly
 * extractive, it provides a genuine, novel coordination function (matching diet to
 * unique biology) that cannot be dismissed. The Tangled Rope classification
 * correctly captures this duality, forcing an analysis of both its benefits and its
 * costs, preventing the system from collapsing the distinction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_pna,
    'Is the extracted biological data primarily a public good (for research) or a private asset (for profit), and can users control this designation?',
    'Audit of platform terms of service, data-sharing agreements with research institutions, and regulatory frameworks for biological data ownership (e.g., GDPR extensions).',
    'If primarily a public good with user control -> closer to Rope. If primarily a private asset without user control -> closer to Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(pna, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction grew as the business model shifted from research to a direct-to-consumer
% subscription service, monetizing the data more effectively.
% Theater ratio remains low and stable, as the service is functional.

% Theater ratio over time (metric_substitution detection):
narrative_ontology:measurement(pna_tr_t0, pna, theater_ratio, 0, 0.10).
narrative_ontology:measurement(pna_tr_t5, pna, theater_ratio, 5, 0.12).
narrative_ontology:measurement(pna_tr_t10, pna, theater_ratio, 10, 0.15).

% Extraction over time (extraction_accumulation detection):
narrative_ontology:measurement(pna_ex_t0, pna, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(pna_ex_t5, pna, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(pna_ex_t10, pna, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system allocates a resource (personalized advice) based on data.
narrative_ontology:coordination_type(pna, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */