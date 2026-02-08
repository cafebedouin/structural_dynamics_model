% ============================================================================
% CONSTRAINT STORY: genetic_predisposition
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_genetic_predisposition, []).

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
 * * constraint_id: genetic_predisposition
 * human_readable: Socio-Economic Response to Genetic Predisposition
 * domain: technological/social/economic
 * * SUMMARY:
 * This constraint models not the biological fact of genetic predisposition (a Mountain),
 * but the socio-economic system built upon it. This system uses genetic data for
 * insurance underwriting, preventative healthcare allocation, and employment screening.
 * It creates a structural filter that influences health and financial outcomes,
 * operating as a constructed layer of risk management on top of biological reality.
 * * KEY AGENTS:
 * - High-Risk Individual: The subject, whose biological data becomes a source of extraction (higher premiums, anxiety, limited choices).
 * - Healthcare/Insurance System: The institutional beneficiary, which uses the data for risk pooling and resource allocation, viewing it as a coordination tool.
 * - Systems Auditor: The analytical observer, who sees both the coordination benefits and the asymmetric extraction imposed on certain groups.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Rationale: High (0.6). The system extracts financial security through "genetic
% discrimination" in insurance premiums, extracts agency by locking individuals
% into jobs for health coverage, and extracts well-being through anxiety.
domain_priors:base_extractiveness(genetic_predisposition, 0.6).

% Rationale: Moderate (0.5). Alternatives like universal risk-pooling or ignoring
% genetic data are suppressed by market-based healthcare systems that prioritize
% actuarial precision over social solidarity.
domain_priors:suppression_score(genetic_predisposition, 0.5).

% Rationale: Low (0.1). The system is highly functional, not theatrical. Its
% operations (risk calculation, screening) are concrete.
domain_priors:theater_ratio(genetic_predisposition, 0.1).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(genetic_predisposition, extractiveness, 0.6).
narrative_ontology:constraint_metric(genetic_predisposition, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(genetic_predisposition, theater_ratio, 0.1).

% The system claims to be a rational coordination mechanism for managing risk.
narrative_ontology:constraint_claim(genetic_predisposition, tangled_rope).

% Binary flags
% The system requires laws (e.g., GINA in the US) and regulations to function
% and to manage its extractive effects, making it a classic Tangled Rope.
domain_priors:requires_active_enforcement(genetic_predisposition).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(genetic_predisposition, insurance_actuaries).
narrative_ontology:constraint_beneficiary(genetic_predisposition, pharmaceutical_industry).
narrative_ontology:constraint_victim(genetic_predisposition, high_risk_individuals).
narrative_ontology:constraint_victim(genetic_predisposition, economically_disadvantaged).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE HIGH-RISK INDIVIDUAL (SNARE)
% For the individual, the system is a Snare. Their genetic code, a biological
% fact, is used to create a trap of high premiums and limited choices.
% χ = 0.6 * π(powerless=1.5) * σ(local=0.8) = 0.72.
constraint_indexing:constraint_classification(genetic_predisposition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE HEALTHCARE SYSTEM (ROPE)
% For the institution, genetic data is a Rope. It enables efficient coordination,
% allocating preventative care and structuring risk pools.
% χ = 0.6 * π(institutional=-0.2) * σ(national=1.0) = -0.12.
constraint_indexing:constraint_classification(genetic_predisposition, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees the complete picture: a system with a genuine coordination
% function (beneficiaries) but also severe asymmetric extraction (victims),
% requiring active legal enforcement to manage its tensions. This is a Tangled Rope.
% χ = 0.6 * π(analytical=1.15) * σ(global=1.2) = 0.828.
constraint_indexing:constraint_classification(genetic_predisposition, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genetic_predisposition_tests).

test(perspectival_gap) :-
    % Verify the gap between the powerless (Snare), institutional (Rope), and analytical (Tangled Rope) views.
    constraint_indexing:constraint_classification(genetic_predisposition, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(genetic_predisposition, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(genetic_predisposition, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % This is a high-extraction constraint, so base extractiveness must be >= 0.46.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(genetic_predisposition, ExtMetricName, E),
    E >= 0.46.

test(tangled_rope_structural_properties) :-
    % Verify that all three conditions for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(genetic_predisposition, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(genetic_predisposition, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(genetic_predisposition).

:- end_tests(genetic_predisposition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core decision was to model the socio-economic *system* reacting to genetic
 * data, not the biological data itself (which would be a Mountain with ε < 0.15).
 * This system has a high base extractiveness (0.6) because it directly translates
 * biological chance into financial liability.
 *
 * The Perspectival Gap is stark:
 * - The individual experiences this as a Snare, a trap they cannot escape that extracts wealth and agency.
 * - The institution sees it as a Rope, a tool for efficient risk management and resource allocation.
 * - The analytical observer, seeing both sides, classifies it as a Tangled Rope. It recognizes the valid coordination function but also the severe, asymmetric extraction that requires constant legal mediation.
 *
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is critical for Mandatrophy resolution. A simpler
 * analysis might call the system a pure Snare, ignoring its genuine (if contested)
 * function in coordinating preventative healthcare. Conversely, calling it a Rope
 * would whitewash the severe financial and social extraction imposed on high-risk
 * individuals. The Tangled Rope classification correctly captures this dual nature,
 * preventing mischaracterization and highlighting the inherent tension that must be managed by policy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_genetic_predisposition,
    'To what degree can environmental/epigenetic factors neutralize genetic risk, making actuarial models based purely on genetics obsolete?',
    'Long-term, large-scale twin and population studies correlating lifestyle/environment with outcomes for high-risk genotypes.',
    'If impact is high, the constraint degrades into a Piton (based on outdated science). If low, it remains a potent Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(genetic_predisposition, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has intensified as genomic sequencing has become cheaper and
% more widespread, increasing its extractive potential.
%
% Theater ratio over time (stable and low):
narrative_ontology:measurement(gp_tr_t0, genetic_predisposition, theater_ratio, 0, 0.05).
narrative_ontology:measurement(gp_tr_t5, genetic_predisposition, theater_ratio, 5, 0.08).
narrative_ontology:measurement(gp_tr_t10, genetic_predisposition, theater_ratio, 10, 0.1).

% Extraction over time (increasing as data becomes more integrated):
narrative_ontology:measurement(gp_ex_t0, genetic_predisposition, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(gp_ex_t5, genetic_predisposition, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(gp_ex_t10, genetic_predisposition, base_extractiveness, 10, 0.6).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The system's coordination function is to allocate risk and healthcare resources.
narrative_ontology:coordination_type(genetic_predisposition, resource_allocation).

% This constraint directly influences the structure of health insurance.
narrative_ontology:affects_constraint(genetic_predisposition, health_insurance_premiums).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */