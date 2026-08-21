% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__hybrid_carveout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__hybrid_carveout_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: derivative_work_statutory_boundary__hybrid_carveout_reading
 *   human_readable: Derivative Work Boundary: Hybrid Commercial Carveout
 *   domain: intellectual_property_law/information_economics
 *
 * SUMMARY:
 *   This constraint defines the boundary of derivative works in intellectual
 *   property law, specifically through a 'hybrid carveout' reading. Under
 *   this interpretation, non-commercial transformative uses of copyrighted
 *   material are generally permitted without authorization, while commercial
 *   uses require explicit licensing from the copyright holder. This creates a
 *   partial extraction mechanism, where commercial developers face costs, but
 *   non-commercial users are exempt. This story is one reading of the broader
 *   'derivative_work_statutory_boundary' kernel, which is highly contested.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.45).
domain_priors:suppression_score(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.6).
domain_priors:theater_ratio(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__hybrid_carveout_reading, tangled_rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__hybrid_carveout_reading, "Derivative Work Boundary: Hybrid Commercial Carveout").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__hybrid_carveout_reading, "intellectual_property_law/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__hybrid_carveout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__hybrid_carveout_reading, 'c56383ad-cd5a-4f76-bbfe-9539c5074446').
narrative_ontology:cs_kernel_codification('c56383ad-cd5a-4f76-bbfe-9539c5074446', fixed_text).
narrative_ontology:cs_authority_grounding('c56383ad-cd5a-4f76-bbfe-9539c5074446', lineage).
narrative_ontology:cs_interpretation_layer_present('c56383ad-cd5a-4f76-bbfe-9539c5074446').
narrative_ontology:cs_reading_relation('c56383ad-cd5a-4f76-bbfe-9539c5074446', derivative_work_statutory_boundary__enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('c56383ad-cd5a-4f76-bbfe-9539c5074446', derivative_work_statutory_boundary__coordination_reading, coexists_with).
narrative_ontology:cs_axiom('c56383ad-cd5a-4f76-bbfe-9539c5074446', foundational, balancing_creator_incentive_and_public_access).
narrative_ontology:cs_axiom_status(balancing_creator_incentive_and_public_access, holdable).
narrative_ontology:cs_axiom_grounding('c56383ad-cd5a-4f76-bbfe-9539c5074446', balancing_creator_incentive_and_public_access, deontological).
narrative_ontology:cs_axiom('c56383ad-cd5a-4f76-bbfe-9539c5074446', foundational, commercial_use_requires_authorization).
narrative_ontology:cs_axiom_status(commercial_use_requires_authorization, holdable).
narrative_ontology:cs_axiom_grounding('c56383ad-cd5a-4f76-bbfe-9539c5074446', commercial_use_requires_authorization, conventional).
narrative_ontology:cs_reference_frame('c56383ad-cd5a-4f76-bbfe-9539c5074446', balanced_incentive_framework).
narrative_ontology:cs_drift_state('c56383ad-cd5a-4f76-bbfe-9539c5074446', contemporary_digital_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('c56383ad-cd5a-4f76-bbfe-9539c5074446', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_transformative_creators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the exclusive right to authorize derivative works. Under this reading, they can license commercial uses for a fee but cannot prevent non-commercial transformative uses. They benefit from the revenue stream from commercial licenses.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_holders, agenda_setter,
    institutional, generational, mobile, global).

% Seek to create new works for commercial gain that incorporate elements of existing copyrighted material. They must obtain licenses from copyright holders, incurring costs and potential delays. Their exit options are to pay, litigate, or abandon commercialization.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_transformative_creators, payer,
    moderate, biographical, constrained, global).

% Create transformative works (e.g., fan fiction, parody, educational materials) without commercial intent. This reading explicitly permits their use without authorization, making them beneficiaries of the carveout.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_creators, beneficiary,
    powerless, immediate, mobile, global).

% Analyze the economic and social impact of this boundary, debating its fairness, efficiency, and alignment with the original intent of copyright law. They observe the outcomes for all parties.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, legal_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances the incentive for original creation (through commercial licensing) with the public interest in cultural discourse and new non-commercial expression (through carveouts for transformative use).
% TRANSFER_FUNCTION: Transfers licensing fees from commercial creators to copyright holders for derivative works, while permitting non-commercial creators to use copyrighted material without cost.
% ABSENT_VOICES: Creators of AI-generated content, who often operate in a grey area between commercial and non-commercial, and whose 'transformative' nature is highly contested, are not explicitly addressed by this reading and would advocate for clearer rules.
% DISAPPEARANCE_RATIONALE: If this boundary vanished, copyright holders would lose a significant revenue stream from commercial licensing, and non-commercial creators would face increased legal uncertainty or demands for payment, fundamentally altering the landscape of creative production and intellectual property enforcement.
% FOUNDING_PROBLEM: To define the scope of copyright protection for new works based on existing ones, balancing the rights of original creators with the need to foster new creativity, particularly in an era of increasing digital remixing.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and policymakers attest that the problem of balancing creator rights and new expression remains live, especially with evolving technologies. Copyright holders emphasize the need for revenue protection, while non-commercial creators highlight the importance of free expression.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__hybrid_carveout_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__hybrid_carveout_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(derivative_work_statutory_boundary__hybrid_carveout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).
:- end_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because it only applies to commercial uses, allowing a significant sphere of non-commercial creativity to flourish without cost. Suppression (0.6) is present as copyright holders actively enforce their rights against unauthorized commercial exploitation, but it's not absolute due to the non-commercial carveout. The theater ratio is low (0.1) as the enforcement is generally functional, distinguishing between commercial and non-commercial intent. The claimed type is 'tangled_rope' because it genuinely coordinates creative incentives while also enabling asymmetric extraction from commercial actors.
 *
 * PERSPECTIVAL GAP:
 *   Copyright holders perceive this as a fair balance that protects their investment while fostering new creativity. Commercial transformative creators, however, may view the licensing costs as an extractive barrier to innovation, especially if their work is highly transformative. Non-commercial creators see it as a necessary protection for free expression. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders are beneficiaries, collecting licensing fees from commercial uses. Non-commercial creators are also beneficiaries, as their transformative uses are protected. Commercial transformative creators are payers, bearing the costs of licensing. Legal scholars act as observers, analyzing the system's effects.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformative_threshold_ambiguity,
    'What degree of transformation is required for a work to be considered ''transformative'' and thus qualify for the non-commercial carveout?',
    'Further judicial precedent or legislative clarification defining specific criteria for ''transformative use'' in various contexts.',
    'If the threshold is set too high, many non-commercial creators would be reclassified as requiring authorization, increasing extractiveness. If too low, copyright holders'' control over their works would diminish, reducing their revenue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_threshold_ambiguity, conceptual, 'Ambiguity in defining ''transformative'' use for the non-commercial carveout.').

omega_variable(
    commercial_intent_ambiguity,
    'How is ''commercial exploitation'' definitively determined, especially for works that generate indirect revenue (e.g., ad-supported fan content, promotional material for a commercial venture)?',
    'Case law establishing clear tests for commercial intent and indirect monetization, or legislative guidelines for digital platforms.',
    'If the definition of ''commercial'' expands, more creators would fall under the licensing requirement, increasing extraction. If it narrows, more uses would be exempt, reducing extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_intent_ambiguity, empirical, 'Ambiguity in defining ''commercial exploitation'' for derivative works.').

omega_variable(
    reading_enclosure_vs_hybrid,
    'Is this ''hybrid_carveout_reading'' a stable interpretation, or is it under constant pressure to revert to an ''enclosure_reading'' that maximizes copyright holder control?',
    'Analysis of legislative proposals, judicial trends, and lobbying efforts over time. If the trend is consistently towards expanding copyright holder rights, the hybrid reading may be unstable.',
    'If the ''enclosure_reading'' gains dominance, the non-commercial carveout would diminish or disappear, significantly increasing extractiveness and suppression for all transformative creators, reclassifying this constraint towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_enclosure_vs_hybrid, conceptual, 'Stability of the hybrid carveout against pressure for broader copyright enclosure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__hybrid_carveout_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(deri_tr_t5, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(deri_tr_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(deri_tr_t15, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(deri_be_t5, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(deri_be_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(deri_be_t15, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 15, 0.46).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(deri_su_t5, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(deri_su_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(deri_su_t15, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement(deri_su_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__hybrid_carveout_reading, resource_allocation).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_term_duration).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, fair_use_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'derivative_work_statutory_boundary' kernel. It is linked to sibling readings 'enclosure_reading' and 'coordination_reading' which represent alternative interpretations of the same statutory kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
