% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__coordination_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: derivative_work_statutory_boundary__coordination_reading
 *   human_readable: Derivative Work Statutory Boundary (Coordination Reading)
 *   domain: intellectual_property/technology_governance/information_economics
 *
 * SUMMARY:
 *   This constraint represents a 'coordination reading' of the derivative
 *   work statutory boundary in intellectual property law. It posits that only
 *   fixed recastings substantially incorporating original expression are
 *   derivative works, explicitly allowing transformative and intermediate
 *   uses (like AI model training) without them being considered infringing.
 *   This reading aims to foster innovation and broad access to information,
 *   treating the boundary as a coordination mechanism rather than an
 *   extraction opportunity for original copyright holders. It is one
 *   interpretation of the broader 'derivative_work_statutory_boundary'
 *   kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__coordination_reading, 0.15).
domain_priors:suppression_score(derivative_work_statutory_boundary__coordination_reading, 0.2).
domain_priors:theater_ratio(derivative_work_statutory_boundary__coordination_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__coordination_reading, rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__coordination_reading, "Derivative Work Statutory Boundary (Coordination Reading)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__coordination_reading, "intellectual_property/technology_governance/information_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__coordination_reading, 'b0fec02f-eafc-4372-93e2-f9ae0d453e97').
narrative_ontology:cs_kernel_codification('b0fec02f-eafc-4372-93e2-f9ae0d453e97', fixed_text).
narrative_ontology:cs_authority_grounding('b0fec02f-eafc-4372-93e2-f9ae0d453e97', lineage).
narrative_ontology:cs_interpretation_layer_present('b0fec02f-eafc-4372-93e2-f9ae0d453e97').
narrative_ontology:cs_reading_relation('b0fec02f-eafc-4372-93e2-f9ae0d453e97', derivative_work_statutory_boundary__enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0fec02f-eafc-4372-93e2-f9ae0d453e97', derivative_work_statutory_boundary__hybrid_carveout_reading, coexists_with).
narrative_ontology:cs_axiom('b0fec02f-eafc-4372-93e2-f9ae0d453e97', foundational, progress_of_science_and_arts_priority).
narrative_ontology:cs_axiom_status(progress_of_science_and_arts_priority, holdable).
narrative_ontology:cs_axiom_grounding('b0fec02f-eafc-4372-93e2-f9ae0d453e97', progress_of_science_and_arts_priority, deontological).
narrative_ontology:cs_axiom('b0fec02f-eafc-4372-93e2-f9ae0d453e97', foundational, intermediate_use_non_infringing).
narrative_ontology:cs_axiom_status(intermediate_use_non_infringing, holdable).
narrative_ontology:cs_axiom_grounding('b0fec02f-eafc-4372-93e2-f9ae0d453e97', intermediate_use_non_infringing, conventional).
narrative_ontology:cs_reference_frame('b0fec02f-eafc-4372-93e2-f9ae0d453e97', fair_use_as_innovation_catalyst).
narrative_ontology:cs_drift_state('b0fec02f-eafc-4372-93e2-f9ae0d453e97', contemporary_generative_ai_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b0fec02f-eafc-4372-93e2-f9ae0d453e97', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, transformative_creators).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, ai_developers).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, public_domain_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, original_copyright_holders).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, fair_use_doctrine).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, progress_of_science_and_arts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These creators build new works that comment on, critique, or significantly alter existing copyrighted material without directly supplanting the original. They benefit from clear legal pathways for their work without needing prior authorization or licensing for transformative uses.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, transformative_creators, beneficiary,
    moderate, biographical, mobile, global).

% Developers of generative AI models rely on access to vast datasets of existing works for training. This reading allows them to use copyrighted material for intermediate, non-expressive purposes (like model training) without it being considered a derivative work requiring licensing.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, ai_developers, beneficiary,
    organized, generational, mobile, global).

% Holders of original copyrights, such as authors, artists, and publishers, see their works used in new contexts without direct compensation or permission. While they retain rights over direct copies and adaptations, this reading limits their control over transformative uses.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, original_copyright_holders, payer,
    powerful, generational, constrained, global).

% The general public and future creators benefit from a richer cultural commons where ideas and expressions can be freely built upon, fostering innovation and access to knowledge without unnecessary gatekeeping.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, public_domain_users, beneficiary,
    powerless, civilizational, arbitrage, universal).

% Organizations that facilitate licensing for copyrighted works would prefer a broader definition of derivative works, as it would expand the scope of activities requiring licenses, increasing their revenue and relevance. This reading limits their market.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, licensing_agencies, excluded,
    organized, biographical, trapped, global).

% These institutions interpret and refine the statutory boundary, balancing the rights of creators with the public interest in innovation and access. Their decisions shape the practical application of this constraint.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, courts_and_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the boundary between original expression and subsequent creative or technological uses, enabling new forms of creativity and innovation (especially generative AI) by clarifying that transformative and intermediate uses are not infringing derivative works.
% TRANSFER_FUNCTION: Facilitates the free flow of information and creative input for transformative purposes, implicitly transferring potential licensing fees from original copyright holders to transformative creators and AI developers, in exchange for a more dynamic and innovative cultural landscape.
% ABSENT_VOICES: Licensing agencies and some copyright maximalists would argue for a broader definition of derivative works, asserting greater control and compensation for any use of their material. Their voices are often marginalized in discussions prioritizing innovation and public access.
% DISAPPEARANCE_RATIONALE: If this boundary vanished, the legal landscape for generative AI and transformative art would become highly uncertain, leading to widespread litigation, chilling effects on innovation, and a significant restructuring of how new technologies interact with existing creative works. The flow of information for training models would cease, and many forms of creative expression would require prohibitive ex-ante licensing.
% FOUNDING_PROBLEM: The original copyright statutes aimed to balance incentives for creators with public access and the promotion of science and arts, but new technologies (like digital sampling and AI) created ambiguity about what constitutes an infringing 'derivative work' versus a legitimate transformative use.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, technology ethicists, and open-source advocates corroborate that the problem of balancing creator rights with technological progress remains live and critical for the future of innovation. This is attested by ongoing legislative debates and court cases globally, from outside the immediate beneficiaries of this reading.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(derivative_work_statutory_boundary__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__coordination_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__coordination_reading_tests).
:- end_tests(derivative_work_statutory_boundary__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) reflects that this reading minimizes the need for ex-ante licensing for many new uses, reducing costs for transformative creators and AI developers. Suppression (0.2) is low because it aims to reduce legal barriers rather than enforce them. Theater ratio (0.05) is minimal as the constraint's function is direct and clear. The metrics are stable, reflecting a consistent application of this interpretation over time.
 *
 * PERSPECTIVAL GAP:
 *   Original copyright holders, particularly those with business models heavily reliant on licensing every use, would experience this constraint as more extractive than the beneficiaries. They would argue it diminishes their property rights. However, from the perspective of innovation and public benefit, this reading is seen as a necessary coordination to prevent over-enclosure.
 *
 * DIRECTIONALITY LOGIC:
 *   Transformative creators and AI developers are clear beneficiaries, gaining freedom to operate. Original copyright holders are positioned as payers, as they forgo potential licensing revenue from these uses. Public domain users are also beneficiaries, as the cultural commons is enriched. Licensing agencies are excluded, as their business model is curtailed by this interpretation. Courts and legislatures act as agenda-setters, shaping and enforcing this boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively prevents mandatrophy by adapting the derivative work concept to new technological realities (like AI). It ensures the constraint's function remains relevant to fostering progress, rather than becoming an inertial mechanism for rent-seeking based on outdated definitions. It resolves the tension by prioritizing the 'progress of science and arts' over maximal control by original creators.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''coordination reading'' of the derivative work boundary, or is it merely a strategic framing to justify specific technological uses?',
    'Analysis of judicial decisions and legislative intent over time: if the interpretation consistently prioritizes innovation and public access over specific industry interests, it supports the coordination reading. If it primarily benefits a narrow set of powerful tech actors, it suggests a strategic framing.',
    'If it''s a strategic framing, the true extractiveness and suppression might be higher, reclassifying it towards a Tangled Rope or Snare, as it would be serving specific beneficiaries under the guise of general coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity between genuine coordination and strategic framing for specific interests.').

omega_variable(
    transformative_threshold_clarity,
    'How clear and consistently applied is the ''transformative use'' threshold in practice, particularly for complex AI-generated outputs?',
    'Empirical study of court rulings and licensing practices: a high degree of consistency and predictability indicates clarity; frequent, unpredictable litigation suggests ongoing ambiguity.',
    'If the threshold is unclear, the effective suppression for creators and AI developers increases due to legal uncertainty, even if the stated intent is coordination. This could push the constraint towards a Tangled Rope due to hidden costs and enforcement risks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_threshold_clarity, empirical, 'Clarity and consistency of the ''transformative use'' legal standard.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__coordination_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(deri_tr_t5, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 5, 0.04).
narrative_ontology:measurement(deri_tr_t10, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(deri_tr_t15, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 15, 0.06).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(deri_be_t5, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 5, 0.14).
narrative_ontology:measurement(deri_be_t10, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(deri_be_t15, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 15, 0.16).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(deri_su_t5, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 5, 0.18).
narrative_ontology:measurement(deri_su_t10, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(deri_su_t15, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 15, 0.22).
narrative_ontology:measurement(deri_su_t20, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__coordination_reading, information_standard).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, ai_model_training_data_access).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, generative_ai_liability_framework).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings (coordination, enclosure, hybrid_carveout) of the 'derivative_work_statutory_boundary' kernel. Each reading presents a distinct structural claim about the boundary's function and impact, with differing ε values and stakeholder dynamics. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
