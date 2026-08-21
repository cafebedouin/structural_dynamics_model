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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   domain: intellectual_property_law/technology_governance/information_economics
 *
 * SUMMARY:
 *   This constraint represents the 'hybrid carveout' reading of the
 *   derivative work statutory boundary, where non-commercial transformative
 *   uses are generally permitted, but commercial uses require authorization.
 *   It's a Tangled Rope because it genuinely coordinates the interests of
 *   original creators and non-commercial transformers, but simultaneously
 *   extracts from commercial transformers through licensing requirements. The
 *   metrics reflect this partial extraction and the active enforcement needed
 *   to maintain the commercial/non-commercial distinction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.62).
domain_priors:suppression_score(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.7).
domain_priors:theater_ratio(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__hybrid_carveout_reading, tangled_rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__hybrid_carveout_reading, "Derivative Work Boundary: Hybrid Commercial Carveout").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__hybrid_carveout_reading, "intellectual_property_law/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__hybrid_carveout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__hybrid_carveout_reading, 'e0e1f31e-ef1e-4f7b-a674-13c5b96a240c').
narrative_ontology:cs_kernel_codification('e0e1f31e-ef1e-4f7b-a674-13c5b96a240c', formalized).
narrative_ontology:cs_authority_grounding('e0e1f31e-ef1e-4f7b-a674-13c5b96a240c', lineage).
narrative_ontology:cs_interpretation_layer_present('e0e1f31e-ef1e-4f7b-a674-13c5b96a240c').
narrative_ontology:cs_reading_relation('e0e1f31e-ef1e-4f7b-a674-13c5b96a240c', derivative_work_statutory_boundary__enclosure_reading, influences).
narrative_ontology:cs_reading_relation('e0e1f31e-ef1e-4f7b-a674-13c5b96a240c', derivative_work_statutory_boundary__coordination_reading, coexists_with).
narrative_ontology:cs_axiom('e0e1f31e-ef1e-4f7b-a674-13c5b96a240c', foundational, commercial_exploitation_triggers_licensing).
narrative_ontology:cs_axiom_status(commercial_exploitation_triggers_licensing, holdable).
narrative_ontology:cs_axiom_grounding('e0e1f31e-ef1e-4f7b-a674-13c5b96a240c', commercial_exploitation_triggers_licensing, conventional).
narrative_ontology:cs_axiom('e0e1f31e-ef1e-4f7b-a674-13c5b96a240c', foundational, non_commercial_transformative_use_is_permitted).
narrative_ontology:cs_axiom_status(non_commercial_transformative_use_is_permitted, holdable).
narrative_ontology:cs_axiom_grounding('e0e1f31e-ef1e-4f7b-a674-13c5b96a240c', non_commercial_transformative_use_is_permitted, conventional).
narrative_ontology:cs_reference_frame('e0e1f31e-ef1e-4f7b-a674-13c5b96a240c', balanced_incentive_access_framework).
narrative_ontology:cs_drift_state('e0e1f31e-ef1e-4f7b-a674-13c5b96a240c', digital_transformation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e0e1f31e-ef1e-4f7b-a674-13c5b96a240c', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_transformative_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_transformative_creators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, digital_platforms).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__hybrid_carveout_reading, incentive_to_create_doctrine).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__hybrid_carveout_reading, fair_use_balancing_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the original copyrights and benefit from the ability to license commercial derivative uses, providing an incentive for their creative output. They actively enforce their rights against unauthorized commercial exploitation.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_holders, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_holders, beneficiary).

% Seek to create new works that transform existing copyrighted material for commercial purposes. They face the cost and complexity of obtaining licenses, which can stifle innovation or lead to legal disputes. Their options are to pay, litigate, or abandon commercialization.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_transformative_creators, payer,
    organized, biographical, constrained, global).

% Create new works that transform existing copyrighted material for non-commercial purposes (e.g., fan fiction, educational remixes). They benefit from the carveout that permits their use without requiring authorization, fostering cultural commentary and artistic expression.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_transformative_creators, beneficiary,
    moderate, biographical, mobile, global).

% Host and distribute user-generated content, including transformative works. They bear the costs of content moderation, copyright enforcement (e.g., takedown notices), and potential liability for commercial infringement by their users, acting as an intermediary enforcer.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, digital_platforms, payer,
    institutional, biographical, constrained, global).

% Interpret and apply the statutory boundary, shaping its evolution through case law and commentary. They observe the practical effects of the hybrid carveout and influence its future application, balancing competing interests.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, legal_scholars_and_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, legal_scholars_and_courts, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_holders).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__hybrid_carveout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To balance the incentive for original creators to produce new works with the public's interest in building upon and transforming existing culture, specifically by differentiating between commercial and non-commercial uses.
% TRANSFER_FUNCTION: Moves licensing fees and control over commercial exploitation from commercial transformative creators to original copyright holders, while granting free use to non-commercial transformative creators.
% ABSENT_VOICES: Creators whose commercial transformative works are deemed non-viable due to prohibitive licensing costs or the risk of legal action. They would argue for a broader definition of 'transformative' that extends to commercial uses, reducing the need for authorization.
% DISAPPEARANCE_RATIONALE: If this boundary vanished, the incentives for original creation would be severely undermined, leading to less new content. Simultaneously, the landscape for transformative works would become either entirely free-for-all (if all uses were permitted) or entirely locked down (if all uses required permission), fundamentally altering cultural production and information economics.
% FOUNDING_PROBLEM: To address the tension between protecting creators' economic rights and fostering subsequent creativity, particularly as new technologies made copying and transformation easier, requiring a legal framework to define the scope of 'derivative work'.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, industry bodies, and courts consistently attest that balancing creator incentives with public access and transformative use remains a live and evolving problem, especially with rapid technological change. This is corroborated by ongoing legislative debates and landmark court cases from outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__hybrid_carveout_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__hybrid_carveout_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(derivative_work_statutory_boundary__hybrid_carveout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.62, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate because while commercial users pay, non-commercial users are exempt, dampening the overall extraction. Suppression (0.70) is substantial due to the active legal enforcement required by copyright holders and digital platforms to police the commercial/non-commercial boundary. Theater ratio (0.20) is low, as the system is largely functional, though some enforcement actions may be performative to deter potential infringers. Accessibility collapse (0.50) is moderate: alternatives are open for non-commercial use but constrained for commercial. Resistance (0.55) is moderate, primarily from commercial creators advocating for broader fair use.
 *
 * PERSPECTIVAL GAP:
 *   Copyright holders perceive this as a necessary balance that protects their incentives. Non-commercial creators see it as a beneficial carveout. Commercial creators, however, experience it as an extractive barrier to innovation. The engine's per-seat classification will reflect these divergent experiences based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders are clear beneficiaries (low d) as they control and profit from commercial licensing. Non-commercial transformative creators are also beneficiaries (low d) due to their exemption. Commercial transformative creators are targets (high d) as they bear the costs of licensing. Digital platforms are payers (moderate d) due to their enforcement costs and liability. Legal scholars and courts are observers (analytical d).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately representing the ''hybrid_carveout_reading'' of the derivative work boundary, or does it lean too heavily towards either enclosure or coordination?',
    'Comparative legal analysis against landmark court decisions and legislative history explicitly adopting a commercial/non-commercial distinction for transformative use.',
    'If the reading is misaligned, the classification could shift towards a more extractive ''enclosure_reading'' (higher ε, Snare) or a more permissive ''coordination_reading'' (lower ε, Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Verifies the fidelity of this constraint story to its declared kernel reading.').

omega_variable(
    transformative_commercial_ambiguity,
    'How clear is the line between ''transformative'' and ''derivative'' when the use is commercial, and how much does this ambiguity contribute to extraction?',
    'Empirical study of licensing negotiations and litigation outcomes for commercial transformative works, quantifying the ''transaction cost of ambiguity''.',
    'If the line is highly ambiguous, the effective extraction and suppression for commercial creators are higher than measured, as they face greater uncertainty and legal risk, potentially pushing the constraint closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_commercial_ambiguity, empirical, 'Assesses the clarity and impact of the ''transformative'' vs. ''derivative'' distinction in commercial contexts.').

omega_variable(
    balance_of_incentive_and_access,
    'Does the current hybrid carveout optimally balance creator incentives with public access and transformative innovation, or is it skewed?',
    'Economic analysis comparing rates of original content creation and transformative work production under different legal regimes (e.g., jurisdictions with different derivative work standards).',
    'If skewed towards incentives, the extraction from commercial creators is disproportionately high, potentially stifling innovation. If skewed towards access, original creators may lack sufficient incentive, reducing the pool of works to transform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(balance_of_incentive_and_access, preference, 'Evaluates the normative balance achieved by the hybrid derivative work boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__hybrid_carveout_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(deri_tr_t5, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement(deri_tr_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(deri_tr_t15, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(deri_tr_t25, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 25, 0.2).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(deri_be_t5, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(deri_be_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(deri_be_t15, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(deri_be_t25, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(deri_su_t5, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(deri_su_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(deri_su_t15, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(deri_su_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(deri_su_t25, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 25, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__hybrid_carveout_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'derivative_work_statutory_boundary' kernel, each with different ε values and structural properties. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
