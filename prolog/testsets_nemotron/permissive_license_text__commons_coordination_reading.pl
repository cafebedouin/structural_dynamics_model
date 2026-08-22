% ============================================================================
% CONSTRAINT STORY: permissive_license_text__commons_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__commons_coordination_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: permissive_license_text__commons_coordination_reading
 *   human_readable: Permissive License Text — Commons Coordination Reading
 *   domain: technological/legal
 *
 * SUMMARY:
 *   This constraint story captures the 'commons coordination' reading of
 *   permissive license texts (BSD, MIT, Apache-2.0 style). The license text
 *   functions as a universal coordination artifact: it minimizes legal
 *   friction for any implementer anywhere, creating a global commons where
 *   code flows without permission structures. The claimed type is rope —
 *   genuine coordination with negligible extraction. The beneficiary set is
 *   the universal implementer pool; there is no victim set in this reading.
 *   The corporate_moat_reading and copyleft_counterfactual_reading are
 *   sibling constraints (different readings of the same kernel) that identify
 *   extraction and victimization this reading does not see. Those are
 *   separate constraint stories linked via network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__commons_coordination_reading, 0.08).
domain_priors:suppression_score(permissive_license_text__commons_coordination_reading, 0.05).
domain_priors:theater_ratio(permissive_license_text__commons_coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__commons_coordination_reading, rope).
narrative_ontology:human_readable(permissive_license_text__commons_coordination_reading, "Permissive License Text — Commons Coordination Reading").
narrative_ontology:topic_domain(permissive_license_text__commons_coordination_reading, "technological/legal").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__commons_coordination_reading, '9c3dbcd8-e9b1-4e1d-95cb-94a8e7889573').
narrative_ontology:cs_kernel_codification('9c3dbcd8-e9b1-4e1d-95cb-94a8e7889573', fixed_text).
narrative_ontology:cs_authority_grounding('9c3dbcd8-e9b1-4e1d-95cb-94a8e7889573', practice).
narrative_ontology:cs_interpretation_layer_present('9c3dbcd8-e9b1-4e1d-95cb-94a8e7889573').
narrative_ontology:cs_reading_relation('9c3dbcd8-e9b1-4e1d-95cb-94a8e7889573', permissive_license_text__corporate_moat_reading, coexists_with).
narrative_ontology:cs_reading_relation('9c3dbcd8-e9b1-4e1d-95cb-94a8e7889573', permissive_license_text__copyleft_counterfactual_reading, coexists_with).
narrative_ontology:cs_axiom('9c3dbcd8-e9b1-4e1d-95cb-94a8e7889573', foundational, legal_friction_minimization_is_coordination_primitive).
narrative_ontology:cs_axiom_status(legal_friction_minimization_is_coordination_primitive, holdable).
narrative_ontology:cs_axiom_grounding('9c3dbcd8-e9b1-4e1d-95cb-94a8e7889573', legal_friction_minimization_is_coordination_primitive, instrumental).
narrative_ontology:cs_axiom('9c3dbcd8-e9b1-4e1d-95cb-94a8e7889573', foundational, universal_implementer_pool_has_no_reciprocity_obligation).
narrative_ontology:cs_axiom_status(universal_implementer_pool_has_no_reciprocity_obligation, holdable).
narrative_ontology:cs_axiom_grounding('9c3dbcd8-e9b1-4e1d-95cb-94a8e7889573', universal_implementer_pool_has_no_reciprocity_obligation, conventional).
narrative_ontology:cs_reference_frame('9c3dbcd8-e9b1-4e1d-95cb-94a8e7889573', permissive_license_as_universal_coordination_artifact).
narrative_ontology:cs_drift_state('9c3dbcd8-e9b1-4e1d-95cb-94a8e7889573', contemporary_cloud_ai_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9c3dbcd8-e9b1-4e1d-95cb-94a8e7889573', '').
narrative_ontology:cs_kernel_id(permissive_license_text__commons_coordination_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, universal_implementer_pool).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, downstream_integrators).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, ecosystem_maintainers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, proprietary_derivative_builders).
narrative_ontology:constraint_vindicates(permissive_license_text__commons_coordination_reading, copyright_minimization_enables_coordination).
narrative_ontology:constraint_vindicates(permissive_license_text__commons_coordination_reading, legal_friction_reduction_is_coordination_function).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All developers, users, and organizations who can freely implement, use, modify, and distribute the licensed work without permission-seeking or royalty obligations. The license text itself is the coordination artifact — no gatekeeper mediates access.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, universal_implementer_pool, beneficiary,
    organized, generational, arbitrage, global).

% Companies and projects that build commercial or non-commercial products incorporating the licensed code. They capture the coordination benefit directly — no licensing negotiation, no compliance overhead, no audit risk from the license terms themselves.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, downstream_integrators, beneficiary,
    powerful, biographical, mobile, global).

% The project maintainers and foundation stewards who curate the license text, manage the trademark (if any), and set contribution norms. They administer the commons but do not extract from it — their authority derives from technical stewardship, not licensing revenue.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, ecosystem_maintainers, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__commons_coordination_reading, ecosystem_maintainers, beneficiary).

% Firms that incorporate permissively-licensed code into proprietary products without contributing changes upstream. They are structural beneficiaries of this reading — the license permits this use without reciprocity requirement. Their gain is the avoided cost of developing equivalent functionality or negotiating licenses.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, proprietary_derivative_builders, beneficiary,
    powerful, biographical, arbitrage, global).

% Advocates and projects that prefer reciprocal licensing (GPL family). They would object to the permissive reading's lack of reciprocity guarantee, arguing it enables the corporate moat dynamic. Their exclusion is structural: the license text does not encode their preferred coordination mechanism.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, copyleft_advocates, excluded,
    organized, generational, analytical, global).

% Analysts who study license ecosystems, compliance patterns, and the empirical effects of permissive vs. reciprocal licensing on innovation, concentration, and commons sustainability. They experience the constraint as an object of study.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, legal_scholars_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal, frictionless legal framework for code reuse and integration — anyone can use the code for any purpose without negotiating terms, seeking permission, or auditing compliance. The coordination problem solved is the transaction cost of permission-seeking across a global, heterogeneous implementer pool.
% TRANSFER_FUNCTION: Moves the legal right to use, modify, and distribute from the default (copyright holder's exclusive control) to the universal implementer pool at zero marginal cost per implementer. No value flows to the licensor per use; the transfer is the removal of the exclusion right.
% ABSENT_VOICES: Copyleft advocates who would encode reciprocity as a condition of use; downstream users who might benefit from reciprocal contributions but have no voice in the license choice; communities in jurisdictions where copyright formalities or moral rights create friction not addressed by the license text.
% DISAPPEARANCE_RATIONALE: If permissive license texts vanished, the default copyright regime would reassert exclusive control — every implementation would require individual licensing negotiation or fair-use defense, dramatically raising transaction costs and fragmenting the global implementer pool. The commons coordination function would collapse.
% FOUNDING_PROBLEM: Early open source and free software needed a legal instrument that allowed unrestricted reuse without the viral reciprocity of the GPL, enabling integration into proprietary and heterogeneously-licensed codebases while still providing a clear, standard license text.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (need for non-viral reuse license) is attested by the original BSD and MIT license authors and early adopters (e.g., UC Berkeley CSRG, X Consortium). The contested status is corroborated by copyleft advocates (FSF, GPL proponents) who argue the problem was misdiagnosed — that the real need was protecting the commons from enclosure, which permissive licenses fail to do. Empirical studies on license adoption patterns (e.g., Vendome et al., 2017; German et al., 2020) document the shift toward permissive licensing in commercial ecosystems, supporting the corporate moat reading's claim that the founding problem's solution enabled extraction.
narrative_ontology:disappearance_verdict(permissive_license_text__commons_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__commons_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__commons_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(permissive_license_text__commons_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__commons_coordination_reading, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__commons_coordination_reading_tests).
:- end_tests(permissive_license_text__commons_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.08) because the license imposes no per-use cost, no royalty, no compliance audit burden — the only 'cost' is attribution preservation, which is negligible. Suppression is minimal (0.05) because alternatives (proprietary licenses, copyleft licenses, public domain dedication) are not suppressed; the license text coexists with them. Theater is low (0.10) — the license text performs its stated function (enabling frictionless reuse) with minimal ceremonial overhead. Accessibility collapse is low (0.15) because implementers can always choose other licenses or public domain; the constraint does not collapse alternatives. Resistance is low (0.20) — the main resistance comes from copyleft advocates who prefer a different coordination mechanism, not from those constrained by this one.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute per-seat types from structural data. For this reading, all beneficiary seats should compute as rope or mountain (negligible extraction). The excluded and observer seats are commentary-grade only. The divergence from sibling readings (corporate_moat_reading, copyleft_counterfactual_reading) is not a seat divergence within this story — it is a constraint-family divergence captured by the omega variables and network links.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared beneficiaries (universal_implementer_pool, downstream_integrators, ecosystem_maintainers, proprietary_derivative_builders) sit at the beneficiary end of directionality (d near 0.0) — the constraint subsidizes their activity by removing legal friction. The excluded seat (copyleft_advocates) is not a target of extraction; their objection is that the constraint fails to encode a reciprocity mechanism they prefer. The observer seat (legal_scholars_analysts) is analytical (d=0.5 by definition). No stakeholder bears net extraction under this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for non-viral reuse license) is contested: the original problem may be solved (universal frictionless reuse achieved), but the arrangement persists and has expanded. Whether this constitutes mandatrophy depends on whether the coordination function remains live or has been captured by the corporate moat dynamic. This reading holds the function is live; the sibling readings disagree. The engine's mandatrophy detection will weigh founding_problem_status=contested against disappearance_verdict=world_rearranges.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is the permissive license text a single constraint with contested interpretation, or are the commons_coordination, corporate_moat, and copyleft_counterfactual readings structurally distinct constraints?',
    'Apply the epsilon-invariance test: if measuring extraction from the proprietary_derivative_builders'' seat yields substantially higher ε than from the universal_implementer_pool seat, the label ''permissive license'' covers multiple constraints. Decompose into separate stories per reading (as done here).',
    'If the readings are one constraint, ε becomes observer-relative and the framework''s ε-invariance principle is violated. Separate stories with distinct ε values (this story: ε≈0.08; corporate_moat_reading: ε≈0.4-0.6) preserve invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Whether the kernel label conflates structurally distinct constraints').

omega_variable(
    proprietary_builder_beneficiary_status,
    'Are proprietary_derivative_builders genuine beneficiaries of the coordination function, or are they extractors exploiting the commons?',
    'Empirical analysis of contribution flows: do proprietary derivatives contribute patches upstream at rates comparable to non-proprietary downstreams? If contribution rates are near zero while capture value is high, the beneficiary classification in this reading may mask a structural extraction captured by the corporate_moat_reading.',
    'If proprietary builders are net extractors, this reading''s ''no victim set'' claim is false from the commons-sustainability perspective — the victim is the commons itself (ecosystem_maintainers, future implementers). The corporate_moat_reading captures this; this reading does not.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proprietary_builder_beneficiary_status, empirical, 'Whether the proprietary derivative builder seat is beneficiary or extractor').

omega_variable(
    attribution_preservation_as_extraction,
    'Does the attribution preservation requirement (present in BSD/MIT/Apache) constitute a non-zero extraction floor that should raise ε above the Boltzmann floor for information_standard coordination?',
    'Measure compliance cost of attribution preservation across jurisdictions and project scales. If the cost is systematically non-negligible for small implementers, the coordination type''s floor may understate true coordination cost.',
    'If attribution cost is material, ε should be higher and the rope classification may need re-examination. The current ε=0.08 treats attribution as negligible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(attribution_preservation_as_extraction, empirical, 'Whether minimal license conditions constitute extractive overhead').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__commons_coordination_reading, 1988, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pl_ccr_tr_t1988, permissive_license_text__commons_coordination_reading, theater_ratio, 1988, 0.05).
narrative_ontology:measurement(pl_ccr_tr_t1998, permissive_license_text__commons_coordination_reading, theater_ratio, 1998, 0.07).
narrative_ontology:measurement(pl_ccr_tr_t2008, permissive_license_text__commons_coordination_reading, theater_ratio, 2008, 0.08).
narrative_ontology:measurement(pl_ccr_tr_t2018, permissive_license_text__commons_coordination_reading, theater_ratio, 2018, 0.09).
narrative_ontology:measurement(pl_ccr_tr_t2025, permissive_license_text__commons_coordination_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(pl_ccr_be_t1988, permissive_license_text__commons_coordination_reading, base_extractiveness, 1988, 0.05).
narrative_ontology:measurement(pl_ccr_be_t1998, permissive_license_text__commons_coordination_reading, base_extractiveness, 1998, 0.06).
narrative_ontology:measurement(pl_ccr_be_t2008, permissive_license_text__commons_coordination_reading, base_extractiveness, 2008, 0.07).
narrative_ontology:measurement(pl_ccr_be_t2018, permissive_license_text__commons_coordination_reading, base_extractiveness, 2018, 0.08).
narrative_ontology:measurement(pl_ccr_be_t2025, permissive_license_text__commons_coordination_reading, base_extractiveness, 2025, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(pl_ccr_su_t1988, permissive_license_text__commons_coordination_reading, suppression_requirement, 1988, 0.02).
narrative_ontology:measurement(pl_ccr_su_t1998, permissive_license_text__commons_coordination_reading, suppression_requirement, 1998, 0.03).
narrative_ontology:measurement(pl_ccr_su_t2008, permissive_license_text__commons_coordination_reading, suppression_requirement, 2008, 0.04).
narrative_ontology:measurement(pl_ccr_su_t2018, permissive_license_text__commons_coordination_reading, suppression_requirement, 2018, 0.05).
narrative_ontology:measurement(pl_ccr_su_t2025, permissive_license_text__commons_coordination_reading, suppression_requirement, 2025, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__commons_coordination_reading, information_standard).
narrative_ontology:boltzmann_floor_override(permissive_license_text__commons_coordination_reading, 0.02).
narrative_ontology:affects_constraint(permissive_license_text__commons_coordination_reading, permissive_license_text__corporate_moat_reading).
narrative_ontology:affects_constraint(permissive_license_text__commons_coordination_reading, permissive_license_text__copyleft_counterfactual_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the permissive_license_text kernel into three readings with distinct ε values and beneficiary/victim structures. The commons_coordination_reading (this story) sees ε≈0.08, universal beneficiaries, no victims. The corporate_moat_reading sees ε≈0.5+, proprietary_derivative_builders as concentrated beneficiaries, ecosystem_maintainers and universal_implementer_pool as victims of commons enclosure. The copyleft_counterfactual_reading sees the permissive text as a constraint that fails to encode reciprocity, making the victim set the commons itself. All three stories link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
