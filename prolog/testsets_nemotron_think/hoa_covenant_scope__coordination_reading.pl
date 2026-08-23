% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__coordination_reading, []).

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
 *   constraint_id: hoa_covenant_scope__coordination_reading
 *   human_readable: HOA Covenant Infrastructure Coordination Reading
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint story represents the COORDINATION READING of the
 *   hoa_covenant_scope kernel — the interpretation that HOA covenants exist
 *   primarily to solve collective action problems in shared infrastructure
 *   maintenance and to resolve genuine, objectively measurable externalities.
 *   Under this reading, the covenant is a genuine rope: low extraction,
 *   symmetrical benefits across homeowners, narrow enforcement limited to
 *   cost recovery and objective nuisance standards. The kernel is contested:
 *   behavioral_control_reading claims the covenant exists for aesthetic
 *   conformity; extraction_reading claims it exists for revenue generation
 *   and board power. This story instantiates ONLY the coordination reading as
 *   a clean, ε-invariant constraint per Rule 1.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__coordination_reading, 0.15).
domain_priors:suppression_score(hoa_covenant_scope__coordination_reading, 0.2).
domain_priors:theater_ratio(hoa_covenant_scope__coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__coordination_reading, rope).
narrative_ontology:human_readable(hoa_covenant_scope__coordination_reading, "HOA Covenant Infrastructure Coordination Reading").
narrative_ontology:topic_domain(hoa_covenant_scope__coordination_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__coordination_reading, '1bb38921-02c1-4797-8ead-6666441f8c3f').
narrative_ontology:cs_kernel_codification('1bb38921-02c1-4797-8ead-6666441f8c3f', formalized).
narrative_ontology:cs_authority_grounding('1bb38921-02c1-4797-8ead-6666441f8c3f', lineage).
narrative_ontology:cs_interpretation_layer_present('1bb38921-02c1-4797-8ead-6666441f8c3f').
narrative_ontology:cs_reading_relation('1bb38921-02c1-4797-8ead-6666441f8c3f', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('1bb38921-02c1-4797-8ead-6666441f8c3f', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('1bb38921-02c1-4797-8ead-6666441f8c3f', foundational, shared_infrastructure_coordination_is_legitimate_purpose).
narrative_ontology:cs_axiom_status(shared_infrastructure_coordination_is_legitimate_purpose, holdable).
narrative_ontology:cs_axiom_grounding('1bb38921-02c1-4797-8ead-6666441f8c3f', shared_infrastructure_coordination_is_legitimate_purpose, conventional).
narrative_ontology:cs_axiom('1bb38921-02c1-4797-8ead-6666441f8c3f', foundational, enforcement_limited_to_cost_recovery_and_objective_nuisance).
narrative_ontology:cs_axiom_status(enforcement_limited_to_cost_recovery_and_objective_nuisance, holdable).
narrative_ontology:cs_axiom_grounding('1bb38921-02c1-4797-8ead-6666441f8c3f', enforcement_limited_to_cost_recovery_and_objective_nuisance, instrumental).
narrative_ontology:cs_reference_frame('1bb38921-02c1-4797-8ead-6666441f8c3f', infrastructure_coordination_framework).
narrative_ontology:cs_drift_state('1bb38921-02c1-4797-8ead-6666441f8c3f', contemporary_hoa_governance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1bb38921-02c1-4797-8ead-6666441f8c3f', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__coordination_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, all_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, free_riders).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__coordination_reading, collective_action_problem_solution).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__coordination_reading, infrastructure_cost_sharing_principle).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__coordination_reading, objective_nuisance_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Each homeowner pays assessments into a collective pool that funds shared infrastructure maintenance (roads, drainage, common areas). They benefit from maintained infrastructure and resolved externalities (noise, runoff) without bearing the full cost individually. Exit means selling the property — possible but costly and slow.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, all_homeowners, beneficiary,
    organized, biographical, constrained, local).

% Homeowners who refuse to pay assessments or comply with objective nuisance standards. They bear enforcement costs (liens, fines, legal fees) when the HOA pursues collection. Their position is structurally extractive toward the collective — they benefit from infrastructure without contributing — so the constraint extracts from them to sustain the coordination function.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, free_riders, payer,
    powerless, immediate, trapped, local).

% Elected homeowner volunteers who administer the covenant: set assessment levels, contract maintenance, enforce objective nuisance standards. They have no independent revenue — their authority derives entirely from the covenant document and homeowner elections. They can resign (mobile exit) but the institution persists.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, hoa_board, agenda_setter,
    institutional, biographical, mobile, local).

% Non-owner residents bound by covenant rules (parking, noise, aesthetics) through lease terms but with no vote in HOA governance. They experience the constraint's externalities and restrictions without representation. Exit means moving — constrained by lease and market.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, renters, excluded,
    powerless, immediate, constrained, local).

% Local government planners who review subdivision plats with private covenants. They evaluate whether HOA infrastructure coordination substitutes for or supplements municipal services. They observe but do not participate in covenant governance.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, municipal_planners, observer,
    institutional, generational, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates shared infrastructure maintenance (roads, drainage, common areas) and resolves genuine externalities (noise, runoff, structural hazards) through collective cost-sharing and objective, measurable standards.
% TRANSFER_FUNCTION: Moves infrastructure maintenance costs from individual homeowners to the collective pool via periodic assessments, ensuring no single owner bears disproportionate burden for shared systems that benefit all.
% ABSENT_VOICES: Renters and non-owner residents who experience covenant restrictions but have no vote in governance; future homeowners bound by covenants they didn't negotiate; adjacent property owners affected by HOA infrastructure decisions but excluded from the covenant.
% DISAPPEARANCE_RATIONALE: Without the covenant's assessment authority and enforcement mechanism, shared infrastructure would deteriorate or require ad-hoc coordination with higher transaction costs and severe free-rider problems. Municipal services would not automatically absorb the maintenance burden.
% FOUNDING_PROBLEM: New suburban developments required coordinated maintenance of shared infrastructure (streets, stormwater, common areas) that no single homeowner could efficiently maintain alone, and municipal services didn't extend to private developments.
% FOUNDING_PROBLEM_CORROBORATION: Municipal planning records and original developer prospectuses confirm the infrastructure coordination purpose; independent urban planning scholarship documents the collective action problem in private communities; state enabling statutes for HOAs cite infrastructure maintenance as primary purpose.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hoa_covenant_scope__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__coordination_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__coordination_reading_tests).
:- end_tests(hoa_covenant_scope__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because assessments track actual infrastructure costs and are distributed symmetrically. Suppression is low (0.20) because enforcement targets only free-riders on cost-sharing and objective nuisance violations — not aesthetic preferences or behavioral conformity. Theater ratio is minimal (0.10) — the coordination function is real and the enforcement machinery serves it directly. Accessibility collapse is moderate (0.30) — homeowners can sell (exit exists) but moving is costly; the constraint doesn't collapse alternatives entirely. Resistance is low-moderate (0.25) — mostly from free-riders, not from the beneficiary class.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (free_riders) and beneficiary seat (all_homeowners) compute differently: from the free-rider's position the assessment looks like extraction; from the homeowner's position it's fair cost-sharing. The engine computes this divergence from the structural data. The agenda_setter (hoa_board) experiences the constraint as administrative duty, not gain.
 *
 * DIRECTIONALITY LOGIC:
 *   All_homeowners are symmetrical beneficiaries (d ≈ 0.3-0.4) — they pay assessments but receive infrastructure value exceeding individual cost. Free_riders are targets (d ≈ 0.8-0.9) — they extract from the collective by not paying, so the constraint's enforcement extracts from them to restore balance. HOA_board sits near symmetric (d ≈ 0.5) — they administer but don't personally profit; their power is institutional but accountable. Renters are excluded — they bear externalities without voice. Municipal_planners are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (infrastructure coordination in private developments) remains live — municipal services still don't maintain private streets and drainage. The coordination reading prevents mislabeling this as pure extraction because the cost-sharing mechanism solves a genuine collective action problem with symmetrical benefits. Mandatrophy is resolved: the mandate matches the function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is the coordination reading a genuinely distinct constraint from the behavioral_control and extraction readings, or do all three readings describe the same constraint evaluated from different angles?',
    'Test ε-invariance: if measuring the covenant''s operation via infrastructure cost recovery yields ε≈0.15 but measuring via fine revenue yields ε≈0.60, they are different constraints. Empirical audit of HOA financials across jurisdictions.',
    'If ε differs by measurement basis, the kernel decomposes into multiple constraint stories (per DP-001). This story would represent only the coordination constraint; the others would be separate stories linked via network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Whether the kernel label ''HOA covenant'' covers one constraint or multiple structurally distinct constraints.').

omega_variable(
    coordination_vs_extraction_boundary,
    'In practice, can the infrastructure coordination function be separated from the extraction function, or does the covenant''s enforcement machinery inevitably serve both?',
    'Compare HOAs with identical infrastructure but different fine structures: if fine revenue correlates with board discretion rather than infrastructure need, extraction is structurally coupled to coordination.',
    'If inseparable, this coordination reading describes an ideal type not found in pure form; the real constraint would be tangled_rope. If separable, this reading identifies a genuine rope component within a constraint family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether coordination and extraction are structurally separable in HOA governance.').

omega_variable(
    enforcement_scope_creep,
    'Does narrow enforcement (cost recovery + objective nuisance) structurally tend to expand into aesthetic/behavioral enforcement over time?',
    'Longitudinal study of HOA covenant amendments and enforcement records: track whether objective standards proliferate into subjective ones.',
    'If scope creep is structurally inevitable, the coordination reading describes a transient phase, not a stable constraint type. The constraint would drift toward behavioral_control_reading over its interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_scope_creep, empirical, 'Whether the coordination reading''s narrow enforcement scope is stable or inherently unstable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__coordination_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa_covenant_coord_tr_t0, hoa_covenant_scope__coordination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hoa_covenant_coord_tr_t10, hoa_covenant_scope__coordination_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(hoa_covenant_coord_tr_t20, hoa_covenant_scope__coordination_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(hoa_covenant_coord_tr_t30, hoa_covenant_scope__coordination_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement(hoa_covenant_coord_tr_t40, hoa_covenant_scope__coordination_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(hoa_covenant_coord_tr_t50, hoa_covenant_scope__coordination_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(hoa_covenant_coord_be_t0, hoa_covenant_scope__coordination_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(hoa_covenant_coord_be_t10, hoa_covenant_scope__coordination_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(hoa_covenant_coord_be_t20, hoa_covenant_scope__coordination_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(hoa_covenant_coord_be_t30, hoa_covenant_scope__coordination_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement(hoa_covenant_coord_be_t40, hoa_covenant_scope__coordination_reading, base_extractiveness, 40, 0.16).
narrative_ontology:measurement(hoa_covenant_coord_be_t50, hoa_covenant_scope__coordination_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hoa_covenant_coord_su_t0, hoa_covenant_scope__coordination_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(hoa_covenant_coord_su_t10, hoa_covenant_scope__coordination_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(hoa_covenant_coord_su_t20, hoa_covenant_scope__coordination_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(hoa_covenant_coord_su_t30, hoa_covenant_scope__coordination_reading, suppression_requirement, 30, 0.21).
narrative_ontology:measurement(hoa_covenant_coord_su_t40, hoa_covenant_scope__coordination_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(hoa_covenant_coord_su_t50, hoa_covenant_scope__coordination_reading, suppression_requirement, 50, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(hoa_covenant_scope__coordination_reading, 0.15).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, municipal_infrastructure_coordination).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, private_government_service_substitution).

% DUAL FORMULATION NOTE:
% Part of hoa_covenant_scope kernel family. This coordination_reading (rope, ε≈0.15) coexists with behavioral_control_reading (tangled_rope/snare, higher ε) and extraction_reading (snare, highest ε). All three share the same formal kernel (the covenant document) but instantiate different constraints with different beneficiary/victim structures and ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
