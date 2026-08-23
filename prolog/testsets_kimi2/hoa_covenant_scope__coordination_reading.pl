% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: HOA Covenant â Shared Infrastructure Coordination Reading
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint story captures the coordination_reading of the
 *   hoa_covenant_scope kernel: the view that a homeowners association
 *   covenant is fundamentally a ropeâa low-extraction coordination
 *   mechanism for shared infrastructure maintenance and genuine externality
 *   resolution. Under this reading, assessments are symmetric cost-recovery
 *   transfers among similarly situated homeowners, and enforcement is
 *   narrowly scoped to infrastructure funding and objective nuisance. The
 *   kernel is contested by two sibling readings: behavioral_control_reading
 *   (aesthetic uniformity and social control) and extraction_reading (revenue
 *   generation and board power consolidation). This reading instantiates a
 *   clean, low-Îµ rope with no victims and symmetric beneficiaries.
 *
 * KEY AGENTS:
 *   - All homeowners (moderate/constrained): Symmetric beneficiaries who pay proportionate assessments and receive maintained shared infrastructure.
 *   - HOA board (organized/constrained): Agenda setter administering the covenant with narrow, cost-recovering enforcement authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__coordination_reading, 0.15).
domain_priors:suppression_score(hoa_covenant_scope__coordination_reading, 0.25).
domain_priors:theater_ratio(hoa_covenant_scope__coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__coordination_reading, rope).
narrative_ontology:human_readable(hoa_covenant_scope__coordination_reading, "HOA Covenant â Shared Infrastructure Coordination Reading").
narrative_ontology:topic_domain(hoa_covenant_scope__coordination_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__coordination_reading, '60480b67-5614-499b-a639-1dde3ba3e0dc').
narrative_ontology:cs_kernel_codification('60480b67-5614-499b-a639-1dde3ba3e0dc', formalized).
narrative_ontology:cs_authority_grounding('60480b67-5614-499b-a639-1dde3ba3e0dc', lineage).
narrative_ontology:cs_interpretation_layer_present('60480b67-5614-499b-a639-1dde3ba3e0dc').
narrative_ontology:cs_reading_relation('60480b67-5614-499b-a639-1dde3ba3e0dc', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('60480b67-5614-499b-a639-1dde3ba3e0dc', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('60480b67-5614-499b-a639-1dde3ba3e0dc', foundational, infrastructure_costs_common_responsibility).
narrative_ontology:cs_axiom_status(infrastructure_costs_common_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('60480b67-5614-499b-a639-1dde3ba3e0dc', infrastructure_costs_common_responsibility, conventional).
narrative_ontology:cs_axiom('60480b67-5614-499b-a639-1dde3ba3e0dc', foundational, objective_nuisance_enforcement_only).
narrative_ontology:cs_axiom_status(objective_nuisance_enforcement_only, holdable).
narrative_ontology:cs_axiom_grounding('60480b67-5614-499b-a639-1dde3ba3e0dc', objective_nuisance_enforcement_only, conventional).
narrative_ontology:cs_reference_frame('60480b67-5614-499b-a639-1dde3ba3e0dc', shared_infrastructure_agreement).
narrative_ontology:cs_drift_state('60480b67-5614-499b-a639-1dde3ba3e0dc', contemporary_hoa_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('60480b67-5614-499b-a639-1dde3ba3e0dc', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__coordination_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, all_homeowners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own property within the covenant-bound community. Receive maintained shared infrastructure such as private roads, drainage, utilities, and common areas funded by proportionate assessments. Bear the cost of that maintenance through dues. Exit requires selling the property or achieving a supermajority vote to dissolve the covenant, which is costly and rare.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, all_homeowners, beneficiary,
    moderate, biographical, constrained, local).

% Administers the recorded declaration of covenants, conditions, and restrictions. Collects assessments, contracts for infrastructure maintenance, and enforces only provisions related to objective nuisance and cost recovery. Authority is functionally limited to preserving the common elements; the board does not capture surplus revenue and is subject to homeowner recall and state fiduciary statutes.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, hoa_board, agenda_setter,
    organized, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__coordination_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective maintenance of non-excludable infrastructure serving multiple parcels within a community: private roads, drainage systems, utility easements, and common areas. Prevents free-riding on maintenance costs and resolves genuine negative externalities such as objective nuisances that degrade neighboring property values or create health hazards.
% TRANSFER_FUNCTION: Moves proportional maintenance assessments from each homeowner to a common fund, which pays directly for infrastructure upkeep and nuisance abatement. Transfers are symmetric and cost-recovering; no party extracts a surplus from the flow.
% ABSENT_VOICES: Homeowners who would prefer purely private maintenance contracts or no collective governance at all are structurally excluded because the covenant runs with the land and binds subsequent purchasers. Renters within the community, who pay assessments indirectly through rent, are excluded from covenant amendment votes despite bearing the costs.
% DISAPPEARANCE_RATIONALE: If the covenant disappeared overnight, the shared infrastructure would lack a statutory or contractual funding mechanism. Homeowners would need to negotiate ad-hoc maintenance contracts for roads, drainage, and common areas, or watch the infrastructure degrade. The current collective financing and dispute-resolution arrangement would collapse and require municipal absorption or private renegotiation.
% FOUNDING_PROBLEM: Privately owned parcels sharing critical infrastructure faced a collective-action problem: no individual homeowner had adequate incentive to maintain roads or drainage benefiting all owners, and uncoordinated maintenance led to free-riding, underprovision, and physical decay of common elements.
% FOUNDING_PROBLEM_CORROBORATION: Urban planning and public finance scholarship external to the HOA board and homeowners attests that common-interest communities require coordinated funding mechanisms for shared infrastructure where municipal provision is absent. Many state statutes explicitly recognize HOAs as infrastructure-substitutes, corroborating that the founding problem remains unresolved without the arrangement.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hoa_covenant_scope__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__coordination_reading, 0.15, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low (0.15) because assessments are designed as proportional cost-sharing for non-excludable infrastructure, not rent collection. Suppression is low-moderate (0.25) because enforcement is limited to dues collection and objective nuisance abatement; alternatives such as municipal absorption or private road maintenance exist but are costly. Theater ratio is very low (0.10) because most board activity is functional maintenance contracting and accounting rather than performative governance. Accessibility collapse is moderate (0.40) because once a buyer enters the covenant, alternatives to the collective arrangement require property sale. Resistance is low (0.20) because homeowners structurally benefit from maintained infrastructure and the arrangement is not experienced as extractive under this reading.
 *
 * PERSPECTIVAL GAP:
 *   Under the coordination reading, all seated agents experience the constraint as symmetric or mildly beneficial. The engine should compute low directionality deviation for homeowners and the board because the structural data declare symmetric beneficiaries and no victims. Divergence from the sibling readings (behavioral control and extraction) is not captured as seat divergence within this story; it manifests across the constraint family as competing interpretations of the same legal instrument.
 *
 * DIRECTIONALITY LOGIC:
 *   All homeowners are symmetric beneficiaries (d near the center, slightly toward beneficiary because they receive infrastructure services that exceed individual provision costs). The HOA board is an agenda setter without a surplus-capture role; its directionality is near symmetric (d â 0.5). No victim group is declared, so no high-d target seat exists. The structural derivation chain therefore produces low effective extraction for all indexed seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination reading prevents mislabeling by rigorously restricting the covenant's legitimate scope to infrastructure and objective nuisance. If the same legal instrument is used to enforce aesthetic preferences or generate fines, that function belongs to a different constraint story (the sibling readings) with a distinct Îµ and victim structure. Mandatrophyâpersistence after function deathâis not present here because the founding problem (shared infrastructure maintenance) remains live and the arrangement directly addresses it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_scope_drift,
    'Does the legal form of the HOA covenant instrument inherently enable drift toward behavioral control and extraction, or can the coordination reading''s narrow scope be institutionally stabilized?',
    'Comparative analysis of jurisdictions with strict statutory scoping limits on HOA enforcement authority versus permissive regimes; measurement of enforcement action categories across regimes.',
    'If drift is inherent, the coordination reading describes an unstable ideal type rather than an operable constraint; if stabilizable, the rope classification holds for well-designed covenants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_scope_drift, conceptual, 'Whether the coordination reading is structurally stable or inevitably drifts').

omega_variable(
    free_rider_enforcement_threshold,
    'At what point does the administrative cost of enforcing assessments against delinquent homeowners exceed the revenue recovered, converting cost-recovery coordination into net extraction?',
    'Empirical accounting studies of HOA enforcement costs versus recovery rates, including legal fees and collection overhead.',
    'If enforcement costs routinely exceed recoveries funded by compliant homeowners, the symmetric beneficiary claim weakens and the constraint shifts toward tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_rider_enforcement_threshold, empirical, 'Enforcement cost threshold against delinquent homeowners').

omega_variable(
    objective_nuisance_boundary,
    'Is the distinction between ''objective nuisance'' (health/safety/property damage) and ''aesthetic preference'' stable in covenant enforcement practice, or does the interpretation layer systematically blur them?',
    'Content analysis of HOA enforcement actions and judicial opinions categorizing covenant violations as health/safety versus aesthetic.',
    'If the boundary is unstable, the coordination reading''s narrow scope cannot be maintained in practice and the constraint family collapses toward the behavioral control reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objective_nuisance_boundary, empirical, 'Stability of the objective nuisance versus aesthetic boundary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__coordination_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__coordination_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hoa__tr_t8, hoa_covenant_scope__coordination_reading, theater_ratio, 8, 0.06).
narrative_ontology:measurement(hoa__tr_t16, hoa_covenant_scope__coordination_reading, theater_ratio, 16, 0.07).
narrative_ontology:measurement(hoa__tr_t24, hoa_covenant_scope__coordination_reading, theater_ratio, 24, 0.08).
narrative_ontology:measurement(hoa__tr_t32, hoa_covenant_scope__coordination_reading, theater_ratio, 32, 0.1).
narrative_ontology:measurement(hoa__tr_t40, hoa_covenant_scope__coordination_reading, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__coordination_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(hoa__be_t8, hoa_covenant_scope__coordination_reading, base_extractiveness, 8, 0.13).
narrative_ontology:measurement(hoa__be_t16, hoa_covenant_scope__coordination_reading, base_extractiveness, 16, 0.14).
narrative_ontology:measurement(hoa__be_t24, hoa_covenant_scope__coordination_reading, base_extractiveness, 24, 0.15).
narrative_ontology:measurement(hoa__be_t32, hoa_covenant_scope__coordination_reading, base_extractiveness, 32, 0.15).
narrative_ontology:measurement(hoa__be_t40, hoa_covenant_scope__coordination_reading, base_extractiveness, 40, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hoa_covenant_scope__coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__behavioral_control_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'HOA covenant' conflates three structurally distinct constraints: genuine infrastructure coordination (this story), behavioral control via aesthetic rules, and extraction via fines and selective enforcement. Each has a different epsilon, beneficiary structure, and enforcement logic. They are modeled as separate stories linked by network edges, not as one constraint with a parameter-dependent classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
