% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__resource_sovereignty_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__resource_sovereignty_primacy, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: provincial_sovereignty_boundary__resource_sovereignty_primacy
 *   human_readable: Provincial Resource Sovereignty Primacy
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   This constraint represents the 'resource_sovereignty_primacy' reading of
 *   the Canadian constitutional kernel concerning provincial sovereignty. It
 *   asserts that provincial ownership of natural resources (s.92A
 *   Constitution Act 1982) grants absolute sovereignty over those resources,
 *   equating resource control with territorial sovereignty. This reading
 *   implies that federal climate or fiscal policies impacting resource
 *   development are illegitimate extraction and that provinces have a
 *   constitutional right to unilateral exit from such federal impositions.
 *   The constraint is claimed as a Tangled Rope because while it coordinates
 *   provincial resource management, it does so by extracting from federal
 *   policy space and other provinces, requiring active enforcement of its
 *   jurisdictional claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.65).
domain_priors:suppression_score(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.75).
domain_priors:theater_ratio(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, extractiveness, 0.65).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__resource_sovereignty_primacy, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__resource_sovereignty_primacy, "Provincial Resource Sovereignty Primacy").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__resource_sovereignty_primacy, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__resource_sovereignty_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'e0a69dc5-6ef1-4ef8-bd92-ca3c3cc2dab1').
narrative_ontology:cs_kernel_codification('e0a69dc5-6ef1-4ef8-bd92-ca3c3cc2dab1', fixed_text).
narrative_ontology:cs_authority_grounding('e0a69dc5-6ef1-4ef8-bd92-ca3c3cc2dab1', lineage).
narrative_ontology:cs_interpretation_layer_present('e0a69dc5-6ef1-4ef8-bd92-ca3c3cc2dab1').
narrative_ontology:cs_reading_relation('e0a69dc5-6ef1-4ef8-bd92-ca3c3cc2dab1', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('e0a69dc5-6ef1-4ef8-bd92-ca3c3cc2dab1', provincial_sovereignty_boundary__compact_federalism, coexists_with).
narrative_ontology:cs_axiom('e0a69dc5-6ef1-4ef8-bd92-ca3c3cc2dab1', foundational, resource_control_equals_territorial_sovereignty).
narrative_ontology:cs_axiom_status(resource_control_equals_territorial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('e0a69dc5-6ef1-4ef8-bd92-ca3c3cc2dab1', resource_control_equals_territorial_sovereignty, deontological).
narrative_ontology:cs_axiom('e0a69dc5-6ef1-4ef8-bd92-ca3c3cc2dab1', secondary, federal_intervention_is_illegitimate_extraction).
narrative_ontology:cs_axiom_status(federal_intervention_is_illegitimate_extraction, holdable).
narrative_ontology:cs_axiom_grounding('e0a69dc5-6ef1-4ef8-bd92-ca3c3cc2dab1', federal_intervention_is_illegitimate_extraction, instrumental).
narrative_ontology:cs_reference_frame('e0a69dc5-6ef1-4ef8-bd92-ca3c3cc2dab1', absolute_provincial_resource_autonomy).
narrative_ontology:cs_drift_state('e0a69dc5-6ef1-4ef8-bd92-ca3c3cc2dab1', contemporary_climate_crisis_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e0a69dc5-6ef1-4ef8-bd92-ca3c3cc2dab1', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_rich_provinces).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_resource_industries).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, national_environmental_groups).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, other_provinces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Provinces with significant natural resource endowments (e.g., Alberta, Saskatchewan). They assert absolute jurisdiction over their resources, interpreting s.92A as granting full sovereignty, and actively resist federal interventions in resource development or revenue sharing. They benefit directly from resource revenues and control.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_rich_provinces, agenda_setter,
    institutional, generational, constrained, national).

% Seeks to implement national policies on climate change, environmental protection, and fiscal equalization, which often conflict with provincial resource jurisdiction. Experiences this constraint as a limitation on its legislative and policy authority, leading to legal challenges and political disputes.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government, payer,
    institutional, generational, constrained, national).

% Operate within resource-rich provinces and benefit from stable, predictable provincial regulatory regimes that prioritize resource development. They lobby provincial governments to maintain strong resource sovereignty claims against federal oversight or taxation.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_resource_industries, beneficiary,
    organized, biographical, mobile, global).

% Advocate for stronger federal environmental regulations and climate action, often clashing with provincial resource development priorities. They experience this constraint as a barrier to achieving national environmental goals and a source of policy fragmentation.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, national_environmental_groups, payer,
    organized, generational, constrained, national).

% Provinces with fewer natural resources or different economic priorities. They may support stronger federal powers for national coherence, environmental protection, or fiscal equalization, and experience this constraint as limiting their share of national wealth or exposing them to negative externalities from resource development in other provinces.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, other_provinces, payer,
    institutional, generational, constrained, national).

% The ultimate arbiter of constitutional disputes. Its rulings shape the interpretation of s.92A and the balance of federal-provincial powers, directly impacting the operational scope and legitimacy of this constraint. It observes the legal arguments and constitutional history.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, supreme_court_of_canada, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_rich_provinces).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__resource_sovereignty_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates provincial resource management by granting clear, if contested, jurisdiction to provinces, allowing them to develop and manage their natural resources without direct federal interference.
% TRANSFER_FUNCTION: Transfers full economic benefit and regulatory control of natural resources from the national commons (or potential federal jurisdiction) to individual resource-rich provinces, and from federal policy space to provincial policy space.
% ABSENT_VOICES: Future generations and the global climate are absent voices; they would argue for a national or international approach to resource management and climate policy, but their interests are not directly represented in the current federal-provincial jurisdictional disputes.
% DISAPPEARANCE_RATIONALE: If this reading of provincial resource sovereignty disappeared, the balance of power in Canadian federalism would fundamentally shift. Federal climate and fiscal policies would gain significant new scope, resource revenues might be subject to greater national sharing, and interprovincial disputes over resource development would be re-litigated under a different constitutional framework.
% FOUNDING_PROBLEM: The original intent of s.92A was to clarify provincial ownership and control over natural resources, particularly in the context of Western provinces seeking greater economic autonomy and control over their resource wealth.
% FOUNDING_PROBLEM_CORROBORATION: Resource-rich provinces and their industries consistently attest that the problem of provincial economic autonomy and control over resources remains live, citing ongoing federal attempts to influence resource development. Constitutional scholars and historical records corroborate the original intent to grant provinces greater control, though the extent of 'absolute sovereignty' remains contested by federal actors and other legal experts.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__resource_sovereignty_primacy, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__resource_sovereignty_primacy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__resource_sovereignty_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__resource_sovereignty_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because this reading allows resource-rich provinces to unilaterally capture the full economic benefit of their resources, often at the expense of national policy coherence or other provinces. Suppression (0.75) is also high, as it requires active political and legal resistance to federal attempts at intervention, effectively suppressing federal policy options. The theater ratio (0.20) is relatively low, as the claims of sovereignty are genuinely asserted and defended, not merely performative. Accessibility collapse (0.40) is moderate, as federal alternatives are constrained but not entirely foreclosed, leading to ongoing disputes. Resistance (0.80) is high, reflecting the strong opposition from federal and national environmental groups to this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   Resource-rich provinces experience this as a Mountain or Rope, a fundamental constitutional right that protects their economic interests. The federal government and other provinces, however, experience it as a Snare or Tangled Rope, as it limits their ability to implement national policies or benefit from shared resources. This divergence is central to the ongoing federal-provincial disputes in Canada.
 *
 * DIRECTIONALITY LOGIC:
 *   Resource-rich provinces and their industries are clear beneficiaries (d near 0.0), as they gain full control and revenue from resource development. The federal government and national environmental groups are targets (d near 1.0), as their policy space and objectives are constrained. Other provinces are also victims, as they may be denied benefits from national resource management or face negative externalities without recourse.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a politically asserted claim of absolute sovereignty as a 'Mountain' of natural law. By identifying it as a 'Tangled Rope', the framework highlights both its coordination function (provincial resource management) and its extractive nature (from federal policy space and other provinces), which requires active enforcement to maintain. The ongoing contestation and high resistance indicate it is not a settled 'Mountain' but a dynamic, enforced constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine expression of constitutional intent, or an interpretive claim advanced by specific provincial interests?',
    'Supreme Court of Canada ruling on the scope of s.92A in relation to federal powers (e.g., POGG, trade and commerce).',
    'If ruled an interpretive claim, the constraint''s legitimacy as a ''mountain'' of constitutional law would collapse, reclassifying it as a ''snare'' or ''tangled_rope'' sustained by political will rather than legal necessity. If upheld, it reinforces the ''mountain'' aspect for resource-rich provinces.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''resource_sovereignty_primacy'' reading of the ''provincial_sovereignty_boundary'' kernel. Sibling readings (''constitutional_subordination'', ''compact_federalism'') would alter the balance of power and the legitimacy of federal interventions.').

omega_variable(
    federal_intervention_legitimacy,
    'To what extent can federal climate or fiscal policies legitimately override provincial resource jurisdiction without being considered ''illegitimate extraction''?',
    'Judicial clarification of the ''double aspect'' doctrine and federal paramountcy in areas of shared jurisdiction, or inter-governmental agreements that define clear boundaries.',
    'If federal intervention is deemed legitimate, the ''suppression'' metric for federal actors would decrease, and the ''extractiveness'' from provinces would be re-framed as a necessary cost of national coordination. If deemed illegitimate, provincial resistance would be constitutionally validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_intervention_legitimacy, empirical, 'Ambiguity regarding the constitutional limits of federal power over provincially-owned resources.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t0, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 0, 0.25).
narrative_ontology:measurement(prov_tr_t10, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 10, 0.22).
narrative_ontology:measurement(prov_tr_t20, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 20, 0.21).
narrative_ontology:measurement(prov_tr_t30, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(prov_be_t0, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(prov_be_t10, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(prov_be_t20, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(prov_be_t30, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t0, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(prov_su_t10, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(prov_su_t20, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(prov_su_t30, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_allocation).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary__constitutional_subordination).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary__compact_federalism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_climate_policy_framework).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, interprovincial_trade_barriers).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'provincial_sovereignty_boundary' kernel, focusing on resource control as absolute sovereignty. It is linked to sibling readings that offer alternative interpretations of provincial powers within the Canadian federation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
