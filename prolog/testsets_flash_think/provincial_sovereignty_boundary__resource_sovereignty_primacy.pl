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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: provincial_sovereignty_boundary__resource_sovereignty_primacy
 *   human_readable: Provincial Resource Sovereignty Primacy (s.92A)
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'resource_sovereignty_primacy'
 *   reading of the 'provincial_sovereignty_boundary' kernel. This reading
 *   asserts that Section 92A of the Constitution Act, 1982, grants provinces
 *   absolute and unchallengeable sovereignty over their natural resources,
 *   equating resource control with territorial sovereignty. From this
 *   perspective, any federal climate or fiscal policy that impacts provincial
 *   resource development is considered an illegitimate extraction or
 *   infringement on constitutional rights. The constraint itself is claimed
 *   as a Mountain, reflecting its proponents' view of it as an unchangeable
 *   constitutional fact, but its beneficiaries trigger False Summit Mountain
 *   detection.
 *
 * KEY AGENTS:
 *   - resource_rich_provinces: Primary beneficiary/agenda_setter (institutional/identity_locked) — benefits from constraint, asserts it.
 *   - federal_government: Primary target (institutional/constrained) — bears costs of constraint (limited policy scope), resists it.
 *   - environmental_advocates: Secondary target (organized/constrained) — bears costs (blocked climate policy).
 *   - constitutional_scholars: Analytical observer (analytical/analytical) — sees full structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.15).
domain_priors:suppression_score(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.1).
domain_priors:theater_ratio(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, extractiveness, 0.15).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__resource_sovereignty_primacy, mountain).
narrative_ontology:human_readable(provincial_sovereignty_boundary__resource_sovereignty_primacy, "Provincial Resource Sovereignty Primacy (s.92A)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__resource_sovereignty_primacy, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__resource_sovereignty_primacy).
domain_priors:emerges_naturally(provincial_sovereignty_boundary__resource_sovereignty_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__resource_sovereignty_primacy, '3ada7083-0dcf-4447-a354-f27ab7c94ec1').
narrative_ontology:cs_kernel_codification('3ada7083-0dcf-4447-a354-f27ab7c94ec1', fixed_text).
narrative_ontology:cs_authority_grounding('3ada7083-0dcf-4447-a354-f27ab7c94ec1', extraction).
narrative_ontology:cs_interpretation_layer_present('3ada7083-0dcf-4447-a354-f27ab7c94ec1').
narrative_ontology:cs_reading_relation('3ada7083-0dcf-4447-a354-f27ab7c94ec1', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('3ada7083-0dcf-4447-a354-f27ab7c94ec1', provincial_sovereignty_boundary__compact_federalism, coexists_with).
narrative_ontology:cs_axiom('3ada7083-0dcf-4447-a354-f27ab7c94ec1', foundational, resource_control_is_absolute_sovereignty).
narrative_ontology:cs_axiom_status(resource_control_is_absolute_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('3ada7083-0dcf-4447-a354-f27ab7c94ec1', resource_control_is_absolute_sovereignty, conventional).
narrative_ontology:cs_axiom('3ada7083-0dcf-4447-a354-f27ab7c94ec1', secondary, federal_encroachment_is_illegitimate_extraction).
narrative_ontology:cs_axiom_status(federal_encroachment_is_illegitimate_extraction, holdable).
narrative_ontology:cs_axiom_grounding('3ada7083-0dcf-4447-a354-f27ab7c94ec1', federal_encroachment_is_illegitimate_extraction, conventional).
narrative_ontology:cs_reference_frame('3ada7083-0dcf-4447-a354-f27ab7c94ec1', unilateral_provincial_resource_control).
narrative_ontology:cs_drift_state('3ada7083-0dcf-4447-a354-f27ab7c94ec1', contemporary_climate_policy_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('3ada7083-0dcf-4447-a354-f27ab7c94ec1', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_rich_provinces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_industries).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, other_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, environmental_advocates).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_autonomy_doctrine).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__resource_sovereignty_primacy, s92a_constitutional_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Provinces that possess significant natural resources and interpret s.92A as granting them absolute, unchallengeable sovereignty over these resources, including the right to develop them without federal interference. They actively assert this claim against federal climate or fiscal policies.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_rich_provinces, agenda_setter,
    institutional, generational, identity_locked, national).

% Seeks to implement national policies (e.g., climate change, fiscal equalization) that may impinge on provincial resource development. From this reading's perspective, federal actions are seen as illegitimate extraction or suppression of provincial rights.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government, payer,
    institutional, generational, constrained, national).

% Provinces with fewer natural resources, often net recipients of federal equalization payments. They may support federal policies that ensure national benefit sharing or environmental protection, thus implicitly challenging the absolute resource sovereignty claim.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, other_provinces, payer,
    institutional, generational, constrained, national).

% Advocate for federal climate policies and environmental regulations that often conflict with provincial resource development plans. They view the absolute resource sovereignty claim as an impediment to necessary national action.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, environmental_advocates, payer,
    organized, generational, constrained, national).

% Benefit from clear, stable provincial jurisdiction over resource development, preferring fewer federal regulatory hurdles. They align with the provincial resource sovereignty claim to ensure predictable operating environments.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_industries, beneficiary,
    powerful, biographical, mobile, global).

% Analyze the constitutional basis and implications of s.92A, offering various interpretations of provincial and federal powers. They observe the contestation without direct participation in its political or economic outcomes.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_rich_provinces).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__resource_sovereignty_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear jurisdictional boundary for natural resource management, intended to reduce inter-governmental conflict over resource development and revenue.
% TRANSFER_FUNCTION: Asserts the right of resource-rich provinces to retain full economic benefit and control over their natural resources, preventing federal transfer or regulatory imposition.
% ABSENT_VOICES: Indigenous communities, whose inherent rights and title often predate and are not fully reconciled with s.92A, are often excluded from the primary federal-provincial discourse on resource sovereignty. They would assert their own sovereignty and co-management rights.
% DISAPPEARANCE_RATIONALE: If the claim of absolute provincial resource sovereignty vanished, the balance of power in Canadian federalism would fundamentally shift. Federal environmental and fiscal policies would gain significant new scope, provincial resource revenues and development strategies would be subject to greater national oversight, and the political economy of resource-rich provinces would be profoundly altered.
% FOUNDING_PROBLEM: To clarify and entrench provincial ownership and legislative authority over natural resources, particularly in the context of historical federal attempts to assert greater control over resource revenues and development.
% FOUNDING_PROBLEM_CORROBORATION: Resource-rich provinces consistently attest that the founding problem of federal overreach into provincial resource jurisdiction remains live. Some constitutional scholars and resource industry groups corroborate this view, emphasizing the ongoing need for provincial control over economic drivers. The federal government and other scholars contest the 'absolute' nature of this claim, arguing for a more integrated federal role.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__resource_sovereignty_primacy, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__resource_sovereignty_primacy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__resource_sovereignty_primacy_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, ExtMetricName, E),
    domain_priors:suppression_score(provincial_sovereignty_boundary__resource_sovereignty_primacy, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(provincial_sovereignty_boundary__resource_sovereignty_primacy),
    narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(provincial_sovereignty_boundary__resource_sovereignty_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is Mountain because proponents of this reading view s.92A as establishing an absolute, fundamental constitutional truth. Consequently, the base metrics (extractiveness, suppression, theater_ratio, resistance) are set low, and accessibility_collapse is high, consistent with a genuine natural law. However, the presence of 'resource_rich_provinces' as beneficiaries triggers the False Summit Mountain (FSM) detection, which will reclassify this constraint as a Tangled Rope by default, reflecting the contestable nature of this 'absolute' claim and the identifiable parties who benefit from it. The 'requires_active_enforcement: true' reflects the ongoing political and legal defense of this interpretation by provinces.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies between the proponents of this reading (resource-rich provinces, resource industries) who see it as an unassailable constitutional right, and those who challenge it (federal government, other provinces, environmental advocates) who view it as an overreach that impedes national policy objectives. The engine's FSM reclassification captures this divergence, moving from the claimed 'Mountain' to a 'Tangled Rope' that acknowledges both coordination (clear provincial jurisdiction) and extraction (from federal policy space).
 *
 * DIRECTIONALITY LOGIC:
 *   Resource-rich provinces are the clear beneficiaries, as this reading grants them maximum control and economic benefit from their resources. The federal government, other provinces (who might seek national benefit sharing), and environmental advocates are effectively targets, as their policy objectives are constrained or thwarted by this interpretation. Resource industries are also beneficiaries, aligning with provincial autonomy to ensure predictable development. Constitutional scholars act as observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine, unchallengeable constitutional truth (Mountain), or a contested interpretation that benefits specific parties (False Summit)?',
    'Judicial review by the Supreme Court of Canada or a constitutional amendment clarifying the scope of s.92A relative to federal powers.',
    'If resolved as a genuine Mountain, the FSM reclassification would be overridden. If resolved as a contested interpretation, the FSM reclassification to Tangled Rope (or Snare) would be affirmed, highlighting the extractive nature of the claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity between a fundamental constitutional principle and a self-serving interpretation.').

omega_variable(
    scope_of_s92a_ambiguity,
    'Does s.92A grant absolute, unqualified provincial sovereignty over resources, or is it subject to implied federal powers (e.g., peace, order, and good government) or other constitutional principles?',
    'Further Supreme Court of Canada jurisprudence clarifying the ''inter-jurisdictional immunity'' or ''paramountcy'' doctrines in the context of s.92A and federal environmental/economic powers.',
    'If absolute, this reading''s claim of Mountain status would be strengthened. If qualified, the claim would be weakened, reinforcing its classification as a contested, extractive construct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_s92a_ambiguity, empirical, 'The precise legal scope and limits of provincial resource jurisdiction under s.92A.').

omega_variable(
    federal_policy_legitimacy,
    'Are federal climate and fiscal policies, when they impact provincial resource development, legitimate exercises of federal power or illegitimate extraction from provincial jurisdiction?',
    'Judicial rulings on specific federal legislation challenged by provinces, or a political consensus on a national energy/climate strategy.',
    'If federal policies are deemed legitimate, the ''illegitimate extraction'' aspect of this reading is undermined. If deemed illegitimate, this reading''s framing of federal actions as extractive is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_policy_legitimacy, preference, 'Contestation over the legitimacy of federal intervention in provincial resource matters.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__resource_sovereignty_primacy, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(prov_tr_t1992, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1992, 0.1).
narrative_ontology:measurement(prov_tr_t2002, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2002, 0.1).
narrative_ontology:measurement(prov_tr_t2012, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2012, 0.1).
narrative_ontology:measurement(prov_tr_t2024, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1982, 0.1).
narrative_ontology:measurement(prov_be_t1992, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1992, 0.12).
narrative_ontology:measurement(prov_be_t2002, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2002, 0.13).
narrative_ontology:measurement(prov_be_t2012, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2012, 0.14).
narrative_ontology:measurement(prov_be_t2024, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1982, 0.1).
narrative_ontology:measurement(prov_su_t1992, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1992, 0.1).
narrative_ontology:measurement(prov_su_t2002, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2002, 0.1).
narrative_ontology:measurement(prov_su_t2012, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2012, 0.1).
narrative_ontology:measurement(prov_su_t2024, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__resource_sovereignty_primacy, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
