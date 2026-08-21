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
 *   human_readable: Provincial Resource Sovereignty Primacy (s.92A Reading)
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   This constraint represents the 'resource sovereignty primacy' reading of
 *   s.92A of the Constitution Act 1982, which asserts that provincial
 *   ownership of natural resources grants absolute territorial sovereignty,
 *   making federal climate or fiscal policies illegitimate extraction. This
 *   reading is a Tangled Rope: it provides coordination for provincial
 *   resource development but extracts from national policy coherence and
 *   other provinces. The metrics reflect the increasing assertiveness of this
 *   reading over time, leading to higher extraction and suppression of
 *   federal initiatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.65).
domain_priors:suppression_score(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.7).
domain_priors:theater_ratio(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, extractiveness, 0.65).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__resource_sovereignty_primacy, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__resource_sovereignty_primacy, "Provincial Resource Sovereignty Primacy (s.92A Reading)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__resource_sovereignty_primacy, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__resource_sovereignty_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'be7c9cc6-c899-4f6d-a97a-edad09d98adb').
narrative_ontology:cs_kernel_codification('be7c9cc6-c899-4f6d-a97a-edad09d98adb', fixed_text).
narrative_ontology:cs_authority_grounding('be7c9cc6-c899-4f6d-a97a-edad09d98adb', lineage).
narrative_ontology:cs_interpretation_layer_present('be7c9cc6-c899-4f6d-a97a-edad09d98adb').
narrative_ontology:cs_reading_relation('be7c9cc6-c899-4f6d-a97a-edad09d98adb', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('be7c9cc6-c899-4f6d-a97a-edad09d98adb', provincial_sovereignty_boundary__compact_federalism, coexists_with).
narrative_ontology:cs_axiom('be7c9cc6-c899-4f6d-a97a-edad09d98adb', foundational, resource_ownership_equals_territorial_sovereignty).
narrative_ontology:cs_axiom_status(resource_ownership_equals_territorial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('be7c9cc6-c899-4f6d-a97a-edad09d98adb', resource_ownership_equals_territorial_sovereignty, conventional).
narrative_ontology:cs_axiom('be7c9cc6-c899-4f6d-a97a-edad09d98adb', secondary, federal_climate_policy_is_illegitimate_extraction).
narrative_ontology:cs_axiom_status(federal_climate_policy_is_illegitimate_extraction, holdable).
narrative_ontology:cs_axiom_grounding('be7c9cc6-c899-4f6d-a97a-edad09d98adb', federal_climate_policy_is_illegitimate_extraction, instrumental).
narrative_ontology:cs_reference_frame('be7c9cc6-c899-4f6d-a97a-edad09d98adb', absolute_provincial_resource_control_1982).
narrative_ontology:cs_drift_state('be7c9cc6-c899-4f6d-a97a-edad09d98adb', contemporary_climate_crisis_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('be7c9cc6-c899-4f6d-a97a-edad09d98adb', '').
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

% Assert absolute control over natural resources within their borders, interpreting s.92A as granting full territorial sovereignty. They benefit from direct resource revenues and the ability to set their own environmental and economic policies, often resisting federal oversight or revenue sharing.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_rich_provinces, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from provincial policies that prioritize resource extraction and minimize regulatory burdens. They align with provincial governments in resisting federal environmental or carbon pricing policies, seeing provincial control as more favorable to their operations.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_resource_industries, beneficiary,
    organized, biographical, mobile, regional).

% Bears the cost of fragmented national policy, particularly on climate change and inter-provincial trade. Its attempts to implement national standards or redistribute resource wealth are met with constitutional challenges and political resistance, limiting its effective sovereignty.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government, payer,
    institutional, generational, constrained, national).

% Advocate for stronger federal environmental regulations and national climate action, but their efforts are often stymied by provincial claims of resource sovereignty. They bear the cost of environmental degradation and policy inaction due to jurisdictional disputes.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, national_environmental_groups, payer,
    organized, generational, constrained, national).

% Provinces without significant natural resource endowments bear the cost of uneven economic development and limited access to resource revenues. They may support federal equalization efforts but face political resistance from resource-rich provinces.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, other_provinces, payer,
    institutional, generational, constrained, national).

% Acts as the ultimate arbiter of constitutional disputes between federal and provincial governments. Its rulings shape the interpretation of s.92A and the balance of power, but it does not directly benefit or pay from the constraint's operation.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, supreme_court_of_canada, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates provincial resource development and economic policy by asserting clear, absolute provincial jurisdiction, reducing ambiguity for industries operating within provincial borders.
% TRANSFER_FUNCTION: Transfers effective control over resource-related policy and revenue from the federal government to resource-rich provinces, and from national interests (like climate action) to provincial economic priorities.
% ABSENT_VOICES: Indigenous communities, whose land rights and resource claims often predate and conflict with both federal and provincial assertions of sovereignty, are often marginalized in this debate. Future generations, who will bear the long-term environmental costs of resource extraction, also lack a direct voice.
% DISAPPEARANCE_RATIONALE: If this interpretation of s.92A vanished, the federal government would likely assert greater authority over resource development and environmental policy, leading to a more unified national approach. Resource-rich provinces would lose a significant basis for their claims of autonomy, and the balance of power within Canadian federalism would fundamentally shift.
% FOUNDING_PROBLEM: The original intent of s.92A was to clarify provincial ownership and control over natural resources, particularly in the context of Western provinces seeking greater economic autonomy and control over their own development.
% FOUNDING_PROBLEM_CORROBORATION: Resource-rich provinces and their industries continue to assert that the problem of federal overreach and the need for provincial autonomy over resources remains live. Legal scholars and historical documents corroborate the original intent to grant provinces greater control, though the extent of 'absolute sovereignty' remains contested by federalists and other provinces.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__resource_sovereignty_primacy, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__resource_sovereignty_primacy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because this reading allows resource-rich provinces to externalize costs (e.g., environmental impact) onto the national commons while retaining full revenue. Suppression is high because it actively resists federal attempts at national policy integration through constitutional challenges and political maneuvering. Theater ratio is moderate, as provinces genuinely manage resources, but the 'absolute sovereignty' claim often serves to deflect federal oversight rather than purely coordinate. The increasing trend in metrics reflects the growing salience of resource issues (e.g., climate change) and the hardening of provincial positions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of resource-rich provinces, this is a legitimate assertion of constitutional rights and a necessary coordination mechanism for their economies. From the federal perspective, it's an extractive constraint that undermines national unity and policy goals. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Resource-rich provinces and their industries are clear beneficiaries, gaining autonomy and revenue. The federal government, national environmental groups, and other provinces are victims, bearing the costs of fragmented policy and environmental impact. The Supreme Court acts as an observer, adjudicating disputes without direct benefit or cost from this specific constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling extraction as coordination by highlighting the asymmetric benefits and costs. While s.92A originally aimed to clarify jurisdiction, this reading has evolved to actively resist federal policy, suggesting a drift from coordination to extraction. The 'live' status of the founding problem is contested, as the original problem of provincial autonomy has arguably been superseded by new national challenges like climate change, making the constraint's persistence potentially mandatrophic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_interpretation_ambiguity,
    'Is s.92A an absolute grant of sovereignty or a specific jurisdictional allocation within a broader federal framework?',
    'A definitive Supreme Court ruling that explicitly limits or expands the scope of s.92A beyond resource ownership to broader territorial sovereignty.',
    'If limited, the constraint''s extractiveness from federal policy would decrease, potentially reclassifying it towards a Rope or even a Mountain (if the interpretation becomes settled law). If expanded, it would solidify as a Snare for federal interests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_interpretation_ambiguity, conceptual, 'Ambiguity in the constitutional interpretation of provincial resource ownership.').

omega_variable(
    federal_paramountcy_scope,
    'To what extent can federal legislation (e.g., on climate change) override provincial resource jurisdiction under the doctrine of federal paramountcy?',
    'Further Supreme Court cases clarifying the application of federal paramountcy in areas of shared or overlapping jurisdiction, particularly concerning environmental policy.',
    'A stronger assertion of federal paramountcy would reduce the provincial reading''s suppressive power and extractiveness, shifting it towards a more balanced Tangled Rope or even a Rope. A weaker assertion would entrench its Snare-like qualities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_paramountcy_scope, empirical, 'Uncertainty regarding the limits of federal paramountcy over provincial resource control.').

omega_variable(
    mandatrophy_of_resource_autonomy,
    'Has the original problem s.92A was meant to solve (provincial economic autonomy) been superseded by new national challenges (e.g., climate crisis), making the ''absolute sovereignty'' claim mandatrophic?',
    'A national consensus or political shift that re-prioritizes national over provincial interests in resource management, or a constitutional amendment clarifying the balance of power.',
    'If deemed mandatrophic, the constraint would be reclassified as a Piton or Snare, highlighting its persistence due to inertia or concentrated benefit rather than genuine coordination. If the problem is still seen as live, it remains a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_resource_autonomy, preference, 'Whether the constraint''s mandate has outlived its original function in the face of new challenges.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__resource_sovereignty_primacy, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1982, 0.2).
narrative_ontology:measurement(prov_tr_t1995, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(prov_tr_t2005, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(prov_tr_t2015, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(prov_tr_t2024, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1982, 0.5).
narrative_ontology:measurement(prov_be_t1995, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(prov_be_t2005, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(prov_be_t2015, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement(prov_be_t2024, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1982, 0.55).
narrative_ontology:measurement(prov_su_t1995, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(prov_su_t2005, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2005, 0.65).
narrative_ontology:measurement(prov_su_t2015, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement(prov_su_t2024, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__resource_sovereignty_primacy, enforcement_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_carbon_pricing_framework).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, interprovincial_trade_barriers).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'provincial_sovereignty_boundary' kernel. It asserts resource control as absolute territorial sovereignty, influencing federal policy and inter-provincial relations. Sibling readings ('constitutional_subordination', 'compact_federalism') offer alternative interpretations of provincial sovereignty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
