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
 *   human_readable: Provincial Resource Sovereignty Primacy
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   This constraint represents the reading of s.92A of the Constitution Act
 *   1982 that asserts absolute provincial sovereignty over natural resources,
 *   equating resource control with territorial sovereignty. Proponents of
 *   this reading argue it is a fundamental, unassailable truth of Canadian
 *   federalism, effectively claiming it as a 'mountain'. However, the metrics
 *   reflect its active enforcement against federal policy and its contested
 *   nature, suggesting it functions more as an extractive mechanism for
 *   resource-rich provinces. The claim/metric gap is intentional to allow the
 *   engine to detect a false summit.
 *
 * KEY AGENTS:
 *   - Resource-rich provinces: Primary agenda-setters (institutional/constrained) – assert and defend this interpretation.
 *   - Provincial resource industries: Primary beneficiaries (powerful/mobile) – profit from stable provincial control.
 *   - Federal government: Primary target/payer (institutional/constrained) – faces resistance to national policies.
 *   - National climate policy advocates: Victims/payers (organized/constrained) – find their agenda suppressed by this claim.
 *   - Constitutional scholars: Analytical observers (analytical/analytical) – analyze the legal basis and implications.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.85).
domain_priors:suppression_score(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.78).
domain_priors:theater_ratio(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, extractiveness, 0.85).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__resource_sovereignty_primacy, mountain).
narrative_ontology:human_readable(provincial_sovereignty_boundary__resource_sovereignty_primacy, "Provincial Resource Sovereignty Primacy").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__resource_sovereignty_primacy, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__resource_sovereignty_primacy).
domain_priors:emerges_naturally(provincial_sovereignty_boundary__resource_sovereignty_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'd93c34b0-41b4-4a8f-98a4-8856a8f02987').
narrative_ontology:cs_kernel_codification('d93c34b0-41b4-4a8f-98a4-8856a8f02987', fixed_text).
narrative_ontology:cs_authority_grounding('d93c34b0-41b4-4a8f-98a4-8856a8f02987', lineage).
narrative_ontology:cs_interpretation_layer_present('d93c34b0-41b4-4a8f-98a4-8856a8f02987').
narrative_ontology:cs_reading_relation('d93c34b0-41b4-4a8f-98a4-8856a8f02987', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('d93c34b0-41b4-4a8f-98a4-8856a8f02987', provincial_sovereignty_boundary__compact_federalism, coexists_with).
narrative_ontology:cs_axiom('d93c34b0-41b4-4a8f-98a4-8856a8f02987', foundational, provincial_resource_ownership_is_absolute).
narrative_ontology:cs_axiom_status(provincial_resource_ownership_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('d93c34b0-41b4-4a8f-98a4-8856a8f02987', provincial_resource_ownership_is_absolute, conventional).
narrative_ontology:cs_axiom('d93c34b0-41b4-4a8f-98a4-8856a8f02987', foundational, s92a_grants_unilateral_resource_control).
narrative_ontology:cs_axiom_status(s92a_grants_unilateral_resource_control, holdable).
narrative_ontology:cs_axiom_grounding('d93c34b0-41b4-4a8f-98a4-8856a8f02987', s92a_grants_unilateral_resource_control, conventional).
narrative_ontology:cs_reference_frame('d93c34b0-41b4-4a8f-98a4-8856a8f02987', unfettered_provincial_resource_control).
narrative_ontology:cs_drift_state('d93c34b0-41b4-4a8f-98a4-8856a8f02987', contemporary_climate_policy_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d93c34b0-41b4-4a8f-98a4-8856a8f02987', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_rich_provinces).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_resource_industries).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, other_provinces_dependent_on_federal_equalization).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, national_climate_policy_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert absolute constitutional control over natural resources within their borders, interpreting s.92A as granting full territorial sovereignty in this domain. They actively resist federal policies perceived as infringing on this control, particularly regarding resource development and revenue.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_rich_provinces, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the stability and predictability of provincial control over resource development, often aligning with provincial governments to resist federal environmental regulations or carbon pricing that could impact their operations and profitability. They lobby for this interpretation of sovereignty.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_resource_industries, beneficiary,
    powerful, biographical, mobile, national).

% Bears the cost of political and legal challenges when attempting to implement national policies (e.g., climate change, equalization payments) that intersect with provincial resource jurisdiction. It seeks to balance provincial rights with national interests, often facing accusations of overreach from resource-rich provinces.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government, payer,
    institutional, generational, constrained, national).

% Indirectly bear costs as the assertion of absolute provincial resource sovereignty can limit the federal government's fiscal capacity or ability to implement national economic strategies that might benefit them. They often support a stronger federal role in resource governance for national cohesion and redistribution.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, other_provinces_dependent_on_federal_equalization, payer,
    organized, biographical, constrained, national).

% Face significant obstacles in advancing national climate policies due to the strong assertion of provincial resource sovereignty. They view this interpretation as a barrier to effective climate action and a mechanism for provinces to externalize environmental costs.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, national_climate_policy_advocates, payer,
    organized, generational, constrained, national).

% Analyze the legal and historical basis of s.92A and its implications for federalism. They offer interpretations that may support or challenge the 'absolute sovereignty' reading, influencing judicial and political discourse without directly participating in the resource extraction or policy implementation.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, constitutional_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_rich_provinces).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__resource_sovereignty_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To clearly delineate jurisdictional control over natural resources, providing certainty for investment and governance within a federal system.
% TRANSFER_FUNCTION: Transfers ultimate decision-making authority and associated revenues from natural resource development to provincial governments, away from federal oversight or national policy objectives.
% ABSENT_VOICES: Indigenous communities, whose inherent rights and title often predate and are not fully reconciled with either provincial or federal claims of sovereignty over land and resources, are often marginalized in this debate. They would advocate for self-determination and co-management.
% DISAPPEARANCE_RATIONALE: If this interpretation of absolute provincial resource sovereignty vanished, the balance of power in Canadian federalism would fundamentally shift. Federal climate and fiscal policies would gain significant leverage, resource development projects would face new national oversight, and provincial economies heavily reliant on resource rents would need to adapt to a new jurisdictional reality.
% FOUNDING_PROBLEM: To resolve ambiguities regarding provincial ownership and control of natural resources, particularly in the context of resource-rich provinces seeking greater autonomy and revenue streams.
% FOUNDING_PROBLEM_CORROBORATION: Resource-rich provinces and their industries consistently attest that the problem of federal overreach into resource jurisdiction remains live. Constitutional scholars, while often disagreeing on the interpretation, corroborate that the jurisdictional tension around resources is an ongoing feature of Canadian federalism, not a settled matter.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__resource_sovereignty_primacy, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__resource_sovereignty_primacy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__resource_sovereignty_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

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
 *   The `extractiveness` is high (0.85) because this reading effectively allows resource-rich provinces to unilaterally control significant economic levers and resist national policy, extracting autonomy and revenue. `Suppression` is also high (0.78) as it actively suppresses federal legislative and policy alternatives through legal and political means. `Theater_ratio` is low (0.15) because the claim is genuinely and actively asserted, not merely performed. `Accessibility_collapse` is high (0.88) from the perspective of federal policy, as this reading aims to collapse federal alternatives in the resource domain. `Resistance` is high (0.70) from federal and other provincial actors who contest this absolute interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of resource-rich provinces and their industries, this is a legitimate, unassailable constitutional right (a 'mountain') that protects their economic interests. From the federal government's and national policy advocates' perspective, it is an extractive 'snare' or 'tangled rope' that allows certain provinces to externalize costs and undermine national objectives. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Resource-rich provinces and their industries are clear beneficiaries, gaining control and revenue. The federal government, other provinces, and national climate policy advocates are targets, bearing the costs of limited policy options and fiscal capacity. The constitutional claim itself acts as a structural subsidy for the beneficiaries and an extraction mechanism from the targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate, from its proponents' view, is to secure provincial autonomy over resources. While this problem is 'live' for them, critics argue that the 'absolute sovereignty' interpretation has outlived its original intent of clarifying jurisdiction and now functions primarily to resist necessary national coordination, particularly on climate change. The high extractiveness and suppression, combined with the 'mountain' claim, are intended to trigger false summit detection, indicating a potential mandatrophy where a coordination function has been co-opted for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_interpretation_vs_natural_law,
    'Is the claim of ''absolute provincial sovereignty'' over resources a genuine, unassailable constitutional truth (a natural law of the federation), or a contested legal interpretation that serves specific political and economic interests?',
    'Supreme Court of Canada rulings on federal-provincial resource jurisdiction, or a constitutional amendment clarifying the division of powers.',
    'If resolved as a contested interpretation, the ''mountain'' claim would be reclassified, likely as a ''tangled_rope'' or ''snare'', reflecting its active enforcement and extractive function. If upheld as an absolute truth, its ''mountain'' classification would be reinforced, but its high extractiveness would remain a critical signal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_interpretation_vs_natural_law, conceptual, 'Ambiguity between a constitutional claim and a natural law.').

omega_variable(
    federal_paramountcy_vs_provincial_sovereignty,
    'To what extent does federal paramountcy (the principle that federal law prevails in cases of conflict with provincial law) apply to s.92A, and how does it modulate provincial resource sovereignty?',
    'Further judicial clarification on the scope of federal paramountcy in areas of shared or overlapping jurisdiction, particularly concerning environmental regulation and interprovincial trade.',
    'A stronger interpretation of federal paramountcy would reduce the effective suppression and extractiveness of provincial resource sovereignty, potentially reclassifying it towards a ''rope'' or ''scaffold'' if it facilitates national coordination. A weaker interpretation would reinforce its current extractive function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_paramountcy_vs_provincial_sovereignty, empirical, 'The interplay between federal and provincial constitutional powers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__resource_sovereignty_primacy, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(prov_tr_t1992, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1992, 0.12).
narrative_ontology:measurement(prov_tr_t2002, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2002, 0.14).
narrative_ontology:measurement(prov_tr_t2012, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2012, 0.15).
narrative_ontology:measurement(prov_tr_t2024, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1982, 0.6).
narrative_ontology:measurement(prov_be_t1992, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1992, 0.68).
narrative_ontology:measurement(prov_be_t2002, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2002, 0.75).
narrative_ontology:measurement(prov_be_t2012, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2012, 0.8).
narrative_ontology:measurement(prov_be_t2024, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1982, 0.55).
narrative_ontology:measurement(prov_su_t1992, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1992, 0.62).
narrative_ontology:measurement(prov_su_t2002, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2002, 0.68).
narrative_ontology:measurement(prov_su_t2012, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2012, 0.73).
narrative_ontology:measurement(prov_su_t2024, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2024, 0.78).


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
