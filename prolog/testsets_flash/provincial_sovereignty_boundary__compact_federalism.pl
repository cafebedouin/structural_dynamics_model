% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__compact_federalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__compact_federalism, []).

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
 *   constraint_id: provincial_sovereignty_boundary__compact_federalism
 *   human_readable: Provincial Sovereignty Boundary (Compact Federalism Reading)
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   This constraint models the 'compact federalism' reading of provincial
 *   sovereignty within a confederation, where provinces are seen as retaining
 *   residual sovereignty and federal authority is conditional on provincial
 *   consent. This reading emphasizes the negotiable nature of
 *   federal-provincial relations, including the possibility of provincial
 *   exit under duress. It is one reading of the broader
 *   'provincial_sovereignty_boundary' kernel, which is highly contested.
 *
 * KEY AGENTS:
 *   - provinces_asserting_autonomy: Primary beneficiary (institutional/constrained) — benefits from conditional federal authority.
 *   - federal_government_seeking_consensus: Secondary beneficiary (institutional/constrained) — benefits from stable, if negotiated, federalism.
 *   - federal_government_seeking_unilateral_action: Primary victim (institutional/constrained) — constrained by provincial consent requirements.
 *   - provinces_seeking_equalization: Secondary victim (institutional/constrained) — may be disadvantaged by 'compact' approach to resource sharing.
 *   - supreme_court_of_canada: Agenda setter (institutional/analytical) — adjudicates constitutional disputes, shaping the boundary.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, 0.45).
domain_priors:suppression_score(provincial_sovereignty_boundary__compact_federalism, 0.3).
domain_priors:theater_ratio(provincial_sovereignty_boundary__compact_federalism, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, extractiveness, 0.45).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__compact_federalism, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__compact_federalism, "Provincial Sovereignty Boundary (Compact Federalism Reading)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__compact_federalism, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__compact_federalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__compact_federalism, '22670c76-1e74-4da1-ac14-436dd5b24713').
narrative_ontology:cs_kernel_codification('22670c76-1e74-4da1-ac14-436dd5b24713', fixed_text).
narrative_ontology:cs_authority_grounding('22670c76-1e74-4da1-ac14-436dd5b24713', lineage).
narrative_ontology:cs_interpretation_layer_present('22670c76-1e74-4da1-ac14-436dd5b24713').
narrative_ontology:cs_reading_relation('22670c76-1e74-4da1-ac14-436dd5b24713', provincial_sovereignty_boundary__constitutional_subordination, coexists_with).
narrative_ontology:cs_reading_relation('22670c76-1e74-4da1-ac14-436dd5b24713', provincial_sovereignty_boundary__resource_sovereignty_primacy, influences).
narrative_ontology:cs_axiom('22670c76-1e74-4da1-ac14-436dd5b24713', foundational, confederation_as_voluntary_compact).
narrative_ontology:cs_axiom_status(confederation_as_voluntary_compact, holdable).
narrative_ontology:cs_axiom_grounding('22670c76-1e74-4da1-ac14-436dd5b24713', confederation_as_voluntary_compact, conventional).
narrative_ontology:cs_axiom('22670c76-1e74-4da1-ac14-436dd5b24713', foundational, provinces_retain_residual_sovereignty).
narrative_ontology:cs_axiom_status(provinces_retain_residual_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('22670c76-1e74-4da1-ac14-436dd5b24713', provinces_retain_residual_sovereignty, deontological).
narrative_ontology:cs_reference_frame('22670c76-1e74-4da1-ac14-436dd5b24713', original_compact_intent).
narrative_ontology:cs_drift_state('22670c76-1e74-4da1-ac14-436dd5b24713', contemporary_federal_centralization_pressure, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('22670c76-1e74-4da1-ac14-436dd5b24713', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, provinces_asserting_autonomy).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, federal_government_seeking_consensus).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, federal_government_seeking_unilateral_action).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, provinces_seeking_equalization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These provinces interpret Confederation as a voluntary compact, asserting their right to residual sovereignty and conditional consent to federal initiatives. They benefit from the leverage this interpretation provides in negotiations over policy and resources, including the perceived right to negotiate exit under duress.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, provinces_asserting_autonomy, beneficiary,
    institutional, generational, constrained, national).

% This aspect of the federal government seeks to maintain national unity and stability through negotiation and compromise with provinces, accepting that federal authority is often conditional on provincial consent. It benefits from the legitimacy and reduced conflict that this consensual approach can bring, even if it limits unilateral action.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, federal_government_seeking_consensus, beneficiary,
    institutional, generational, constrained, national).

% This aspect of the federal government aims to implement national policies (e.g., climate change, social programs) uniformly across the country, often preferring to act without requiring explicit provincial consent. It 'pays' by having its authority constrained by provincial demands for consultation, negotiation, and potential override, leading to delays or diluted policies.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, federal_government_seeking_unilateral_action, payer,
    institutional, biographical, constrained, national).

% These provinces rely on federal equalization payments to provide comparable public services. Under a 'compact federalism' reading that emphasizes provincial resource sovereignty and conditional federal authority, their ability to demand greater equalization or federal intervention in resource-rich provinces' affairs may be limited, effectively 'paying' by receiving less redistribution.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, provinces_seeking_equalization, payer,
    institutional, generational, constrained, national).

% The highest judicial authority, responsible for interpreting the Constitution and adjudicating disputes between federal and provincial governments. Its rulings shape the legal boundaries of sovereignty, influencing the balance of power and the interpretation of federalism. It sets the agenda for constitutional discourse.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, supreme_court_of_canada, agenda_setter,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__compact_federalism, provinces_asserting_autonomy).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__compact_federalism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the division of powers and responsibilities between federal and provincial governments, providing a framework for intergovernmental relations, policy implementation, and dispute resolution within a multi-level governance system.
% TRANSFER_FUNCTION: Transfers political leverage and policy autonomy to provinces, allowing them to assert control over areas like resources and social policy, while requiring the federal government to negotiate and seek consent for national initiatives.
% ABSENT_VOICES: A unified 'Canadian people' as a single sovereign entity is often absent from the compact federalism discourse, which prioritizes provincial interests. Such a voice would argue for stronger federal powers to ensure national standards and collective action, potentially viewing provincial autonomy as an impediment.
% DISAPPEARANCE_RATIONALE: If the compact federalism reading vanished, the entire structure of federal-provincial relations would collapse. Provinces would either assert absolute sovereignty (leading to potential secession or a much looser confederation) or be fully subordinated to federal authority, fundamentally altering the Canadian political landscape.
% FOUNDING_PROBLEM: The original problem was how to unite diverse colonial entities into a single nation while preserving their distinct identities and local governance, balancing central authority with regional autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists widely corroborate that balancing central and regional powers remains a live and ongoing challenge in Canadian federalism. Provincial premiers, federal leaders, and constitutional scholars consistently engage in debates and negotiations that reflect this foundational tension, confirming its persistence from outside the immediate beneficiaries of any single reading.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__compact_federalism, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__compact_federalism, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__compact_federalism, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(provincial_sovereignty_boundary__compact_federalism, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__compact_federalism_tests).
:- end_tests(provincial_sovereignty_boundary__compact_federalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates federal-provincial relations (beneficiaries: provinces asserting autonomy, federal government seeking consensus) but also involves asymmetric extraction (victims: federal government seeking unilateral action, provinces seeking equalization) and requires active enforcement (e.g., through constitutional challenges and political negotiation) to maintain its specific balance. Extractiveness is moderate (0.45) as provinces concede some federal authority but retain significant leverage. Suppression is low (0.3) because provinces have strong political and legal avenues to resist federal overreach, and the threat of exit, while not easy, is not entirely suppressed. Theater ratio is low (0.1) as the negotiations and disputes are genuine contests over power, not mere performance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of provinces asserting autonomy, this is a legitimate coordination mechanism that preserves their foundational status. From the perspective of the federal government seeking to implement national policies, it can be seen as an extractive constraint that forces concessions and limits effective governance. Provinces seeking equalization may view it as extractive if it prioritizes resource-rich provinces' autonomy over national wealth redistribution.
 *
 * DIRECTIONALITY LOGIC:
 *   Provinces asserting autonomy are beneficiaries (d=0.15) as the constraint grants them significant leverage. The federal government seeking consensus is also a beneficiary (d=0.3) as it provides a framework for stable governance, even if negotiated. The federal government seeking unilateral action is a victim (d=0.8) as its power is curtailed. Provinces seeking equalization are victims (d=0.7) if the compact federalism reading allows resource-rich provinces to retain more wealth. The Supreme Court is an analytical observer/agenda setter (d=0.5) as it interprets the rules.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (ignoring the extraction from federal unilateralism and some provinces) or a pure Snare (ignoring the genuine coordination function and benefits to autonomous provinces). The 'compact federalism' reading, while contested, serves a live function in mediating federal-provincial power dynamics, preventing mandatrophy by actively shaping ongoing political and legal contests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compact_vs_constitutional_origin,
    'Is the Canadian Confederation fundamentally a compact among sovereign provinces, or a constitutional act creating subordinate provinces?',
    'Supreme Court ruling explicitly affirming one origin theory over the other, or a constitutional amendment clarifying provincial status.',
    'If affirmed as a compact, provincial autonomy and exit options are strengthened; if affirmed as constitutional subordination, federal authority is enhanced, and provincial exit is foreclosed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compact_vs_constitutional_origin, conceptual, 'Ambiguity regarding the foundational nature of Canadian federalism.').

omega_variable(
    resource_sovereignty_scope,
    'Does provincial ownership of natural resources (s.92A) imply absolute territorial sovereignty, or is it limited by federal powers?',
    'Supreme Court clarification on the scope of s.92A in relation to federal environmental or economic powers.',
    'If absolute, provinces gain significant leverage over federal policy; if limited, federal authority can override provincial resource decisions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_sovereignty_scope, empirical, 'The extent to which resource ownership translates to broader provincial sovereignty.').

omega_variable(
    exit_negotiability_under_duress,
    'Is provincial exit from Confederation genuinely negotiable under duress, or does it require federal consent and constitutional amendment?',
    'A clear legal framework for secession, or a precedent-setting negotiation that establishes the terms of exit.',
    'If negotiable, provinces have a credible threat for leverage; if requiring federal consent, their bargaining power is significantly reduced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exit_negotiability_under_duress, preference, 'The legal and political feasibility of provincial secession.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__compact_federalism, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t0, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0, 0.15).
narrative_ontology:measurement(prov_tr_t10, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 10, 0.12).
narrative_ontology:measurement(prov_tr_t20, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 20, 0.11).
narrative_ontology:measurement(prov_tr_t30, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(prov_be_t0, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(prov_be_t10, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(prov_be_t20, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 20, 0.43).
narrative_ontology:measurement(prov_be_t30, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t0, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(prov_su_t10, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(prov_su_t20, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 20, 0.29).
narrative_ontology:measurement(prov_su_t30, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 30, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__compact_federalism, enforcement_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, constitutional_subordination).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, resource_sovereignty_primacy).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'provincial_sovereignty_boundary' kernel. Its structural delta (federal authority conditional on provincial consent, equalization negotiable, climate policy subject to provincial override, exit requires negotiation not permission) differentiates it from sibling readings like 'constitutional_subordination' and 'resource_sovereignty_primacy', which assert different power balances and exit conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
