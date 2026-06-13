% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__integration_reading, []).

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
 *   constraint_id: federation_membership_kernel__integration_reading
 *   human_readable: EU Free Movement (Integration Reading)
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint represents the 'integration_reading' of the EU's free
 *   movement principle, where supranational authority (ECJ) interprets the
 *   scope expansively to maximize labor mobility and equal treatment across
 *   member states. This reading prioritizes the completion of the single
 *   market and the rights of EU citizens to move and reside freely, often at
 *   the expense of national welfare state autonomy and local labor market
 *   protections. The constraint is claimed as a Tangled Rope due to its
 *   genuine coordination function (single market, citizen rights) coupled
 *   with significant asymmetric extraction (costs borne by specific member
 *   states and labor groups).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, 0.65).
domain_priors:suppression_score(federation_membership_kernel__integration_reading, 0.7).
domain_priors:theater_ratio(federation_membership_kernel__integration_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__integration_reading, "EU Free Movement (Integration Reading)").
narrative_ontology:topic_domain(federation_membership_kernel__integration_reading, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__integration_reading, '2cd1f19f-9818-43fa-bff4-512ad1b9077e').
narrative_ontology:cs_kernel_codification('2cd1f19f-9818-43fa-bff4-512ad1b9077e', fixed_text).
narrative_ontology:cs_authority_grounding('2cd1f19f-9818-43fa-bff4-512ad1b9077e', lineage).
narrative_ontology:cs_interpretation_layer_present('2cd1f19f-9818-43fa-bff4-512ad1b9077e').
narrative_ontology:cs_reading_relation('2cd1f19f-9818-43fa-bff4-512ad1b9077e', federation_membership_kernel__member_sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('2cd1f19f-9818-43fa-bff4-512ad1b9077e', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('2cd1f19f-9818-43fa-bff4-512ad1b9077e', foundational, free_movement_as_fundamental_right).
narrative_ontology:cs_axiom_status(free_movement_as_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('2cd1f19f-9818-43fa-bff4-512ad1b9077e', free_movement_as_fundamental_right, deontological).
narrative_ontology:cs_axiom('2cd1f19f-9818-43fa-bff4-512ad1b9077e', foundational, single_market_completion_priority).
narrative_ontology:cs_axiom_status(single_market_completion_priority, holdable).
narrative_ontology:cs_axiom_grounding('2cd1f19f-9818-43fa-bff4-512ad1b9077e', single_market_completion_priority, instrumental).
narrative_ontology:cs_reference_frame('2cd1f19f-9818-43fa-bff4-512ad1b9077e', ever_closer_union_principle).
narrative_ontology:cs_drift_state('2cd1f19f-9818-43fa-bff4-512ad1b9077e', contemporary_eurozone_crises_and_migration_pressures, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2cd1f19f-9818-43fa-bff4-512ad1b9077e', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__integration_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, mobile_eu_citizens).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, eu_employers).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, receiving_state_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, sending_state_tax_base).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the right to live and work freely across EU member states, accessing diverse labor markets and social benefits. Their mobility is a core right protected by this reading.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, mobile_eu_citizens, beneficiary,
    moderate, biographical, mobile, continental).

% Benefit from access to a larger, more flexible labor pool across the EU, allowing them to optimize labor costs and skill matching. They lobby for expansive free movement.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, eu_employers, beneficiary,
    organized, biographical, arbitrage, continental).

% Face increased competition for jobs and downward pressure on wages in receiving states due to an influx of mobile EU citizens. Their options are limited by local labor market conditions and lack of mobility.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, displaced_local_labor, payer,
    powerless, immediate, trapped, local).

% Bear the fiscal costs of providing social benefits and public services to mobile EU citizens, often without corresponding tax contributions or fiscal transfers from the EU budget. Their ability to restrict access is suppressed by ECJ rulings.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_welfare_systems, payer,
    institutional, generational, constrained, national).

% Suffers from brain drain as skilled workers migrate to wealthier EU states, eroding the tax base and human capital needed for national development. Their ability to retain talent is limited by free movement.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, sending_state_tax_base, payer,
    institutional, generational, constrained, national).

% Acts as the guardian of the treaties, promoting and enforcing the expansive interpretation of free movement. It benefits from increased supranational authority and the deepening of EU integration.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, european_commission, agenda_setter,
    institutional, generational, analytical, continental).

% The ultimate arbiter of EU law, consistently issuing rulings that expand the scope of free movement rights and limit member state derogations. Its authority is central to this reading's persistence.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, ecj, agenda_setter,
    institutional, civilizational, analytical, continental).

% Advocate for greater national control over welfare access and labor market policies, arguing that free movement should be balanced against national sovereignty. Their policy space is actively constrained by ECJ jurisprudence.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, national_governments_member_sovereignty_advocates, excluded,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__integration_reading, european_commission).
narrative_ontology:fixing_cost_class(federation_membership_kernel__integration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified labor market and ensures equal treatment for EU citizens across member states, reducing transaction costs for cross-border employment and residency.
% TRANSFER_FUNCTION: Transfers the right to reside and work freely (and access associated welfare benefits) to mobile EU citizens, while transferring fiscal and social costs to receiving state welfare systems and economic costs to displaced local labor and sending state tax bases.
% ABSENT_VOICES: National governments advocating for greater member state sovereignty and local labor unions concerned about wage depression are structurally excluded from the ECJ's interpretive process, which prioritizes supranational integration. They would argue for a more balanced interpretation of free movement.
% DISAPPEARANCE_RATIONALE: If the expansive interpretation of free movement vanished, national borders would immediately become more restrictive, labor mobility would plummet, and the EU single market would fragment. Member states would reassert control over welfare access and migration, fundamentally altering the European political and economic landscape.
% FOUNDING_PROBLEM: The original problem was to overcome national barriers to trade and labor mobility, fostering economic integration and preventing discrimination against citizens of other member states.
% FOUNDING_PROBLEM_CORROBORATION: The European Commission and ECJ attest that the founding problem of market fragmentation and discrimination remains live, requiring continuous enforcement. However, national governments and academic critics (outside the benefiting parties) argue that while the original problem is largely solved, the current expansive interpretation has created new problems of fiscal strain and social dumping, suggesting the constraint's function has drifted beyond its original mandate.
narrative_ontology:disappearance_verdict(federation_membership_kernel__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__integration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__integration_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership_kernel__integration_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the expansive interpretation of free movement leads to significant fiscal and social costs for receiving states, without corresponding fiscal transfers or compensation. Suppression is also high (0.7) as ECJ rulings actively override national attempts to limit welfare access or protect local labor, effectively suppressing member state sovereignty in these areas. Theater ratio is low (0.1) as the ECJ's enforcement is direct and effective, not performative. Accessibility collapse is moderate (0.4) as member states retain some policy levers, but their ability to use them is constrained by supranational law. Resistance is high (0.6) from national governments and local populations facing the costs.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of mobile EU citizens and EU employers, this constraint is a Rope, facilitating beneficial labor mobility and market access. From the perspective of receiving state welfare systems and displaced local labor, it operates as a Snare, imposing costs without commensurate benefits and suppressing national policy choices. The ECJ, as the agenda-setter, views it as a Mountain (fundamental right) or Rope (essential coordination).
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile EU citizens and EU employers are primary beneficiaries (d near 0.0) due to enhanced labor mobility and access to wider talent pools. Receiving state welfare systems and displaced local labor are victims (d near 1.0) as they bear the fiscal and social costs without direct compensation or protection. Sending state tax bases are also victims due to brain drain. The ECJ is the agenda-setter, enforcing the expansive interpretation (d near 0.15, as it benefits from increased supranational authority).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (single market, citizen rights) is still live, but its expansive interpretation has led to a significant shift in its operational character. What began as a coordination mechanism has accumulated substantial extractive elements, particularly for specific member states and labor groups. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring coordination). The rising extractiveness and suppression over time indicate an enforcement ratchet, where the coordination function is increasingly leveraged for asymmetric transfers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine expression of EU integration, or an extractive interpretation of free movement?',
    'Analysis of ECJ jurisprudence evolution and its impact on national welfare systems and labor markets, compared against the original treaties'' intent.',
    'If extractive, the classification shifts from Tangled Rope to Snare for receiving states and local labor; if genuine integration, the coordination function is affirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''integration_reading'' of the ''federation_membership_kernel''.').

omega_variable(
    welfare_system_sustainability,
    'Can receiving state welfare systems sustain the fiscal and social costs of expansive free movement without fiscal transfers or policy harmonization?',
    'Empirical studies on long-term fiscal impacts, social cohesion metrics, and comparative analysis with other federal systems'' fiscal equalization mechanisms.',
    'If unsustainable, the ''integration_reading'' becomes a Snare for receiving states, requiring either fiscal transfers (shifting the burden) or re-evaluation of the reading''s scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_system_sustainability, empirical, 'Sustainability of welfare systems under current free movement interpretation.').

omega_variable(
    member_sovereignty_clash,
    'How would a ''member_sovereignty_reading'' alter the victim set and extractiveness for national labor markets and welfare systems?',
    'Counterfactual analysis: if member states could restrict economically inactive migrants, the burden on welfare systems would decrease, but mobile citizens'' rights would be curtailed.',
    'A ''member_sovereignty_reading'' would shift extraction from receiving states to mobile citizens, potentially reclassifying the constraint as a Rope for member states and a Snare for mobile citizens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(member_sovereignty_clash, conceptual, 'Impact of a ''member_sovereignty_reading'' on free movement.').

omega_variable(
    welfare_coordination_alternative,
    'What would be the classification impact of adopting a ''welfare_coordination_reading''?',
    'Modeling the effects of EU-enforced anti-social-dumping rules combined with member state welfare design autonomy on labor mobility and welfare costs.',
    'A ''welfare_coordination_reading'' would likely reduce extractiveness on receiving state welfare systems and local labor, potentially reclassifying the constraint as a Rope or Tangled Rope with more balanced benefits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_coordination_alternative, conceptual, 'Impact of a ''welfare_coordination_reading'' on free movement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__integration_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_kernel__integration_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fede_tr_t10, federation_membership_kernel__integration_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(fede_tr_t20, federation_membership_kernel__integration_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(fede_tr_t30, federation_membership_kernel__integration_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_kernel__integration_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fede_be_t10, federation_membership_kernel__integration_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(fede_be_t20, federation_membership_kernel__integration_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(fede_be_t30, federation_membership_kernel__integration_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_kernel__integration_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(fede_su_t10, federation_membership_kernel__integration_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(fede_su_t20, federation_membership_kernel__integration_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(fede_su_t30, federation_membership_kernel__integration_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__integration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, member_sovereignty_kernel__member_sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, federation_membership_kernel__welfare_coordination_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, eu_single_market_regulation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'federation_membership_kernel'. Its expansive interpretation of free movement directly influences the operational space and legitimacy claims of the 'member_sovereignty_reading' and 'welfare_coordination_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
