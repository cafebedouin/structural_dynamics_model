% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__livelihood_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__livelihood_security_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: performance_legitimacy__livelihood_security_reading
 *   human_readable: Livelihood Security Legitimacy Constraint
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   The state's legitimacy rests on delivering tangible livelihood
 *   improvements — employment, healthcare, education, elderly care — that
 *   citizens directly experience. This reading of the performance legitimacy
 *   kernel makes service delivery and social safety net the primary
 *   constraints on policy: consumption support is prioritized over
 *   investment, redistribution mechanisms are strengthened, and the fiscal
 *   burden falls on capital-intensive industrial expansion and local
 *   government infrastructure spending. The constraint is a tangled rope: it
 *   solves a genuine coordination problem (universal service provision in a
 *   market economy with gaps) but operates through asymmetric extraction from
 *   industrial and infrastructure sectors. The claimed type is tangled_rope;
 *   the metrics describe rising extractiveness and theater as demographic
 *   pressures and fiscal stress intensify.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, 0.62).
domain_priors:suppression_score(performance_legitimacy__livelihood_security_reading, 0.48).
domain_priors:theater_ratio(performance_legitimacy__livelihood_security_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__livelihood_security_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__livelihood_security_reading, "Livelihood Security Legitimacy Constraint").
narrative_ontology:topic_domain(performance_legitimacy__livelihood_security_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__livelihood_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__livelihood_security_reading, '59473be5-5b0f-4566-931a-4db78a46624f').
narrative_ontology:cs_kernel_codification('59473be5-5b0f-4566-931a-4db78a46624f', implicit).
narrative_ontology:cs_authority_grounding('59473be5-5b0f-4566-931a-4db78a46624f', extraction).
narrative_ontology:cs_interpretation_layer_present('59473be5-5b0f-4566-931a-4db78a46624f').
narrative_ontology:cs_reading_relation('59473be5-5b0f-4566-931a-4db78a46624f', performance_legitimacy__quantitative_growth_reading, coexists_with).
narrative_ontology:cs_reading_relation('59473be5-5b0f-4566-931a-4db78a46624f', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('59473be5-5b0f-4566-931a-4db78a46624f', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_axiom('59473be5-5b0f-4566-931a-4db78a46624f', foundational, citizen_welfare_as_primary_legitimacy).
narrative_ontology:cs_axiom_status(citizen_welfare_as_primary_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('59473be5-5b0f-4566-931a-4db78a46624f', citizen_welfare_as_primary_legitimacy, deontological).
narrative_ontology:cs_axiom('59473be5-5b0f-4566-931a-4db78a46624f', secondary, redistribution_from_investment_to_consumption).
narrative_ontology:cs_axiom_status(redistribution_from_investment_to_consumption, holdable).
narrative_ontology:cs_axiom_grounding('59473be5-5b0f-4566-931a-4db78a46624f', redistribution_from_investment_to_consumption, empirically_contingent).
narrative_ontology:cs_reference_frame('59473be5-5b0f-4566-931a-4db78a46624f', performance_bargain).
narrative_ontology:cs_drift_state('59473be5-5b0f-4566-931a-4db78a46624f', post_2015_new_normal, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('59473be5-5b0f-4566-931a-4db78a46624f', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__livelihood_security_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, service_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, household_consumption).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, capital_intensive_industry).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, local_government_infrastructure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, central_state_leadership).
narrative_ontology:constraint_vindicates(performance_legitimacy__livelihood_security_reading, livelihood_security_as_legitimacy_basis).
narrative_ontology:constraint_vindicates(performance_legitimacy__livelihood_security_reading, consumption_priority_over_investment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the legitimacy bargain: regime survival depends on delivering tangible livelihood improvements. Controls fiscal allocation between investment and consumption. Benefits from stabilized legitimacy but bears political risk if delivery fails. Can pivot policy framing but not abandon the bargain without legitimacy collapse.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, central_state_leadership, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__livelihood_security_reading, central_state_leadership, beneficiary).

% Healthcare, education, elderly care, and employment service providers receive prioritized fiscal flows. Their expansion is the visible proof of legitimacy delivery. They lobby for sustained funding but can exit to private provision if public funding becomes unreliable.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, service_sectors, beneficiary,
    organized, biographical, mobile, national).

% Urban and rural households directly experience service improvements: subsidized healthcare, education access, pension coverage, employment programs. Their perceived wellbeing is the legitimacy metric. Exit is constrained — they cannot easily leave the polity, but can express discontent through localized protests or migration.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, household_consumption, beneficiary,
    organized, biographical, constrained, national).

% Heavy manufacturing, infrastructure construction, and strategic sectors face reduced credit allocation, higher compliance costs, and mandated wage/benefit increases. They argue this undermines long-term competitiveness. Exit is constrained by market access dependence and regulatory licensing — they can lobby, relocate marginally, but cannot escape the fiscal squeeze.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, capital_intensive_industry, payer,
    powerful, biographical, constrained, national).

% County and municipal governments bear unfunded mandates for service delivery while central transfers shift to consumption subsidies. Infrastructure budgets are cut; land finance revenue declines. They implement the mandate but lack fiscal autonomy. Exit is institutionally impossible — they are the delivery layer.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, local_government_infrastructure, payer,
    institutional, biographical, constrained, regional).

% Foreign capital seeking exposure to industrial upgrading and infrastructure returns finds policy environment shifting against capital-intensive sectors. They would argue for investment-led growth but have no formal voice in the legitimacy bargain. Exit is mobile — capital reallocation to competing economies.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, international_investors, excluded,
    powerful, biographical, mobile, global).

% Analyze whether consumption-led legitimacy delivery is sustainable, whether it avoids middle-income trap, and whether the fiscal arithmetic holds. They see the full structural trade-off but hold no enforcement power. Their exit is analytical — they change frameworks, not positions.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, development_economists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Delivers essential services (employment, healthcare, education, elderly care) that citizens directly experience, solving the coordination problem of universal service provision in a developing economy where market provision leaves gaps.
% TRANSFER_FUNCTION: Moves fiscal resources and credit allocation from capital-intensive industrial investment and local infrastructure spending toward service sectors and household consumption subsidies, mediated through central fiscal policy and local mandate structures.
% ABSENT_VOICES: Rural populations dependent on infrastructure investment for market access, future generations who bear the cost of deferred industrial upgrading and productivity growth, and local officials constrained by unfunded service mandates without corresponding revenue authority.
% DISAPPEARANCE_RATIONALE: If the livelihood security legitimacy constraint vanished overnight, fiscal priority would shift back to industrial investment and infrastructure, service sectors would face funding cuts, the social safety net would erode, and the legitimacy bargain would revert to growth-first performance — triggering a restructuring of the state-society compact.
% FOUNDING_PROBLEM: Post-reform legitimacy crisis: rapid growth created inequality and left basic services inadequate, threatening regime stability as citizens experienced rising costs in healthcare, education, housing, and elderly care without corresponding security.
% FOUNDING_PROBLEM_CORROBORATION: Independent scholars (Wang Feng on demographic pressures and pension gaps, Cai Yongshun on local fiscal stress and unfunded mandates, Martin Whyte on inequality perception) attest the founding problem persists; state media and policy documents acknowledge ongoing service gaps and demographic challenges as active threats to legitimacy.
narrative_ontology:disappearance_verdict(performance_legitimacy__livelihood_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__livelihood_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__livelihood_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(performance_legitimacy__livelihood_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__livelihood_security_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__livelihood_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__livelihood_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises (0.45→0.68) as the fiscal transfer from investment to consumption deepens under demographic aging and slowing growth. Suppression requirement increases (0.35→0.58) as local governments resist unfunded mandates and industrial sectors lobby against credit reallocation — enforcement shifts from policy guidance to binding fiscal rules. Theater ratio climbs (0.25→0.48) as visible service expansions (new hospitals, pension adjustments) coexist with quality dilution and access rationing. The measurement series share one time grid so every metric is authored at each examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the central leadership seat, the constraint is coordination: it solves the legitimacy-service delivery nexus. From capital-intensive industry and local government seats, the same structure operates as extraction: their resources are commandeered for a bargain they did not negotiate. The engine computes this divergence; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Central leadership is the agenda-setter with arbitrage-grade exit (can reframe the bargain); its directionality is near-beneficiary (d~0.15) because legitimacy collection subsidizes its position. Service sectors and households are beneficiaries with mobile/constrained exit — d~0.25-0.35. Capital-intensive industry and local infrastructure are payers with constrained exit — d~0.75-0.85. International investors are excluded (not coordinated, not extracting from them directly). The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-reform inequality and service gaps) remains live per independent corroboration. The constraint has not atrophied into piton — extraction is rising, not theatrical. But the coordination function is straining: demographic aging makes the service promise increasingly expensive, and the extraction from investment undermines the productivity growth that funds the services. This is a tangled rope approaching a sustainability boundary, not a degraded piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Is the service delivery coordination function structurally inseparable from the extraction from investment, or can universal services be funded without suppressing industrial upgrading?',
    'Counterfactual fiscal modeling: whether productivity gains from human capital investment (health/education) offset the industrial investment reduction within a 15-year horizon, using comparable East Asian transitions as reference.',
    'If inseparable, the extraction is the price of coordination — the constraint is a genuine tangled rope. If separable, the extraction is a policy choice — the constraint leans toward snare for the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether livelihood security delivery requires extraction from industrial investment or is a fiscal allocation choice.').

omega_variable(
    committer_kernel_structure,
    'How does this reading''s structural position within the performance_legitimacy kernel affect its classification stability?',
    'Track policy discourse shifts: if central documents increasingly frame legitimacy in livelihood security terms while marginalizing growth/techno-nationalist framings, this reading becomes the dominant constraint; if framing oscillates, the kernel remains contested and this reading''s extraction profile fluctuates.',
    'If this reading becomes hegemonic, its extraction profile stabilizes as the primary legitimacy constraint. If the kernel stays contested, this reading''s enforcement intensity varies with factional balance, making its classification temporally unstable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_structure, conceptual, 'Commitment-system framing: this reading''s structural relationship to sibling readings in the performance_legitimacy kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (unfunded mandates, credit reallocation) structural fiscal pressure, or does it include internalized compliance where local officials and industrial managers self-censor policy dissent?',
    'Compare suppression metrics during policy relaxation windows (e.g., 2020 pandemic stimulus) vs. tightening phases: if suppression drops sharply when central pressure eases, it is primarily structural; if it persists, internalized compliance is significant.',
    'If internalized, effective suppression is higher than the structural measure — payer seats carry the constraint internally, making exit less effective at reducing χ.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the payer seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__livelihood_security_reading, 2015, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t2015, performance_legitimacy__livelihood_security_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(perf_tr_t2018, performance_legitimacy__livelihood_security_reading, theater_ratio, 2018, 0.32).
narrative_ontology:measurement(perf_tr_t2021, performance_legitimacy__livelihood_security_reading, theater_ratio, 2021, 0.38).
narrative_ontology:measurement(perf_tr_t2024, performance_legitimacy__livelihood_security_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement(perf_tr_t2027, performance_legitimacy__livelihood_security_reading, theater_ratio, 2027, 0.45).
narrative_ontology:measurement(perf_tr_t2030, performance_legitimacy__livelihood_security_reading, theater_ratio, 2030, 0.48).

% Extraction over time
narrative_ontology:measurement(perf_be_t2015, performance_legitimacy__livelihood_security_reading, base_extractiveness, 2015, 0.45).
narrative_ontology:measurement(perf_be_t2018, performance_legitimacy__livelihood_security_reading, base_extractiveness, 2018, 0.52).
narrative_ontology:measurement(perf_be_t2021, performance_legitimacy__livelihood_security_reading, base_extractiveness, 2021, 0.58).
narrative_ontology:measurement(perf_be_t2024, performance_legitimacy__livelihood_security_reading, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement(perf_be_t2027, performance_legitimacy__livelihood_security_reading, base_extractiveness, 2027, 0.65).
narrative_ontology:measurement(perf_be_t2030, performance_legitimacy__livelihood_security_reading, base_extractiveness, 2030, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t2015, performance_legitimacy__livelihood_security_reading, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement(perf_su_t2018, performance_legitimacy__livelihood_security_reading, suppression_requirement, 2018, 0.42).
narrative_ontology:measurement(perf_su_t2021, performance_legitimacy__livelihood_security_reading, suppression_requirement, 2021, 0.48).
narrative_ontology:measurement(perf_su_t2024, performance_legitimacy__livelihood_security_reading, suppression_requirement, 2024, 0.52).
narrative_ontology:measurement(perf_su_t2027, performance_legitimacy__livelihood_security_reading, suppression_requirement, 2027, 0.55).
narrative_ontology:measurement(perf_su_t2030, performance_legitimacy__livelihood_security_reading, suppression_requirement, 2030, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__livelihood_security_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__livelihood_security_reading, 0.15).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is the livelihood_security_reading of the performance_legitimacy kernel. It decomposes the kernel into a specific constraint: service delivery and social safety net as primary legitimacy constraints, with consumption prioritized over investment. The sibling readings (quantitative_growth, qualitative_development, techno_nationalist) are separate constraints with distinct ε values, beneficiaries, and victims, linked here as a constraint family. The ε-invariance principle applies: each reading measures extraction against the same standing arrangement (the state's legitimacy bargain) but from different structural premises, yielding different ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy__livelihood_security_reading, institutional, 0.15).
constraint_indexing:directionality_override(performance_legitimacy__livelihood_security_reading, powerful, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
