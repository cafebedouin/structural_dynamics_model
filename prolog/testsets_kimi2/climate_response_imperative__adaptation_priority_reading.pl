% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__adaptation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__adaptation_priority_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: climate_response_imperative__adaptation_priority_reading
 *   human_readable: Climate Response Imperative: Adaptation Priority Reading
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This constraint is the adaptation-priority reading of the contested
 *   climate_response_imperative kernel. It treats resilience-building and
 *   damage reduction in exposed regions as the primary climate response, with
 *   mitigation as aspirational. The sibling readings are
 *   mitigation_priority_reading (emissions reduction first) and
 *   degrowth_reading (structural economic transformation). The expected
 *   structural delta for this reading is that present-day developing nations
 *   enter the victim set through immediate capital requirements they cannot
 *   meet, creating a vicious circle where those least responsible bear the
 *   highest costs. The constraint is authored as a tangled_rope because it
 *   combines a genuine coordination need (vulnerable regions require
 *   protection) with asymmetric extraction (wealthy nations defer mitigation
 *   while poor nations assume debt and diversion).
 *
 * KEY AGENTS:
 *   - Developed nations (Global North): Primary agenda-setter and beneficiary (institutional/arbitrage) â defer mitigation costs, control finance architecture, preserve growth pathways.
 *   - Multilateral climate institutions: Agenda-setter (institutional/arbitrage) â administer adaptation finance, certify projects, sustain the policy frame.
 *   - Developing nations (Global South exposed states): Primary payer (moderate/constrained) â bear adaptation capital requirements, conditionality, and diverted development budgets.
 *   - Climate-vulnerable populations: Secondary payer (powerless/trapped) â bear physical damages and displacement despite resilience rhetoric.
 *   - Resilience industry: Beneficiary (organized/mobile) â profits from adaptation contracts and technologies.
 *   - Future generations: Excluded (powerless/trapped) â cannot contest deferred mitigation.
 *   - Climate justice movements: Observer (organized/analytical) â document asymmetry and advocate for alternative frames.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, 0.72).
domain_priors:suppression_score(climate_response_imperative__adaptation_priority_reading, 0.65).
domain_priors:theater_ratio(climate_response_imperative__adaptation_priority_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__adaptation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__adaptation_priority_reading, "Climate Response Imperative: Adaptation Priority Reading").
narrative_ontology:topic_domain(climate_response_imperative__adaptation_priority_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__adaptation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__adaptation_priority_reading, '093db67a-7b6f-4986-a402-dcf00bf655f2').
narrative_ontology:cs_kernel_codification('093db67a-7b6f-4986-a402-dcf00bf655f2', distributed).
narrative_ontology:cs_authority_grounding('093db67a-7b6f-4986-a402-dcf00bf655f2', distributed).
narrative_ontology:cs_reading_relation('093db67a-7b6f-4986-a402-dcf00bf655f2', climate_response_imperative__mitigation_priority_reading, influences).
narrative_ontology:cs_reading_relation('093db67a-7b6f-4986-a402-dcf00bf655f2', climate_response_imperative__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('093db67a-7b6f-4986-a402-dcf00bf655f2', foundational, adaptation_as_primary_climate_response).
narrative_ontology:cs_axiom_status(adaptation_as_primary_climate_response, holdable).
narrative_ontology:cs_axiom_grounding('093db67a-7b6f-4986-a402-dcf00bf655f2', adaptation_as_primary_climate_response, conventional).
narrative_ontology:cs_axiom('093db67a-7b6f-4986-a402-dcf00bf655f2', foundational, deferrable_mitigation_under_capital_constraints).
narrative_ontology:cs_axiom_status(deferrable_mitigation_under_capital_constraints, holdable).
narrative_ontology:cs_axiom_grounding('093db67a-7b6f-4986-a402-dcf00bf655f2', deferrable_mitigation_under_capital_constraints, instrumental).
narrative_ontology:cs_reference_frame('093db67a-7b6f-4986-a402-dcf00bf655f2', resilience_first_framework).
narrative_ontology:cs_drift_state('093db67a-7b6f-4986-a402-dcf00bf655f2', post_glasgow_finance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('093db67a-7b6f-4986-a402-dcf00bf655f2', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__adaptation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, developed_nations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, resilience_industry).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, developing_nations).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, climate_vulnerable_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dominate UNFCCC negotiations, IMF and World Bank conditionality, and COP agendas. Benefit from deferred decarbonization costs and continued fossil-fuel-dependent growth by framing climate response as adaptation rather than mitigation. Can exit or reshape the framework via diplomatic or fiscal policy shifts.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, developed_nations, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__adaptation_priority_reading, developed_nations, beneficiary).

% Administer adaptation finance funds, set reporting standards for National Adaptation Plans and NDCs, and certify resilience projects. Their institutional mandate and staffing depend on the adaptation-priority architecture. They enforce the framing through grant and loan allocation rules and monitoring regimes.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, multilateral_climate_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Must prepare National Adaptation Plans and incur capital expenditures for resilience infrastructure to access climate finance. Finance is predominantly loan-based and condition-laden, diverting domestic budgets from health, education, and self-directed development. Cannot opt out without losing access to principal international climate funding streams.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, developing_nations, payer,
    moderate, biographical, constrained, national).

% Bear the physical costs of climate impacts and displacement despite resilience projects. Receive adaptation interventions designed by external consultants that often overwrite local knowledge. No exit from rising seas, drought, or storm intensity; no meaningful voice in how adaptation finance is spent.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, climate_vulnerable_populations, payer,
    powerless, immediate, trapped, local).

% Private contractors, consultancies, and technology vendors that design and build adaptation infrastructure. Profit directly from adaptation grants and loans. Can move between markets and jurisdictions, extracting rents from project-based resilience finance.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, resilience_industry, beneficiary,
    organized, biographical, mobile, global).

% Will inhabit the atmosphere produced by deferred mitigation. Cannot participate in present negotiations and are systematically discounted by biographical-horizon decision makers. Their exclusion is structural to a framework that treats mitigation as aspirational.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, future_generations, excluded,
    powerless, civilizational, trapped, global).

% Document the asymmetry between historical responsibility and present vulnerability. Advocate for grant-based non-conditional finance and mandatory mitigation. Do not collect from the constraint but produce the analytical frame that identifies its extractive structure.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, climate_justice_movements, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__adaptation_priority_reading, developed_nations).
narrative_ontology:fixing_cost_class(climate_response_imperative__adaptation_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international capital flows toward immediate resilience needs in climate-exposed regions, establishing a common damage-reduction framework where no single vulnerable nation can protect itself alone.
% TRANSFER_FUNCTION: Moves adaptation finance, debt obligations, and infrastructure costs from multilateral institutions and donor states to developing nations and local populations; simultaneously transfers the deferred costs of mitigation to future generations and the atmospheric commons.
% ABSENT_VOICES: Future generations who cannot negotiate for mitigation; degrowth advocates excluded from mainstream finance architecture; indigenous communities whose territorial adaptation knowledge is overwritten by state-led resilience frameworks.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority imperative vanished, finance flows would reallocate toward emissions reduction and technological transition; developing nations would face unmitigated damages without the resilience infrastructure pipeline but also without the debt and conditionality; the political economy of climate finance would shift from project-based resilience to regulatory and decarbonization investment.
% FOUNDING_PROBLEM: Climate change is already causing loss and damage in vulnerable regions that lack the capital and technology to protect themselves; a coordinated international response is needed to save lives and infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: IPCC Working Group II and climate-vulnerable nation governments attest to ongoing impacts. Independent climate justice movements and critical development scholars corroborate that the founding problem persists, but attest that the current adaptation-priority arrangement extracts more than it protects; no corroboration from outside the benefiting parties supports the claim that the current arrangement optimally solves the problem.
narrative_ontology:disappearance_verdict(climate_response_imperative__adaptation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__adaptation_priority_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__adaptation_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_imperative__adaptation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__adaptation_priority_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__adaptation_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__adaptation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the framework diverts scarce capital from development to adaptation, imposes debt and conditionality, and allows wealthy nations to defer costly decarbonization. Suppression (0.65) reflects the marginalization of degrowth and mitigation-priority alternatives in mainstream climate finance. Theater_ratio (0.42) captures the growing share of adaptation expenditure that serves diplomatic cover for mitigation inaction rather than genuine resilience. Accessibility_collapse (0.60) models how vulnerable nations, once inside the adaptation-priority finance architecture, find alternatives politically unreachable because exit means losing all international climate support. Resistance (0.55) is driven by climate justice movements and some developing-nation coalitions. The measurement series show extraction and theater rising together over the interval as the architecture matured and finance gaps widened.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (developed nations, multilateral institutions) compute the constraint as coordination they built and maintain for genuine protection; the payer seats (developing nations, vulnerable populations) compute it as extraction that capitalizes their exposure and defers the obligations of the wealthy. The engine derives this divergence from the same structural data: identical scope and enforcement, but diametrically opposed directionality derived from beneficiary versus victim position and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed nations and the resilience industry are declared beneficiaries, yielding low directionality (d near 0.0) and damped effective extraction; they are subsidized by the constraint. Developing nations and climate-vulnerable populations are declared victims/payers, yielding high directionality (d near 1.0) and amplified effective extraction. Multilateral institutions sit near the agenda-setter pole with arbitrage exit, experiencing the constraint as administrative coordination rather than cost. Future generations are identity-locked and powerless, sitting at the extreme target end despite being excluded from the negotiation table.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as either pure coordination (rope) or pure extraction (snare). The founding problem â vulnerable regions need protection â is genuinely live, which blocks snare classification. However, the presence of identifiable victims, active enforcement through conditionality and finance access control, and the absence of a sunset clause block rope classification. The persistent asymmetry where those least responsible bear highest costs is the signature of tangled_rope: coordination and extraction fused in the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    finance_modality_vs_framing,
    'Is the extraction from developing nations caused by the adaptation-priority framing itself, or by the loan-based, conditionality-laden finance modalities layered onto it?',
    'Comparison of grant-based versus debt-based adaptation finance flows and their respective development impacts; historical analysis of structural adjustment parallels.',
    'If extraction is modality-driven, the constraint could be reframed as a rope with cleaner finance; if framing-driven, the extraction is structural to adaptation-priority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(finance_modality_vs_framing, conceptual, 'Whether extraction is inherent to the framing or to its finance instruments.').

omega_variable(
    mitigation_deferral_causality,
    'Does the adaptation-priority reading structurally cause mitigation deferral, or does it merely provide rhetorical cover for mitigation inaction driven by fossil-fuel political economy?',
    'Counterfactual analysis of mitigation investment in jurisdictions with high versus low adaptation-priority policy frames, controlling for fossil fuel dependence.',
    'If causality is cover-story only, the constraint is less extractive than measured; if structural, the extraction includes intergenerational damages from deferred mitigation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mitigation_deferral_causality, empirical, 'Causal role of adaptation priority in mitigation deferral.').

omega_variable(
    kernel_reading_adaptation_delta,
    'How would the stakeholder structure and epsilon change if the mitigation-priority or degrowth reading were adopted as the operative framework?',
    'Sibling reading analysis: mitigation-priority would shift victim set toward fossil-dependent regions and workers; degrowth would redistribute costs to Global North consumers.',
    'The victim/beneficiary structure is reading-dependent, confirming this constraint is one reading of a contested kernel rather than an objective natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_adaptation_delta, conceptual, 'Structural sensitivity of the constraint to kernel reading selection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__adaptation_priority_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__adaptation_priority_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(clim_tr_t6, climate_response_imperative__adaptation_priority_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(clim_tr_t12, climate_response_imperative__adaptation_priority_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(clim_tr_t18, climate_response_imperative__adaptation_priority_reading, theater_ratio, 18, 0.34).
narrative_ontology:measurement(clim_tr_t24, climate_response_imperative__adaptation_priority_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(clim_tr_t30, climate_response_imperative__adaptation_priority_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clim_be_t6, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 6, 0.44).
narrative_ontology:measurement(clim_be_t12, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(clim_be_t18, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 18, 0.62).
narrative_ontology:measurement(clim_be_t24, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(clim_be_t30, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 30, 0.72).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(climate_response_imperative__adaptation_priority_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__adaptation_priority_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, degrowth_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested climate_response_imperative kernel. The adaptation-priority, mitigation-priority, and degrowth readings are structurally distinct constraints with different beneficiary/victim profiles and epsilon values. They form a constraint family linked by shared kernel origin but divergent empirical and normative commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
