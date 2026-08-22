% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__adaptation_priority, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: climate_harm_prevention__adaptation_priority
 *   human_readable: Climate Harm Prevention: Adaptation-Priority Reading
 *   domain: climate_policy_political_economy
 *
 * SUMMARY:
 *   This constraint is one reading of the contested kernel
 *   'climate_harm_prevention'. The adaptation_priority reading holds that
 *   legitimate climate response must prioritize near-term resilience building
 *   because rapid mitigation is politically and economically infeasible; it
 *   accepts a higher warming trajectory and externalizes residual costs to
 *   future generations and low-adaptation-capacity regions. Sibling readings
 *   include mitigation_priority (rapid emissions reduction within growth
 *   framework) and degrowth_reading (planned economic contraction in the
 *   Global North). The constraint coordinates genuine protection for present
 *   vulnerable populations while extracting asymmetrically from those who
 *   cannot exit the warming trajectory.
 *
 * KEY AGENTS:
 *   - Present vulnerable populations: Primary beneficiary (powerless/trapped/local) â receive adaptation resources.
 *   - Fossil fuel incumbents: Primary beneficiary (institutional/arbitrage/global) â avoid mitigation costs and stranded assets.
 *   - Wealthy nation governments: Agenda setter and beneficiary (institutional/arbitrage/national) â set adaptation-priority frameworks and avoid political costs of decarbonization.
 *   - Future generations: Primary target (powerless/trapped/universal) â bear locked-in warming and damages.
 *   - Low-adaptation-capacity regions: Target (powerless/trapped/regional) â suffer residual damages beyond adaptation limits.
 *   - Mitigation advocacy movement: Excluded voice (organized/constrained/global) â structurally marginalized in adaptation-priority governance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_harm_prevention__adaptation_priority, 0.71).
domain_priors:theater_ratio(climate_harm_prevention__adaptation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__adaptation_priority, "Climate Harm Prevention: Adaptation-Priority Reading").
narrative_ontology:topic_domain(climate_harm_prevention__adaptation_priority, "climate_policy_political_economy").

domain_priors:requires_active_enforcement(climate_harm_prevention__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__adaptation_priority, '6aff3d16-8c6b-4138-b7a4-5c0e6b66269e').
narrative_ontology:cs_kernel_codification('6aff3d16-8c6b-4138-b7a4-5c0e6b66269e', distributed).
narrative_ontology:cs_authority_grounding('6aff3d16-8c6b-4138-b7a4-5c0e6b66269e', distributed).
narrative_ontology:cs_reading_relation('6aff3d16-8c6b-4138-b7a4-5c0e6b66269e', climate_harm_prevention__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('6aff3d16-8c6b-4138-b7a4-5c0e6b66269e', climate_harm_prevention__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('6aff3d16-8c6b-4138-b7a4-5c0e6b66269e', foundational, mitigation_infeasibility).
narrative_ontology:cs_axiom_status(mitigation_infeasibility, holdable).
narrative_ontology:cs_axiom_grounding('6aff3d16-8c6b-4138-b7a4-5c0e6b66269e', mitigation_infeasibility, empirically_contingent).
narrative_ontology:cs_axiom('6aff3d16-8c6b-4138-b7a4-5c0e6b66269e', foundational, present_vulnerability_priority).
narrative_ontology:cs_axiom_status(present_vulnerability_priority, holdable).
narrative_ontology:cs_axiom_grounding('6aff3d16-8c6b-4138-b7a4-5c0e6b66269e', present_vulnerability_priority, deontological).
narrative_ontology:cs_reference_frame('6aff3d16-8c6b-4138-b7a4-5c0e6b66269e', incremental_resilience_governance).
narrative_ontology:cs_drift_state('6aff3d16-8c6b-4138-b7a4-5c0e6b66269e', contemporary_climate_policy, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6aff3d16-8c6b-4138-b7a4-5c0e6b66269e', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__adaptation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, present_vulnerable_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, wealthy_nation_governments).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive prioritized adaptation finance, resilient infrastructure, and social protection framed as the only politically tractable climate response; remain geographically exposed to climate shocks but with improved coping capacity.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, present_vulnerable_populations, beneficiary,
    powerless, immediate, trapped, local).

% Continue extraction and asset operation without bearing climate damages or stranded asset risk; the political framing of mitigation infeasibility preserves their revenue streams and licenses continued investment.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, fossil_fuel_incumbents, beneficiary,
    institutional, biographical, arbitrage, global).

% Set climate policy agendas that prioritize near-term resilience spending and frame rapid decarbonization as economically or politically impossible; avoid domestic political costs of fossil fuel phase-out and systemic economic transformation.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, wealthy_nation_governments, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__adaptation_priority, wealthy_nation_governments, beneficiary).

% Inherit a higher locked-in warming trajectory and associated damages due to current prioritization of adaptation over mitigation; bear the deferred costs of emissions without voice or recourse in present policy.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Regions with limited fiscal and technical capacity that cannot adapt effectively to the warming trajectory accepted by the global policy framework; suffer residual climate damages and loss despite adaptation rhetoric.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions, payer,
    powerless, generational, trapped, regional).

% Demand rapid emissions reduction and systemic transformation; systematically marginalized in policy processes and finance flows where adaptation framing dominates as the realistic climate response.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, mitigation_advocacy_movement, excluded,
    organized, civilizational, constrained, global).

narrative_ontology:fixing_cost_class(climate_harm_prevention__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates immediate protection of present vulnerable populations through resilience investment, infrastructure hardening, and disaster preparedness in a political economy where rapid emissions reduction is treated as blocked.
% TRANSFER_FUNCTION: Moves climate finance, political attention, and infrastructural investment toward present adaptation needs; transfers residual climate damages and locked-in warming costs to future generations and low-adaptation-capacity regions.
% ABSENT_VOICES: Future generations have no institutional voice; mitigation advocates demanding rapid decarbonization are marginalized in adaptation-priority governance forums; low-adaptation-capacity regions are consulted but lack power to alter the global warming trajectory.
% DISAPPEARANCE_RATIONALE: If the legitimating framework that prioritizes adaptation over mitigation disappeared, climate governance would reallocate finance and political capital toward emissions reduction, fossil fuel incumbents would face accelerated transition pressure, and present vulnerable populations would lose their prioritized funding stream â the global climate policy architecture would reorganize around a different harm-prevention logic.
% FOUNDING_PROBLEM: How to prevent climate harm to human populations and ecological systems given perceived political and economic constraints on rapid systemic transformation.
% FOUNDING_PROBLEM_CORROBORATION: IPCC Working Group II and independent development economists attest to present vulnerability as a live problem from seats outside the benefiting parties. The claim that mitigation is infeasible is primarily asserted by wealthy nation governments and fossil fuel incumbents who benefit from the arrangement, with limited corroboration from disinterested economic analysis.
narrative_ontology:disappearance_verdict(climate_harm_prevention__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_harm_prevention__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__adaptation_priority, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint locks in a warming trajectory that imposes severe residual costs on future generations and regions with limited adaptation capacity. Suppression (0.71) is high because the constraint's persistence depends on actively framing mitigation as infeasible and excluding mitigation advocates from governance forums. Theater ratio (0.42) reflects significant performative activity â resilience pledges, adaptation finance commitments, and COP rhetoric that outstrips delivered finance and masks continued fossil fuel dependence. Accessibility collapse (0.60) captures the framing of rapid mitigation and degrowth as politically impossible, collapsing them from the policy menu. Resistance (0.55) is moderate and growing, driven by youth movements, vulnerable nations, and scientific pressure. The measurement series share a single time grid to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (future generations, low-adaptation-capacity regions) and the beneficiary/agenda-setter seats experience the same constraint structurally differently: from the wealthy nation government and fossil fuel incumbent positions, the arrangement is realistic, humane coordination that protects the vulnerable; from the future-generation and low-capacity-region positions, it is intertemporal and geographic cost-shipping that locks in harm they cannot escape.
 *
 * DIRECTIONALITY LOGIC:
 *   Present vulnerable populations are beneficiaries (receive adaptation flows) but powerless and trapped, so their d sits low â they are subsidized by the constraint. Fossil fuel incumbents and wealthy nation governments are beneficiaries/agenda-setters with arbitrage-grade exit, placing them near the full-beneficiary end. Future generations and low-adaptation-capacity regions are declared victims with trapped exit and large scope, placing them near the full-target end and amplifying their effective extraction. Mitigation advocates are excluded rather than targeted, sitting outside the primary extraction flow but bearing the suppression cost of marginalization.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing climate harm â is contested rather than dead: mitigation advocates argue the problem requires emissions reduction, while adaptation advocates argue immediate harm reduction is the tractable path. The constraint is not a piton because it has identifiable concentrated beneficiaries (fossil fuel incumbents, present vulnerable populations receiving prioritized flows). It is not a snare because the adaptation function is genuine coordination, not cover. Tangled rope is the structurally accurate classification: a real coordination mechanism (resilience investment) fused with asymmetric extraction (intertemporal and geographic cost externalization) and maintained by active enforcement (political suppression of mitigation alternatives).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_competition,
    'Does the adaptation_priority reading structurally foreclose mitigation investment by capturing climate finance and policy bandwidth, or do the readings remain genuinely coexisting framings?',
    'Track national climate budget allocations and NDC revision patterns: if adaptation finance systematically crowds out mitigation investment, the readings are structurally competitive rather than merely coexisting.',
    'Would upgrade the reading relation from coexists_with to influences or forecloses, altering the constraint family topology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_competition, conceptual, 'Structural competition between adaptation and mitigation readings').

omega_variable(
    mitigation_infeasibility_empirical_status,
    'Is the political and economic infeasibility of rapid mitigation a structural feature of the global political economy or an internalized TINA narrative among policymakers?',
    'Comparative policy analysis across jurisdictions with varying fossil fuel dependence and democratic mechanisms; assess whether mitigation feasibility rises where structural barriers are lower.',
    'If infeasibility is largely internalized, effective suppression exceeds structural measures and the constraint operates partly through discursive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_infeasibility_empirical_status, empirical, 'Structural versus internalized suppression of mitigation alternatives').

omega_variable(
    intergenerational_discount_ambiguity,
    'Does the prioritization of present vulnerable populations over future generations reflect an objective ethical discount rate or a power asymmetry between present and future agents?',
    'Evaluate whether institutionalized future-generation representation shifts climate budgets toward mitigation; cross-cultural comparison of intertemporal climate preferences.',
    'If representation shifts priorities, the arrangement reflects power asymmetry and extraction rather than stable coordination preference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_discount_ambiguity, preference, 'Ethical discounting versus power asymmetry in temporal priority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__adaptation_priority, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_harm_prevention__adaptation_priority, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clim_tr_t10, climate_harm_prevention__adaptation_priority, theater_ratio, 10, 0.25).
narrative_ontology:measurement(clim_tr_t20, climate_harm_prevention__adaptation_priority, theater_ratio, 20, 0.32).
narrative_ontology:measurement(clim_tr_t30, climate_harm_prevention__adaptation_priority, theater_ratio, 30, 0.4).
narrative_ontology:measurement(clim_tr_t40, climate_harm_prevention__adaptation_priority, theater_ratio, 40, 0.45).
narrative_ontology:measurement(clim_tr_t50, climate_harm_prevention__adaptation_priority, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_harm_prevention__adaptation_priority, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clim_be_t10, climate_harm_prevention__adaptation_priority, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(clim_be_t20, climate_harm_prevention__adaptation_priority, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(clim_be_t30, climate_harm_prevention__adaptation_priority, base_extractiveness, 30, 0.61).
narrative_ontology:measurement(clim_be_t40, climate_harm_prevention__adaptation_priority, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(clim_be_t50, climate_harm_prevention__adaptation_priority, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_harm_prevention__adaptation_priority, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(clim_su_t10, climate_harm_prevention__adaptation_priority, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(clim_su_t20, climate_harm_prevention__adaptation_priority, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(clim_su_t30, climate_harm_prevention__adaptation_priority, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(clim_su_t40, climate_harm_prevention__adaptation_priority, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(clim_su_t50, climate_harm_prevention__adaptation_priority, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__degrowth_reading).

% DUAL FORMULATION NOTE:
% The climate_harm_prevention kernel decomposes into three structurally distinct constraints: adaptation_priority (this file), mitigation_priority, and degrowth_reading. Each reading carries a different epsilon, beneficiary/victim structure, and coordination/extraction balance. They are linked as a constraint family via network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
