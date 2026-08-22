% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__adaptation_priority, []).

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
 *   constraint_id: climate_response_obligation__adaptation_priority
 *   human_readable: Climate Response Obligation: Adaptation Priority Reading
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint story instantiates the adaptation_priority reading of the
 *   climate_response_obligation kernel. The reading treats 2-3Â°C warming as
 *   inevitable and constructs a policy obligation to prioritize resilience
 *   investment over costly prevention. Structurally, it operates as a tangled
 *   rope: it coordinates present-day capital around adaptation infrastructure
 *   and disaster risk management, while asymmetrically extracting from future
 *   generations and Global South populations who bear the locked-in climate
 *   impacts without commensurate prevention. Fossil capital is protected, and
 *   adaptation finance concentrates in wealthy regions. The claim is
 *   tangled_rope; the metrics are authored independently to reflect high
 *   extraction, substantial suppression, and rising theater as the gap
 *   between adaptation promises and delivery grows.
 *
 * KEY AGENTS:
 *   - wealthy_nation_governments: Primary agenda-setter (institutional/constrained) â administers the adaptation-priority policy frame and allocates climate finance
 *   - fossil_capital_complex: Primary beneficiary (powerful/arbitrage) â avoids stranded assets and transition costs through the inevitability narrative
 *   - future_generations: Primary target (powerless/trapped/civilizational) â bears locked-in warming impacts with no voice or exit
 *   - global_south_populations: Secondary target (powerless/trapped/global) â faces disproportionate climate damages and adaptation deficits
 *   - climate_resilience_industry: Secondary beneficiary (moderate/mobile) â captures adaptation capital flows concentrated in wealthy regions
 *   - mitigation_advocacy_movement: Excluded voice (organized/constrained) â structurally marginalized in policy forums dominated by adaptation framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, 0.78).
domain_priors:suppression_score(climate_response_obligation__adaptation_priority, 0.75).
domain_priors:theater_ratio(climate_response_obligation__adaptation_priority, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, extractiveness, 0.78).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__adaptation_priority, "Climate Response Obligation: Adaptation Priority Reading").
narrative_ontology:topic_domain(climate_response_obligation__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__adaptation_priority, 'd031789f-add0-4a0f-9eb8-24079130dcf5').
narrative_ontology:cs_kernel_codification('d031789f-add0-4a0f-9eb8-24079130dcf5', distributed).
narrative_ontology:cs_authority_grounding('d031789f-add0-4a0f-9eb8-24079130dcf5', distributed).
narrative_ontology:cs_reading_relation('d031789f-add0-4a0f-9eb8-24079130dcf5', climate_response_obligation__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('d031789f-add0-4a0f-9eb8-24079130dcf5', climate_response_obligation__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('d031789f-add0-4a0f-9eb8-24079130dcf5', foundational, warming_inevitability_pragmatic_acceptance).
narrative_ontology:cs_axiom_status(warming_inevitability_pragmatic_acceptance, holdable).
narrative_ontology:cs_axiom_grounding('d031789f-add0-4a0f-9eb8-24079130dcf5', warming_inevitability_pragmatic_acceptance, empirically_contingent).
narrative_ontology:cs_axiom('d031789f-add0-4a0f-9eb8-24079130dcf5', foundational, present_cost_aversion_over_future_risk).
narrative_ontology:cs_axiom_status(present_cost_aversion_over_future_risk, holdable).
narrative_ontology:cs_axiom_grounding('d031789f-add0-4a0f-9eb8-24079130dcf5', present_cost_aversion_over_future_risk, instrumental).
narrative_ontology:cs_reference_frame('d031789f-add0-4a0f-9eb8-24079130dcf5', locked_in_warming_governance).
narrative_ontology:cs_drift_state('d031789f-add0-4a0f-9eb8-24079130dcf5', post_1p5_science_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d031789f-add0-4a0f-9eb8-24079130dcf5', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__adaptation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, fossil_capital_complex).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, climate_resilience_industry).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, current_generation_affluent).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, global_south_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers national and multilateral climate policy frameworks that treat 2-3Â°C warming as locked-in, prioritizing adaptation finance and resilience planning over binding mitigation or fossil-fuel phase-out. Political economy constraints limit rapid pivot to prevention.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, wealthy_nation_governments, agenda_setter,
    institutional, biographical, constrained, global).

% Profits from continued fossil-fuel production and avoided stranded-asset risk. The inevitability narrative protects existing capital structure and delays decarbonization mandates.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, fossil_capital_complex, beneficiary,
    powerful, biographical, arbitrage, global).

% Captures public and private adaptation financeâconsultancy, infrastructure, insurance, early-warning systemsâconcentrated in wealthy-region markets. Revenue depends on the policy pivot from prevention to resilience.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, climate_resilience_industry, beneficiary,
    moderate, biographical, mobile, global).

% Avoids direct transition costs (higher energy prices, consumption constraints, asset repricing) that rapid mitigation would impose. Benefits from continued cheap fossil-energy access and deferred behavioral change.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, current_generation_affluent, beneficiary,
    moderate, biographical, mobile, national).

% Will inhabit a world with 2-3Â°C locked-in warming, irreversible sea-level rise, and intensified extreme events. They bear the full impact of foregone prevention but have no seat in current policy decisions and cannot opt out of time.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Face disproportionate climate impacts despite minimal historical emissions. Adaptation finance flows remain inadequate and fragmented, while loss and damage accumulate. Geographic and economic immobility prevents exit from frontline vulnerability.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, global_south_populations, payer,
    powerless, generational, trapped, global).

% Argues for rapid decarbonization, binding emission caps, and prevention-first finance. Structurally marginalized in UNFCCC and national budget processes where adaptation-framing dominates; treated as politically unrealistic.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, mitigation_advocacy_movement, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__adaptation_priority, fossil_capital_complex).
narrative_ontology:fixing_cost_class(climate_response_obligation__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates present-day actors around a shared expectation of 2-3Â°C warming, pooling capital and policy attention into resilience infrastructure, disaster risk management, and incremental adjustment rather than systemic prevention.
% TRANSFER_FUNCTION: Moves climate impact burdens, mitigation costs, and stranded-asset risks from present wealthy emitters and fossil capital to future generations and vulnerable Global South populations, while concentrating adaptation capital in wealthy-region firms and governments.
% ABSENT_VOICES: Future generations have no representative seat; Global South negotiators are present but structurally disadvantaged in finance allocation; degrowth and rapid-mitigation advocates are marginalized in mainstream policy forums where adaptation priority dominates.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, the political and economic justification for deferring decarbonization would collapse. Capital would reallocate toward mitigation and away from fossil fuels; adaptation finance would compete on equal terms with prevention rather than replacing it; Global South demands for loss-and-damage would gain leverage.
% FOUNDING_PROBLEM: The perceived impracticality or prohibitive cost of rapid global decarbonization combined with already-committed warming from historical emissions, requiring a pragmatic pivot to managing unavoidable impacts.
% FOUNDING_PROBLEM_CORROBORATION: Wealthy nation governments and fossil capital attest the problem is live, citing economic and political constraints on rapid transition. Climate scientists, Global South negotiators, and independent IPCC assessments note remaining carbon budgets compatible with lower warming if rapid action is taken, corroborating from outside the benefiting parties that the founding problem is contested rather than closed.
narrative_ontology:disappearance_verdict(climate_response_obligation__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_obligation__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__adaptation_priority, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint locks in warming that will be borne by parties with no voice, while protecting present emitters. Suppression (0.75) reflects the structural marginalization of mitigation and degrowth alternatives in policy forums. Theater_ratio (0.45) captures the growing performative dimension: adaptation is promised but underfunded relative to needs, and the 'inevitability' framing serves to normalize inaction. Accessibility_collapse (0.60) indicates that while alternatives physically exist, they are politically inaccessible. Resistance (0.55) acknowledges strong but so-far insufficient counter-mobilization.
 *
 * PERSPECTIVAL GAP:
 *   The wealthy-nation government seat experiences the constraint as pragmatic governance under difficult conditions. The fossil-capital and resilience-industry seats experience it as a protective subsidy. The future-generation and Global-South seats experience it as a compulsory risk transfer with no exit. The engine computes these divergences from power, exit, and role.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (fossil_capital_complex, climate_resilience_industry, current_generation_affluent) are structurally positioned near the beneficiary pole: they collect avoided costs or direct rents, and possess mobile or arbitrage exit. Victims (future_generations, global_south_populations) are positioned at the full-target pole: powerless, trapped, and scoped globally or civilizationaly, so their effective extraction is amplified. The agenda_setter (wealthy_nation_governments) sits in between, capturing short-term political relief while bearing partial long-term exposure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination and extraction evidence. Pure rope would need no victims; pure snare would need no genuine coordination function. Here, adaptation infrastructure is real (coordination), but the distribution of its finance and the protection of fossil capital demonstrate asymmetric extraction. If the coordination function were to atrophy entirely, the constraint would degrade toward piton or snare; if the extraction were removed, it would approach scaffold or rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Does the adaptation_priority reading represent a coherent normative position within the climate response obligation kernel, or is it a defensive framing by incumbent beneficiaries?',
    'Trace the funding and institutional lineage of the adaptation-priority narrative; if it originates primarily from fossil-capital-funded think tanks and wealthy-nation finance ministries with no independent civil-society genesis, it reclassifies as extraction-backed snare rather than tangled rope.',
    'If the reading is beneficiary-originated without independent corroboration, its coordination function is cover and the constraint computes as snare; if it has independent policy-intellectual lineage, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, empirical, 'Provenance of the adaptation-priority reading within the kernel').

omega_variable(
    adaptation_finance_geography,
    'Does adaptation investment under this reading flow to frontline vulnerable populations as a coordination benefit, or does it concentrate in wealthy-region consultancy and infrastructure firms?',
    'Track adaptation finance flows via OECD and UNFCCC registries; compare shares reaching LDCs versus returning to donor-country firms.',
    'Concentration in wealthy regions would raise extractiveness and confirm asymmetric extraction; genuine flow to vulnerable regions would support a larger coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_finance_geography, empirical, 'Geographic concentration of adaptation finance').

omega_variable(
    warming_inevitability_empirical_status,
    'Is 2-3Â°C warming genuinely locked in given remaining carbon budgets and socio-technical constraints, or is the inevitability claim a political construct?',
    'IPCC scenario analysis and integrated assessment models; compare feasible mitigation pathways against the political barriers asserted by the reading.',
    'If the inevitability claim is empirically unsupported, the coordination rationale collapses toward pure extraction; if supported, the tangled rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warming_inevitability_empirical_status, empirical, 'Empirical status of the warming inevitability claim').

omega_variable(
    suppression_mechanism_nature,
    'Is the suppression of mitigation alternatives achieved through material resource diversion, or through narrative control and agenda-setting?',
    'Compare budgetary allocations (material) against media framing and policy-discourse analysis (narrative).',
    'Material suppression would indicate enforcement via resource competition; narrative suppression indicates a softer but potentially more totalizing lock-in of the discourse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_nature, conceptual, 'Material versus narrative suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__adaptation_priority, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__adaptation_priority, theater_ratio, 0, 0.25).
narrative_ontology:measurement(clim_tr_t8, climate_response_obligation__adaptation_priority, theater_ratio, 8, 0.3).
narrative_ontology:measurement(clim_tr_t16, climate_response_obligation__adaptation_priority, theater_ratio, 16, 0.35).
narrative_ontology:measurement(clim_tr_t24, climate_response_obligation__adaptation_priority, theater_ratio, 24, 0.4).
narrative_ontology:measurement(clim_tr_t32, climate_response_obligation__adaptation_priority, theater_ratio, 32, 0.43).
narrative_ontology:measurement(clim_tr_t40, climate_response_obligation__adaptation_priority, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__adaptation_priority, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(clim_be_t8, climate_response_obligation__adaptation_priority, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(clim_be_t16, climate_response_obligation__adaptation_priority, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(clim_be_t24, climate_response_obligation__adaptation_priority, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(clim_be_t32, climate_response_obligation__adaptation_priority, base_extractiveness, 32, 0.74).
narrative_ontology:measurement(clim_be_t40, climate_response_obligation__adaptation_priority, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__adaptation_priority, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(clim_su_t8, climate_response_obligation__adaptation_priority, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(clim_su_t16, climate_response_obligation__adaptation_priority, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(clim_su_t24, climate_response_obligation__adaptation_priority, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(clim_su_t32, climate_response_obligation__adaptation_priority, suppression_requirement, 32, 0.73).
narrative_ontology:measurement(clim_su_t40, climate_response_obligation__adaptation_priority, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
