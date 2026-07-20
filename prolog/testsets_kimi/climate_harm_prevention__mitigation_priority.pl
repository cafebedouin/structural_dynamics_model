% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__mitigation_priority, []).

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
 *   constraint_id: climate_harm_prevention__mitigation_priority
 *   human_readable: Climate Harm Prevention: Mitigation Priority Reading
 *   domain: climate policy / political economy / intergenerational ethics
 *
 * SUMMARY:
 *   This constraint instantiates the mitigation_priority reading of the
 *   contested climate_harm_prevention kernel. It asserts that legitimate
 *   climate response prioritizes emissions reduction through technological
 *   transition within a growth-compatible framework. The constraint operates
 *   as an international policy paradigm enforced through UNFCCC architecture,
 *   NDCs, carbon markets, and national transition policies. Future
 *   generations are structurally positioned as primary beneficiaries, while
 *   present carbon-intensive sectors and workers bear concentrated transition
 *   costs. The constraint is claimed as tangled_rope because it coordinates
 *   genuine collective-action harm prevention while asymmetrically extracting
 *   from fossil fuel actors and present consumers to subsidize a
 *   technological transition whose benefits accrue to temporally and
 *   geographically displaced populations.
 *
 * KEY AGENTS:
 *   - future_generations (powerless/trapped beneficiary) â structurally benefits from reduced damages but has no policy voice
 *   - fossil_fuel_sector (powerful/constrained payer) â bears stranded assets and regulatory phase-out costs
 *   - international_climate_institutions (institutional/constrained agenda_setter) â administers the framework and enforces legitimacy boundaries
 *   - carbon_intensive_workers (powerless/trapped payer) â bears job losses and identity disruption with limited exit
 *   - clean_energy_sector (powerful/mobile beneficiary) â captures policy-driven investment and market share
 *   - degrowth_advocates (moderate/constrained excluded) â argues the framework's growth premise is impossible, structurally marginalized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, 0.62).
domain_priors:suppression_score(climate_harm_prevention__mitigation_priority, 0.48).
domain_priors:theater_ratio(climate_harm_prevention__mitigation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__mitigation_priority, "Climate Harm Prevention: Mitigation Priority Reading").
narrative_ontology:topic_domain(climate_harm_prevention__mitigation_priority, "climate policy / political economy / intergenerational ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__mitigation_priority, 'b7ba0878-a0e9-4860-8768-7fcf4e18a9b3').
narrative_ontology:cs_kernel_codification('b7ba0878-a0e9-4860-8768-7fcf4e18a9b3', distributed).
narrative_ontology:cs_authority_grounding('b7ba0878-a0e9-4860-8768-7fcf4e18a9b3', distributed).
narrative_ontology:cs_reading_relation('b7ba0878-a0e9-4860-8768-7fcf4e18a9b3', climate_harm_prevention__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('b7ba0878-a0e9-4860-8768-7fcf4e18a9b3', climate_harm_prevention__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('b7ba0878-a0e9-4860-8768-7fcf4e18a9b3', foundational, technological_transition_sufficiency).
narrative_ontology:cs_axiom_status(technological_transition_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('b7ba0878-a0e9-4860-8768-7fcf4e18a9b3', technological_transition_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('b7ba0878-a0e9-4860-8768-7fcf4e18a9b3', foundational, intergenerational_mitigation_obligation).
narrative_ontology:cs_axiom_status(intergenerational_mitigation_obligation, holdable).
narrative_ontology:cs_axiom_grounding('b7ba0878-a0e9-4860-8768-7fcf4e18a9b3', intergenerational_mitigation_obligation, deontological).
narrative_ontology:cs_reference_frame('b7ba0878-a0e9-4860-8768-7fcf4e18a9b3', growth_compatible_mitigation).
narrative_ontology:cs_drift_state('b7ba0878-a0e9-4860-8768-7fcf4e18a9b3', post_paris_agreement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b7ba0878-a0e9-4860-8768-7fcf4e18a9b3', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__mitigation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, clean_energy_sector).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, fossil_fuel_sector).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, carbon_intensive_workers).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, present_generation_consumers).
narrative_ontology:constraint_vindicates(climate_harm_prevention__mitigation_priority, green_growth_hypothesis).
narrative_ontology:constraint_vindicates(climate_harm_prevention__mitigation_priority, technological_optimism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the global climate governance architecture including UNFCCC processes, NDC frameworks, and carbon accounting standards. Set the terms of legitimate climate response and enforce compliance through reporting mechanisms and peer pressure. Locked into institutional mandates that assume growth-compatible decarbonization.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, international_climate_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Receives policy subsidies, carbon pricing revenue, regulatory preference, and investment flows channeled by mitigation priority frameworks. Benefits from the phase-out of fossil competitors and mandated transition timelines. Can pivot across jurisdictions as policy landscapes shift.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, clean_energy_sector, beneficiary,
    powerful, biographical, mobile, global).

% Bears stranded asset risk, regulatory phase-out mandates, carbon pricing costs, and exclusion from finance under mitigation frameworks. Sunk capital and long asset lifetimes constrain exit. Actively resists the constraint through lobbying and regulatory capture attempts.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, fossil_fuel_sector, payer,
    powerful, biographical, constrained, global).

% Face higher energy costs, appliance and transport upgrade requirements, and building retrofit burdens imposed by mitigation policy. Locked into existing infrastructure with limited short-term alternatives. Resistance manifests as electoral backlash against energy price spikes.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, present_generation_consumers, payer,
    organized, immediate, constrained, national).

% Receive the benefit of reduced climate damages if mitigation succeeds, but bear the full cost of failure or insufficient action. Have no seat at current policy tables and no mechanism to enforce the intergenerational social contract being made on their behalf.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, future_generations, beneficiary,
    powerless, generational, trapped, global).

% Low-lying island states, drought-prone regions, and coastal communities who benefit from reduced future warming but already pay adaptation costs. Bear disproportionate climate impacts despite minimal historical emissions. Geographic and economic immobility trap them in harm's way.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_vulnerable_populations, beneficiary,
    powerless, immediate, trapped, global).

% Coal miners, steelworkers, and manufacturing labor in carbon-intensive regions. Bear job losses, community collapse, and identity disruption from transition policies. Lack retraining resources and geographic mobility. Professional identity is often fused with industrial work that the constraint renders illegitimate.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, carbon_intensive_workers, payer,
    powerless, immediate, trapped, regional).

% Argue that mitigation within a growth framework is physically impossible and that planned economic contraction in the Global North is necessary. Present in academic and activist discourse but structurally excluded from mainstream policy forums, IPCC scenario primacy, and UNFCCC negotiating tracks.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, degrowth_advocates, excluded,
    moderate, generational, constrained, global).

% Provide empirical basis for climate harm projections through institutions like the IPCC. Some are embedded in scenario frameworks that privilege growth-compatible pathways; others question decoupling assumptions. Their findings shape but do not determine the normative policy constraint.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_scientists, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preventing catastrophic anthropogenic climate change by coordinating global greenhouse gas emissions reductions through technological transition, carbon pricing, and international compliance frameworks.
% TRANSFER_FUNCTION: Moves transition costs, stranded asset burdens, and higher energy prices from present carbon-intensive actors and consumers to future generations in the form of reduced climate damages; moves investment flows and regulatory preference toward clean energy sectors.
% ABSENT_VOICES: Degrowth scholars and deep ecology proponents who argue growth-compatible mitigation is physically impossible; fossil fuel workers from developing nations lacking representation in international climate negotiations; future generations themselves have no formal seat at policy tables despite being the named primary beneficiaries.
% DISAPPEARANCE_RATIONALE: Global climate governance, carbon markets, green investment flows, national energy transition plans, and the legitimacy architecture of international environmental diplomacy are organized around this framework. Its disappearance would create a vacuum filled by competing readings (adaptation-only or degrowth), reorganizing trillions in capital flows and policy incentives.
% FOUNDING_PROBLEM: Anthropogenic climate change threatening future human and ecological systems; the need to coordinate collective global action to reduce greenhouse gas emissions and prevent dangerous interference with the climate system.
% FOUNDING_PROBLEM_CORROBORATION: IPCC physical science working group assessments corroborate ongoing warming and emission trajectories as a live problem. Independent climate scientists and some Global South coalitions corroborate that emissions reduction remains necessary. However, the specific framing of growth-compatible mitigation as the sole legitimate response is primarily asserted by international climate institutions, green growth coalitions, and developed nations benefiting from technological export pathways; degrowth scholars and Global South justice movements contest that the founding problem requires this specific solution architecture.
narrative_ontology:disappearance_verdict(climate_harm_prevention__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__mitigation_priority, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_harm_prevention__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__mitigation_priority, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the framework imposes concentrated costs on fossil fuel sectors and present consumers while delivering diffuse benefits to future generations and vulnerable populations. Suppression (0.48) reflects moderate active marginalization of degrowth and deep adaptation alternatives within mainstream policy, plus regulatory enforcement of transition timelines. Theater ratio (0.42) captures extensive greenwashing, net-zero pledges without delivery, carbon offset theater, and performative COP processes that outrun actual decoupling. Accessibility collapse (0.58) is moderate-high because once inside the mitigation framework, alternatives (degrowth, adaptation-only) lose legitimacy within policy discourse despite remaining structurally possible. Resistance (0.68) is high due to fossil fuel lobbying, consumer price backlash, and geopolitical resistance from carbon-intensive developing nations. Measurements share a single time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (international climate institutions) experiences the constraint as necessary coordination solving a collective action problem; the engine should compute a milder type from that position. The payer seats (fossil fuel sector, carbon intensive workers, present consumers) experience the same structure as imposed extraction with constrained exit; the engine should compute a more extractive type. The beneficiary seats (future generations, vulnerable populations) experience it as protective scaffolding, though future generations' trapped status amplifies their structural dependence. The analytical observer seat (climate scientists) sees the full tension between coordination function and asymmetric cost distribution.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and climate vulnerable populations are structural beneficiaries (low d) receiving reduced climate damages. Clean energy sector is a beneficiary (low-to-moderate d) capturing policy rents. Fossil fuel sector, carbon intensive workers, and present generation consumers are structural targets (high d) paying transition costs, stranded assets, and energy price premiums. International climate institutions sit near symmetric: they enforce the constraint and bear institutional costs of maintenance but do not personally collect extraction. Degrowth advocates are excluded from the directionality derivation as they operate outside the constraint's legitimating boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids snare classification because the coordination function is genuine: atmospheric physics creates a real collective-action problem that unilateral defection cannot solve, and harm prevention is structurally verifiable. It avoids rope classification because costs are not symmetrically distributed: fossil fuel sectors and specific worker classes bear concentrated losses while benefits diffuse across future populations. The Tangled Rope classification captures this hybridity. Mandatrophy would arise if the founding problem (climate change) were solved but the framework persisted to maintain institutional rents; current measurements show rising theater_ratio but founding_problem_status remains live, so mandatrophy is not yet triggered.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_compatibility_empirical_status,
    'Is absolute decoupling of economic growth from emissions physically possible at global scale, or does the mitigation_priority reading rest on an empirically false premise?',
    'Long-term macroeconomic and material flow accounting; observation of whether global emissions peak and decline while GDP grows across sufficient time horizons and sectoral scope.',
    'If empirically false, the foundational axiom of technological_transition_sufficiency is contradicted, and the constraint shifts toward scaffold (failing transition) or snare (maintaining extractive cost impositions despite known physical impossibility).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_compatibility_empirical_status, empirical, 'Whether green growth decoupling is physically achievable').

omega_variable(
    future_generations_beneficiary_status,
    'Can temporally displaced actors who lack current agency structurally qualify as beneficiaries in the directionality derivation, or does this produce observer-relative epsilon instability?',
    'Philosophical analysis of intergenerational beneficiary relationships and cross-corpus comparison with other intergenerational constraints (pension systems, long-term debt).',
    'If future generations cannot be beneficiaries, the coordination function is misidentified and the constraint may recompute as snare (present extraction with no live beneficiary seat) or the beneficiary set must be restricted to currently acting agents like clean_energy_sector.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generations_beneficiary_status, conceptual, 'Intergenerational beneficiary ontological status').

omega_variable(
    suppression_of_alternatives,
    'Are degrowth and deep adaptation alternatives genuinely marginalized by the mitigation framework, or do they coexist as live options within the same policy space?',
    'Discourse analysis of IPCC scenario selection, UNFCCC negotiating documents, and national policy frameworks measuring allocation of agenda space and funding to alternative paradigms.',
    'If alternatives are actively suppressed rather than merely minority positions, suppression metric is higher and the extractive component of the tangled rope increases relative to the coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternatives, conceptual, 'Alternative paradigm marginalization mechanism').

omega_variable(
    kernel_committer_ambiguity,
    'Does the climate_harm_prevention kernel structurally admit the three readings as permanent alternatives, or will empirical progress on decoupling or warming trajectories foreclose one or more?',
    'Observation of emissions trajectories, decoupling evidence, and climate impacts through 2040-2050; tracking whether any reading is abandoned by its own adherents.',
    'Could convert reading_relations from coexists_with to forecloses or influences, altering the constraint family topology and contamination propagation paths.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_ambiguity, empirical, 'Kernel stability versus empirical foreclosure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__mitigation_priority, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_harm_prevention__mitigation_priority, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clim_tr_t5, climate_harm_prevention__mitigation_priority, theater_ratio, 5, 0.26).
narrative_ontology:measurement(clim_tr_t10, climate_harm_prevention__mitigation_priority, theater_ratio, 10, 0.32).
narrative_ontology:measurement(clim_tr_t15, climate_harm_prevention__mitigation_priority, theater_ratio, 15, 0.37).
narrative_ontology:measurement(clim_tr_t20, climate_harm_prevention__mitigation_priority, theater_ratio, 20, 0.4).
narrative_ontology:measurement(clim_tr_t25, climate_harm_prevention__mitigation_priority, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_harm_prevention__mitigation_priority, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clim_be_t5, climate_harm_prevention__mitigation_priority, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(clim_be_t10, climate_harm_prevention__mitigation_priority, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(clim_be_t15, climate_harm_prevention__mitigation_priority, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(clim_be_t20, climate_harm_prevention__mitigation_priority, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(clim_be_t25, climate_harm_prevention__mitigation_priority, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_harm_prevention__mitigation_priority, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t5, climate_harm_prevention__mitigation_priority, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(clim_su_t10, climate_harm_prevention__mitigation_priority, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(clim_su_t15, climate_harm_prevention__mitigation_priority, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(clim_su_t20, climate_harm_prevention__mitigation_priority, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(clim_su_t25, climate_harm_prevention__mitigation_priority, suppression_requirement, 25, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__mitigation_priority, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
