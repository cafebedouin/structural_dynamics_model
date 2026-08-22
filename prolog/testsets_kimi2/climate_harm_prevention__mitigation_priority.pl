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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: climate_harm_prevention__mitigation_priority
 *   human_readable: Climate Harm Prevention: Mitigation Priority Reading
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint story instantiates the mitigation_priority reading of the
 *   contested climate_harm_prevention kernel. It treats the policy framework
 *   that prioritizes emissions reduction via technological transition within
 *   a growth paradigm as a standing arrangement. The constraint coordinates
 *   global decarbonization effort but asymmetrically extracts transition
 *   costs from present carbon-intensive sectors and consumers, while
 *   positioning future generations as primary beneficiaries. The claim/metric
 *   independence is maintained: the reading claims tangled_rope (genuine
 *   coordination with asymmetric extraction) while the metrics describe
 *   substantial extractiveness and rising enforcement requirements. Sibling
 *   readings (adaptation_priority, degrowth_reading) are modeled as separate
 *   constraints in the same kernel family.
 *
 * KEY AGENTS:
 *   - Future generations (powerless/trapped) â structural beneficiaries who cannot negotiate or exit the climate future
 *   - Carbon-intensive industries (powerful/constrained) â primary payers facing stranded assets and regulatory compression
 *   - Fossil fuel dependent workers (moderate/constrained) â diffuse payers with geographic and skill lock-in
 *   - Green technology sectors (powerful/mobile) â present beneficiaries capturing redirected capital and policy support
 *   - Climate policy institutions (institutional/analytical) â agenda-setters administering the carbon transition framework
 *   - Present generation consumers (organized/constrained) â dual-positioned payers bearing energy cost increases
 *   - Degrowth advocates (moderate/mobile) â excluded voices contesting the growth-compatibility assumption
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, 0.68).
domain_priors:suppression_score(climate_harm_prevention__mitigation_priority, 0.62).
domain_priors:theater_ratio(climate_harm_prevention__mitigation_priority, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__mitigation_priority, "Climate Harm Prevention: Mitigation Priority Reading").
narrative_ontology:topic_domain(climate_harm_prevention__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__mitigation_priority, '6b22c915-f32c-4905-a8bf-36082453a767').
narrative_ontology:cs_kernel_codification('6b22c915-f32c-4905-a8bf-36082453a767', distributed).
narrative_ontology:cs_authority_grounding('6b22c915-f32c-4905-a8bf-36082453a767', expertise).
narrative_ontology:cs_interpretation_layer_present('6b22c915-f32c-4905-a8bf-36082453a767').
narrative_ontology:cs_reading_relation('6b22c915-f32c-4905-a8bf-36082453a767', climate_harm_prevention__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('6b22c915-f32c-4905-a8bf-36082453a767', climate_harm_prevention__degrowth_reading, influences).
narrative_ontology:cs_axiom('6b22c915-f32c-4905-a8bf-36082453a767', foundational, growth_compatible_mitigation_sufficient).
narrative_ontology:cs_axiom_status(growth_compatible_mitigation_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('6b22c915-f32c-4905-a8bf-36082453a767', growth_compatible_mitigation_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('6b22c915-f32c-4905-a8bf-36082453a767', foundational, intergenerational_equity_priority).
narrative_ontology:cs_axiom_status(intergenerational_equity_priority, holdable).
narrative_ontology:cs_axiom_grounding('6b22c915-f32c-4905-a8bf-36082453a767', intergenerational_equity_priority, deontological).
narrative_ontology:cs_reference_frame('6b22c915-f32c-4905-a8bf-36082453a767', growth_compatible_decarbonization).
narrative_ontology:cs_drift_state('6b22c915-f32c-4905-a8bf-36082453a767', contemporary_implementation_gap, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6b22c915-f32c-4905-a8bf-36082453a767', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__mitigation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, green_technology_sectors).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, carbon_intensive_industries).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, fossil_fuel_dependent_workers).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, present_generation_consumers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, present_generation_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Will inherit the atmospheric conditions and climate stability resulting from present mitigation choices. Cannot participate in present policy decisions but are structurally positioned as the primary intended beneficiaries of emissions reductions and technological transition.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Bear the direct costs of decarbonization policy through carbon pricing, regulatory compliance, stranded fossil fuel assets, and loss of social license. Have significant lobbying capacity but face tightening policy constraints that limit their operating space.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, carbon_intensive_industries, payer,
    powerful, biographical, constrained, global).

% Face job displacement and community decline from the phase-out of carbon-intensive industries without guaranteed transition support. Geographic concentration and skill specificity limit exit to alternative employment.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, fossil_fuel_dependent_workers, payer,
    moderate, biographical, constrained, regional).

% Receive policy subsidies, regulatory preferences, and capital flows redirected by the mitigation framework. Benefit from the constraint's reallocation of energy infrastructure investment toward renewables and low-carbon technology.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, green_technology_sectors, beneficiary,
    powerful, biographical, mobile, global).

% Set the mitigation targets, carbon accounting rules, and enforcement mechanisms through international agreements and national regulations. Justify the framework by citing climate science and intergenerational ethics, and administer the transfer of costs from present carbon sectors to future beneficiaries.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_policy_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Pay higher energy prices and bear consumption adjustment costs from mitigation policies, while also receiving diffuse benefits from reduced climate impacts and improved air quality. Limited ability to opt out of the energy transition.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, present_generation_consumers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__mitigation_priority, present_generation_consumers, beneficiary).

% Argue that growth-compatible decarbonization is physically impossible and that planned economic contraction is necessary. Are systematically marginalized in mainstream climate policy forums that assume technological transition within existing growth frameworks.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, degrowth_advocates, excluded,
    moderate, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates present global action to reduce greenhouse gas emissions and prevent catastrophic climate change, distributing the costs of technological transition across the present generation to protect the climate stability required by future societies.
% TRANSFER_FUNCTION: Moves capital, labor, and energy infrastructure from carbon-intensive present sectors to low-carbon technologies, transferring expected future avoided damages to future generations while imposing transition costs on present carbon-intensive industries and consumers.
% ABSENT_VOICES: Degrowth scholars and some Global South adaptation advocates are marginalized in mainstream policy frameworks that assume growth-compatible decarbonization; fossil fuel workers are often consulted performatively without structural transition guarantees.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority framework vanished overnight, carbon-intensive industries would expand unchecked, renewable transition capital would collapse, and the intergenerational transfer of climate risk would reverse â the global energy economy and atmospheric commons would reorganize around carbon-intensive growth.
% FOUNDING_PROBLEM: Unregulated greenhouse gas emissions from industrial activity create a collective action problem and intergenerational externality: present economic activity degrades the shared atmospheric commons and climate stability required by future generations, with no market mechanism to price the future cost.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists (IPCC), economists studying externalities, and youth climate movements attest to the founding problem from seats outside the direct beneficiary structure; fossil fuel industry representatives contest the urgency or feasibility of the mitigation framing but generally do not contest the physical greenhouse effect itself.
narrative_ontology:disappearance_verdict(climate_harm_prevention__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_harm_prevention__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__mitigation_priority, 0.68, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness rises from 0.45 to 0.68 over the interval as mitigation policy tightens and the gap between pledged and realized action widens, increasing the transfer burden on present carbon actors. Suppression rises from 0.40 to 0.62 as enforcement mechanisms (carbon pricing, regulatory standards, disclosure mandates) mature and harden. Theater ratio rises from 0.15 to 0.38 as the divergence between climate pledges and physical outcomes grows, increasing the performative component of mitigation policy. Accessibility collapse is moderate (0.45) because degrowth and adaptation alternatives remain intellectually live despite marginalization. Resistance is high (0.70) due to sustained political opposition from carbon-intensive industries and affected worker communities.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (climate institutions) experiences the constraint as necessary coordination solving a global collective action problem; the payer seats (carbon industries, workers, consumers) experience it as a forced transfer of costs with uncertain future return. Future generations, as intended beneficiaries, have no seat at the table but are structurally assigned low directionality. The engine computes this divergence from power, exit, and role declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and green technology sectors are declared beneficiaries, feeding low directionality. Carbon-intensive industries, fossil fuel workers, and present consumers are declared victims/payers, feeding high directionality. Climate policy institutions are agenda-setters with analytical exit, placing them near the beneficiary end despite not directly capturing rents. Degrowth advocates are excluded, receiving no directionality weight. The structural asymmetry between present payers and future beneficiaries is the core axis.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework prevents mislabeling by requiring both genuine coordination (beneficiaries, enforced decarbonization function) and asymmetric extraction (victims, transition costs). A pure snare reading would suppress alternatives completely and show no coordination function; a pure rope would show no concentrated victims. The temporal measurements show extraction accumulation without coordination collapse, supporting tangled_rope over scaffold (no sunset) or piton (theater is not the dominant feature).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_compatibility_physical_limit,
    'Is growth-compatible decarbonization physically and energetically feasible at the speed required to meet climate targets, or do biophysical limits (energy return on investment, mineral extraction rates, land-use constraints) make the mitigation-priority reading operationally impossible?',
    'Empirical tracking of decarbonization rates in advanced economies against IPCC pathway requirements; mineral supply chain analysis; energy return on investment studies for renewable transitions.',
    'If growth-compatible mitigation is physically impossible, this reading collapses toward the degrowth reading; if possible, the tangled_rope classification holds as a genuine coordination mechanism with asymmetric costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_compatibility_physical_limit, empirical, 'Physical feasibility of growth-compatible decarbonization').

omega_variable(
    intergenerational_beneficiary_status,
    'Can future generations, who do not yet exist and cannot negotiate, function as structural beneficiaries in the same sense as present actors, or does their non-existence make the beneficiary-victim asymmetry a present-generation redistribution disguised as intergenerational ethics?',
    'Comparative analysis of climate policy incidence: who actually receives present mitigation rents (subsidies, regulatory protection) versus who pays, tracked independently of stated future benefits.',
    'If present green technology sectors and policy institutions capture most present gains while future benefits remain speculative, the constraint reads more as present-day redistribution than intergenerational coordination, shifting classification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_beneficiary_status, conceptual, 'Whether future generations can be treated as live beneficiaries').

omega_variable(
    enforcement_geographic_asymmetry,
    'Does the mitigation framework''s enforcement fall disproportionately on Global South present-generation actors while Global North historic emitters evade transition costs, making the extraction geographically asymmetric beyond the generational asymmetry?',
    'Analysis of NDC compliance burdens, climate finance flows, and per-capita emission reduction obligations across national income categories.',
    'If enforcement is geographically asymmetric, directionality varies by spatial scope and the constraint exhibits nested tangled_rope dynamics; if symmetric, the generational framing is structurally accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_geographic_asymmetry, empirical, 'Geographic asymmetry in transition cost enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__mitigation_priority, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_harm_prevention__mitigation_priority, theater_ratio, 0, 0.15).
narrative_ontology:measurement(clim_tr_t8, climate_harm_prevention__mitigation_priority, theater_ratio, 8, 0.2).
narrative_ontology:measurement(clim_tr_t16, climate_harm_prevention__mitigation_priority, theater_ratio, 16, 0.25).
narrative_ontology:measurement(clim_tr_t24, climate_harm_prevention__mitigation_priority, theater_ratio, 24, 0.3).
narrative_ontology:measurement(clim_tr_t32, climate_harm_prevention__mitigation_priority, theater_ratio, 32, 0.34).
narrative_ontology:measurement(clim_tr_t40, climate_harm_prevention__mitigation_priority, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_harm_prevention__mitigation_priority, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(clim_be_t8, climate_harm_prevention__mitigation_priority, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(clim_be_t16, climate_harm_prevention__mitigation_priority, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(clim_be_t24, climate_harm_prevention__mitigation_priority, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(clim_be_t32, climate_harm_prevention__mitigation_priority, base_extractiveness, 32, 0.64).
narrative_ontology:measurement(clim_be_t40, climate_harm_prevention__mitigation_priority, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_harm_prevention__mitigation_priority, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(clim_su_t8, climate_harm_prevention__mitigation_priority, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(clim_su_t16, climate_harm_prevention__mitigation_priority, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(clim_su_t24, climate_harm_prevention__mitigation_priority, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(clim_su_t32, climate_harm_prevention__mitigation_priority, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(clim_su_t40, climate_harm_prevention__mitigation_priority, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__mitigation_priority, global_infrastructure).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, adaptation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, degrowth_reading).

% DUAL FORMULATION NOTE:
% The climate_harm_prevention kernel decomposes into three structurally distinct constraints because the mitigation_priority, adaptation_priority, and degrowth readings have different epsilon values, different beneficiary/victim structures, and different empirical premises about the physical and political possibility of decarbonization. Each reading is linked as a family member.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
