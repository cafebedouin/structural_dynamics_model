% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__degrowth_reading, []).

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
 *   constraint_id: climate_harm_prevention__degrowth_reading
 *   human_readable: Degrowth Reading of Climate Harm Prevention
 *   domain: climate policy / political economy / intergenerational ethics
 *
 * SUMMARY:
 *   This constraint story models the degrowth_reading of the
 *   climate_harm_prevention kernel. The reading asserts that legitimate
 *   climate response is constrained by the physical and political
 *   impossibility of mitigating climate change within a growth framework,
 *   requiring planned economic contraction in the Global North. This creates
 *   an asymmetric structure where Global South populations and future
 *   generations benefit from avoided climate harm, while Global North
 *   corporations, citizens, and states bear the costs of reduced consumption
 *   and economic restructuring. The constraint coordinates global harm
 *   prevention but extracts differentially from wealthy economies, requiring
 *   active international and domestic policy enforcement to suppress
 *   growth-oriented development pathways.
 *
 * KEY AGENTS:
 *   - global_north_states: Agenda setter (institutional/global) â must design and enforce contraction policies domestically while negotiating international frameworks
 *   - global_north_corporations: Primary target (powerful/constrained) â bear costs of stranded assets, regulatory limits, and demand contraction
 *   - global_north_citizens: Primary target (moderate/constrained) â experience reduced consumption and economic restructuring
 *   - global_south_populations: Primary beneficiary (powerless/constrained) â receive climate harm reduction and development space
 *   - future_generations: Primary beneficiary (powerless/trapped) â inherit stabilized climate but have no present voice
 *   - climate_science_community: Analytical observer (analytical/analytical) â produces the planetary boundary knowledge that grounds the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, 0.75).
domain_priors:suppression_score(climate_harm_prevention__degrowth_reading, 0.7).
domain_priors:theater_ratio(climate_harm_prevention__degrowth_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__degrowth_reading, "Degrowth Reading of Climate Harm Prevention").
narrative_ontology:topic_domain(climate_harm_prevention__degrowth_reading, "climate policy / political economy / intergenerational ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__degrowth_reading, '0ddd5e21-d7d8-4a7a-ad8a-4151d7a50ea1').
narrative_ontology:cs_kernel_codification('0ddd5e21-d7d8-4a7a-ad8a-4151d7a50ea1', distributed).
narrative_ontology:cs_authority_grounding('0ddd5e21-d7d8-4a7a-ad8a-4151d7a50ea1', expertise).
narrative_ontology:cs_interpretation_layer_present('0ddd5e21-d7d8-4a7a-ad8a-4151d7a50ea1').
narrative_ontology:cs_reading_relation('0ddd5e21-d7d8-4a7a-ad8a-4151d7a50ea1', climate_harm_prevention__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('0ddd5e21-d7d8-4a7a-ad8a-4151d7a50ea1', climate_harm_prevention__adaptation_priority, influences).
narrative_ontology:cs_axiom('0ddd5e21-d7d8-4a7a-ad8a-4151d7a50ea1', foundational, growth_compatible_mitigation_refuted).
narrative_ontology:cs_axiom_status(growth_compatible_mitigation_refuted, holdable).
narrative_ontology:cs_axiom_grounding('0ddd5e21-d7d8-4a7a-ad8a-4151d7a50ea1', growth_compatible_mitigation_refuted, empirically_contingent).
narrative_ontology:cs_axiom('0ddd5e21-d7d8-4a7a-ad8a-4151d7a50ea1', foundational, contraction_legitimacy).
narrative_ontology:cs_axiom_status(contraction_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('0ddd5e21-d7d8-4a7a-ad8a-4151d7a50ea1', contraction_legitimacy, deontological).
narrative_ontology:cs_reference_frame('0ddd5e21-d7d8-4a7a-ad8a-4151d7a50ea1', planetary_boundaries_steady_state).
narrative_ontology:cs_drift_state('0ddd5e21-d7d8-4a7a-ad8a-4151d7a50ea1', anthropocene_policy_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('0ddd5e21-d7d8-4a7a-ad8a-4151d7a50ea1', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__degrowth_reading, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_north_corporations).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_north_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Must design and enforce domestic economic contraction policies while negotiating international climate frameworks that bind their own economies; they administer the constraint even as their populations and corporations bear its costs.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_states, agenda_setter,
    institutional, generational, constrained, global).

% Face stranded assets, regulatory caps on production, and demand contraction as the Global North economy shrinks; their profit model depends on growth trajectories that the constraint explicitly suppresses.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_corporations, payer,
    powerful, biographical, constrained, global).

% Experience reduced consumption possibilities, higher energy costs, and economic restructuring as aggregate demand is deliberately contracted; their political voice is constrained by institutional inertia and the absence of growth alternatives within this framework.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_citizens, payer,
    moderate, biographical, constrained, national).

% Receive reduced climate harm and preserved ecological space as Global North contraction lowers emissions; they do not bear the direct economic contraction costs and may gain policy space for sustainable development.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_south_populations, beneficiary,
    powerless, generational, constrained, global).

% Inherit a climate system with stabilized or reduced warming due to contraction-driven mitigation; they have no present voice or exit from the decisions made now.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Produces the planetary boundary and emissions-gap assessments that ground the constraint's empirical claims; operates as an analytical seat without direct extraction or cost-bearing.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, climate_science_community, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents catastrophic climate harm by aligning global emissions with planetary boundaries through coordinated reduction of Global North consumption and production, solving the collective-action problem of atmospheric overshoot.
% TRANSFER_FUNCTION: Moves economic capacity, carbon budget space, and ecological resilience from Global North present consumption and production to Global South populations and future generations via planned contraction.
% ABSENT_VOICES: Growth-economist policymakers, fossil-fuel industry representatives, and techno-optimist advocates are structurally marginal in this reading's framing of legitimate response; their green-growth and adaptation-first alternatives are treated as outside the boundary of serious consideration.
% DISAPPEARANCE_RATIONALE: If the constraint vanished (i.e., if growth-framework mitigation were accepted as legitimate), Global North economies would resume high-carbon expansion, international climate finance would shrink, and the policy window for South-led sustainable development would narrow â the global climate policy landscape would reorganize around technological transition and adaptation rather than contraction.
% FOUNDING_PROBLEM: Uncontrolled greenhouse gas emissions from industrialized economies threatening planetary habitability, with Global North historical emissions exhausting the carbon budget and leaving insufficient ecological space for Global South development and intergenerational survival.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessments attest to the emissions gap and planetary boundary breach; Global South climate justice movements corroborate the inequitable distribution of atmospheric space. Growth-economist critics and mainstream OECD policymakers contest the contraction framing, arguing absolute decoupling can resolve the tension without economic contraction.
narrative_ontology:disappearance_verdict(climate_harm_prevention__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_harm_prevention__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__degrowth_reading, 0.75, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75) is high because the constraint requires substantial reduction in Global North economic activity and consumption. Suppression (0.70) is high because maintaining contraction against growth-oriented political economy requires active policy enforcement, carbon budgeting, and suppression of fossil-capital expansion. Theater ratio (0.32) reflects moderate performativity in international climate negotiations where contraction rhetoric often exceeds implementation, though the degrowth framework itself is analytically substantive. Accessibility collapse (0.48) is moderate: from within the degrowth framework, green-growth alternatives appear structurally inadequate, but those alternatives remain widely visible and institutionally dominant outside this reading. Resistance (0.78) is very high due to entrenched opposition from growth-dependent industries, policymakers, and publics in wealthy economies.
 *
 * PERSPECTIVAL GAP:
 *   The Global North payer seats experience this constraint as aggressive extraction imposed by ecological limits and international justice claims, whereas the Global South and future-generation beneficiary seats experience it as necessary coordination to prevent existential harm. The agenda-setter seat (Global North states enforcing contraction) experiences internal tension between the coordination function (climate stability) and the domestic political costs of extraction (contraction). The engine will compute these seats differently: beneficiaries should type toward rope or mountain, payers toward snare or tangled_rope, and the agenda setter toward tangled_rope given its dual position.
 *
 * DIRECTIONALITY LOGIC:
 *   Global South populations and future generations are declared beneficiaries, deriving low directionality (d near 0.0) â the constraint subsidizes their climate security. Global North corporations and citizens are declared payers, deriving high directionality (d near 1.0) â the constraint extracts from their present economic activity. Global North states sit ambiguously as agenda setters who must enforce extraction upon their own economies; their structural position is mixed but their role as enforcement apparatus does not exempt them from the costs of contraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by its explicit coordination function: it is not pure extraction because it solves a genuine collective-action problem (preventing catastrophic climate change that market mechanisms fail to address). Without this coordination component, the asymmetric cost-bearing would read as a snare. The presence of a real coordination function (atmospheric stabilization) alongside asymmetric extraction (Global North contraction) makes tangled_rope the structurally accurate classification. If the coordination function were to atrophy â if climate stabilization were achieved and the constraint persisted as pure redistribution â it would degrade toward snare or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_impossibility_status,
    'Is mitigation within a growth framework physically impossible due to planetary boundaries, or politically infeasible due to institutional lock-in?',
    'Empirical assessment of absolute decoupling rates against emissions reduction requirements; historical analysis of whether any economy has achieved sufficient decoupling to meet Paris-compatible trajectories.',
    'If physically impossible, the constraint''s core premise is a Mountain-grade natural law; if merely politically infeasible, the constraint is a constructed Tangled Rope maintained by institutional inertia and power asymmetries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_impossibility_status, empirical, 'Physical versus political impossibility of growth-compatible mitigation').

omega_variable(
    kernel_reading_relationship,
    'Does the degrowth reading foreclose the mitigation_priority reading, or do they coexist as alternative policy frameworks within a single commitment system?',
    'Logical analysis of whether green growth (mitigation_priority) and planned contraction (degrowth) can both be held as live options within a single climate policy framework without contradiction.',
    'If foreclosed, the kernel generates irreconcilable factions and the engine should flag hard boundary disputes; if coexisting, the classification should allow for divergent seat perceptions without logical contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relationship, conceptual, 'Structural relationship between degrowth and mitigation-priority sibling readings').

omega_variable(
    beneficiary_victim_boundary,
    'Do Global South populations and future generations genuinely benefit from Global North contraction, or do they bear secondary costs from reduced trade, aid, and technology transfer?',
    'Integrated assessment models comparing climate harm reduction against economic spillover effects of Global North contraction; empirical analysis of aid and trade dependency.',
    'If secondary costs exceed primary benefits, the beneficiary/victim structure inverts for Global South populations and directionality must be recomputed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_victim_boundary, empirical, 'Net benefit versus secondary cost ambiguity for Global South beneficiaries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__degrowth_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chp_degrowth_tr_t0, climate_harm_prevention__degrowth_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(chp_degrowth_tr_t4, climate_harm_prevention__degrowth_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(chp_degrowth_tr_t8, climate_harm_prevention__degrowth_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(chp_degrowth_tr_t12, climate_harm_prevention__degrowth_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(chp_degrowth_tr_t16, climate_harm_prevention__degrowth_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(chp_degrowth_tr_t20, climate_harm_prevention__degrowth_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(chp_degrowth_tr_t24, climate_harm_prevention__degrowth_reading, theater_ratio, 24, 0.32).

% Extraction over time
narrative_ontology:measurement(chp_degrowth_be_t0, climate_harm_prevention__degrowth_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(chp_degrowth_be_t4, climate_harm_prevention__degrowth_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(chp_degrowth_be_t8, climate_harm_prevention__degrowth_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(chp_degrowth_be_t12, climate_harm_prevention__degrowth_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement(chp_degrowth_be_t16, climate_harm_prevention__degrowth_reading, base_extractiveness, 16, 0.69).
narrative_ontology:measurement(chp_degrowth_be_t20, climate_harm_prevention__degrowth_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(chp_degrowth_be_t24, climate_harm_prevention__degrowth_reading, base_extractiveness, 24, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(chp_degrowth_su_t0, climate_harm_prevention__degrowth_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(chp_degrowth_su_t4, climate_harm_prevention__degrowth_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(chp_degrowth_su_t8, climate_harm_prevention__degrowth_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(chp_degrowth_su_t12, climate_harm_prevention__degrowth_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(chp_degrowth_su_t16, climate_harm_prevention__degrowth_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(chp_degrowth_su_t20, climate_harm_prevention__degrowth_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(chp_degrowth_su_t24, climate_harm_prevention__degrowth_reading, suppression_requirement, 24, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__degrowth_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
