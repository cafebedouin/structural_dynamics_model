% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__degrowth_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: climate_response_imperative__degrowth_reading
 *   human_readable: Climate Response Imperative: Degrowth Reading
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   The degrowth reading of climate response asserts that mitigation and
 *   adaptation cannot succeed without structural contraction of material
 *   throughput in the Global North, accompanied by radical redistribution and
 *   post-growth institutional redesign. Present-day Global North populations,
 *   particularly affluent consumers and workers in carbon-intensive
 *   industries, enter the victim set via mandatory consumption reduction,
 *   shortened working hours, and asset devaluation. Future generations and
 *   Global South populations are the beneficiaries: stabilized climate,
 *   preserved carbon budget allocation, and equitable adaptation capacity.
 *   The reading rejects technological solutions (carbon capture/removal) as
 *   insufficient and false hope, treating them as enabling delay and lock-in.
 *   This is ONE reading of the contested climate response kernel; sibling
 *   readings (mitigation-priority and adaptation-priority) stake different
 *   claims about sequencing, technological sufficiency, and distribution of
 *   burden. The engine computes how this reading's structural premises
 *   diverge from the others; this story presents only the degrowth reading's
 *   internal coherence.
 *
 * KEY AGENTS:
 *   - global_north_working_class: wage earners in carbon-intensive sectors, facing occupational obsolescence and income contraction (moderate/biographical/constrained)
 *   - global_north_affluent_consumers: high-consumption populations whose identity is fused with material throughput (powerful/biographical/identity_locked)
 *   - future_generations: civilizational beneficiaries with no voice in present negotiation (powerless/civilizational/trapped)
 *   - global_south_populations: organized beneficiaries with uneven access to transition resources (organized/generational/constrained)
 *   - carbon_intensive_industries: institutional payers facing structural incompatibility with the constraint (institutional/biographical/trapped)
 *   - global_north_climate_technocrats: agenda-setters who frame the reading via research and policy (institutional/generational/analytical)
 *   - mitigation_technology_advocates: excluded because high-technology scaling contradicts degrowth premise (institutional/biographical/trapped)
 *   - adaptation_localist_communities: excluded because distributed adaptation is treated as insufficient without Global North emissions collapse (organized/generational/constrained)
 *   - analytical observer: holds no material stake, traces structural tensions across seats (analytical/generational/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, 0.68).
domain_priors:suppression_score(climate_response_imperative__degrowth_reading, 0.52).
domain_priors:theater_ratio(climate_response_imperative__degrowth_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__degrowth_reading, "Climate Response Imperative: Degrowth Reading").
narrative_ontology:topic_domain(climate_response_imperative__degrowth_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__degrowth_reading, '7f3f7221-63ea-4c8b-b009-9b34a52ebbdd').
narrative_ontology:cs_kernel_codification('7f3f7221-63ea-4c8b-b009-9b34a52ebbdd', fixed_text).
narrative_ontology:cs_authority_grounding('7f3f7221-63ea-4c8b-b009-9b34a52ebbdd', extraction).
narrative_ontology:cs_interpretation_layer_present('7f3f7221-63ea-4c8b-b009-9b34a52ebbdd').
narrative_ontology:cs_reading_relation('7f3f7221-63ea-4c8b-b009-9b34a52ebbdd', climate_response_imperative__mitigation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f3f7221-63ea-4c8b-b009-9b34a52ebbdd', climate_response_imperative__adaptation_priority_reading, coexists_with).
narrative_ontology:cs_axiom('7f3f7221-63ea-4c8b-b009-9b34a52ebbdd', foundational, growth_emissions_decoupling_impossible).
narrative_ontology:cs_axiom_status(growth_emissions_decoupling_impossible, holdable).
narrative_ontology:cs_axiom_grounding('7f3f7221-63ea-4c8b-b009-9b34a52ebbdd', growth_emissions_decoupling_impossible, empirically_contingent).
narrative_ontology:cs_axiom('7f3f7221-63ea-4c8b-b009-9b34a52ebbdd', foundational, intergenerational_burden_equity).
narrative_ontology:cs_axiom_status(intergenerational_burden_equity, holdable).
narrative_ontology:cs_axiom_grounding('7f3f7221-63ea-4c8b-b009-9b34a52ebbdd', intergenerational_burden_equity, deontological).
narrative_ontology:cs_reference_frame('7f3f7221-63ea-4c8b-b009-9b34a52ebbdd', planetary_boundaries_integrity).
narrative_ontology:cs_drift_state('7f3f7221-63ea-4c8b-b009-9b34a52ebbdd', contemporary_2026, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7f3f7221-63ea-4c8b-b009-9b34a52ebbdd', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__degrowth_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, global_south_populations).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, present_day_global_north_workers).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, present_day_global_north_consumers).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, carbon_dependent_industries).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__degrowth_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(climate_response_imperative__degrowth_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is 0.68 at interval end, reflecting the substantial transfer of material consumption and working-time capacity from present Global North to future/Global South. Extraction rises from 0.48 to peak at 0.71 (t=30) as the transition deepens and compliance costs intensify, then slightly declines (0.68 at t=40) as institutional adaptations and alternative livelihoods reduce friction. Suppression is 0.52, moderate because the constraint relies less on coercive enforcement (border controls, consumption policing) and more on internalized acceptance of contraction as necessary—but substantial enforcement machinery exists to enforce carbon pricing, redirect investment away from expansion, and prevent exit into high-consumption niches. Theater is 0.41 and rising: as the constraint matures, a growing share of compliance performance is theatrical (corporate net-zero pledges, green-growth messaging, adaptation announcements) that mask the underlying material contraction required. Accessibility of alternatives collapses to 0.67—theoretically one could opt out via affluent informal consumption or migration, but the constraint's logic is thermodynamic (no escape into a parallel high-consumption future once global carbon budget depletes), so alternatives are genuinely foreclosed by physical limits rather than institutional rules alone. Resistance is high (0.71) because the constraint directly attacks identity and material position for powerful actors (affluent consumers, carbon-intensive industries) who have resources to resist: litigation over carbon pricing, exit to lower-regulation jurisdictions, rhetorical campaigns against degrowth framing. The measurement series track observed values (t=0 to t=30) and projected values (t=40) reflecting the 2026 present moment and speculative extension.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute radically different types from the same structural data. Global North workers experience the constraint as imposed extraction (d toward 0.8, high χ): their material security is attacked, exit routes are constrained, and they did not author the constraint. Global North affluent consumers sit at even higher d (0.85+) because identity-locking amplifies the directionality—the constraint is not merely expensive, it is existentially threatening to their sense of self. Carbon-intensive industries are trapped by definition (d=1.0): the constraint makes their core function impossible. But the agenda-setter seat (climate technocrats) experiences d much lower (0.15-0.25): they designed the constraint, have analytical distance from its extraction costs, and accumulate symbolic capital (expertise authority, moral framing) through its advocacy. Future generations and Global South populations have d values inverted (negative effective extraction = subsidy, d toward 0.0) because the constraint transfers resources to them. The engine computes these per-seat divergences from power, exit, beneficiary/victim declarations, and scope; the authored metrics describe the story-level average, which conceals the per-seat divergence. That divergence is the diagnostic output the framework exists to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary structure: future_generations and global_south_populations are named beneficiaries because the constraint's causal logic is that their material survival and climate stability depend on present Global North contraction. This is a causal claim about intergenerational and international redistribution, not a market claim about who gains income. Victim structure: present_day_global_north_workers and present_day_global_north_consumers are victims because the constraint requires they accept reduced throughput, shortened work weeks, lower consumption expectations, and asset devaluation. Carbon_dependent_industries are victims because their fundamental product (carbon-scale throughput) is incompatible with the constraint's thermodynamic logic—no efficiency gain resolves the contradiction. Exit analysis: global_north_working_class exit is constrained (skill retraining is expensive, cross-border relocation faces visa/climate migration barriers). Global_north_affluent_consumers have theoretical exit (wealthy informality, jurisdictional arbitrage) but identity_locked status (high-consumption identity is fused with social position) amplifies constraint costs psychologically even where material exit routes exist. Carbon_intensive_industries are trapped (their product is anti-functional to the constraint). Global_south_populations have constrained exit (climate damages accelerate regardless of their choices; they cannot opt out of adaptation costs even if they benefit from Global North emissions reduction). Future_generations have no exit (they inherit the planetary state set by present choices). Directionality is computed by the engine from these structural facts; the framework's derivation chain ensures that beneficiary/victim declarations, power atoms, and exit options feed d automatically without requiring manual d assignment.
 *
 * MANDATROPHY ANALYSIS:
 *   The degrowth reading exhibits mandatrophy characteristics but does not resolve into a fully degraded piton. The founding problem (growth-driven emissions incompatible with habitable climate) is live and recognized by climate physics and Global South negotiators—the problem has not died and the mandate has not outlived its function. However, theater is rising (0.41 at interval end, increasing from 0.22 at t=0), indicating growing gap between performative (corporate pledges, green-growth messaging) and functional (actual material contraction) compliance. This rising theater does NOT indicate piton degradation yet—it indicates transition-state dissonance where institutions claim adherence to degrowth while maintaining growth-compatible financial structures. If theater continues rising and actual contraction stalls, the constraint could degrade into piton status (mandate persists theatrically, actual function atrophies, no concentrated beneficiary maintains it actively). The mandatrophy question hinges on whether institutions complete the transition to post-growth operations or freeze in the performative stage. Current measurements show theatre rising but not yet dominant; the constraint remains tangled_rope (genuine coordination function + active extraction) rather than piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intergenerational_enforcement_mechanism,
    'By what mechanism can present-day populations be held accountable to future generations for compliance with the degrowth constraint? Future generations have no voice, no legal standing in present institutions, and no enforcement power.',
    'Institutional innovation: constitutional entrenchment of climate commitments, intergenerational ombudsperson offices with enforcement authority, damage litigation frameworks that anticipate future harms, or institutional mechanisms that give future-generation interests standing in present decisions.',
    'If no enforcement mechanism exists, the constraint relies entirely on present-generation moral commitment to a powerless constituency—high risk of atrophy. If strong mechanisms exist, the constraint binds present choices to future outcomes. This determines whether intergenerational transfer is enforceable or merely aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_enforcement_mechanism, conceptual, 'Whether intergenerational justice can be institutionally enforced or remains voluntary.').

omega_variable(
    global_south_transition_capacity,
    'Can Global South populations absorb and benefit from Global North degrowth-driven transition without catastrophic adaptation costs in the interim? They benefit from the degrowth endpoint but face accelerating climate damages during the transition interval.',
    'Modeling of climate damages vs. adaptation cost under different degrowth timelines and transfer magnitudes. Empirical observation of whether Global North redistribution actually reaches Global South adaptation capacity in real time, or whether it lags behind damage acceleration.',
    'If Global South faces worse outcomes under degrowth-transition than under continued high-emission trajectories, the constraint''s beneficiary framing is false—Global South populations would be victims, not beneficiaries, and the degrowth reading''s ethical foundation collapses. If redistribution is effective and timely, the constraint''s equity claim holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(global_south_transition_capacity, empirical, 'Whether Global South populations actually benefit from the degrowth transition given damage acceleration timeline.').

omega_variable(
    technological_decoupling_viability,
    'Is absolute decoupling of emissions from growth thermodynamically possible via technology (as mitigation-priority reading asserts) or are degrowth-level consumption reductions mandatory (as this reading asserts)?',
    'Empirical tracking of global emissions per unit GDP over next 10-15 years under aggressive renewable expansion, battery scaling, and efficiency improvement. If decoupling accelerates sufficiently, the foundational premise of the degrowth reading is undermined. If decoupling plateaus, the reading''s necessity claim strengthens.',
    'If decoupling is viable at required pace, mitigation-priority and adaptation-priority readings become coherent alternatives and degrowth becomes optional rather than mandatory. If decoupling is not viable, the degrowth reading''s extraction costs become justified as necessary rather than ideological.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_decoupling_viability, empirical, 'Whether technological decoupling of growth from emissions is feasible at required pace.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.52) structural (enforced by policy, pricing, border controls) or internalized (Global North populations accept degrowth as necessary self-discipline)? The distinction determines whether the constraint persists if enforcement capacity decays.',
    'Post-policy suppression trajectory: if degrowth enforcement suddenly ceased (carbon pricing revoked, border controls lifted, consumption restrictions removed), would Global North populations maintain reduced consumption? If yes, suppression is largely internalized. If consumption rebounds, suppression was structural.',
    'If suppression is internalized, the constraint is robust to enforcement decay and relies on shared belief in climate imperative. If structural, the constraint requires constant enforcement and is vulnerable to political reversal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether compliance with degrowth is internalized or requires structural enforcement.').

omega_variable(
    kernel_reading_contest,
    'Which reading of the climate response kernel is structurally correct: degrowth (this reading), mitigation-priority, or adaptation-priority? The readings offer different causal premises about what climate response requires and what it enables.',
    'The contest is conceptual and empirical: empirical via tracking whether decoupling/technology viability becomes clear (shifts toward mitigation-priority) or whether degrowth-level contraction proves unavoidable (strengthens this reading). Conceptual via clarifying whether climate physics and thermodynamics mandate degrowth or whether they permit growth-compatible alternatives. The engine routes this as an omega because the three readings are one kernel with unresolved interpretation.',
    'If mitigation-priority or adaptation-priority readings prove structurally correct, this reading is a false constraint claiming necessity where alternatives exist. If this reading is correct, the others are dangerous false solutions that enable delay. The resolution determines whether the constraint''s extraction costs are justified necessity or ideological imposition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the climate response kernel is structurally correct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__degrowth_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__degrowth_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(clim_tr_t5, climate_response_imperative__degrowth_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(clim_tr_t10, climate_response_imperative__degrowth_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(clim_tr_t15, climate_response_imperative__degrowth_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(clim_tr_t20, climate_response_imperative__degrowth_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(clim_tr_t25, climate_response_imperative__degrowth_reading, theater_ratio, 25, 0.44).
narrative_ontology:measurement(clim_tr_t30, climate_response_imperative__degrowth_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(clim_tr_t40, climate_response_imperative__degrowth_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__degrowth_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(clim_be_t5, climate_response_imperative__degrowth_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(clim_be_t10, climate_response_imperative__degrowth_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(clim_be_t15, climate_response_imperative__degrowth_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(clim_be_t20, climate_response_imperative__degrowth_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(clim_be_t25, climate_response_imperative__degrowth_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(clim_be_t30, climate_response_imperative__degrowth_reading, base_extractiveness, 30, 0.71).
narrative_ontology:measurement(clim_be_t40, climate_response_imperative__degrowth_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__degrowth_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(clim_su_t5, climate_response_imperative__degrowth_reading, suppression_requirement, 5, 0.44).
narrative_ontology:measurement(clim_su_t10, climate_response_imperative__degrowth_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(clim_su_t15, climate_response_imperative__degrowth_reading, suppression_requirement, 15, 0.53).
narrative_ontology:measurement(clim_su_t20, climate_response_imperative__degrowth_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(clim_su_t25, climate_response_imperative__degrowth_reading, suppression_requirement, 25, 0.56).
narrative_ontology:measurement(clim_su_t30, climate_response_imperative__degrowth_reading, suppression_requirement, 30, 0.57).
narrative_ontology:measurement(clim_su_t40, climate_response_imperative__degrowth_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__degrowth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_imperative__degrowth_reading, 0.18).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, climate_response_imperative__mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, climate_response_imperative__adaptation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, carbon_budget_thermodynamic_constraint).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, intergenerational_justice_imperative).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, global_inequality_redistribution_requirement).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_response_imperative kernel. Sibling readings (mitigation_priority, adaptation_priority) instantiate different causal claims about climate response sequencing and sufficiency. The three readings are not measurements of the same constraint from different angles—they are structurally distinct constraint specifications with different beneficiary/victim structures, different ε values, and different enforcement mechanisms. The degrowth reading treats technology and distributed adaptation as insufficient; the mitigation-priority reading treats degrowth as unnecessary; the adaptation-priority reading treats both as secondary to local resilience. All three affect downstream constraints in climate policy, economic transition, and equity frameworks. The kernel contest is unresolved; all three readings remain live in policy discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_imperative__degrowth_reading, powerless, 0.05).
constraint_indexing:directionality_override(climate_response_imperative__degrowth_reading, powerful, 0.88).
constraint_indexing:directionality_override(climate_response_imperative__degrowth_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
