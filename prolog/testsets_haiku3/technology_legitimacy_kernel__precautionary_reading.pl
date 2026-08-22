% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__precautionary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__precautionary_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__precautionary_reading
 *   human_readable: Technology Legitimacy via Precautionary Reversibility (Climate Mitigation Kernel)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The precautionary reading of the technology legitimacy kernel establishes
 *   reversibility-within-a-generation as the gate for climate mitigation
 *   technology acceptance. Under this reading, renewable energy sources
 *   (solar, wind, battery storage with recycling pathways) are legitimate
 *   because their failure modes and material legacies can be remediated
 *   within a human lifetime. Nuclear, deep-sea carbon sequestration, and
 *   geoengineering are excluded because their failure modes (spent fuel,
 *   accident zones, unintended planetary effects) persist across generational
 *   boundaries, constraining future choice sets irreversibly. The reading is
 *   one instantiation of a contested kernel: reliability-primacy and
 *   velocity-primacy readings compete with precaution, each grounding
 *   technology legitimacy on different criteria. The constraint's operation
 *   is substantially extractive because it privileges current
 *   renewable-industry beneficiaries while suppressing nuclear and other
 *   excluded technologies, yet frames this preference as protection of future
 *   generations (who have no voice in the decision). The measurement series
 *   tracks rising suppression and extraction as the precautionary frame
 *   consolidates in policy (EU Green Deal, California frameworks, Nordic
 *   climate pledges) and as excluded technologies face increasing regulatory
 *   and investment barriers.
 *
 * KEY AGENTS:
 *   - renewable_energy_operators — primary beneficiaries, institutional power, globally mobile exit
 *   - nuclear_technology_advocates — primary payers, institutional power, constrained exit due to sunk infrastructure
 *   - future_generations — nominally protected but structurally powerless, completely trapped, civilizational time horizon
 *   - climate_science_authorities — agenda-setters who specify what reversibility means and certify technologies
 *   - baseload_dependent_systems — payers bearing grid complexity costs, moderate power, regionally constrained
 *   - velocity_coalition and reliability_maximizers — excluded from this reading's frame, would challenge reversibility as sufficient criterion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__precautionary_reading, 0.68).
domain_priors:suppression_score(technology_legitimacy_kernel__precautionary_reading, 0.72).
domain_priors:theater_ratio(technology_legitimacy_kernel__precautionary_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__precautionary_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__precautionary_reading, "Technology Legitimacy via Precautionary Reversibility (Climate Mitigation Kernel)").
narrative_ontology:topic_domain(technology_legitimacy_kernel__precautionary_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__precautionary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__precautionary_reading, '688d570e-e425-4301-8c41-28d75c1e48e4').
narrative_ontology:cs_kernel_codification('688d570e-e425-4301-8c41-28d75c1e48e4', fixed_text).
narrative_ontology:cs_authority_grounding('688d570e-e425-4301-8c41-28d75c1e48e4', extraction).
narrative_ontology:cs_interpretation_layer_present('688d570e-e425-4301-8c41-28d75c1e48e4').
narrative_ontology:cs_reading_relation('688d570e-e425-4301-8c41-28d75c1e48e4', technology_legitimacy_kernel__reliability_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('688d570e-e425-4301-8c41-28d75c1e48e4', technology_legitimacy_kernel__velocity_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('688d570e-e425-4301-8c41-28d75c1e48e4', foundational, irreversible_harm_unacceptable).
narrative_ontology:cs_axiom_status(irreversible_harm_unacceptable, holdable).
narrative_ontology:cs_axiom_grounding('688d570e-e425-4301-8c41-28d75c1e48e4', irreversible_harm_unacceptable, deontological).
narrative_ontology:cs_axiom('688d570e-e425-4301-8c41-28d75c1e48e4', foundational, future_choice_set_preservation).
narrative_ontology:cs_axiom_status(future_choice_set_preservation, holdable).
narrative_ontology:cs_axiom_grounding('688d570e-e425-4301-8c41-28d75c1e48e4', future_choice_set_preservation, deontological).
narrative_ontology:cs_axiom('688d570e-e425-4301-8c41-28d75c1e48e4', secondary, generational_timescale_boundary).
narrative_ontology:cs_axiom_status(generational_timescale_boundary, holdable).
narrative_ontology:cs_axiom_grounding('688d570e-e425-4301-8c41-28d75c1e48e4', generational_timescale_boundary, conventional).
narrative_ontology:cs_reference_frame('688d570e-e425-4301-8c41-28d75c1e48e4', intergenerational_option_value_preservation).
narrative_ontology:cs_drift_state('688d570e-e425-4301-8c41-28d75c1e48e4', carbon_budget_acceleration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('688d570e-e425-4301-8c41-28d75c1e48e4', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, renewable_energy_operators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, near_term_climate_goals_constituency).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, future_generations).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, nuclear_technology_advocates).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, baseload_dependent_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, future_generations).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, precautionary_governance_bodies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wind, solar, and battery storage operators benefit directly from the precautionary reading: their technologies (decommissionable panels, retrievable turbine materials, battery recycling pathways) satisfy the reversibility criterion and thus receive policy priority, investment, and grid access guarantees. They benefit from exclusion of competing baseload sources.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, renewable_energy_operators, beneficiary,
    organized, biographical, mobile, global).

% Nuclear engineers, reactor operators, and nations dependent on nuclear baseload face exclusion under this reading: spent fuel repos, accident-zone exclusion (Fukushima, Chernobyl timescales measured in centuries), and decommissioning waste persist beyond a generation. The constraint's enforcement (licensing boards, public comment periods, investment gatekeeping) actively suppresses new nuclear construction in jurisdictions adopting the precautionary frame.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, nuclear_technology_advocates, payer,
    institutional, generational, constrained, global).

% Structurally benefit from the constraint's core logic—they inherit only reversible legacy costs—yet are completely trapped as unrepresented stakeholders in today's energy decisions. If reversibility fails (climate tipping point accelerates, carbon budget exhausted before transition completes), they bear the irreversible outcome. They are named beneficiaries of the rule but have no seat at the table where the rule is enforced.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__precautionary_reading, future_generations, payer).

% Industrial processes, hospitals, data centers, and grid operators dependent on continuous firm power face higher costs and technical complexity under the precautionary reading. Intermittency management, storage buildout, and demand-side flexibility are necessary but expensive. They pay in grid infrastructure cost and operational complexity, constrained by the reading's exclusion of low-carbon firm power sources.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, baseload_dependent_systems, payer,
    moderate, biographical, constrained, regional).

% IPCC, national climate agencies, and peer-review bodies adjudicate the precautionary reading's legitimacy claims. They specify what 'bounded failure modes' and 'reversible within a generation' mean operationally, set the carbon budget timelines, and certify which technologies meet the criteria. Their authority grounds the constraint's enforcement.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, climate_science_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Nations and institutions prioritizing deployment speed over precaution (China's rapid nuclear buildout, India's coal transition, Germany's energy-security-driven policy shifts) are structurally excluded from the precautionary reading's framing. They would argue that reversibility is a luxury if climate tipping points occur before the transition completes; their voice in the contest is not heard in this reading's bounded deliberative frame.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, velocity_coalition, excluded,
    powerful, biographical, trapped, global).

% Grid operators, grid-scale frequency regulators, and reliability-centric policy-makers prioritize dispatchability and baseload capacity. They would argue that intermittent sources cannot stabilize grids without dispatchable backup; the precautionary reading's exclusion of low-carbon baseload means they must accept higher reliability risk or maintain fossil-fuel backup. Excluded from the deliberation because they challenge the reversibility criterion's adequacy for climate stability.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, reliability_maximizers, excluded,
    powerful, biographical, trapped, global).

% Regulatory agencies, legislative bodies, and municipal governments that have adopted or are considering precautionary technology frameworks (EU Green Deal with reversibility language, California's technology assessment boards, Nordic climate frameworks) set and enforce the constraint. They benefit from the clarity the framework provides, though it locks them into a specific reading of legitimacy that may conflict with velocity or reliability readings as grid stress increases.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, precautionary_governance_bodies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__precautionary_reading, precautionary_governance_bodies, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__precautionary_reading, renewable_energy_operators).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__precautionary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared legitimacy criterion for technology selection in climate mitigation that protects against irreversible harms to future generations and keeps option value open: a decentralized set of energy investors can evaluate technologies against a coherent standard rather than guessing at hidden long-term costs.
% TRANSFER_FUNCTION: Moves investment capital and policy priority from non-reversible (nuclear, fossil CCS with permanent storage, deep-sea deployment) toward reversible technologies (solar, wind, batteries with recycling paths). Transfer also moves decision-making authority upstream into precautionary science bodies and away from purely economic/grid-optimization seats.
% ABSENT_VOICES: Future generations are nominally protected by the reading but have no voice in certifying what counts as 'reversible within a generation' or in setting the timescales. Nations and constituencies for which the precautionary timeline conflicts with their carbon-budget exhaustion timeline (e.g., countries facing imminent climate impacts) are excluded from the deliberation; they would argue that waiting for reversibility is a luxury they cannot afford.
% DISAPPEARANCE_RATIONALE: If the precautionary legitimacy criterion vanished and reverted to purely economic efficiency or grid-stability criteria, investment would redirect toward cheaper/more-reliable sources (new nuclear, gas with CCS, potentially risky geoengineering), waste management frameworks would shift (less recycling emphasis), and long-term liability structures would change. Energy policy architecture would reorganize around different technology clusters within months.
% FOUNDING_PROBLEM: Post-industrial energy systems create legacy costs (radioactive waste, climate-disrupted ecosystems, stranded fossil infrastructure) that persist far beyond the decision-makers' lifetimes and constrain future options. The founding problem is: how do we choose climate mitigation technologies without gambling with future generations' choice sets?
% FOUNDING_PROBLEM_CORROBORATION: Climate science literature (IPCC synthesis reports, Nature Climate Change journals, paleoclimate studies showing tipping-point irreversibility) corroborates that some climate impacts are irreversible on human timescales and that technology choices today constrain future decarbonization paths. Nuclear waste repository studies (U.S. Geological Survey on Yucca Mountain, international radioactive waste forums) corroborate that certain energy legacies are not reversible within a generation. Independent technology assessment bodies (MIT Energy Initiative, International Energy Agency technology roadmaps) and future-generations advocacy groups (not energy industry) corroborate the problem's persistence.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__precautionary_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__precautionary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__precautionary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__precautionary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__precautionary_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__precautionary_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__precautionary_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__precautionary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.51 to 0.68 over the interval as precautionary governance frameworks become operationalized and deployed (observed data through t=20, projected rise through t=30, then projected decline as pressure accumulates). The trajectory reflects two offsetting dynamics: (1) consolidation of precaution as the governing principle, locking in renewable beneficiary advantage and nuclear exclusion — extraction rises; (2) climate emergency pressure mounting (carbon budget exhaustion nearing, climate impacts accelerating) — excluded parties and velocity-coalition advocates gain voice and challenge the reading's adequacy, creating pressure for framework revision — extraction declining slightly at t=35 as the reading faces delegitimization. Suppression requirement tracks enforcement intensity: as the precautionary frame locks into policy (t=0-20), regulatory suppression of nuclear and baseload development must increase to maintain the boundary (observed rise to 0.73). As climate velocity concerns mount (t=20-35), suppression requirements rise further (projected 0.78 at t=30) to hold the line against reliability-primacy and velocity-primacy challenges, then decline slightly (0.72 at t=35) as the reading's authority begins to erode. Theater ratio (0.25→0.46→0.41) captures the rising performative content: early reversibility rhetoric is substantive (decommissioning plans, recycling infrastructure); as climate pressure mounts, the same rhetoric becomes increasingly theatrical — the reversibility criterion is maintained in policy language while real deployment accelerates in ways that violate it (utility-scale battery storage with uncertain end-of-life, rooftop solar waste streams poorly tracked, grid transition speed outpacing material supply chains). The decline at t=35 reflects breakdown of the performative consensus.
 *
 * PERSPECTIVAL GAP:
 *   The precautionary reading's beneficiaries (renewable operators, climate-justice constituencies, science authorities) perceive the constraint as protective coordination: a shared legitimacy standard that prevents gambling with future generations. From their seat, enforcement is justified and suppression is gatekeeping against dangerous recklessness. Payers (nuclear advocates, baseload operators, velocity-focused nations) perceive the same structure as extraction: a legitimacy criterion designed to privilege already-competitive renewables while removing low-carbon alternatives that compete with renewable market share, masked as future-generations protection. Future generations are nominally the beneficiaries but have zero voice — from their prospective seat (unrepresented, trapped, the actual long-term target of the constraint), the arrangement is a cover story: today's beneficiaries are deciding what reversibility means without input from those who will live with the consequences if reversibility fails or if climate velocity overwhelms the transition before it completes. The engine computes per-seat classification: renewable-operator and science-authority seats should see coordination; nuclear-advocate and velocity-coalition seats should see snare or tangled-rope extraction; future-generations seats should show asymmetric powerless-target directionality despite nominal beneficiary status.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable operators: beneficiaries with institutional power and exit mobility (arbitrage — they can shift to other technologies or geographies; they can lobby to reshape the legitimacy standard). Derived d is low (~0.15-0.3 range, subsidized by the reading). Nuclear advocates: institutional power but constrained exit (sunk reactors, trained workforces, regulatory infrastructure that locks in their identity with nuclear technology). Derived d is high (~0.75-0.85 range, full targets of extraction). Baseload-dependent systems: moderate power, regionally constrained (grid operators cannot easily leave their jurisdiction). Derived d is moderate-high (~0.65-0.75). Future generations: powerless, completely trapped (no exit), civilizational time horizon, nominally beneficiaries but structurally targets. This is the critical directionality anomaly: the reading declares future generations as beneficiaries (the ones protected from irreversible harm), yet the structural data (powerlessness, trapedness, no voice in defining what counts as reversible, no seat at enforcement tables) produces d≈1.0 — the full-target directionality of the most vulnerable seat in the system. The engine's per-seat computation should flag this: a nominal beneficiary with target directionality is exactly how a false-summit constraint appears to those it claims to protect. This is NOT an error — it is precisely what the precautionary reading's structure entails. The commentary must make this explicit.
 *
 * MANDATROPHY ANALYSIS:
 *   The precautionary reading avoids straightforward mandatrophy because its founding problem (choosing mitigation technologies without gambling with future generations' choice sets) remains live: climate systems are still on trajectory toward tipping points, and irreversible technology legacies still persist. However, a secondary mandatrophy is emerging: the operationalization problem. The founding problem is 'protect future choice sets'; the mandated solution is 'enforce reversibility-within-a-generation as the legitimacy gate.' As climate velocity pressure mounts (carbon budget exhaustion nearing 2030/2050 targets), the mandate is beginning to conflict with its own founding purpose: maintaining reversibility-within-a-generation cannot occur if the climate transition takes longer than a generation because technology choices made now will lock in outcomes they cannot reverse. The measurement series (theater_ratio rising from 0.25 to 0.46, then slightly declining as the reading faces challenge) and the projected suppression peak at t=30 (followed by decline) reflect this: the constraint faces delegitimization as the founding problem's conditions shift. The mandatrophy is conditional on velocity: if carbon-budget exhaustion arrives before the transition completes, the precautionary reading's mandate dissolves because reversibility becomes structurally impossible. The reading is not resolved-mandatrophy yet (founding problem still live), but it is approaching the threshold where mandatrophy becomes inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reversibility_boundary_definition,
    'What precisely counts as ''reversible within a generation''? Is it material reversibility (can we physically recover, recycle, or remediate the technology), causal reversibility (can we undo the climate impacts if the technology fails), or choice-set reversibility (does the technology''s legacy constrain future options)?',
    'Comparative technology assessment: study the end-of-life pathways for accepted (solar, wind, battery) vs. excluded (nuclear, deep CCS, geoengineering) technologies. Map the long-tail costs that emerge 20-50 years post-deployment. Identify which definition of reversibility each technology actually satisfies.',
    'If the boundary is material reversibility, some renewable technologies (rooftop solar with no recycling infrastructure, battery supply chains with rare-earth mining legacies) may fail the criterion as deployed. If the boundary is choice-set reversibility, some nuclear applications (small modular reactors with passive cooling) might meet it. The precautionary reading''s legitimacy depends on which definition governs the gate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reversibility_boundary_definition, conceptual, 'Operationalization ambiguity: what counts as reversible under the precautionary standard.').

omega_variable(
    future_generation_representation,
    'Can a constraint nominate future generations as beneficiaries when they have zero representation in the decision process? Is this precaution or paternalism, and does the distinction matter for the constraint''s legitimacy?',
    'Empirical: track whether future-generation representation mechanisms (discounting conventions, intergenerational equity boards, long-term accountability structures) are actually implemented and whether they reshape technology choices. Conceptual: resolve whether precautionary framing without representation is consistent with climate justice frameworks that ground legitimacy in voice and participation.',
    'If representation is not implemented, the constraint is exposed as paternalistic extraction masked as protection — future generations are the stated beneficiaries but are being used as rhetorical cover for a technology preference that benefits today''s renewables operators. This would reclassify the constraint from Tangled Rope (coordination + extraction) to Snare (pure extraction with false beneficiary). If representation is implemented and meaningful, the constraint''s legitimacy is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_generation_representation, preference, 'Representation gap: whether nominally protected parties have actual voice in the constraint''s operation.').

omega_variable(
    velocity_climate_tipping_point_conflict,
    'If climate tipping points arrive before the transition to reversible technologies completes (e.g., carbon budget exhausted before renewables reach 90% grid penetration), does the precautionary reading''s mandate collapse? Can reversibility protect choice sets if the climate system''s reversibility window has already closed?',
    'Empirical: model the intersection of technology transition timelines and climate tipping-point timescales. Identify whether precautionary timelines are compatible with carbon-budget timelines. Conceptual: articulate what ''reversible'' means if the planetary climate system itself is on an irreversible trajectory — does protecting human technology reversibility matter if the climate context has already closed future options?',
    'If tipping points arrive first, the precautionary reading''s founding problem dissolves: reversibility becomes structurally impossible because the climate context will not allow a transition-at-leisure timescale. This is conditional mandatrophy. The reading''s authority erodes and reclassification pressure increases. The constraint becomes a false promise (protecting future choice sets while the climate system removes them).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(velocity_climate_tipping_point_conflict, empirical, 'Timeline conflict: whether precautionary reversibility timescales align with climate urgency timescales.').

omega_variable(
    reading_contingency_on_kernel_interpretation,
    'The precautionary reading instantiates ONE normative claim about technology legitimacy, but the kernel (the technology legitimacy commitment) is contested among three readings. If one sibling reading (velocity or reliability primacy) gains institutional dominance, does this reading become foreclosed, or does it persist as a live alternative held by a subordinated coalition?',
    'Historical precedent study: examine how contested kernels have resolved in other domains (constitutional interpretation, medical ethics, scientific standards). Track whether foreclosure (one reading becomes normatively impossible) or coexistence (readings persist as live alternatives) is the typical pattern.',
    'If foreclosure, the precautionary reading''s authority is temporary — it persists only as long as its supporting coalition maintains institutional power. If coexistence, it persists as one legitimate reading indefinitely even if others dominate policy. The durability and stability of the constraint depends on whether this is a transient policy frame or a stable normative commitment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contingency_on_kernel_interpretation, conceptual, 'Meta-kernel uncertainty: whether contested readings can coexist indefinitely or whether one eventually forecloses the others.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__precautionary_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(tech_tr_t5, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(tech_tr_t10, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(tech_tr_t15, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(tech_tr_t25, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 25, 0.44).
narrative_ontology:measurement(tech_tr_t30, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 30, 0.46).
narrative_ontology:measurement(tech_tr_t35, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 0, 0.51).
narrative_ontology:measurement(tech_be_t5, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(tech_be_t10, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(tech_be_t15, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(tech_be_t25, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(tech_be_t30, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(tech_be_t35, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(tech_su_t5, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(tech_su_t10, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(tech_su_t15, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement(tech_su_t25, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 25, 0.76).
narrative_ontology:measurement(tech_su_t30, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(tech_su_t35, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 35, 0.72).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=35
narrative_ontology:measurement(tech_grid_01, technology_legitimacy_kernel__precautionary_reading, accessibility_collapse(class), 0, 0.68).
narrative_ontology:measurement(tech_grid_02, technology_legitimacy_kernel__precautionary_reading, accessibility_collapse(class), 35, 0.74).
narrative_ontology:measurement(tech_grid_03, technology_legitimacy_kernel__precautionary_reading, accessibility_collapse(individual), 0, 0.48).
narrative_ontology:measurement(tech_grid_04, technology_legitimacy_kernel__precautionary_reading, accessibility_collapse(individual), 35, 0.62).
narrative_ontology:measurement(tech_grid_05, technology_legitimacy_kernel__precautionary_reading, accessibility_collapse(organizational), 0, 0.55).
narrative_ontology:measurement(tech_grid_06, technology_legitimacy_kernel__precautionary_reading, accessibility_collapse(organizational), 35, 0.68).
narrative_ontology:measurement(tech_grid_07, technology_legitimacy_kernel__precautionary_reading, accessibility_collapse(structural), 0, 0.71).
narrative_ontology:measurement(tech_grid_08, technology_legitimacy_kernel__precautionary_reading, accessibility_collapse(structural), 35, 0.79).
narrative_ontology:measurement(tech_grid_09, technology_legitimacy_kernel__precautionary_reading, resistance(class), 0, 0.58).
narrative_ontology:measurement(tech_grid_10, technology_legitimacy_kernel__precautionary_reading, resistance(class), 35, 0.62).
narrative_ontology:measurement(tech_grid_11, technology_legitimacy_kernel__precautionary_reading, resistance(individual), 0, 0.52).
narrative_ontology:measurement(tech_grid_12, technology_legitimacy_kernel__precautionary_reading, resistance(individual), 35, 0.48).
narrative_ontology:measurement(tech_grid_13, technology_legitimacy_kernel__precautionary_reading, resistance(organizational), 0, 0.64).
narrative_ontology:measurement(tech_grid_14, technology_legitimacy_kernel__precautionary_reading, resistance(organizational), 35, 0.58).
narrative_ontology:measurement(tech_grid_15, technology_legitimacy_kernel__precautionary_reading, resistance(structural), 0, 0.51).
narrative_ontology:measurement(tech_grid_16, technology_legitimacy_kernel__precautionary_reading, resistance(structural), 35, 0.55).
narrative_ontology:measurement(tech_grid_17, technology_legitimacy_kernel__precautionary_reading, stakes_inflation(class), 0, 0.64).
narrative_ontology:measurement(tech_grid_18, technology_legitimacy_kernel__precautionary_reading, stakes_inflation(class), 35, 0.71).
narrative_ontology:measurement(tech_grid_19, technology_legitimacy_kernel__precautionary_reading, stakes_inflation(individual), 0, 0.41).
narrative_ontology:measurement(tech_grid_20, technology_legitimacy_kernel__precautionary_reading, stakes_inflation(individual), 35, 0.53).
narrative_ontology:measurement(tech_grid_21, technology_legitimacy_kernel__precautionary_reading, stakes_inflation(organizational), 0, 0.52).
narrative_ontology:measurement(tech_grid_22, technology_legitimacy_kernel__precautionary_reading, stakes_inflation(organizational), 35, 0.68).
narrative_ontology:measurement(tech_grid_23, technology_legitimacy_kernel__precautionary_reading, stakes_inflation(structural), 0, 0.73).
narrative_ontology:measurement(tech_grid_24, technology_legitimacy_kernel__precautionary_reading, stakes_inflation(structural), 35, 0.78).
narrative_ontology:measurement(tech_grid_25, technology_legitimacy_kernel__precautionary_reading, suppression(class), 0, 0.74).
narrative_ontology:measurement(tech_grid_26, technology_legitimacy_kernel__precautionary_reading, suppression(class), 35, 0.81).
narrative_ontology:measurement(tech_grid_27, technology_legitimacy_kernel__precautionary_reading, suppression(individual), 0, 0.52).
narrative_ontology:measurement(tech_grid_28, technology_legitimacy_kernel__precautionary_reading, suppression(individual), 35, 0.64).
narrative_ontology:measurement(tech_grid_29, technology_legitimacy_kernel__precautionary_reading, suppression(organizational), 0, 0.68).
narrative_ontology:measurement(tech_grid_30, technology_legitimacy_kernel__precautionary_reading, suppression(organizational), 35, 0.79).
narrative_ontology:measurement(tech_grid_31, technology_legitimacy_kernel__precautionary_reading, suppression(structural), 0, 0.76).
narrative_ontology:measurement(tech_grid_32, technology_legitimacy_kernel__precautionary_reading, suppression(structural), 35, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__precautionary_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(technology_legitimacy_kernel__precautionary_reading, 0.12).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel__reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel__velocity_primacy_reading).

% DUAL FORMULATION NOTE:
% The technology_legitimacy_kernel decomposes into three structurally distinct constraint stories, one per contested reading. The precautionary_reading (this file) establishes reversibility-within-a-generation as the legitimacy gate; reliability_primacy reading grounds legitimacy on dispatchability/baseload capacity; velocity_primacy reading grounds legitimacy on deployment speed. Each reading produces a different beneficiary/victim structure, different extractiveness profile, and different classification. The three are linked via network.affects_constraints in both directions: each reading influences the others' policy landscape and legitimacy conditions. They do not merge into a single constraint — the ε-invariance principle requires separate stories when different measurement bases (different definitions of legitimacy) produce different ε values for the same domain. Sibling relationships: precautionary coexists_with reliability (can coexist in same framework via technological layering — renewables for variable generation, nuclear for baseload); precautionary coexists_with velocity (can coexist via prioritization rules — velocity within precautionary bounds); reliability and velocity readings coexist_with each other (both prioritize non-precautionary criteria, can be held simultaneously).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_legitimacy_kernel__precautionary_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
