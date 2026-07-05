% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__degrowth_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: climate_response_obligation__degrowth_reading
 *   human_readable: Degrowth Reading of the Climate Response Obligation: Sufficiency Over Efficiency
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This story instantiates the degrowth reading of the
 *   climate_response_obligation kernel: the claim that staying within
 *   planetary boundaries requires absolute reductions in material and energy
 *   throughput, not merely efficiency improvements layered onto continued
 *   growth. Under this reading, capital accumulation itself becomes the
 *   extractive mechanism against planetary limits, current Global North
 *   consumption enters the victim set (asked to accept sufficiency limits),
 *   and Global South development is structurally constrained unless the North
 *   reduces first and transfers headroom. This is a distinct constraint from
 *   the mitigation_priority reading (which treats decarbonization speed as
 *   the object, largely compatible with continued growth via technology
 *   substitution) and the adaptation_priority reading (which treats warming
 *   as a sunk cost and shifts the object to resilience investment). The three
 *   readings have different beneficiary/victim structures, different theories
 *   of what must change, and would be measured with different epsilon values
 *   — they are linked here only through network.affects_constraints and
 *   cs_structure.reading_relations, never merged.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, 0.58).
domain_priors:suppression_score(climate_response_obligation__degrowth_reading, 0.62).
domain_priors:theater_ratio(climate_response_obligation__degrowth_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__degrowth_reading, "Degrowth Reading of the Climate Response Obligation: Sufficiency Over Efficiency").
narrative_ontology:topic_domain(climate_response_obligation__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__degrowth_reading, '2209cb89-5773-489f-9e37-2a2dd1343e8d').
narrative_ontology:cs_kernel_codification('2209cb89-5773-489f-9e37-2a2dd1343e8d', distributed).
narrative_ontology:cs_authority_grounding('2209cb89-5773-489f-9e37-2a2dd1343e8d', distributed).
narrative_ontology:cs_reading_relation('2209cb89-5773-489f-9e37-2a2dd1343e8d', climate_response_obligation__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('2209cb89-5773-489f-9e37-2a2dd1343e8d', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('2209cb89-5773-489f-9e37-2a2dd1343e8d', foundational, absolute_throughput_reduction_required_not_efficiency_alone).
narrative_ontology:cs_axiom_status(absolute_throughput_reduction_required_not_efficiency_alone, holdable).
narrative_ontology:cs_axiom_grounding('2209cb89-5773-489f-9e37-2a2dd1343e8d', absolute_throughput_reduction_required_not_efficiency_alone, empirically_contingent).
narrative_ontology:cs_axiom('2209cb89-5773-489f-9e37-2a2dd1343e8d', foundational, capital_accumulation_is_extractive_against_planetary_limits).
narrative_ontology:cs_axiom_status(capital_accumulation_is_extractive_against_planetary_limits, holdable).
narrative_ontology:cs_axiom_grounding('2209cb89-5773-489f-9e37-2a2dd1343e8d', capital_accumulation_is_extractive_against_planetary_limits, conventional).
narrative_ontology:cs_reference_frame('2209cb89-5773-489f-9e37-2a2dd1343e8d', planetary_boundaries_biophysical_baseline).
narrative_ontology:cs_drift_state('2209cb89-5773-489f-9e37-2a2dd1343e8d', post_paris_agreement_implementation_gap, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('2209cb89-5773-489f-9e37-2a2dd1343e8d', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__degrowth_reading, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, planetary_biophysical_systems).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, global_south_frontline_communities).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_north_consumer_classes).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_south_development_aspirants).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, extractive_capital_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_south_frontline_communities).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, planetary_boundaries_framework).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, sufficiency_over_efficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Climate, biodiversity, freshwater, and biogeochemical systems absorb the consequences of material throughput. Under this reading, reducing extraction and consumption directly relieves pressure on these systems; they have no voice and no exit, only accumulating or receding load.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, planetary_biophysical_systems, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, planetary_biophysical_systems).

% Inherit whatever stock of planetary boundaries current consumption leaves intact. They benefit from throughput reduction now but have no seat at any negotiating table and cannot bargain for their own interests; their claim is asserted by proxies.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Bear the physical brunt of climate and ecological breakdown driven overwhelmingly by historical Global North throughput. Benefit if global extraction slows, but many also depend on continued industrial development for basic material security, so a throughput cap applied uniformly could constrain their own path to adequacy.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_south_frontline_communities, beneficiary,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__degrowth_reading, global_south_frontline_communities, payer).

% Asked to accept absolute reductions in material consumption, energy use, and mobility — not efficiency gains that let consumption keep rising, but sufficiency limits that cap it. Exit is constrained: individual reduction has negligible planetary effect, but political and social structures make opting out of the ambient consumption economy costly and identity-disruptive.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_north_consumer_classes, payer,
    moderate, biographical, constrained, national).

% Nations and populations seeking to industrialize, electrify, and raise material living standards find that a throughput ceiling framed against present global averages effectively asks them to forgo the accumulation trajectory the Global North already completed, unless the North reduces first and transfers headroom. Their development path is constrained by a rule they had no part in writing.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_south_development_aspirants, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__degrowth_reading, global_south_development_aspirants, excluded).

% Own or control resource-extraction, fossil-fuel, and mass-production capital whose valuation depends on continued or growing material throughput. Under this reading, capital accumulation itself is treated as an extractive mechanism against planetary limits; these holders face demands for stranded-asset write-downs and can relocate capital across jurisdictions faster than any single regime can enforce sufficiency limits.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, extractive_capital_holders, payer,
    powerful, biographical, arbitrage, global).

% Academic networks, NGOs, and some state actors who advocate binding throughput caps, work-time reduction, and consumption ceilings, and who administer or lobby for the policy instruments (resource caps, luxury consumption limits, GDP-delinked welfare metrics) that would enforce sufficiency. They set the agenda but currently hold no enforcement power comparable to states or capital markets.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, degrowth_policy_coalitions, agenda_setter,
    organized, generational, mobile, national).

% Would need to legislate and enforce absolute throughput limits, resource quotas, or degrowth-aligned industrial policy. Face competing pressure from GDP-growth political mandates, capital flight risk, and international competitiveness concerns, making enforcement politically costly even where technically feasible.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, state_regulators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__degrowth_reading, state_regulators, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a collective reduction in aggregate material and energy throughput so that combined human activity stays within biophysical limits that no single actor's unilateral restraint could achieve alone — a genuine collective-action problem, since planetary boundaries are breached by aggregate load regardless of who contributes it.
% TRANSFER_FUNCTION: Moves consumption capacity, resource access, and accumulation headroom away from current high-throughput populations (chiefly Global North consumer classes and extractive capital) toward planetary system stability, future generations, and — contingent on redistribution actually occurring — toward Global South development space.
% ABSENT_VOICES: Global South populations and states are frequently spoken for by Global North degrowth advocates rather than setting the terms themselves; extractive capital holders are present in negotiations but the workers dependent on extractive industries for livelihood are largely absent from the framing, as are future generations who cannot bargain at all.
% DISAPPEARANCE_RATIONALE: If the degrowth reading vanished as a policy frame, planetary-boundary pressure would not disappear (it is a biophysical reality independent of framing), but the specific political program — throughput caps, sufficiency policy, capital stranding — would evaporate, and consumption and accumulation would likely continue on a growth trajectory. Whether that constitutes 'world rearranges' or 'world unchanged' depends on whether one credits the degrowth program with counterfactual restraint it has not yet achieved at scale — hence contested.
% FOUNDING_PROBLEM: Ecological economists and biophysical scientists observed that continued exponential growth in material and energy throughput was breaching identifiable planetary boundaries (climate, biodiversity, nitrogen/phosphorus cycles, freshwater), and that efficiency gains alone (relative decoupling) were not achieving absolute reductions in throughput (Jevons paradox, rebound effects) — the founding claim is that only absolute sufficiency limits, not efficiency improvements, can hold aggregate throughput within limits.
% FOUNDING_PROBLEM_CORROBORATION: Independent biophysical indicators (Stockholm Resilience Centre planetary boundaries assessments, IPCC carbon budget accounting, material footprint data from UNEP) corroborate that throughput continues to breach multiple boundaries and that efficiency gains have not produced absolute decoupling at the global level — this corroboration comes from earth-system science bodies outside the degrowth policy coalitions themselves, though the SPECIFIC POLICY CONCLUSION (sufficiency over efficiency, absolute throughput caps) is contested even among climate policy actors who accept the underlying biophysical data.
narrative_ontology:disappearance_verdict(climate_response_obligation__degrowth_reading, contested).
narrative_ontology:founding_problem_status(climate_response_obligation__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__degrowth_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_obligation__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__degrowth_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate-rising 0.35→0.58: the coordination function (aggregate throughput reduction to preserve shared planetary systems) is genuine, but the mechanism for achieving it — imposing consumption ceilings, capital stranding, and development constraints — extracts real capacity from specific named groups (Global North consumers, Global South development aspirants, capital holders) in a directionally asymmetric way. Suppression rises over the interval (0.30→0.62) as the political program moves from advocacy toward enforceable policy instruments (resource quotas, luxury taxes, growth-delinked metrics) requiring real coercive infrastructure to hold against resistance from growth-dependent political economies. Theater ratio falls over time (0.55→0.40) as the movement's early phase (largely academic/discursive, high performative-to-functional ratio) gives way to more concrete policy proposals, though a meaningful performative residue remains since most jurisdictions have adopted no binding throughput caps. Resistance is authored high (0.78) because this reading meets active political and economic opposition from growth-coalitions, labor movements tied to industrial employment, and Global South governments who read the framing as freezing existing inequality.
 *
 * DIRECTIONALITY LOGIC:
 *   Planetary systems and future generations sit at the pure-beneficiary end of directionality despite having zero agency — the engine should treat them as low-d recipients of reduced extraction pressure, structurally distinct from any party that could bargain. Global North consumer classes and extractive capital holders sit at the target end: the constraint's core transfer function runs consumption capacity and accumulation headroom away from them. Global South development aspirants are the most structurally ambiguous seat — nominal beneficiaries of planetary stability but actual victims of a throughput ceiling if it is applied without a compensating transfer of headroom from the North, which is why they carry a secondary excluded role: their voice in whether that transfer actually happens is largely absent from the negotiations that set the ceiling.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (aggregate throughput breaching planetary boundaries, efficiency gains failing to deliver absolute decoupling) is corroborated as live by biophysical science outside the degrowth coalition itself, which forecloses treating this as pure institutional self-perpetuation — the underlying problem is real and worsening. But the specific institutional apparatus proposed to solve it (sufficiency mandates, throughput caps, capital stranding) has not yet been implemented at binding scale anywhere, so there is no mature institution yet capable of the mandatrophy failure mode (mandate outliving function) — this constraint is better read as a proposed enforcement mechanism whose tangled-rope character (genuine coordination function plus asymmetric extraction from specific consumption/capital groups) is present from inception rather than acquired through drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    throughput_reduction_vs_redistribution_sequencing,
    'Does the degrowth reading require the Global North to reduce throughput BEFORE Global South development proceeds (sequential), or can both happen simultaneously via redistribution of existing headroom (parallel)?',
    'Track whether actual international climate finance and technology-transfer mechanisms scale to match rhetorical commitments to ''the North reduces first'' — sustained large-scale transfer would support the parallel reading; continued underfunding would confirm the sequential reading is aspirational rhetoric masking continued Northern consumption.',
    'If sequential and the North does not reduce first, the degrowth reading functions as a constraint on Global South development with no compensating Northern sacrifice — sharpening its tangled_rope/snare character toward the Global South victim class. If genuinely parallel and funded, the coordination function dominates and the extraction is more evenly distributed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(throughput_reduction_vs_redistribution_sequencing, empirical, 'Whether Northern throughput reduction actually precedes or accompanies Southern development constraint, or remains rhetorical.').

omega_variable(
    capital_accumulation_as_extraction_framing,
    'Is treating capital accumulation itself as ''extractive against planetary limits'' a structurally accurate causal claim, or a rhetorical reframing that conflates a proximate driver (throughput-intensive accumulation models) with accumulation as such?',
    'Comparative analysis of decoupled vs. non-decoupled accumulation trajectories across economies — if some economies demonstrate sustained accumulation with declining absolute material throughput, the ''accumulation is inherently extractive'' framing weakens; if none do at required scale, it strengthens.',
    'If accumulation can be decoupled from throughput, this reading''s core axiom (sufficiency over efficiency) overclaims and the mitigation_priority reading''s efficiency-and-substitution path becomes more credible as an alternative route to the same planetary-boundary goal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capital_accumulation_as_extraction_framing, conceptual, 'Whether capital accumulation is intrinsically throughput-extractive or contingently so.').

omega_variable(
    who_speaks_for_global_south_in_this_reading,
    'When Global North-based degrowth advocacy networks frame Global South development constraint as a necessary feature of planetary-boundary compliance, is this a genuine representation of Global South interests or an instance of the excluded-voices problem the constraint itself creates?',
    'Compile positions from Global South governments, movements, and scholars directly (e.g. G77 negotiating positions, Global South ecological economics scholarship) and compare to the framing offered by Northern degrowth coalitions — convergence would support representative legitimacy, divergence would support the excluded-voices concern.',
    'Divergence would mean the degrowth reading''s beneficiary claim for Global South communities is partly self-serving cover for a framing that primarily protects planetary systems and future Northern flexibility while imposing real costs on Southern development — pushing this reading''s computed type toward more extractive relative to its coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(who_speaks_for_global_south_in_this_reading, conceptual, 'Whether Northern degrowth advocacy accurately represents or overrides Global South development interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__degrowth_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__degrowth_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(clim_tr_t8, climate_response_obligation__degrowth_reading, theater_ratio, 8, 0.5).
narrative_ontology:measurement(clim_tr_t16, climate_response_obligation__degrowth_reading, theater_ratio, 16, 0.46).
narrative_ontology:measurement(clim_tr_t24, climate_response_obligation__degrowth_reading, theater_ratio, 24, 0.43).
narrative_ontology:measurement(clim_tr_t32, climate_response_obligation__degrowth_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(clim_tr_t40, climate_response_obligation__degrowth_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__degrowth_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clim_be_t8, climate_response_obligation__degrowth_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(clim_be_t16, climate_response_obligation__degrowth_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(clim_be_t24, climate_response_obligation__degrowth_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(clim_be_t32, climate_response_obligation__degrowth_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(clim_be_t40, climate_response_obligation__degrowth_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__degrowth_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t8, climate_response_obligation__degrowth_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(clim_su_t16, climate_response_obligation__degrowth_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(clim_su_t24, climate_response_obligation__degrowth_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(clim_su_t32, climate_response_obligation__degrowth_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(clim_su_t40, climate_response_obligation__degrowth_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__degrowth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__degrowth_reading, 0.12).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__adaptation_priority).

% DUAL FORMULATION NOTE:
% This story is the degrowth_reading member of the climate_response_obligation kernel family (3 readings: mitigation_priority, degrowth_reading [this file], adaptation_priority). Each reading is a structurally distinct constraint with its own epsilon, beneficiary/victim set, and mechanism, sharing only the persisting commitment that the climate system imposes some present obligation. mitigation_priority treats decarbonization speed as the object and is largely growth-compatible; adaptation_priority treats warming as accepted and shifts to resilience investment, structurally deprioritizing prevention-focused victim classes (small island states, subsistence agriculture) relative to this reading; degrowth_reading (here) treats absolute throughput reduction as the object and names capital accumulation itself as an extractive mechanism. Do not merge these into one constraint or average their epsilon values — they are linked via affects_constraints and cs_structure.reading_relations, never folded into a single classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
