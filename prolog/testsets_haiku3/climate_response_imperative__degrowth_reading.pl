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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: climate_response_imperative__degrowth_reading
 *   human_readable: Climate Response as Structural Economic Transformation (Degrowth Reading)
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This constraint story instantiates the degrowth reading of the climate
 *   response kernel. The reading asserts that climate stabilization requires
 *   structural transformation of Global North economies: mandatory reduction
 *   in material consumption, working-time restructuring, institutional
 *   redesign away from growth dependence, and wealth redistribution to Global
 *   South and future generations. This reading differs fundamentally from the
 *   mitigation-priority reading (which treats technology and markets as
 *   sufficient) and the adaptation-priority reading (which treats
 *   resilience-building as primary). The degrowth reading names present-day
 *   Global North populations as the victim set (bearing the cost of
 *   transformation) and future generations plus Global South populations as
 *   the beneficiary set. The constraint is structured as a tangled_rope: it
 *   performs real coordination (solving climate physics and intergenerational
 *   justice simultaneously) while extracting substantially from its victim
 *   set. Enforcement is required because consumption reduction and wealth
 *   redistribution contradict the growth incentives embedded in incumbent
 *   institutions. The constraint's persistence depends on active suppression
 *   of alternatives (technological-fix narratives, adaptation-only framing,
 *   continued growth) and on enforcement of consumption limits and
 *   redistribution mechanisms.
 *
 * KEY AGENTS:
 *   - global_north_present_populations — payer seat; faces reduced consumption and working-time restructuring
 *   - future_generations — powerless beneficiary; receives stabilized climate and post-growth institutional inheritance
 *   - global_south_populations — moderate-power beneficiary; gains adaptation capacity and decoupled development pathways
 *   - post_growth_institutional_designers — agenda_setter; administers consumption policy, working-time regulation, redistribution
 *   - incumbent_fossil_fuel_regimes — excluded; structurally incompatible with degrowth; their exclusion is constitutive
 *   - technological_carbon_removal_advocates — excluded; propose CDR as alternative to transformation; this reading rejects CDR reliance
 *   - climate_justice_advocates — observer seat; validates or contests the distributional justice of the transformation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, 0.68).
domain_priors:suppression_score(climate_response_imperative__degrowth_reading, 0.72).
domain_priors:theater_ratio(climate_response_imperative__degrowth_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__degrowth_reading, "Climate Response as Structural Economic Transformation (Degrowth Reading)").
narrative_ontology:topic_domain(climate_response_imperative__degrowth_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__degrowth_reading, 'f4eff3e2-0118-43c4-a3ae-8fd4a2ed4172').
narrative_ontology:cs_kernel_codification('f4eff3e2-0118-43c4-a3ae-8fd4a2ed4172', distributed).
narrative_ontology:cs_authority_grounding('f4eff3e2-0118-43c4-a3ae-8fd4a2ed4172', distributed).
narrative_ontology:cs_reading_relation('f4eff3e2-0118-43c4-a3ae-8fd4a2ed4172', climate_response_imperative__mitigation_priority_reading, forecloses).
narrative_ontology:cs_reading_relation('f4eff3e2-0118-43c4-a3ae-8fd4a2ed4172', climate_response_imperative__adaptation_priority_reading, coexists_with).
narrative_ontology:cs_axiom('f4eff3e2-0118-43c4-a3ae-8fd4a2ed4172', foundational, consumption_reduction_transformation_necessary).
narrative_ontology:cs_axiom_status(consumption_reduction_transformation_necessary, holdable).
narrative_ontology:cs_axiom_grounding('f4eff3e2-0118-43c4-a3ae-8fd4a2ed4172', consumption_reduction_transformation_necessary, empirically_contingent).
narrative_ontology:cs_axiom('f4eff3e2-0118-43c4-a3ae-8fd4a2ed4172', foundational, intergenerational_justice_redistribution_required).
narrative_ontology:cs_axiom_status(intergenerational_justice_redistribution_required, holdable).
narrative_ontology:cs_axiom_grounding('f4eff3e2-0118-43c4-a3ae-8fd4a2ed4172', intergenerational_justice_redistribution_required, deontological).
narrative_ontology:cs_reference_frame('f4eff3e2-0118-43c4-a3ae-8fd4a2ed4172', anthropogenic_climate_responsibility_with_intergenerational_justice).
narrative_ontology:cs_drift_state('f4eff3e2-0118-43c4-a3ae-8fd4a2ed4172', contemporary_climate_science_consensus_moment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f4eff3e2-0118-43c4-a3ae-8fd4a2ed4172', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(climate_response_imperative__degrowth_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, global_south_populations).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, global_north_present_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face mandated reduction in material consumption, working-time restructuring, and wealth redistribution as the price of enabling mitigation and adaptation. Their current consumption levels and income structures are the metric against which 'transformation' is measured. Exit options are constrained by the sovereignty of the nation-state adopting degrowth policy and by the interdependence of global supply chains.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_north_present_populations, payer,
    organized, biographical, constrained, global).

% Inherit a stabilized climate trajectory and post-growth institutions that decouple human flourishing from consumption growth. They cannot exit the constraint; their entire institutional and ecological inheritance is constituted by whether the transformation succeeds. They are powerless in present-day climate policy negotiation but receive the primary benefit of constraint.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Gain a reduced-extraction climate system where Global North overconsumption no longer drives catastrophic warming they did not cause, and where redistribution of North wealth enables genuine adaptation capacity. Their development pathways are decoupled from replicating North growth patterns. They remain constrained by North energy and capital flows even post-transformation.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_south_populations, beneficiary,
    moderate, generational, constrained, global).

% Are structurally incompatible with degrowth transformation: extraction-dependent revenue models, political-economy capture of energy policy, and entrenchment in North consumption growth. They are excluded from the solution set by definition. Their exclusion is the point of structural transformation, not a contingent side effect.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, incumbent_fossil_fuel_regimes, excluded,
    institutional, biographical, trapped, global).

% Operate the institutional framework that enforces reduced consumption, redistributes resources, and structures the transition to post-growth economics. They administer working-time policy, consumption quotas, wealth distribution mechanisms, and alternative measures of social welfare. Their power derives from the authority to redefine legitimate economic life.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, post_growth_institutional_designers, agenda_setter,
    institutional, generational, arbitrage, global).

% Propose carbon dioxide removal (CDR) and negative-emissions technology as the primary or significant climate response pathway, arguing it preserves existing consumption patterns and institutions. This reading explicitly rejects CDR reliance as neither technically reliable nor socially just. They are excluded because their core mechanism—avoiding transformation while removing carbon—contradicts the degrowth premise.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, technological_carbon_removal_advocates, excluded,
    institutional, biographical, constrained, global).

% Monitor whether the redistribution and transformation actually reduce extraction and create just outcomes, or whether degrowth language masks new forms of dispossession or paternalism. They do not set policy but validate or contest its legitimacy.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, climate_justice_advocates, observer,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__degrowth_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_response_imperative__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of climate stabilization by aligning present-day consumption reduction with emissions cuts, while creating institutions that enable genuine adaptation for populations that did not cause the crisis. Coordinates the decoupling of human welfare from consumption growth, moving from extraction-dependent economics to post-growth redistribution.
% TRANSFER_FUNCTION: Moves material resources, work time, and wealth from Global North present-day populations to future generations and Global South populations via mandatory consumption reduction, working-time restructuring, and institutional redistribution. The constraint extracts from present North by lowering living standards, income, and consumption entitlements relative to the pre-transformation baseline.
% ABSENT_VOICES: Fossil fuel regimes and technological-fix advocates are structurally excluded; they would argue for preservation of growth-dependent institutions and carbon-removal alternatives, but their exclusion is constitutive of the reading, not accidental. Unrepresented Global North working-class and precariat populations whose material interests diverge from elite North populations would object to the burden allocation if they had organized voice—distributional questions within the North payer set are actively contested.
% DISAPPEARANCE_RATIONALE: If this degrowth transformation disappeared, Global North consumption levels would revert to extraction-driven trajectories, emissions would rise from post-transformation baselines, and Global South adaptation capacity would contract. Future-generation welfare would degrade substantially. The entire post-growth institutional architecture would collapse and be replaced by growth-dependent arrangements.
% FOUNDING_PROBLEM: Climate physics makes continued high-consumption, extraction-based economic growth in the Global North physically impossible without catastrophic warming. Technological mitigation alone cannot close the gap between required emissions cuts and feasible deployment at scale. Adaptation alone abandons populations and ecosystems to damages that could have been prevented. The founding problem is the structural incompatibility between climate stability and capitalist growth.
% FOUNDING_PROBLEM_CORROBORATION: Climate physics consensus (IPCC, peer-reviewed carbon budgets) attests the founding problem from outside any degrowth institutional beneficiary. Emissions trajectory models from non-advocacy research communities confirm the feasibility gap for technological-only pathways. Adaptation vulnerability assessments from development and humanitarian organizations outside the North confirm that adaptation funding and technology transfer remain insufficient under growth-dependent allocation. The founding problem is corroborated by scientific and humanitarian testimony independent of degrowth advocacy.
narrative_ontology:disappearance_verdict(climate_response_imperative__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_imperative__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__degrowth_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness measures 0.68 at interval end because the constraint transfers substantial material resources from present Global North (via reduced consumption) to future and Global South beneficiaries. The ε is stable across the interval: the core transformation is not deepening over time, but the initial measurement (0.38) reflects pre-transformation baseline where the constraint is nascent. Suppression measures 0.72 because the constraint requires active suppression of alternatives—primarily the narrative that technology alone solves climate, and secondarily the fossil-fuel-regime resistance that captures energy policy. Theater_ratio at 0.41 reflects that institutional performance (working-time accounting, consumption-quota administration, redistribution ceremonial) occupies a measurable share of enforcement activity, but most energy goes to actual coordination and redistribution work, not performance. Accessibility_collapse at 0.62: alternatives (continued growth, CDR-only, adaptation-only) are not completely eliminated—they remain live political positions—but become substantially harder to pursue once the degrowth institutional framework is established. Resistance at 0.58: the constraint meets real opposition from incumbent regimes and from present-generation populations bearing the cost, though over the interval organized resistance weakens as the institutions settle. The coercion_grid shows leveled dynamics: suppression is highest at the organizational and class levels (fossil-fuel firms, investment capital, high-consumption sectors) and lower at the individual and structural levels; stakes_inflation accelerates at class and organizational levels (the cost of noncompliance rises faster for organized economic actors); accessibility_collapse rises fastest at the organizational level (alternative energy and production paths close off for businesses first); resistance decays over time as institutional legitimacy settles, fastest at the organizational level (incumbent firms shift to compliance after initial resistance period).
 *
 * PERSPECTIVAL GAP:
 *   The structural asymmetry is acute and intentional. From the agenda-setter (post-growth institutional designer) seat, this constraint solves a genuine coordination problem: aligning climate physics with human welfare by decoupling welfare from extraction. From the payer seat (global_north_present_populations), the same structure appears as extraction—their consumption is reduced, their working-time is restructured, and their wealth is transferred. The engine should compute different type classifications at these seats: the agenda-setter seat may perceive coordination or rope-grade benefit; the payer seat perceives substantial extraction and suppression, supporting a snare or tangled_rope classification from their perspective. This reading explicitly instantiates asymmetric extraction (the hallmark of tangled_rope) because it requires genuine coordination (climate stability) while imposing costs on one set of parties (present North) to benefit another (future and Global South). The constraint persists because the beneficiary set (future generations, Global South) has no present voice to capture rents, so no seat is enriched enough to maintain it independently—only the agenda-setter's commitment to intergenerational and global justice sustains it. The payer set (present North) bears enough cost that they would exit if they could, but exit is constrained by sovereignty and collective action.
 *
 * DIRECTIONALITY LOGIC:
 *   Global_north_present_populations carry full-target directionality (d near 1.0): they are mandatorily reduced in consumption and working-time, their wealth is transferred, and their exit options are constrained by national-level policy and global coordination. Future_generations carry full-beneficiary directionality (d near 0.0): they receive the primary benefit (stabilized climate, post-growth institutions) and bear no cost in their present selves. Global_south_populations carry beneficiary-toward-neutral directionality (d around 0.3-0.4): they gain adaptation capacity and decoupled development but remain constrained by North energy/capital flows and by the global coordination required to enforce the constraint. Post_growth_institutional_designers carry near-beneficiary directionality (d around 0.15): they set the constraint and derive legitimacy from it, though they bear the burden of enforcement and public resistance. Fossil_fuel_regimes are excluded (d undefined for non-parties). The directionality derivation is straightforward: beneficiaries have low d (they gain), victims have high d (they lose), and the agenda-setter has low d (they benefit from legitimacy and perceived coordination success). No overrides are needed—the structural data (beneficiary/victim + power + exit) produces accurate d values for each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   This degrowth reading avoids the mandatrophy trap by maintaining genuine coordination alongside extraction: the founding problem (climate physics + intergenerational justice) remains live, and the constraint solves it. The function does not atrophy because Global South adaptation and future-generation welfare remain dependent on present-day Global North consumption reduction. However, the reading faces contestation on whether the founding problem truly requires this specific solution (structural economic transformation) or whether alternatives (technological pathways, efficiency improvements, adaptation investment) could address it with lower extraction costs. The mandatrophy analysis must address: (1) whether the founding problem (climate stability) remains live or has been superseded by other political priorities, and (2) whether the constraint's function remains necessary or whether alternative mechanisms could achieve the same coordination at lower cost. This reading asserts both are true (founding problem is live, the transformation is necessary), but the assertion is contestable—hence the omegas. The theater_ratio at 0.41 is moderate, not high: most institutional activity serves actual coordination or redistribution work, not performative cover. If theater_ratio were to rise above 0.6, that would signal mandatrophy (the coordination function is being replaced by theatrical maintenance). The coercion_grid shows some evolution: organizational resistance decays (fossil-fuel firms are suppressed; alternatives close off; stakes for noncompliance rise), while individual and structural resistance remain stable or slight-rise. This pattern is consistent with institutional maturation of a genuine coordination constraint, not mandatrophy drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformation_feasibility_vs_political_will,
    'Is the structural economic transformation of Global North consumption and production technically feasible and socially achievable, or do incumbent institutions and present-generation opposition render it politically impossible regardless of feasibility?',
    'Pilot programs and regional experiments in working-time reduction, consumption quotas, and wealth redistribution; longitudinal political-economy analysis of institutional change capacity; comparative-historical cases of rapid social transformation (war mobilization, post-colonial restructuring, post-soviet transition).',
    'If feasible and politically possible, the degrowth reading''s extraction and enforcement mechanisms are justifiable as the price of necessary coordination. If politically impossible, the constraint becomes a performative fantasy—theater_ratio rises sharply, and the constraint reclassifies as piton or snare (extraction without achievable function). This fundamentally changes whether the reading is a legitimate climate strategy or a cover for dispossession.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transformation_feasibility_vs_political_will, empirical, 'Whether structural transformation is politically achievable at the scale required.').

omega_variable(
    distributional_justice_within_payer_set,
    'Does consumption reduction fall uniformly across Global North populations, or are working-class and precariat populations bearing disproportionate costs while elite and high-consumption populations retain privilege?',
    'Institutional design analysis of quota allocation (per-capita vs. progressive vs. sector-based); income-redistributive policy detail; post-implementation equity audits; comparison to regressive vs. progressive tax structures.',
    'If reduction is uniform or progressive (elite bear more), the constraint maintains legitimacy as coordinated burden-sharing and intergenerational justice. If regressive (working-class bear more), the constraint becomes extraction of precariat populations by institutional designers claiming climate necessity—reclassifying from tangled_rope toward snare, with a new victim identification (working-class North vs. elite North). This splits the payer seat and potentially reverses directionality for some agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_justice_within_payer_set, empirical, 'Whether distributional consequences within the payer set are equitable or reinforce existing inequalities.').

omega_variable(
    reading_kernel_ambiguity,
    'Does the climate response kernel commit genuinely to intergenerational justice and Global South equity, or is intergenerational justice a readings-layer claim that alternative readings (mitigation-priority, adaptation-priority) can also adopt?',
    'Textual and historical analysis of climate science consensus, IPCC framings, and UNFCCC commitment language; investigation of whether the kernel itself privileges any reading or whether all three readings claim the kernel equally.',
    'If the kernel genuinely commits to intergenerational and Global South justice, then the degrowth reading is justified in naming these as the beneficiary set and transformation as necessary. If intergenerational justice is contestable across readings (mitigation-priority also claims it will deliver intergenerational benefit via technology; adaptation-priority claims it will deliver Global South equity via resilience investment), then the degrowth reading''s framing of its beneficiary set is reading-specific, not kernel-mandated. This affects whether the beneficiary set (future generations, Global South) is structurally necessary or strategically claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether intergenerational and Global South justice are kernel commitments or reading-specific framings.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) structural (external barriers: policy enforcement, institutional barriers, capital flight pressure) or internalized (North populations believe they deserve consumption reduction, have been culturally reshaped to accept austerity, or have fused their identity with environmental stewardship)?',
    'Post-exit or post-policy-removal dynamics: if suppression persists after the institutional framework is removed or evaded, it is internalized; if it collapses, it was structural. Comparative cases of policy failure and institutional rollback. Longitudinal attitude surveys tracking whether acceptance grows with institutional settling or remains externally coerced.',
    'If structural, the constraint''s effective suppression is captured by the 0.72 scalar and remains dependent on institutional enforcement—a brittle equilibrium if enforcement capacity weakens. If partially internalized, the constraint carries suppression with it even after formal rules are lifted—more durable but also a deeper form of extraction because it colonizes internal motivation. If deeply internalized, the constraint can withdraw enforcement and still persist, lowering the institutional cost but raising questions about whether consent is genuine or manufactured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized.').

omega_variable(
    mitigation_adaptation_tradeoff_empirics,
    'Does the degrowth reading''s assertion that transformation enables both mitigation AND adaptation rest on sound climate economics, or are there genuine tradeoffs where spending on consumption-reduction enforcement competes with direct adaptation investment?',
    'Climate impact modeling comparing transformation scenarios to technology-priority and adaptation-priority scenarios; cost-effectiveness analysis of emissions reduction via consumption reduction vs. emissions reduction via technological deployment; adaptation funding requirements under each reading.',
    'If the reading is correct that transformation enables both mitigation and adaptation simultaneously without tradeoff, the constraint solves a more coherent coordination problem than technology-only or adaptation-only readings. If there are genuine tradeoffs (resources spent on consumption enforcement could have funded direct adaptation in Global South), then the reading''s coordination function is less complete, and it may not deliver for the beneficiary set as promised—potentially reclassifying as a snare where the stated coordination function is cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_adaptation_tradeoff_empirics, empirical, 'Whether the dual-coordination claim (mitigation + adaptation) is empirically supported.').

omega_variable(
    reading_foreclosure_structure,
    'Does the degrowth reading genuinely foreclose the mitigation-priority reading, or do they coexist as competing interpretations of the same climate commitment?',
    'Logical analysis of whether carbon-removal-sufficiency (mitigation-priority''s core premise) is incompatible with consumption-transformation-necessity (degrowth''s core premise). Can both be true in a single policy framework, or does acceptance of one require rejection of the other?',
    'If foreclosure: the two readings represent genuinely incompatible institutional strategies, and policy adoption of degrowth forecloses technological pathways and vice versa. The readings are structural alternatives, not negotiable tradeoffs. If coexistence: the readings can cohabitate (some emissions reduced via technology, some via consumption reduction, some via adaptation), and the constraint is one component of a multi-pathway response rather than a singular alternative. This affects whether the degrowth reading is justified in excluding technological solutions or whether it should embrace them as complementary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Whether degrowth forecloses technological mitigation or coexists with it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__degrowth_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__degrowth_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t8, climate_response_imperative__degrowth_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement_basis(clim_tr_t8, observed).
narrative_ontology:measurement(clim_tr_t16, climate_response_imperative__degrowth_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement_basis(clim_tr_t16, observed).
narrative_ontology:measurement(clim_tr_t24, climate_response_imperative__degrowth_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(clim_tr_t24, projected).
narrative_ontology:measurement(clim_tr_t32, climate_response_imperative__degrowth_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(clim_tr_t32, projected).
narrative_ontology:measurement(clim_tr_t40, climate_response_imperative__degrowth_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(clim_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__degrowth_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t8, climate_response_imperative__degrowth_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(clim_be_t8, observed).
narrative_ontology:measurement(clim_be_t16, climate_response_imperative__degrowth_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement_basis(clim_be_t16, observed).
narrative_ontology:measurement(clim_be_t24, climate_response_imperative__degrowth_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(clim_be_t24, projected).
narrative_ontology:measurement(clim_be_t32, climate_response_imperative__degrowth_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement_basis(clim_be_t32, projected).
narrative_ontology:measurement(clim_be_t40, climate_response_imperative__degrowth_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(clim_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__degrowth_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t8, climate_response_imperative__degrowth_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(clim_su_t8, observed).
narrative_ontology:measurement(clim_su_t16, climate_response_imperative__degrowth_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(clim_su_t16, observed).
narrative_ontology:measurement(clim_su_t24, climate_response_imperative__degrowth_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement_basis(clim_su_t24, projected).
narrative_ontology:measurement(clim_su_t32, climate_response_imperative__degrowth_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement_basis(clim_su_t32, projected).
narrative_ontology:measurement(clim_su_t40, climate_response_imperative__degrowth_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(clim_su_t40, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(clim_grid_01, climate_response_imperative__degrowth_reading, accessibility_collapse(class), 0, 0.58).
narrative_ontology:measurement(clim_grid_02, climate_response_imperative__degrowth_reading, accessibility_collapse(class), 40, 0.68).
narrative_ontology:measurement(clim_grid_03, climate_response_imperative__degrowth_reading, accessibility_collapse(individual), 0, 0.48).
narrative_ontology:measurement(clim_grid_04, climate_response_imperative__degrowth_reading, accessibility_collapse(individual), 40, 0.58).
narrative_ontology:measurement(clim_grid_05, climate_response_imperative__degrowth_reading, accessibility_collapse(organizational), 0, 0.65).
narrative_ontology:measurement(clim_grid_06, climate_response_imperative__degrowth_reading, accessibility_collapse(organizational), 40, 0.72).
narrative_ontology:measurement(clim_grid_07, climate_response_imperative__degrowth_reading, accessibility_collapse(structural), 0, 0.55).
narrative_ontology:measurement(clim_grid_08, climate_response_imperative__degrowth_reading, accessibility_collapse(structural), 40, 0.65).
narrative_ontology:measurement(clim_grid_09, climate_response_imperative__degrowth_reading, resistance(class), 0, 0.62).
narrative_ontology:measurement(clim_grid_10, climate_response_imperative__degrowth_reading, resistance(class), 40, 0.58).
narrative_ontology:measurement(clim_grid_11, climate_response_imperative__degrowth_reading, resistance(individual), 0, 0.52).
narrative_ontology:measurement(clim_grid_12, climate_response_imperative__degrowth_reading, resistance(individual), 40, 0.48).
narrative_ontology:measurement(clim_grid_13, climate_response_imperative__degrowth_reading, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(clim_grid_14, climate_response_imperative__degrowth_reading, resistance(organizational), 40, 0.55).
narrative_ontology:measurement(clim_grid_15, climate_response_imperative__degrowth_reading, resistance(structural), 0, 0.58).
narrative_ontology:measurement(clim_grid_16, climate_response_imperative__degrowth_reading, resistance(structural), 40, 0.62).
narrative_ontology:measurement(clim_grid_17, climate_response_imperative__degrowth_reading, stakes_inflation(class), 0, 0.48).
narrative_ontology:measurement(clim_grid_18, climate_response_imperative__degrowth_reading, stakes_inflation(class), 40, 0.72).
narrative_ontology:measurement(clim_grid_19, climate_response_imperative__degrowth_reading, stakes_inflation(individual), 0, 0.42).
narrative_ontology:measurement(clim_grid_20, climate_response_imperative__degrowth_reading, stakes_inflation(individual), 40, 0.68).
narrative_ontology:measurement(clim_grid_21, climate_response_imperative__degrowth_reading, stakes_inflation(organizational), 0, 0.55).
narrative_ontology:measurement(clim_grid_22, climate_response_imperative__degrowth_reading, stakes_inflation(organizational), 40, 0.78).
narrative_ontology:measurement(clim_grid_23, climate_response_imperative__degrowth_reading, stakes_inflation(structural), 0, 0.52).
narrative_ontology:measurement(clim_grid_24, climate_response_imperative__degrowth_reading, stakes_inflation(structural), 40, 0.75).
narrative_ontology:measurement(clim_grid_25, climate_response_imperative__degrowth_reading, suppression(class), 0, 0.62).
narrative_ontology:measurement(clim_grid_26, climate_response_imperative__degrowth_reading, suppression(class), 40, 0.78).
narrative_ontology:measurement(clim_grid_27, climate_response_imperative__degrowth_reading, suppression(individual), 0, 0.48).
narrative_ontology:measurement(clim_grid_28, climate_response_imperative__degrowth_reading, suppression(individual), 40, 0.62).
narrative_ontology:measurement(clim_grid_29, climate_response_imperative__degrowth_reading, suppression(organizational), 0, 0.58).
narrative_ontology:measurement(clim_grid_30, climate_response_imperative__degrowth_reading, suppression(organizational), 40, 0.75).
narrative_ontology:measurement(clim_grid_31, climate_response_imperative__degrowth_reading, suppression(structural), 0, 0.55).
narrative_ontology:measurement(clim_grid_32, climate_response_imperative__degrowth_reading, suppression(structural), 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__degrowth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_imperative__degrowth_reading, 0.18).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, climate_response_imperative__mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, climate_response_imperative__adaptation_priority_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the climate_response_imperative kernel. The degrowth_reading instantiates the structural economic transformation interpretation; sibling readings (mitigation_priority and adaptation_priority) decompose the same climate commitment via different institutional and temporal priorities. Each reading has distinct beneficiary/victim structures, ε values, and enforcement mechanisms. The three readings form a constraint family; each story links to its siblings via network.affects_constraints. Structural relationship: degrowth forecloses or constrains technological-sufficiency claims (mitigation_priority axiom); coexists with adaptation-priority as complementary framings. The kernel itself (climate_response_imperative) does not appear as a separate story—it is the contested commitment that all three readings interpret.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
