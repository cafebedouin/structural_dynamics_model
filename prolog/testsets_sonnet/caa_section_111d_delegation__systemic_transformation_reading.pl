% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__systemic_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_caa_section_111d_delegation__systemic_transformation_reading, []).

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
 *   constraint_id: caa_section_111d_delegation__systemic_transformation_reading
 *   human_readable: CAA §111(d) 'Best System' as Grid-Wide Generation-Shifting Authority
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   This story instantiates the systemic-transformation reading of the
 *   §111(d) 'best system' kernel: EPA's 2015 Clean Power Plan interpretation,
 *   under which 'best system of emission reduction' authorizes grid-wide
 *   generation-shifting — substituting renewable and gas generation for coal
 *   at the system level rather than confining compliance to what an
 *   individual coal facility can achieve through its own equipment. This
 *   reading treats the electricity grid, not the smokestack, as the regulated
 *   unit. Its structural delta from the facility_constraint_reading (a
 *   separate constraint, not modeled here): under this reading coal utilities
 *   and coal-region workforces become extraction-bearing parties whose asset
 *   base and employment are restructured on a federally set timeline,
 *   renewable developers gain a regulation-created demand channel functioning
 *   as an implicit subsidy, and coal-producing states face compliance costs
 *   they did not choose and structurally cannot exit. The 2022 West Virginia
 *   v. EPA ruling repudiated this reading under the major questions doctrine,
 *   but the underlying statutory ambiguity persists and the reading remains
 *   live in policy and litigation discourse (e.g. debates over the successor
 *   Clean Power Plan 2.0 rule's scope).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, 0.62).
domain_priors:suppression_score(caa_section_111d_delegation__systemic_transformation_reading, 0.58).
domain_priors:theater_ratio(caa_section_111d_delegation__systemic_transformation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__systemic_transformation_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__systemic_transformation_reading, "CAA §111(d) 'Best System' as Grid-Wide Generation-Shifting Authority").
narrative_ontology:topic_domain(caa_section_111d_delegation__systemic_transformation_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__systemic_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__systemic_transformation_reading, 'f898d0c3-abe2-4866-89a2-eb0fd5c2f58e').
narrative_ontology:cs_kernel_codification('f898d0c3-abe2-4866-89a2-eb0fd5c2f58e', fixed_text).
narrative_ontology:cs_authority_grounding('f898d0c3-abe2-4866-89a2-eb0fd5c2f58e', extraction).
narrative_ontology:cs_interpretation_layer_present('f898d0c3-abe2-4866-89a2-eb0fd5c2f58e').
narrative_ontology:cs_reading_relation('f898d0c3-abe2-4866-89a2-eb0fd5c2f58e', caa_section_111d_delegation__facility_constraint_reading, forecloses).
narrative_ontology:cs_axiom('f898d0c3-abe2-4866-89a2-eb0fd5c2f58e', foundational, best_system_means_grid_level_generation_mix).
narrative_ontology:cs_axiom_status(best_system_means_grid_level_generation_mix, holdable).
narrative_ontology:cs_axiom_grounding('f898d0c3-abe2-4866-89a2-eb0fd5c2f58e', best_system_means_grid_level_generation_mix, conventional).
narrative_ontology:cs_axiom('f898d0c3-abe2-4866-89a2-eb0fd5c2f58e', secondary, agency_may_select_transformative_instruments_absent_clear_prohibition).
narrative_ontology:cs_axiom_status(agency_may_select_transformative_instruments_absent_clear_prohibition, overridden).
narrative_ontology:cs_axiom_grounding('f898d0c3-abe2-4866-89a2-eb0fd5c2f58e', agency_may_select_transformative_instruments_absent_clear_prohibition, instrumental).
narrative_ontology:cs_reference_frame('f898d0c3-abe2-4866-89a2-eb0fd5c2f58e', facility_level_technology_standard_1970_baseline).
narrative_ontology:cs_drift_state('f898d0c3-abe2-4866-89a2-eb0fd5c2f58e', post_clean_power_plan_litigation, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('f898d0c3-abe2-4866-89a2-eb0fd5c2f58e', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, renewable_generation_developers).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, grid_scale_storage_industry).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, downwind_states_and_populations).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, climate_advocacy_organizations).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_dependent_utilities).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_mining_workforce).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_state_ratepayers).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_producing_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs the 'best system of emission reduction' guidelines under §111(d), setting state-level emission-rate or mass-based targets that presume generation can be shifted from coal to gas and renewables across the grid rather than only reduced facility-by-facility. Administers compliance timelines, approves or rejects state plans, and enforces against noncompliant states. Its authority to define 'system' this broadly is the contested premise.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, epa_air_office, agenda_setter,
    institutional, generational, analytical, national).

% Own capital-intensive coal fleets built on decades-long depreciation schedules. Under a grid-wide reading, compliance effectively requires early retirement or steep derating of coal assets regardless of that unit's own achievable heat-rate improvement, because the 'system' contemplated is generation-shifting, not equipment retrofit. They can litigate, seek state variances, or accelerate retirement and reinvest in gas/renewables, but stranded-asset costs are largely unrecoverable and exit from the regulatory regime itself is not available while remaining a regulated utility.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_dependent_utilities, payer,
    powerful, biographical, constrained, national).

% Employment is tied to coal plant operating schedules set by utility compliance decisions made far from the mine. When generation-shifting compliance accelerates plant retirement, jobs disappear on a timeline the workforce has no voice in setting. Relocation, retraining, or absorbing wage loss are the only available responses; there is no coalition-scale lever inside the §111(d) proceeding itself, though political mobilization at the state and congressional level is a slower, indirect channel.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_mining_workforce, payer,
    powerless, biographical, trapped, regional).

% Live in states whose grid mix is heavily coal-dependent and whose replacement generation (gas, renewables, transmission buildout) is not yet built. Bear near-term rate increases from stranded-asset cost recovery and new capital investment mandated by compliance timelines calibrated to a national 'best system,' not to their state's actual buildout capacity. Exit means moving states or absorbing the cost; there is no mechanism to opt out of the utility's compliance-driven rate base.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_state_ratepayers, payer,
    moderate, biographical, constrained, regional).

% Depend on coal severance taxes, mining employment, and captive utility rate bases for state revenue and political coalitions. A grid-wide reading of 'best system' effectively nationalizes the pace of their energy transition, overriding state discretion about which facilities to retire and when. They can sue (as several did, culminating in West Virginia v. EPA), petition Congress, or seek compliance flexibility, but cannot unilaterally opt out of a federally administered emission-guideline regime.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_producing_state_governments, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__systemic_transformation_reading, coal_producing_state_governments, excluded).

% Benefit directly when state compliance plans favor renewable substitution as the lowest-cost pathway to meet grid-wide targets, since the regulation itself creates guaranteed demand for new generation capacity that would not otherwise displace embedded coal assets on this timeline. Can site projects wherever compliance-driven demand and interconnection capacity align; largely indifferent to which state adopts the 'best system' reading as long as some do.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, renewable_generation_developers, beneficiary,
    organized, biographical, arbitrage, national).

% Receive reduced cross-state air pollution and carbon loading when upwind coal generation is curtailed faster than facility-level retrofits alone would achieve. Benefit passively from EPA's broader authority without bearing the compliance cost directly, though they have no formal seat in setting the pace of upwind states' compliance plans.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, downwind_states_and_populations, beneficiary,
    moderate, biographical, constrained, regional).

% Adjudicates whether the systemic-transformation reading exceeds EPA's delegated authority, applying doctrines like major questions to decide whether Congress spoke clearly enough to authorize generation-shifting mandates. Its ruling in West Virginia v. EPA (2022) rejected this reading as the operative one, making this constraint's authority currently repudiated at the highest adjudicative level even though the statutory text and agency practice history remain contested.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, supreme_court, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__systemic_transformation_reading, supreme_court, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: If read broadly, §111(d) would coordinate a national decarbonization pathway across a fragmented, state-by-state utility regulatory system, allowing emission targets to reflect grid-wide least-cost abatement (substituting cheaper renewable generation for expensive facility retrofits) rather than forcing each coal unit to solve its own emissions independently.
% TRANSFER_FUNCTION: Moves compliance burden and stranded-asset risk from the atmosphere (diffuse, unpriced harm) onto coal-dependent utilities, coal workers, and fossil-locked ratepayers, while moving new capital demand and market share toward renewable developers and downwind populations who receive the pollution reduction without bearing the transition cost.
% ABSENT_VOICES: Coal mining communities are represented only through state government intermediaries and union political action, not directly in EPA rulemaking or in the Supreme Court's separation-of-powers analysis, which is framed almost entirely around agency-versus-Congress authority rather than worker impact. Future ratepayers who would benefit from climate stabilization decades out have no seat at all in a proceeding whose stakes are litigated at biographical time horizons.
% DISAPPEARANCE_RATIONALE: If this reading of §111(d) authority disappeared entirely (as West Virginia v. EPA moved toward), EPA would be confined to facility-level 'best system' measures, coal retirement timelines would revert to state and market-driven pacing rather than federal target-setting, renewable developers would lose a compliance-driven demand channel, and the entire post-2015 Clean Power Plan architecture would need to be rebuilt on a narrower statutory theory or replaced by new legislation.
% FOUNDING_PROBLEM: Congress in the 1970 Clean Air Act needed a mechanism to regulate emissions from existing stationary sources not otherwise covered by the Act's other programs, and left 'best system of emission reduction' undefined, anticipating case-by-case agency judgment about what abatement technology or practice was adequately demonstrated for a given source category.
% FOUNDING_PROBLEM_CORROBORATION: EPA and environmental petitioners attest the founding problem (unregulated existing-source emissions, now including grid-scale carbon) remains live and that generation-shifting is the only 'system' capable of achieving meaningful reductions at the pace required. The Supreme Court majority in West Virginia v. EPA, an institution with no stake in either coal or renewable outcomes, attests that Congress's 1970/1990 text does not corroborate this expansive reading and that the 'major questions doctrine' requires clearer congressional authorization before EPA may restructure the national electricity generation mix — corroboration for the founding-problem's *scope* thus comes from outside both benefiting coalitions and points toward contestation, not resolution.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__systemic_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__systemic_transformation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__systemic_transformation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(caa_section_111d_delegation__systemic_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__systemic_transformation_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(caa_section_111d_delegation__systemic_transformation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(caa_section_111d_delegation__systemic_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises through the measurement window (0.40 to 0.62) tracking the Clean Power Plan's 2015 promulgation, its 2016 stay, vacatur litigation, and the 2022 Supreme Court repudiation — the reading's practical bite intensified as EPA leaned harder on generation-shifting as the only mechanism capable of hitting stated targets, then plateaued once judicially constrained. Theater ratio peaks around 2017-2019 (0.30-0.35) reflecting the period when the rule was stayed but litigation and compliance planning continued performatively without operative legal effect — genuine enforcement paused while institutional posturing continued on both sides. Suppression is substantial (0.58) because compliance, where operative, left coal utilities no facility-level alternative path to compliance; the 'system' itself, not the individual unit, was the regulated object, closing off the retrofit-and-continue option that the facility_constraint_reading would have preserved.
 *
 * PERSPECTIVAL GAP:
 *   From EPA's seat this operates as coordination: a nationally consistent decarbonization pathway solving a genuine collective-action problem (no single state or utility can unilaterally address atmospheric carbon, and facility-by-facility retrofit mandates would be dramatically more expensive per ton abated). From the coal utility and coal workforce seats, the identical structure operates as targeted extraction: their asset base and livelihoods are restructured on a timeline set by an agency they cannot exit, to solve a problem whose costs they bear disproportionately relative to the diffuse national and global benefit. The engine's per-seat computation should register this asymmetry directly from the constrained/trapped exit options and payer roles authored here.
 *
 * DIRECTIONALITY LOGIC:
 *   EPA is agenda_setter with institutional power but only analytical exit from the underlying statutory dispute — its authority is precisely what is contested. Coal utilities and coal workforce are structural targets: utilities have constrained exit (can restructure investment but not escape the regulatory jurisdiction), while coal workforce is trapped (employment is entirely downstream of utility compliance decisions in which workers have no seat). Renewable developers are beneficiaries with arbitrage-grade exit — they can site capacity wherever compliance-driven demand appears, largely indifferent to which specific state's plan drives that demand. The Supreme Court sits as an observer/agenda_setter hybrid: analytically positioned but functionally exercising agenda-setting power by determining which reading is legally operative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (regulating existing-source emissions where no other CAA program reaches) remains structurally live — atmospheric carbon loading from existing power plants is real and growing more urgent, not less. What is contested is whether the *systemic-transformation instrument* is the legitimate tool for that live problem, or whether it is a repurposing of a facility-level statutory hook to achieve a grid-level policy goal Congress never clearly authorized. This is precisely the mandatrophy question: a coordination problem (decarbonizing existing generation) that is undeniably live does not by itself validate any particular administrative instrument chosen to solve it. The Supreme Court's repudiation functions as an external, non-beneficiary corroboration that the instrument's scope, not the underlying problem, is what should be treated as contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    major_questions_doctrine_scope,
    'Does the major questions doctrine, as applied in West Virginia v. EPA, permanently foreclose the systemic-transformation reading, or could a future Court or a clearer congressional statute revive it under different political and evidentiary conditions?',
    'Track subsequent EPA rulemakings (e.g., the 2024 power plant rule built on facility-level CCS rather than generation-shifting) and any Supreme Court treatment of major-questions doctrine in adjacent domains; a durable pattern of facility-confined rules would corroborate foreclosure, while congressional legislation explicitly authorizing generation-shifting would revive this reading on new textual footing.',
    'If durably foreclosed, this reading becomes primarily of historical/comparative interest and its extractiveness trajectory should be read as terminated in 2022; if revivable, the constraint remains live and its measurement series should be extended forward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(major_questions_doctrine_scope, conceptual, 'Whether West Virginia v. EPA is a durable or contingent foreclosure of the systemic-transformation reading.').

omega_variable(
    coordination_versus_industrial_policy,
    'Is grid-wide generation-shifting best understood as a genuine coordination solution to an otherwise unsolvable collective-action problem (atmospheric carbon), or as industrial policy favoring renewable developers dressed in the language of emission-rate regulation?',
    'Compare cost-effectiveness and emission-reduction outcomes of generation-shifting compliance pathways against counterfactual facility-level-only compliance pathways using EPA''s own regulatory impact analyses and independent economic review; a large efficiency gap favoring generation-shifting supports the coordination reading, while evidence that renewable subsidization exceeds what carbon-reduction alone would justify supports the industrial-policy reading.',
    'Affects whether the tangled_rope classification (genuine coordination plus asymmetric extraction) is correct, or whether the constraint is more accurately read as a snare wearing coordination language.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_versus_industrial_policy, conceptual, 'Whether the coordination function is genuine or a cover story for directed industrial subsidy.').

omega_variable(
    coal_workforce_transition_support_adequacy,
    'Would adequately funded, targeted worker-transition programs (retraining, relocation support, pension backstops) resolve most of the extraction borne by the coal mining workforce, or is the harm structurally irreducible given the workforce''s geographic and skill concentration?',
    'Evaluate outcomes of existing federal/state coal-transition programs (e.g., POWER Initiative, IRA energy-community provisions) against pre-transition income and employment baselines in affected counties over a decade.',
    'If transition support proves adequate, the workforce''s victim status is a policy-design failure correctable without revisiting the underlying delegation question; if structurally irreducible, the extraction is intrinsic to any generation-shifting instrument regardless of ancillary support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coal_workforce_transition_support_adequacy, empirical, 'Whether coal workforce harm is a fixable policy gap or an intrinsic feature of the generation-shifting instrument.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__systemic_transformation_reading, 2015, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t2015, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(caa__tr_t2017, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2017, 0.3).
narrative_ontology:measurement(caa__tr_t2019, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2019, 0.35).
narrative_ontology:measurement(caa__tr_t2021, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2021, 0.2).
narrative_ontology:measurement(caa__tr_t2022, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2022, 0.22).
narrative_ontology:measurement(caa__tr_t2024, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(caa__be_t2015, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(caa__be_t2017, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2017, 0.48).
narrative_ontology:measurement(caa__be_t2019, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2019, 0.52).
narrative_ontology:measurement(caa__be_t2021, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2021, 0.58).
narrative_ontology:measurement(caa__be_t2022, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2022, 0.62).
narrative_ontology:measurement(caa__be_t2024, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t2015, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement(caa__su_t2017, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2017, 0.55).
narrative_ontology:measurement(caa__su_t2019, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2019, 0.5).
narrative_ontology:measurement(caa__su_t2021, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2021, 0.6).
narrative_ontology:measurement(caa__su_t2022, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2022, 0.58).
narrative_ontology:measurement(caa__su_t2024, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__systemic_transformation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, facility_constraint_reading).

% DUAL FORMULATION NOTE:
% This constraint and facility_constraint_reading are sibling readings of the caa_section_111d_delegation kernel: the same nine statutory words ('best system of emission reduction') support two structurally incompatible administrative-law claims with different victim sets, different beneficiary sets, and different ε trajectories. This story (systemic_transformation_reading) claims grid-wide generation-shifting authority and shows substantial rising extraction concentrated on coal utilities/workers/states with an implicit renewable subsidy channel. The facility_constraint_reading claims narrower facility-level retrofit authority and would show a different, lower extraction profile concentrated on individual plant compliance costs with no comparable subsidy channel. The reading_relations edge is 'forecloses': under West Virginia v. EPA's major questions framing, adopting the systemic reading as legally operative logically displaces the facility-confined reading as the operative one within a single controlling framework — the two cannot both be 'the' current governing interpretation, though they can and do coexist as competing positions across different institutional actors and time periods.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
