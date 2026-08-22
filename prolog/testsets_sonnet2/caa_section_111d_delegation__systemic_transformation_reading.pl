% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__systemic_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: EPA Section 111(d) 'Best System' as Grid-Wide Generation-Shifting Mandate (Systemic Transformation Reading)
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   This story instantiates the systemic_transformation_reading of the
 *   contested Section 111(d) kernel: that EPA's statutory authority to
 *   determine the 'best system of emission reduction' for existing power
 *   plants extends beyond facility-level equipment retrofits to grid-wide,
 *   generation-shifting measures — mandating a state's overall generation mix
 *   move away from coal toward gas and renewables, including early retirement
 *   schedules. Under this reading (the Obama-era Clean Power Plan's theory,
 *   later displaced by West Virginia v. EPA's adoption of the sibling
 *   facility_constraint_reading), coal-dependent utilities and coal
 *   communities become the compliance targets of a regulatory mechanism whose
 *   beneficiaries are renewable developers, gas generators positioned to
 *   backfill dispatch, and populations who benefit from reduced emissions.
 *   The measurement series traces the rise (CPP proposal 2014-15), retreat
 *   (2017 EPA repeal attempt, ACE rule), renewed assertion (2021-22
 *   rulemaking activity pre-decision), and post-West-Virginia-v.-EPA legal
 *   contraction (2022-24) of this reading's practical force — even though the
 *   reading itself, as an interpretive claim, persists as a live minority
 *   position pending any future statutory or judicial reopening.
 *
 * KEY AGENTS:
 *   - EPA: agenda-setter under this reading — claims authority to set state-level generation-mix targets via 111(d) 'best system' determinations
 *   - coal_dependent_utilities: primary target — bear compliance costs of early retirement and dispatch reallocation they did not choose on their own investment timeline
 *   - coal_mining_communities and coal_plant_workers: downstream victims — bear employment and tax-base losses from accelerated retirement schedules set by federal rule rather than market timing
 *   - renewable_generation_developers and natural_gas_generators: beneficiaries — capture displaced generation share and receive regulatory-compliance-driven demand
 *   - state_governments_administering_compliance: intermediate agents — must design state implementation plans meeting federal generation-shift targets, caught between EPA mandate and in-state fossil interests
 *   - Supreme_Court (analytical/observer within the kernel contest, not named as stakeholder in this reading's own terms since the reading treats the Court's holding as the rejected alternative) — analytical seat resolving the kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, 0.62).
domain_priors:suppression_score(caa_section_111d_delegation__systemic_transformation_reading, 0.58).
domain_priors:theater_ratio(caa_section_111d_delegation__systemic_transformation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__systemic_transformation_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__systemic_transformation_reading, "EPA Section 111(d) 'Best System' as Grid-Wide Generation-Shifting Mandate (Systemic Transformation Reading)").
narrative_ontology:topic_domain(caa_section_111d_delegation__systemic_transformation_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__systemic_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__systemic_transformation_reading, '2f25a9f1-d52e-4ef9-bcfd-2cae3d35e86f').
narrative_ontology:cs_kernel_codification('2f25a9f1-d52e-4ef9-bcfd-2cae3d35e86f', fixed_text).
narrative_ontology:cs_authority_grounding('2f25a9f1-d52e-4ef9-bcfd-2cae3d35e86f', extraction).
narrative_ontology:cs_interpretation_layer_present('2f25a9f1-d52e-4ef9-bcfd-2cae3d35e86f').
narrative_ontology:cs_reading_relation('2f25a9f1-d52e-4ef9-bcfd-2cae3d35e86f', caa_section_111d_delegation__facility_constraint_reading, forecloses).
narrative_ontology:cs_axiom('2f25a9f1-d52e-4ef9-bcfd-2cae3d35e86f', foundational, best_system_encompasses_grid_level_measures).
narrative_ontology:cs_axiom_status(best_system_encompasses_grid_level_measures, holdable).
narrative_ontology:cs_axiom_grounding('2f25a9f1-d52e-4ef9-bcfd-2cae3d35e86f', best_system_encompasses_grid_level_measures, conventional).
narrative_ontology:cs_axiom('2f25a9f1-d52e-4ef9-bcfd-2cae3d35e86f', secondary, generation_shifting_is_cost_effective_emission_reduction).
narrative_ontology:cs_axiom_status(generation_shifting_is_cost_effective_emission_reduction, holdable).
narrative_ontology:cs_axiom_grounding('2f25a9f1-d52e-4ef9-bcfd-2cae3d35e86f', generation_shifting_is_cost_effective_emission_reduction, instrumental).
narrative_ontology:cs_reference_frame('2f25a9f1-d52e-4ef9-bcfd-2cae3d35e86f', clean_power_plan_generation_shifting_authority).
narrative_ontology:cs_drift_state('2f25a9f1-d52e-4ef9-bcfd-2cae3d35e86f', post_west_virginia_v_epa, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('2f25a9f1-d52e-4ef9-bcfd-2cae3d35e86f', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, renewable_generation_developers).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, natural_gas_generators).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, downwind_states_receiving_cleaner_air).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, climate_stabilization_beneficiaries).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_dependent_utilities).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_mining_communities).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_state_ratepayers).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_plant_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_state_ratepayers).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, state_governments_administering_compliance).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__systemic_transformation_reading, epa_technical_expertise_deference_doctrine).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__systemic_transformation_reading, clean_air_act_purposive_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, EPA determines the 'best system of emission reduction' for existing fossil-fuel power plants and asserts that this determination may include grid-wide generation-shifting targets — effectively setting state-level decarbonization pathways rather than only facility equipment standards. EPA does not collect revenue from this mechanism but administers and enforces the compliance framework, reviewing state implementation plans against federally-set generation-mix targets.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, epa, agenda_setter,
    institutional, generational, analytical, national).

% Own coal generation assets with decades of remaining projected operating life and must retire or repurpose them on a federally-mandated schedule faster than their own capital planning anticipated. Substantial lobbying and litigation resources exist, but sunk capital in coal infrastructure and long asset lifespans mean exit from the constraint (continuing to operate as planned) is not realistically available once a rule is finalized and upheld; their main leverage is contesting the rule itself, which they did successfully in West Virginia v. EPA.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_dependent_utilities, payer,
    powerful, biographical, constrained, national).

% Depend on coal extraction and coal-fired generation for employment and local tax base. An accelerated, federally-timed retirement schedule compresses the transition window they would otherwise have had under market-driven decline, with limited geographic or occupational mobility to absorb the shock. They have essentially no direct voice in EPA rulemaking and no litigation capacity of their own comparable to utilities or state attorneys general.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_mining_communities, payer,
    powerless, generational, trapped, regional).

% Face job loss on a timeline set by federal regulatory determination rather than employer decision or market timing. Retraining and relocation are available in principle but costly and uncertain in practice; most bargaining occurs through unions and state political channels rather than direct participation in the federal rulemaking process.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_plant_workers, payer,
    powerless, biographical, trapped, local).

% Gain expanded demand for wind, solar, and storage capacity as generation share is reallocated away from retiring coal plants under compliance-driven timelines. Can site new projects in whichever states and interconnection queues offer the most favorable combination of resources and regulatory treatment — mobile capital with strong exit optionality relative to fixed-asset coal incumbents.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, renewable_generation_developers, beneficiary,
    organized, biographical, arbitrage, national).

% Positioned to backfill dispatchable capacity as coal retires faster than it otherwise would, capturing displaced generation share and capacity market revenue. Their existing infrastructure and permitting relationships give them an advantageous position to expand under the compliance timeline this reading creates.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, natural_gas_generators, beneficiary,
    powerful, biographical, mobile, national).

% Bear some pass-through cost of accelerated plant retirement and new generation buildout through utility rate cases, while also gaining reduced air pollution exposure and diversified generation. Their state's grid mix is substantially determined by the compliance pathway EPA sets rather than by state-level preference alone under this reading, and switching electricity providers or relocating to avoid rate impacts is impractical for most households.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_state_ratepayers, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_state_ratepayers, beneficiary).

% Must design state implementation plans that satisfy EPA's federally-set generation-mix targets, translating the systemic mandate into concrete utility-level obligations. Caught between federal compliance deadlines and in-state political and economic resistance from coal-dependent constituencies; some states embraced the flexibility this reading afforded (allowing multi-source compliance trading) while others litigated against the underlying authority.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, state_governments_administering_compliance, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__systemic_transformation_reading, state_governments_administering_compliance, payer).

% Future populations and current populations vulnerable to climate impacts benefit diffusely from any acceleration of decarbonization this reading achieves, but have no direct voice in the rulemaking or litigation process and bear none of the concentrated compliance costs — a genuinely dispersed, non-participating beneficiary class.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, climate_stabilization_beneficiaries, beneficiary,
    powerless, civilizational, analytical, global).

% Adjudicate whether EPA's 'best system' determination under this reading exceeds statutory authority. Under the major questions doctrine analysis ultimately applied in West Virginia v. EPA (2022), reviewing courts held for the sibling facility_constraint_reading, treating the systemic_transformation_reading as requiring clearer congressional authorization than Section 111(d)'s text provides.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, reviewing_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a nationwide decarbonization trajectory for the electricity sector by allowing compliance flexibility across an entire generation fleet (trading, averaging, and substitution across sources) rather than forcing each individual facility to meet the same fixed technological standard regardless of cost-effectiveness — in principle a more efficient coordination mechanism than facility-by-facility mandates.
% TRANSFER_FUNCTION: Moves generation market share, and the associated revenue and employment, from coal-dependent utilities, coal mining communities, and coal plant workers to renewable developers and natural gas generators, on a timeline set by federal rule rather than by independent market or utility investment decisions.
% ABSENT_VOICES: Coal mining communities and coal plant workers are structurally distant from the EPA rulemaking process itself — they participate, if at all, through intermediary unions, state officials, or public comment, not as direct parties with standing comparable to utilities or state attorneys general who litigated the underlying authority question directly to the Supreme Court.
% DISAPPEARANCE_RATIONALE: If this reading's authority were fully affirmed and then vanished, states would lose the federal generation-shift compliance framework and revert to facility-level or state-only decarbonization tools; coal retirement timelines would decouple from federal mandate and follow market and state-policy timelines instead, utilities would re-plan capital expenditure around longer coal asset lives, and renewable/gas buildout driven by compliance deadlines would slow to whatever market and state-incentive pace remained.
% FOUNDING_PROBLEM: Facility-level pollution controls (the traditional tool under prior Clean Air Act sections) were understood by EPA's Clean Power Plan architects to be poorly matched to the actual least-cost path for reducing power-sector carbon emissions, since redispatching generation toward cleaner sources is frequently cheaper and more effective per ton of emissions avoided than retrofitting individual coal units with capture equipment.
% FOUNDING_PROBLEM_CORROBORATION: EPA under the Obama administration and allied environmental and public health organizations attest the founding problem (facility controls are cost-ineffective relative to generation-shifting) remains live and the systemic reading remains the technically superior solution. Coal-state attorneys general, coal utilities, and ultimately a majority of the Supreme Court in West Virginia v. EPA attest, from outside the beneficiary set, that whatever the technical merits, the statutory text does not clearly authorize this scope of authority — corroboration from the reviewing courts is corroboration against the reading's own legal viability, not against the underlying technical premise, which remains genuinely contested rather than settled either way.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__systemic_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__systemic_transformation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__systemic_transformation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.62) reflects that this reading authorizes a regulatory mechanism whose central function, from the systemic_transformation_reading's own lights, is to reallocate generation share away from coal-dependent actors on a federally-set timeline rather than a market or negotiated one — a genuine transfer, not merely an emissions limit. Suppression (0.58) is moderate-high: compliance is not optional once EPA finalizes a rule under this reading, and states with high coal dependence have limited practical alternatives to a state implementation plan that satisfies EPA's targets, though litigation and political change remain real (and ultimately successful, per West Virginia v. EPA) avenues of resistance. Resistance (0.78) is high and was decisive — states, coal utilities, and ultimately five Justices actively resisted this reading, which is precisely why it lost the kernel contest to the facility_constraint_reading. Accessibility collapse (0.50) is moderate: alternatives to compliance (litigation, political reversal, slow-walking) were real and were in fact exercised successfully, so collapse was never complete under this reading's operative period. Theater ratio (0.20) is low-moderate: much of what EPA did under this reading was substantive (real generation-shift compliance-pathway design), though some proposal-stage activity (2017-19 repeal-and-replace maneuvering) was more contested-terrain positioning than functional regulation.
 *
 * DIRECTIONALITY LOGIC:
 *   EPA is the agenda_setter under this reading, exercising claimed statutory authority with the widest possible scope of the 'best system' term; it does not itself collect extraction but administers the mechanism that redistributes generation share. Coal-dependent utilities and coal communities are targets: their exit options are structurally constrained by sunk capital in existing coal infrastructure and, for mining communities, by economic and geographic immobility, pushing their derived directionality toward the full-target end. Renewable developers and gas generators are beneficiaries with mobile-to-arbitrage exit options — they can relocate capital toward whichever jurisdictions and technologies the compliance pathway rewards, placing them near the beneficiary end. Fossil-locked state ratepayers occupy an intermediate position: they bear some pass-through compliance cost but also receive some benefit from cleaner air and diversified generation, though the balance under this reading skews toward cost-bearing given accelerated retirement timelines exceeding what utilities would have chosen unprompted.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that facility-level pollution controls were the only tool Congress had traditionally used and were poorly suited to a generation-mix transformation problem — has not disappeared (climate mitigation remains live), but the mechanism designed to solve it via this specific statutory reading was itself declared substantially foreclosed by the reviewing court under the sibling reading. This is not simple mandatrophy (the founding problem persisting while the mechanism atrophies from disuse) but rather an active kernel contest: the mechanism was structurally displaced by a competing interpretation before its function could be said to have atrophied on its own terms. Classifying this as tangled_rope rather than snare or mountain matters because the reading genuinely coordinates a real problem (grid decarbonization requires more than fenceline retrofits) even as it imposes asymmetric, federally-timed costs on a concentrated set of coal actors — both the coordination function and the extraction are real and simultaneous, which is exactly the tangled_rope signature, not a pure extraction story where the coordination claim is mere cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    major_questions_doctrine_applicability,
    'Is grid-wide generation-shifting a routine exercise of EPA''s technical ''best system of emission reduction'' authority, or does it trigger the major questions doctrine as an assertion of transformative economic and political authority Congress did not clearly grant?',
    'Supreme Court disposition (West Virginia v. EPA, 2022, held for the facility_constraint_reading) versus any subsequent legislative clarification explicitly authorizing generation-shifting.',
    'If the systemic transformation reading is correct, EPA''s 111(d) authority is broad and the facility_constraint_reading is the artificially narrowed one; if major questions doctrine controls, this reading is constitutionally foreclosed and the facility reading is the only valid one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(major_questions_doctrine_applicability, conceptual, 'Whether generation-shifting authority is ordinary delegation or a major-questions violation.').

omega_variable(
    systemic_reading_kernel_committer_structure,
    'This story instantiates the systemic_transformation_reading of the caa_section_111d_delegation kernel. What would the sibling facility_constraint_reading change structurally, and where precisely does the disagreement sit?',
    'The disagreement is located at the definition of ''best system of emission reduction'' — whether ''system'' can refer to grid-level dispatch reallocation across generation sources, or whether it is bounded to technology applicable within the fenceline of a single regulated facility. This is not a factual dispute but an interpretive-scope dispute resolvable only by whichever authority (agency, court, Congress) controls the final reading.',
    'Under the sibling facility_constraint_reading, coal plants face only heat-rate/CCS retrofit obligations (much lower compliance cost, no early retirement mandate, no renewable substitution credit pathway) — the victim set (coal_mining_communities, coal_plant_workers) would not be extraction victims of THIS mechanism, though they might face slower-moving market-driven decline instead.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(systemic_reading_kernel_committer_structure, conceptual, 'Committer-frame documentation: this story is one reading of a contested statutory kernel; the sibling reading changes the victim set and the extraction mechanism entirely.').

omega_variable(
    extraction_vs_internalization_of_externality,
    'Is the extraction from coal-dependent actors better characterized as extraction (rent transfer to renewable/gas incumbents via regulatory mandate) or as internalization of a previously externalized cost (carbon and health harms coal imposed on downwind/future populations)?',
    'Social cost of carbon accounting compared against compliance cost distribution; if compliance costs are smaller than the previously unpriced externality coal imposed, the ''extraction'' framing is contestable and the arrangement is closer to correcting a prior subsidy.',
    'If internalization framing dominates, ε should be read downward and beneficiaries reframed as harm-avoidance recipients rather than rent recipients — this story''s ε (0.62) reflects the systemic_transformation_reading''s own lights, which treats coal actors as the standing arrangement''s targets, not the externality''s historical beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_internalization_of_externality, conceptual, 'Whether the transfer is extraction or externality correction, which changes how ε should be read even within this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__systemic_transformation_reading, 2014, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t2014, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(caa__tr_t2015, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2015, 0.12).
narrative_ontology:measurement(caa__tr_t2017, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2017, 0.25).
narrative_ontology:measurement(caa__tr_t2019, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2019, 0.3).
narrative_ontology:measurement(caa__tr_t2021, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2021, 0.22).
narrative_ontology:measurement(caa__tr_t2022, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2022, 0.18).
narrative_ontology:measurement(caa__tr_t2024, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(caa__be_t2014, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2014, 0.35).
narrative_ontology:measurement(caa__be_t2015, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2015, 0.48).
narrative_ontology:measurement(caa__be_t2017, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2017, 0.4).
narrative_ontology:measurement(caa__be_t2019, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2019, 0.3).
narrative_ontology:measurement(caa__be_t2021, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2021, 0.45).
narrative_ontology:measurement(caa__be_t2022, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2022, 0.58).
narrative_ontology:measurement(caa__be_t2024, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t2014, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2014, 0.4).
narrative_ontology:measurement(caa__su_t2015, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(caa__su_t2017, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2017, 0.35).
narrative_ontology:measurement(caa__su_t2019, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2019, 0.2).
narrative_ontology:measurement(caa__su_t2021, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2021, 0.4).
narrative_ontology:measurement(caa__su_t2022, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2022, 0.15).
narrative_ontology:measurement(caa__su_t2024, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__systemic_transformation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(caa_section_111d_delegation__systemic_transformation_reading, 0.12).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation__facility_constraint_reading).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, west_virginia_v_epa_major_questions_doctrine).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, state_implementation_plan_compliance_flexibility).

% DUAL FORMULATION NOTE:
% This story and caa_section_111d_delegation__facility_constraint_reading are sibling readings of the same statutory kernel (caa_section_111d_delegation): the phrase 'best system of emission reduction' in Section 111(d) of the Clean Air Act. This story (systemic_transformation_reading) authors ε=0.62, reflecting genuine grid-wide extraction from coal-dependent actors under a broad reading of EPA's authority. The sibling facility_constraint_reading authors a substantially lower ε reflecting a narrower, uncontested facility-level compliance obligation with no generation-shift mandate. The two are not the same constraint measured differently — they are structurally distinct claims about what the statute permits, with different victim sets, different compliance costs, and different legal outcomes. They are linked here because the kernel contest between them (ultimately resolved by West Virginia v. EPA in favor of the facility reading) is itself a determinate structural event affecting both stories' practical force.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
