% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__systemic_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    constraint_indexing:directionality_override/3,
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
 *   human_readable: CAA Section 111(d) Systemic Transformation Delegation Reading
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   This constraint story captures the systemic transformation reading of CAA
 *   Section 111(d) — the interpretation that 'best system of emission
 *   reduction' authorizes EPA to set standards based on grid-wide generation
 *   shifting (renewable substitution, coal retirement) rather than
 *   facility-level measures. The Clean Power Plan (2015) operationalized this
 *   reading; the Affordable Clean Energy rule (2019) repudiated it; West
 *   Virginia v. EPA (2022) constrained it via the major questions doctrine;
 *   the 2024 EPA rule attempts to revive it within a narrower frame. The
 *   constraint is the standing arrangement of EPA asserting systemic
 *   authority, not the text of Section 111(d) itself.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, 0.78).
domain_priors:suppression_score(caa_section_111d_delegation__systemic_transformation_reading, 0.82).
domain_priors:theater_ratio(caa_section_111d_delegation__systemic_transformation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__systemic_transformation_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__systemic_transformation_reading, "CAA Section 111(d) Systemic Transformation Delegation Reading").
narrative_ontology:topic_domain(caa_section_111d_delegation__systemic_transformation_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__systemic_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__systemic_transformation_reading, 'f6ee8f71-412c-4100-92bf-9b720ab524b3').
narrative_ontology:cs_kernel_codification('f6ee8f71-412c-4100-92bf-9b720ab524b3', formalized).
narrative_ontology:cs_authority_grounding('f6ee8f71-412c-4100-92bf-9b720ab524b3', lineage).
narrative_ontology:cs_interpretation_layer_present('f6ee8f71-412c-4100-92bf-9b720ab524b3').
narrative_ontology:cs_reading_relation('f6ee8f71-412c-4100-92bf-9b720ab524b3', caa_section_111d_delegation__facility_constraint_reading, coexists_with).
narrative_ontology:cs_axiom('f6ee8f71-412c-4100-92bf-9b720ab524b3', foundational, best_system_includes_generation_shifting).
narrative_ontology:cs_axiom_status(best_system_includes_generation_shifting, holdable).
narrative_ontology:cs_axiom_grounding('f6ee8f71-412c-4100-92bf-9b720ab524b3', best_system_includes_generation_shifting, instrumental).
narrative_ontology:cs_axiom('f6ee8f71-412c-4100-92bf-9b720ab524b3', foundational, cooperative_federalism_enables_systemwide_mandates).
narrative_ontology:cs_axiom_status(cooperative_federalism_enables_systemwide_mandates, holdable).
narrative_ontology:cs_axiom_grounding('f6ee8f71-412c-4100-92bf-9b720ab524b3', cooperative_federalism_enables_systemwide_mandates, conventional).
narrative_ontology:cs_reference_frame('f6ee8f71-412c-4100-92bf-9b720ab524b3', clean_air_act_1970_delegation).
narrative_ontology:cs_drift_state('f6ee8f71-412c-4100-92bf-9b720ab524b3', post_west_virginia_v_epa, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f6ee8f71-412c-4100-92bf-9b720ab524b3', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, epa_regulatory_authority).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_sector).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, environmental_ngo_coalition).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_mining_sector).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_fired_power_operators).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_state_governments).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, industrial_energy_intensive_users).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__systemic_transformation_reading, administrative_state_climate_authority).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__systemic_transformation_reading, generation_shifting_as_best_system).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__systemic_transformation_reading, cooperative_federalism_as_flexible_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Section 111(d) to authorize grid-wide, generation-shifting strategies including renewable substitution and early coal retirement. Sets emissions guidelines that states must implement through state plans. Gains institutional authority and mission scope; the reading expands EPA's regulatory reach from facility-level to system-level. Can pivot to alternative interpretations if courts foreclose, but bureaucratic momentum and institutional identity are invested in the systemic reading.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, epa_regulatory_authority, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__systemic_transformation_reading, epa_regulatory_authority, beneficiary).

% Receives de facto subsidies through regulatory compliance pathways: state plans under the systemic reading create guaranteed demand for wind, solar, and storage via generation-shifting requirements. Benefits from coal retirement schedules that open market share. Can deploy capital across jurisdictions; exit is mobile but policy dependency creates path dependence.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_sector, beneficiary,
    organized, biographical, mobile, national).

% Gains the regulatory lever they have lobbied for: a federal mandate that forces economy-wide decarbonization through the Clean Air Act without new legislation. Their institutional model depends on administrative pathway viability. Exit is constrained — if this reading fails, their primary federal climate strategy collapses; they have limited alternative pathways at this scale.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, environmental_ngo_coalition, beneficiary,
    organized, generational, constrained, national).

% Faces regulated early retirement of its primary customer base (coal-fired power plants). State compliance plans under the systemic reading explicitly schedule coal unit retirements and substitute renewable generation. Capital is specialized, geography-bound, and workforce is identity-fused to mining communities. Exit is trapped — assets cannot be repurposed, and political resistance is existential.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_mining_sector, payer,
    powerful, biographical, trapped, regional).

% Must retire or retrofit coal units ahead of economic life to meet state plan targets. Can pivot to gas or renewables but at high stranded-asset cost and with regulatory risk. Some have diversified generation portfolios; others are pure-play coal. Exit is constrained — they can change generation mix but the transition cost is imposed by the mandate, not chosen.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_fired_power_operators, payer,
    powerful, biographical, constrained, national).

% States whose budgets, employment, and political identity are built on fossil extraction (WV, WY, ND, KY, etc.). Must write and implement state plans that dismantle their own economic base. Litigation is their primary exit; compliance is coerced. Identity_locked because state political leadership defines itself in opposition to federal climate mandates — exit from the conflict would fracture the governing coalition.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_state_governments, payer,
    institutional, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_state_governments, excluded).

% Face rising electricity costs as generation-shifting costs are rate-based. Can relocate but supply chains and workforce are sticky. Some have on-site generation options; most are price-takers. Exit is constrained — they bear the pass-through costs without the political voice of the energy producers.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, industrial_energy_intensive_users, payer,
    moderate, biographical, constrained, national).

% The ultimate adjudicator of the delegation's scope. West Virginia v. EPA (2022) invoked major questions doctrine to constrain the systemic reading; the Court's composition makes further foreclosure likely. Does not collect or pay; its rulings determine whether the constraint persists, mutates, or collapses.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, supreme_court_majority, observer,
    institutional, generational, analytical, national).

% The legislative body that enacted Section 111(d) in 1970 and amended it in 1990. Has not legislated on climate-specific authority since. Could resolve the delegation ambiguity by statute but is gridlocked. Excluded from the operational constraint — the systemic reading operates in the vacuum of legislative silence. Can always re-enter by passing a law, but political arithmetic makes that arbitrage-grade exit unlikely.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, congress, excluded,
    institutional, generational, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a national decarbonization trajectory across 50 state electricity systems by converting a facility-level performance standard into a system-wide generation-shifting mandate, solving the collective-action problem of free-riding states and leaky borders.
% TRANSFER_FUNCTION: Moves compliance costs and stranded-asset losses from the coal value chain (miners, plant owners, fossil-locked states) to electricity ratepayers and renewable developers, mediated through state implementation plans that subsidize renewable deployment via regulatory mandate.
% ABSENT_VOICES: Coal mining communities and fossil-locked state legislatures are structurally excluded from the EPA rulemaking process that defines 'best system' — they participate only in litigation after the fact. Future generations who inherit the climate trajectory are excluded by definition. Grid reliability engineers who warn of resource adequacy risks are consulted but not empowered to veto.
% DISAPPEARANCE_RATIONALE: If the systemic reading vanished overnight, EPA would revert to facility-level standards (heat-rate improvements, carbon capture); coal plants would operate longer; renewable deployment would lose its primary federal regulatory driver; state implementation plans would be rewritten; the entire architecture of federal climate regulation through the Clean Air Act would collapse. The world rearranges.
% FOUNDING_PROBLEM: The 1970 Clean Air Act gave EPA authority to set standards for existing sources via 'best system of emission reduction' but left 'system' undefined. The founding problem was how to regulate pollutants from heterogeneous, long-lived industrial sources without prescribing specific technologies — a delegation designed for flexibility, not for climate.
% FOUNDING_PROBLEM_CORROBORATION: EPA and environmental NGOs attest the founding problem is live: climate change is the novel manifestation of the same delegation logic. Coal sector and fossil-locked states attest the founding problem is dead: the statute was never understood to authorize generation-shifting or fuel-switching mandates. The D.C. Circuit and Supreme Court majorities have split — the textualist reading (facility constraint) and purposivist reading (systemic transformation) both claim fidelity to the founding problem. No neutral corroborator exists; the contest is the point.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__systemic_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__systemic_transformation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__systemic_transformation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(caa_section_111d_delegation__systemic_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__systemic_transformation_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.78) is high because the reading transfers massive compliance costs and stranded-asset losses to the coal value chain and fossil-locked states while subsidizing renewables through regulatory mandate — the transfer is the mechanism, not a side effect. Suppression (0.82) is high because persistence depends on active enforcement: litigation defense, state plan review, compliance deadlines, and the major questions doctrine threat requires continuous institutional effort. Theater ratio (0.28) is moderate — the facility-level coordination function (heat-rate improvements) is real but shrinking relative to the generation-shifting mandate. Accessibility collapse (0.68) reflects that alternatives (facility-only reading) are legally contested but not foreclosed; resistance (0.71) reflects sustained litigation, congressional opposition, and state non-cooperation. The claimed_type is tangled_rope because there IS a genuine coordination function (national decarbonization across leaky state borders) AND asymmetric extraction (coal sector pays, renewables gain).
 *
 * PERSPECTIVAL GAP:
 *   From EPA's seat, the constraint is rope: it coordinates a genuine collective-action problem (interstate pollution leakage, free-riding) with minimal coercion relative to the alternative of 50 uncoordinated state policies. From coal mining's seat, it is snare: the coordination story is cover for a predetermined phase-out; suppression is the point. From fossil-locked states' seat, it is snare with identity-lock: their political existence is defined by opposition. The engine computes this divergence from the structural data — the same constraint is experienced as different types.
 *
 * DIRECTIONALITY LOGIC:
 *   EPA is the agenda-setter and institutional beneficiary — it gains mission scope and regulatory authority (d near 0.15). Renewable sector and environmental NGOs are beneficiaries with mobile/constrained exit (d near 0.2-0.35). Coal mining sector is trapped (d near 0.95) — specialized assets, geographic lock-in, identity-fused workforce. Coal plant operators are constrained (d near 0.75) — can pivot but at imposed cost. Fossil-locked states are identity-locked (d near 0.85) — political identity binds them to resistance even as compliance is coerced. Industrial users are constrained payers (d near 0.65). Supreme Court is analytical observer (d = 0.5). Congress is excluded with arbitrage exit (d undefined — not in the constraint's operation).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (regulating existing source pollution via 'best system') has not atrophied — the problem (climate pollution from power plants) is live and growing. But the *reading* has mutated: the 1970 delegation was for facility-level technology forcing; the 2015 systemic reading repurposed the delegation for economy-wide energy system transformation. This is not mandatrophy (purpose atrophy) — it is mandate *expansion* via interpretive capture. The classification prevents mislabeling: the coordination function is real (interstate leakage IS a collective-action problem), but the extraction is asymmetric and enforcement-dependent — tangled_rope, not rope, not snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    major_questions_doctrine_trajectory,
    'Will the Supreme Court''s major questions doctrine foreclose the systemic reading entirely, or leave a narrowed but viable path?',
    'Future Supreme Court decisions on the 2024 EPA rule or successor rules; the composition of the Court and the doctrine''s evolving test (clear statement vs. major questions as non-delegation).',
    'If foreclosed, the constraint collapses to facility_constraint_reading (extraction drops, coordination narrows). If narrowed but viable, the systemic reading persists as a constrained tangled_rope with lower extractiveness but ongoing enforcement burden.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(major_questions_doctrine_trajectory, conceptual, 'Whether the major questions doctrine is a hard ceiling or a shaping constraint on the systemic reading.').

omega_variable(
    coordination_extraction_separability,
    'Is the grid-wide coordination function (solving interstate leakage) structurally separable from the generation-shifting extraction (coal-to-renewable transfer)?',
    'Counterfactual analysis: could a facility-level standard with interstate trading achieve comparable leakage reduction without the generation-shifting mandate? Empirical evidence from RGGI, CAISO, and other regional markets.',
    'If separable, the extraction is gratuitous — the constraint is a snare wearing a rope''s coat. If inseparable, the extraction is the price of the coordination — tangled_rope is the honest classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether the coordination and extraction components of the systemic reading are structurally separable.').

omega_variable(
    reading_foreclosure_structure,
    'Does the systemic transformation reading logically foreclose the facility constraint reading within a single legal framework, or do they coexist as competing interpretations?',
    'Court opinions analyzing whether the two readings are mutually exclusive: if ''best system'' means grid-wide, can it also mean facility-only? The statutory text''s ambiguity and the Court''s interpretive methodology (textualism vs. purposivism) determine the relation.',
    'If forecloses, the kernel has a binary structure — one reading must win. If coexists_with, both readings remain live in different institutional venues (EPA vs. Courts vs. States). This determines the reading_relations classification and the kernel''s long-term dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Whether the sibling readings foreclose each other or coexist as live interpretive positions.').

omega_variable(
    identity_lock_mechanism_fossil_states,
    'What specific identity-fusion mechanism binds fossil-locked state governments to resistance — partisan polarization, economic dependency, cultural identity, or institutional path dependence?',
    'Comparative analysis of state responses: states with similar fossil dependency but different political cultures (e.g., TX vs. WV vs. NM); tracking whether resistance persists after economic transition begins.',
    'If identity-lock is partisan, it may shift with electoral realignment. If economic-cultural, it persists across administrations. This determines whether exit_options should be modeled as identity_locked (persistent) or constrained (conditional).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_fossil_states, empirical, 'The mechanism of identity lock for fossil-locked state governments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__systemic_transformation_reading, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa111d_sys_tr_t2015, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2015, 0.12).
narrative_ontology:measurement(caa111d_sys_tr_t2017, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2017, 0.15).
narrative_ontology:measurement(caa111d_sys_tr_t2019, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2019, 0.18).
narrative_ontology:measurement(caa111d_sys_tr_t2021, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2021, 0.22).
narrative_ontology:measurement(caa111d_sys_tr_t2023, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2023, 0.25).
narrative_ontology:measurement(caa111d_sys_tr_t2025, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(caa111d_sys_be_t2015, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(caa111d_sys_be_t2017, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2017, 0.42).
narrative_ontology:measurement(caa111d_sys_be_t2019, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2019, 0.48).
narrative_ontology:measurement(caa111d_sys_be_t2021, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2021, 0.58).
narrative_ontology:measurement(caa111d_sys_be_t2023, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2023, 0.71).
narrative_ontology:measurement(caa111d_sys_be_t2025, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(caa111d_sys_su_t2015, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2015, 0.45).
narrative_ontology:measurement(caa111d_sys_su_t2017, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2017, 0.55).
narrative_ontology:measurement(caa111d_sys_su_t2019, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2019, 0.65).
narrative_ontology:measurement(caa111d_sys_su_t2021, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2021, 0.72).
narrative_ontology:measurement(caa111d_sys_su_t2023, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2023, 0.79).
narrative_ontology:measurement(caa111d_sys_su_t2025, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2025, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__systemic_transformation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(caa_section_111d_delegation__systemic_transformation_reading, 0.12).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation__facility_constraint_reading).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111b_new_source_performance_standards).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_state_plan_federal_implementation_plan).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, west_virginia_v_epa_major_questions_doctrine).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, ira_clean_electricity_provisions).

% DUAL FORMULATION NOTE:
% Kernel caa_section_111d_delegation decomposes into two constraint stories: this systemic_transformation_reading (tangled_rope, high extraction, grid-wide authority) and facility_constraint_reading (rope or mountain, low extraction, facility-only authority). The systemic reading claims the delegation's flexibility authorizes generation-shifting; the facility reading claims the delegation's text limits 'system' to the source. They share the same statutory text but instantiate different constraints with different ε, different beneficiaries/victims, and different types. This story is the systemic reading; the sibling is the facility reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(caa_section_111d_delegation__systemic_transformation_reading, institutional, 0.15).
constraint_indexing:directionality_override(caa_section_111d_delegation__systemic_transformation_reading, organized, 0.25).
constraint_indexing:directionality_override(caa_section_111d_delegation__systemic_transformation_reading, powerful, 0.75).
constraint_indexing:directionality_override(caa_section_111d_delegation__systemic_transformation_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
