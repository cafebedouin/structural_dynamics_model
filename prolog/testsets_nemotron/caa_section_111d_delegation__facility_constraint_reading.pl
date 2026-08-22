% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__facility_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_caa_section_111d_delegation__facility_constraint_reading, []).

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
 *   constraint_id: caa_section_111d_delegation__facility_constraint_reading
 *   human_readable: CAA Section 111(d) Facility-Constraint Delegation Reading
 *   domain: administrative/environmental/constitutional
 *
 * SUMMARY:
 *   This constraint story models the 'facility-constraint reading' of CAA
 *   Section 111(d) — the interpretation that 'best system of emission
 *   reduction' is limited to measures implementable at and within individual
 *   stationary sources (heat-rate improvements, carbon capture, operational
 *   efficiencies). This reading emerged from the litigation trajectory
 *   culminating in West Virginia v. EPA (2022) and structures the EPA's
 *   current regulatory authority for existing power plants. The sibling
 *   reading (systemic_transformation_reading) authorizes generation-shifting
 *   across the grid; this reading forecloses that authority. The claimed type
 *   is tangled_rope: a genuine coordination function (federalism-preserving
 *   incrementalism) coexists with asymmetric extraction (coal fleet
 *   protection at the expense of deeper reductions).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, 0.68).
domain_priors:suppression_score(caa_section_111d_delegation__facility_constraint_reading, 0.72).
domain_priors:theater_ratio(caa_section_111d_delegation__facility_constraint_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__facility_constraint_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__facility_constraint_reading, "CAA Section 111(d) Facility-Constraint Delegation Reading").
narrative_ontology:topic_domain(caa_section_111d_delegation__facility_constraint_reading, "administrative/environmental/constitutional").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__facility_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__facility_constraint_reading, 'd38ee281-3d80-49dc-9571-2e42fda9d2c0').
narrative_ontology:cs_kernel_codification('d38ee281-3d80-49dc-9571-2e42fda9d2c0', fixed_text).
narrative_ontology:cs_authority_grounding('d38ee281-3d80-49dc-9571-2e42fda9d2c0', lineage).
narrative_ontology:cs_interpretation_layer_present('d38ee281-3d80-49dc-9571-2e42fda9d2c0').
narrative_ontology:cs_reading_relation('d38ee281-3d80-49dc-9571-2e42fda9d2c0', caa_section_111d_delegation__systemic_transformation_reading, forecloses).
narrative_ontology:cs_axiom('d38ee281-3d80-49dc-9571-2e42fda9d2c0', foundational, system_means_facility_boundary).
narrative_ontology:cs_axiom_status(system_means_facility_boundary, holdable).
narrative_ontology:cs_axiom_grounding('d38ee281-3d80-49dc-9571-2e42fda9d2c0', system_means_facility_boundary, conventional).
narrative_ontology:cs_axiom('d38ee281-3d80-49dc-9571-2e42fda9d2c0', foundational, major_questions_bars_generation_shifting).
narrative_ontology:cs_axiom_status(major_questions_bars_generation_shifting, holdable).
narrative_ontology:cs_axiom_grounding('d38ee281-3d80-49dc-9571-2e42fda9d2c0', major_questions_bars_generation_shifting, conventional).
narrative_ontology:cs_reference_frame('d38ee281-3d80-49dc-9571-2e42fda9d2c0', cooperative_federalism_111d_framework).
narrative_ontology:cs_drift_state('d38ee281-3d80-49dc-9571-2e42fda9d2c0', post_west_virginia_v_epa, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d38ee281-3d80-49dc-9571-2e42fda9d2c0', '2026-08-20T14:30:00Z').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, coal_fleet_operators).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, state_energy_offices).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, fossil_fuel_suppliers).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, climate_advocacy_organizations).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, downwind_communities).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, renewable_developers).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, major_questions_doctrine).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, cooperative_federalism_presumption).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, statutory_textualism_111d).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues emission guidelines under Section 111(d) but constrained by judicial precedent to facility-level measures; faces litigation risk if guidelines exceed the reading's boundary. The administrator's authority to regulate is real but channeled by the reading's interpretive ceiling.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, epa_administrator, agenda_setter,
    institutional, biographical, constrained, national).

% Avoid forced retirement or generation-shifting mandates; compliance reduces to heat-rate improvements and optional carbon capture at existing units. The reading protects their asset base from regulatory stranding while imposing manageable compliance costs.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, coal_fleet_operators, beneficiary,
    organized, biographical, mobile, national).

% Retain authority over generation mix and resource planning; EPA guidelines become advisory inputs to state plans rather than prescriptive mandates. States can satisfy compliance through facility upgrades without restructuring their energy portfolios.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, state_energy_offices, beneficiary,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__facility_constraint_reading, state_energy_offices, agenda_setter).

% Bear the extraction of a regulatory ceiling: the reading caps achievable emissions reductions at what facility-level measures can deliver, foreclosing the systemic transformations their mission requires. Exit means abandoning the statutory vehicle they invested decades in; identity is fused to Clean Act advocacy.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, climate_advocacy_organizations, payer,
    organized, generational, identity_locked, national).

% Experience continued co-pollutant exposure from coal plants that avoid retirement; facility-level heat-rate improvements marginally reduce but do not eliminate local pollution burden. No structural exit from the airshed; regulatory constraint is the only lever.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, downwind_communities, payer,
    powerless, biographical, trapped, local).

% Lose the regulatory driver (generation-shifting) that would create compliance demand for new renewable capacity; facility upgrades substitute for replacement. Market access depends on state-level policies outside EPA's constrained reach.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, renewable_developers, payer,
    moderate, biographical, constrained, national).

% Coal and gas suppliers retain demand from existing fleet; the reading removes the largest regulatory threat to fuel throughput. Carbon capture creates a potential new revenue stream without reducing primary fuel consumption.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, fossil_fuel_suppliers, beneficiary,
    organized, generational, arbitrage, global).

% Adjudicate the boundary of EPA's authority; their opinions instantiate and refine the reading. They see the full structural field but do not bear its costs or collect its rents.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, dc_circuit_judges, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a unified national emissions guideline process that respects state primacy over energy mix while achieving incremental reductions at existing sources — a federalism-preserving alternative to prescriptive federal mandates.
% TRANSFER_FUNCTION: Transfers regulatory stringency from the coal fleet and state energy planners to climate advocates and downwind communities: the coal sector avoids forced retirement costs, states avoid federal energy-planning intrusion, and the foregone reductions are borne as continued emissions and co-pollutant exposure.
% ABSENT_VOICES: Future generations who inherit the climate consequences of the regulatory ceiling; international partners who rely on U.S. systemic decarbonization for global mitigation credibility. Neither is represented in the domestic administrative proceeding.
% DISAPPEARANCE_RATIONALE: If the facility-constraint reading vanished, EPA could mandate generation-shifting under 111(d), coal retirements would accelerate under federal guideline pressure, state energy offices would lose planning autonomy, and renewable deployment would gain a federal compliance driver — the entire regulatory architecture of power-sector decarbonization would reorganize.
% FOUNDING_PROBLEM: How to regulate existing source emissions under Section 111(d) without triggering non-delegation or major questions objections, while preserving the cooperative federalism structure the Act establishes for state implementation.
% FOUNDING_PROBLEM_CORROBORATION: The textualist/major-questions reading (this reading's family) is attested by the conservative legal movement and coal-state attorneys general. The systemic-transformation reading is attested by EPA's own 2015 Clean Power Plan rulemaking record, the D.C. Circuit's pre-West Virginia dicta, and climate law scholars. No neutral arbiter outside the beneficiary sets corroborates either framing exclusively.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__facility_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__facility_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__facility_constraint_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(caa_section_111d_delegation__facility_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__facility_constraint_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(caa_section_111d_delegation__facility_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(caa_section_111d_delegation__facility_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the gap between what facility-level measures can achieve (~10-15% fleet-wide reduction) and what systemic measures could achieve (~30-40%) — the delta is extracted from climate beneficiaries as foregone mitigation. Suppression (0.72) is high because the reading is actively enforced through judicial invalidation of broader guidelines; alternatives (generation-shifting) are not merely unavailable but legally foreclosed. Theater ratio (0.42) captures the growing gap between the 'cooperative federalism' framing and the actual operation: states receive guidelines they cannot meaningfully shape, while the coal fleet's protection becomes the de facto outcome. Accessibility collapse (0.61) and resistance (0.58) are moderate: alternatives exist in the sibling reading and in state-level action, but the federal statutory vehicle is substantially closed.
 *
 * PERSPECTIVAL GAP:
 *   From the coal_fleet_operator seat, the constraint is a rope: genuine coordination that avoids disruptive federal overreach. From the climate_advocacy_organization seat, it is a snare: the coordination story is cover for protecting incumbent assets. From the state_energy_office seat, it is a scaffold: temporary federal guidance that preserves state primacy. The engine computes this divergence from the structural data — the authored claim (tangled_rope) captures the hybrid reality that no single seat experiences uniformly.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (coal_fleet_operators, state_energy_offices, fossil_fuel_suppliers) occupy the low-d end: the constraint subsidizes their asset continuity and planning autonomy. Victims (climate_advocacy_organizations, downwind_communities, renewable_developers) occupy the high-d end: they bear the extraction of a regulatory ceiling. The EPA administrator sits near symmetric (d ~ 0.5): constrained authority with real but channeled power. Downwind communities are trapped (exit_options: trapped) — no mobility from the airshed, no regulatory lever beyond this constraint. Climate advocates are identity_locked — their organizational identity is fused to Clean Act advocacy, making exit from the constraint unthinkable even as it extracts from them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (regulating without triggering major-questions objections) was live in 2015-2019 but has been resolved by West Virginia v. EPA: the Court has now authoritatively settled the boundary. The constraint persists not because the founding problem remains, but because the reading has become the doctrinal ceiling — a piton dynamic where the constraint's function has atrophied to maintaining the boundary itself. However, active litigation over the Inflation Reduction Act's interaction with 111(d) and EPA's 2024 rules keeps the extraction live, so tangled_rope remains the honest claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    system_boundary_indeterminacy,
    'Is the facility boundary in ''best system of emission reduction'' a textual necessity or a doctrinal choice driven by major questions avoidance?',
    'Comparative statutory analysis: does ''system'' in other CAA provisions (e.g., 111(a), 112) carry facility-bound or system-wide meaning? Historical examination of 1970/1977 legislative intent regarding existing-source regulation.',
    'If textual necessity, the reading is a mountain of statutory interpretation (low extraction, high naturalness). If doctrinal choice, the reading is a constructed constraint serving coal-sector protection (high extraction, tangled_rope/snare). This omega drives the false_summit_mountain evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(system_boundary_indeterminacy, conceptual, 'Whether the facility constraint is compelled by text or chosen by doctrine.').

omega_variable(
    carbon_capture_viability,
    'Does the inclusion of carbon capture as a ''facility-level'' measure genuinely expand the reading''s reduction potential, or is it a theatrical inclusion that preserves the reading''s coherence while knowing CCS is economically non-viable at scale?',
    'Track CCS deployment at regulated EGUs under the 2024 rules; compare actual capture rates and costs to the rule''s assumptions. Monitor whether CCS becomes a compliance path or remains a symbolic option.',
    'If CCS is non-viable, the reading''s coordination function (meaningful reduction via facility measures) collapses, theater_ratio approaches 1.0, and the constraint reclassifies toward piton/snare. If viable, the reading retains genuine coordination substance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(carbon_capture_viability, empirical, 'Whether the facility-level toolkit contains a real reduction pathway or a performative one.').

omega_variable(
    state_plan_autonomy_reality,
    'Do state implementation plans under this reading exercise genuine autonomy, or does the facility-constraint ceiling make state plans functionally identical regardless of state preference?',
    'Compare submitted state plans under the 2024 framework: measure variance in stringency, timeline, and measure selection across states with different political orientations.',
    'If plans converge to the federal floor, the ''cooperative federalism'' coordination story is theater — the reading extracts state autonomy while performing its preservation. If plans diverge meaningfully, the coordination function is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_plan_autonomy_reality, empirical, 'Whether state autonomy under the reading is substantive or ceremonial.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__facility_constraint_reading, 2015, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa_111d_facility_tr_t2015, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(caa_111d_facility_tr_t2017, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2017, 0.18).
narrative_ontology:measurement(caa_111d_facility_tr_t2019, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2019, 0.28).
narrative_ontology:measurement(caa_111d_facility_tr_t2021, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2021, 0.35).
narrative_ontology:measurement(caa_111d_facility_tr_t2022, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2022, 0.39).
narrative_ontology:measurement(caa_111d_facility_tr_t2024, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(caa_111d_facility_be_t2015, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2015, 0.45).
narrative_ontology:measurement(caa_111d_facility_be_t2017, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2017, 0.38).
narrative_ontology:measurement(caa_111d_facility_be_t2019, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2019, 0.52).
narrative_ontology:measurement(caa_111d_facility_be_t2021, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2021, 0.61).
narrative_ontology:measurement(caa_111d_facility_be_t2022, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2022, 0.65).
narrative_ontology:measurement(caa_111d_facility_be_t2024, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(caa_111d_facility_su_t2015, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(caa_111d_facility_su_t2017, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2017, 0.48).
narrative_ontology:measurement(caa_111d_facility_su_t2019, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2019, 0.62).
narrative_ontology:measurement(caa_111d_facility_su_t2021, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2021, 0.68).
narrative_ontology:measurement(caa_111d_facility_su_t2022, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2022, 0.7).
narrative_ontology:measurement(caa_111d_facility_su_t2024, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__facility_constraint_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(caa_section_111d_delegation__facility_constraint_reading, 0.12).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation__systemic_transformation_reading).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, caa_section_111_new_source_performance_standards).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, inflation_reduction_act_section_136_clean_energy_provisions).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, west_virginia_v_epa_doctrinal_ceiling).

% DUAL FORMULATION NOTE:
% This reading and systemic_transformation_reading form a constraint family decomposing the 'CAA Section 111(d) delegation' kernel. The facility-constraint reading has higher empirical confidence post-West Virginia but lower reduction potential; the systemic-transformation reading has higher reduction potential but was foreclosed by the Court. The extraction delta (this reading's ε ≈ 0.68 vs. sibling's ε ≈ 0.35 if implemented) tracks the cost of the doctrinal ceiling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(caa_section_111d_delegation__facility_constraint_reading, institutional, 0.45).
constraint_indexing:directionality_override(caa_section_111d_delegation__facility_constraint_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
