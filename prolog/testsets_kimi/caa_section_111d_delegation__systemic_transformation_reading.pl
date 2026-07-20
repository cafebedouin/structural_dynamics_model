% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__systemic_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: caa_section_111d_delegation__systemic_transformation_reading
 *   human_readable: CAA Section 111(d) Systemic Transformation Reading
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   This constraint instantiates the systemic_transformation_reading of the
 *   CAA Section 111(d) delegation kernel. It claims that the statutory phrase
 *   best system of emission reduction authorizes EPA to require states to
 *   restructure their electricity grids through generation shifting,
 *   renewable substitution, and early coal retirement. The sibling
 *   facility_constraint_reading limits the same phrase to measures
 *   implementable at individual facilities. This reading was operationalized
 *   in the 2015 Clean Power Plan and rejected by the Supreme Court in West
 *   Virginia v. EPA (2022), but persists as a legal and political argument.
 *   KEY AGENTS (by structural relationship): EPA (agenda-setter,
 *   institutional/analytical) asserts and enforces the reading; renewable
 *   energy developers (beneficiary, powerful/mobile) gain markets from
 *   compliance-driven procurement; coal operators (payer,
 *   powerful/constrained) bear stranded asset costs from early retirement
 *   mandates; fossil-dependent states (payer, institutional/constrained) must
 *   redesign energy regulation and absorb transition costs; federal judiciary
 *   (observer, institutional/analytical) reviewed and rejected the reading in
 *   West Virginia v. EPA.
 *
 * KEY AGENTS:
 *   - EPA: Primary agenda-setter (institutional/analytical) â sets emission guidelines and enforces state plans.
 *   - Renewable energy developers: Primary beneficiary (powerful/mobile) â gain market share from generation-shifting mandates.
 *   - Coal operators: Primary target (powerful/constrained) â bear asset stranding and early retirement costs.
 *   - Fossil-dependent states: Secondary target (institutional/constrained) â lose regulatory autonomy and absorb transition costs.
 *   - Federal judiciary: Analytical observer (institutional/analytical) â adjudicates statutory scope and rejected this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, 0.76).
domain_priors:suppression_score(caa_section_111d_delegation__systemic_transformation_reading, 0.8).
domain_priors:theater_ratio(caa_section_111d_delegation__systemic_transformation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__systemic_transformation_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__systemic_transformation_reading, "CAA Section 111(d) Systemic Transformation Reading").
narrative_ontology:topic_domain(caa_section_111d_delegation__systemic_transformation_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__systemic_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__systemic_transformation_reading, '4e21f400-0ad8-4334-b327-497e1ad0fabe').
narrative_ontology:cs_kernel_codification('4e21f400-0ad8-4334-b327-497e1ad0fabe', formalized).
narrative_ontology:cs_authority_grounding('4e21f400-0ad8-4334-b327-497e1ad0fabe', lineage).
narrative_ontology:cs_interpretation_layer_present('4e21f400-0ad8-4334-b327-497e1ad0fabe').
narrative_ontology:cs_reading_relation('4e21f400-0ad8-4334-b327-497e1ad0fabe', caa_section_111d_delegation__facility_constraint_reading, forecloses).
narrative_ontology:cs_axiom('4e21f400-0ad8-4334-b327-497e1ad0fabe', foundational, best_system_encompasses_generation_shifting).
narrative_ontology:cs_axiom_status(best_system_encompasses_generation_shifting, holdable).
narrative_ontology:cs_axiom_grounding('4e21f400-0ad8-4334-b327-497e1ad0fabe', best_system_encompasses_generation_shifting, conventional).
narrative_ontology:cs_axiom('4e21f400-0ad8-4334-b327-497e1ad0fabe', secondary, epa_may_restructure_state_energy_mix).
narrative_ontology:cs_axiom_status(epa_may_restructure_state_energy_mix, holdable).
narrative_ontology:cs_axiom_grounding('4e21f400-0ad8-4334-b327-497e1ad0fabe', epa_may_restructure_state_energy_mix, conventional).
narrative_ontology:cs_reference_frame('4e21f400-0ad8-4334-b327-497e1ad0fabe', grid_wide_transformative_authority).
narrative_ontology:cs_drift_state('4e21f400-0ad8-4334-b327-497e1ad0fabe', post_west_virginia_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('4e21f400-0ad8-4334-b327-497e1ad0fabe', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, low_carbon_states).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_operators).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, fossil_dependent_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, environmental_advocacy_groups).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, regional_grid_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Section 111(d) to authorize grid-wide generation shifting; promulgates emission guidelines requiring state plans to restructure electricity portfolios toward low-carbon sources; enforces compliance through plan approval, sanctions, and federal implementation.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, epa, agenda_setter,
    institutional, generational, analytical, national).

% Receive expanded market share and implicit price support when states must procure renewable generation to comply with EPA-mandated decarbonization pathways; invest in new capacity to serve compliance-driven demand.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_developers, beneficiary,
    powerful, biographical, mobile, national).

% Face accelerated asset stranding, loss of dispatch priority, and early retirement mandates under state plans that shift the grid away from coal; exit is blocked by sunk capital, site remediation obligations, and lack of alternative revenue streams for thermal assets.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_operators, payer,
    powerful, biographical, constrained, national).

% Must redesign electricity regulation, retire in-state coal infrastructure, and restructure utility resource plans to meet EPA emission targets; bear political backlash, ratepayer transition costs, and loss of energy autonomy under federal supremacy.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, fossil_dependent_states, payer,
    institutional, generational, constrained, national).

% Gain competitive advantage and regulatory flexibility when EPA mandates decarbonization pathways that leverage their existing renewable infrastructure; attract clean-energy investment and grid-modernization capital.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, low_carbon_states, beneficiary,
    institutional, generational, mobile, national).

% Advance climate goals through federal regulatory mandate; benefit from the legal and political precedent that systemic transformation is authorized under the Clean Air Act.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, environmental_advocacy_groups, beneficiary,
    organized, biographical, mobile, national).

% Must manage bulk-power reliability during rapid, compliance-driven generation shifts and renewable integration; bear operational costs, balancing risks, and accelerated transmission upgrade obligations.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, regional_grid_operators, payer,
    powerful, biographical, constrained, regional).

% Reviews whether the statutory phrase best system of emission reduction encompasses generation-shifting beyond the facility fence line; in West Virginia v. EPA, rejected this reading under the major questions doctrine as lacking clear congressional authorization.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates interstate decarbonization of the electricity sector by requiring states to submit plans that shift generation from high-emitting sources to low-emitting sources, solving the collective-action problem of fossil-dependent states free-riding on emission reductions.
% TRANSFER_FUNCTION: Moves compliance obligation and economic value from coal-fired generation and fossil-dependent states to renewable developers and low-carbon states, via EPA-mandated state decarbonization pathways and renewable procurement requirements.
% ABSENT_VOICES: Coal plant workers and affected mining communities are largely excluded from state plan formulation; rural electric cooperatives that depend on baseload coal and lack capital for grid restructuring are underrepresented in the regulatory record.
% DISAPPEARANCE_RATIONALE: If the systemic transformation authority vanished, states would revert to facility-level compliance such as heat-rate improvements and minor retrofits, coal retirements would slow or stop, renewable buildout would lose its regulatory compliance premium, and grid planning would reorganize around unit-level efficiency rather than system-wide fuel switching.
% FOUNDING_PROBLEM: The Clean Power Plan and its systemic reading were built to reduce greenhouse gas emissions from existing fossil fuel-fired power plants under Section 111(d), addressing the gap between stationary source regulation and climate-forcing pollution.
% FOUNDING_PROBLEM_CORROBORATION: EPA and environmental advocates attest the climate problem is live and requires systemic action. Coal operators and fossil-dependent states attest the problem, while real, does not justify this statutory reading. The Supreme Court in West Virginia v. EPA â an observer outside the beneficiary set â corroborated that the reading lacks clear statutory authorization, though it did not resolve the climate problem's liveness.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__systemic_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__systemic_transformation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__systemic_transformation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(caa_section_111d_delegation__systemic_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__systemic_transformation_reading, 0.76, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.76) because the reading mandates a massive transfer of economic value from fossil generation to renewable generation and forces states to restructure their energy systems. Suppression is high (0.80) because the constraint depends on EPA enforcement of state plan approval and preemption of less stringent facility-level alternatives. Theater ratio is moderate (0.30) because the decarbonization outcome is genuine, but the legal reasoning involved stretching statutory text to cover grid-wide restructuring. Resistance is very high (0.85) because the reading triggered immediate legal challenge, a Supreme Court stay, and eventual rejection in West Virginia v. EPA. Accessibility collapse is high (0.70) because under this reading facility-level alternatives are legally disallowed â states cannot comply solely with unit-level heat-rate improvements.
 *
 * PERSPECTIVAL GAP:
 *   The EPA seat experiences the constraint as necessary climate coordination backed by statutory text; the coal operator and fossil-state seats experience it as federal overreach that extracts asset value and regulatory autonomy. The renewable developer seat experiences it as a market-expanding coordination mechanism. The federal judiciary seat (post-West Virginia) experiences it as an unauthorized structural transformation that exceeds delegated authority.
 *
 * DIRECTIONALITY LOGIC:
 *   EPA is the agenda-setter with analytical exit (can revise interpretation). Renewable developers and low-carbon states are declared beneficiaries with mobile or institutional exit, yielding low directionality. Coal operators and fossil-dependent states are declared victims with constrained exit, yielding high directionality near full target. Regional grid operators sit as payers with constrained exit, bearing implementation costs without capturing the compliance premium.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading prevents mislabeling pure coordination by requiring both a genuine coordination function (decarbonization) and asymmetric extraction (coal/asset losses). Without the victim declarations, grid restructuring could be mistaken for a scaffold or rope. The active enforcement requirement and beneficiary-victim asymmetry force Tangled Rope classification. Temporal measurements show rising theater after judicial rejection, signaling potential piton drift â a mandate whose operative function atrophied but whose political performance persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    systemic_reading_kernel_location,
    'Is this constraint a reading of the CAA Section 111(d) kernel as systemic transformation, where the sibling facility reading would restrict best system to individual facility measures?',
    'Comparison of the two constraints in the family: facility_constraint_reading limits EPA to source-specific controls, while this reading authorizes grid-wide restructuring. The disagreement is located at the statutory scope of system.',
    'Resolving this ambiguity determines whether EPA can mandate state-level decarbonization or only unit-level efficiency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(systemic_reading_kernel_location, conceptual, 'Kernel reading location for systemic transformation').

omega_variable(
    mandate_judicial_viability,
    'Does the systemic transformation reading persist as a live legal authority after West Virginia v. EPA, or has it become a piton â a dead mandate maintained only by political theater?',
    'Track EPA subsequent rulemakings: if EPA abandons generation-shifting and retreats to facility measures, the reading is atrophied; if EPA reasserts it under new statutory theories, it remains live.',
    'If atrophied, the constraint should be reclassified toward piton; if reasserted, it remains a contested tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_judicial_viability, empirical, 'Judicial rejection and mandate atrophy').

omega_variable(
    renewable_subsidy_or_rent,
    'Do renewable developers capture regulatory rents from the compliance pathway, or do the gains diffuse to ratepayers and the climate public good?',
    'Economic analysis of renewable energy credit prices and PPA premiums in jurisdictions with 111(d)-style mandates versus those without.',
    'Concentrated capture by developers would confirm asymmetric extraction; diffuse gains would shift classification toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_subsidy_or_rent, empirical, 'Beneficiary concentration in renewable transition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__systemic_transformation_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa_111d_sys_tr_t0, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(caa_111d_sys_tr_t2, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2, 0.25).
narrative_ontology:measurement(caa_111d_sys_tr_t4, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(caa_111d_sys_tr_t5, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement(caa_111d_sys_tr_t7, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 7, 0.55).
narrative_ontology:measurement(caa_111d_sys_tr_t8, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 8, 0.7).

% Extraction over time
narrative_ontology:measurement(caa_111d_sys_be_t0, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(caa_111d_sys_be_t2, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2, 0.78).
narrative_ontology:measurement(caa_111d_sys_be_t4, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 4, 0.7).
narrative_ontology:measurement(caa_111d_sys_be_t5, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(caa_111d_sys_be_t7, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 7, 0.3).
narrative_ontology:measurement(caa_111d_sys_be_t8, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 8, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(caa_111d_sys_su_t0, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(caa_111d_sys_su_t2, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2, 0.82).
narrative_ontology:measurement(caa_111d_sys_su_t4, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 4, 0.75).
narrative_ontology:measurement(caa_111d_sys_su_t5, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(caa_111d_sys_su_t7, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 7, 0.35).
narrative_ontology:measurement(caa_111d_sys_su_t8, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 8, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation__facility_constraint_reading).

% DUAL FORMULATION NOTE:
% This constraint and its sibling facility_constraint_reading are two readings of the same statutory kernel (CAA Section 111(d)), decomposed per the epsilon-invariance principle. The facility reading treats best system as bounded by the source fence line; this reading treats it as encompassing the grid. Their epsilon values differ because the structural claims about authorized coercion differ.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
