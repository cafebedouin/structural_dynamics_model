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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   domain: administrative/environmental/regulatory
 *
 * SUMMARY:
 *   The Clean Air Act Section 111(d) directs EPA to set emission guidelines
 *   for existing sources based on the 'best system of emission reduction.'
 *   This constraint instantiates the systemic transformation reading: the
 *   statutory phrase authorizes grid-wide, generation-shifting
 *   strategiesâincluding renewable substitution and early coal
 *   retirementârather than measures confined to individual facility
 *   fence-lines. Under this reading, EPA can mandate state-level
 *   decarbonization pathways, making fossil-dependent utilities and
 *   fossil-locked states the structural targets while clean-energy states and
 *   renewable developers benefit from compliance-driven market expansion. The
 *   constraint is actively enforced through state implementation plans (SIPs)
 *   and federal implementation plans (FIPs). It is contested by the
 *   facility-bound sibling reading, which holds that 'best system' is limited
 *   to heat-rate improvements and carbon capture at individual plants.
 *
 * KEY AGENTS:
 *   - epa: Agenda-setter (institutional/constrained) â interprets statute and enforces guidelines
 *   - coal_operators: Payer (organized/constrained) â bear stranded asset costs and forced retirement
 *   - fossil_locked_states: Payer (institutional/trapped) â face state-level economic restructuring and tax base loss
 *   - renewable_energy_developers: Beneficiary (organized/mobile) â capture market share via compliance-driven procurement
 *   - clean_energy_states: Beneficiary (institutional/mobile) â gain comparative advantage in compliance cost
 *   - federal_judiciary: Observer (institutional/analytical) â adjudicates statutory boundary between readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, 0.62).
domain_priors:suppression_score(caa_section_111d_delegation__systemic_transformation_reading, 0.58).
domain_priors:theater_ratio(caa_section_111d_delegation__systemic_transformation_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__systemic_transformation_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__systemic_transformation_reading, "CAA Section 111(d) Systemic Transformation Reading").
narrative_ontology:topic_domain(caa_section_111d_delegation__systemic_transformation_reading, "administrative/environmental/regulatory").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__systemic_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__systemic_transformation_reading, 'e7ff9f15-7419-49e6-b3a6-d08110eb04be').
narrative_ontology:cs_kernel_codification('e7ff9f15-7419-49e6-b3a6-d08110eb04be', fixed_text).
narrative_ontology:cs_authority_grounding('e7ff9f15-7419-49e6-b3a6-d08110eb04be', lineage).
narrative_ontology:cs_interpretation_layer_present('e7ff9f15-7419-49e6-b3a6-d08110eb04be').
narrative_ontology:cs_reading_relation('e7ff9f15-7419-49e6-b3a6-d08110eb04be', caa_section_111d_delegation__facility_constraint_reading, forecloses).
narrative_ontology:cs_axiom('e7ff9f15-7419-49e6-b3a6-d08110eb04be', foundational, best_system_exceeds_facility_boundary).
narrative_ontology:cs_axiom_status(best_system_exceeds_facility_boundary, holdable).
narrative_ontology:cs_axiom_grounding('e7ff9f15-7419-49e6-b3a6-d08110eb04be', best_system_exceeds_facility_boundary, conventional).
narrative_ontology:cs_axiom('e7ff9f15-7419-49e6-b3a6-d08110eb04be', foundational, generation_shifting_is_statutorily_authorized).
narrative_ontology:cs_axiom_status(generation_shifting_is_statutorily_authorized, holdable).
narrative_ontology:cs_axiom_grounding('e7ff9f15-7419-49e6-b3a6-d08110eb04be', generation_shifting_is_statutorily_authorized, conventional).
narrative_ontology:cs_reference_frame('e7ff9f15-7419-49e6-b3a6-d08110eb04be', broad_statutory_decarbonization_mandate).
narrative_ontology:cs_drift_state('e7ff9f15-7419-49e6-b3a6-d08110eb04be', post_west_virginia_v_epa, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e7ff9f15-7419-49e6-b3a6-d08110eb04be', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, clean_energy_states).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_operators).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets 'best system of emission reduction' to authorize grid-wide decarbonization pathways including generation shifting and early coal retirement; sets emission guidelines, reviews state implementation plans, and imposes federal implementation plans on non-complying states.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, epa, agenda_setter,
    institutional, generational, constrained, national).

% Own and operate fossil-fuel generating assets targeted for early retirement by state plans; face stranded capital and lost revenue as dispatch priority shifts to renewables through regulatory mandate rather than market price.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_operators, payer,
    organized, biographical, constrained, regional).

% State governments with fossil-dependent economies and tax bases must submit SIPs that restructure in-state generation; compliance requires replacing a pillar of local employment and revenue with externally supplied renewable capacity.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_states, payer,
    institutional, generational, trapped, regional).

% Gain guaranteed market access and procurement mandates through state compliance pathways; the regulatory shift creates demand for wind, solar, and storage projects that displaces incumbent fossil generation.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_developers, beneficiary,
    organized, biographical, mobile, national).

% States with abundant renewable resources and low fossil dependence gain comparative advantage because their existing asset base satisfies compliance at lower cost; their resource endowments become systemically valuable under the federal framework.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, clean_energy_states, beneficiary,
    institutional, generational, mobile, regional).

% Adjudicates the statutory boundary of 'best system'; the Major Questions Doctrine has become the primary frame for evaluating whether Congress clearly authorized the systemic transformation reading.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_developers).
narrative_ontology:fixing_cost_class(caa_section_111d_delegation__systemic_transformation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of reducing greenhouse-gas emissions from the interconnected electricity grid where individual states and utilities face incentives to free-ride on decarbonization investments and leakage undermines unilateral action.
% TRANSFER_FUNCTION: Moves generation market share, compliance obligation, and investment capital from fossil-fuel generators and fossil-dependent states to renewable energy developers and low-carbon states via federally mandated state implementation plans that shift dispatch and retire assets.
% ABSENT_VOICES: Coal-reliant communities and workers are structurally underrepresented in regulatory design; their representation is mediated by fossil-locked state agencies that are themselves regulated parties. Consumer ratepayers bearing pass-through integration costs are rarely at the table.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, states would revert to facility-level or no emission controls, coal retirement timelines would collapse, renewable build-out would slow without the guaranteed demand signal, and interstate grid planning would lose the federal coordination mechanism â the power sector would reorganize around state-by-state fuel choices and existing asset economics.
% FOUNDING_PROBLEM: Uncontrolled greenhouse-gas emissions from existing power plants causing interstate and global climate harms that state-by-state regulation cannot adequately address due to leakage and free-riding.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists and public-health economists outside the renewable-energy beneficiary set attest to the ongoing harms from uncontrolled power-sector emissions. Fossil-locked states and some utility economists contest that the specific systemic mechanism is necessary, arguing facility-level heat-rate improvements suffice.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__systemic_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__systemic_transformation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__systemic_transformation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(caa_section_111d_delegation__systemic_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__systemic_transformation_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is substantial because the constraint forces early retirement of productive fossil assets and redirects market share through regulatory fiat rather than marginal cost competition. Suppression (0.58) reflects active federal enforcement (SIP/FIP machinery) tempered by judicial review and political resistance. Theater (0.25) is relatively low because the emission-reduction function is genuine, though some compliance planning is performative. Accessibility collapse (0.45) captures the limited but real alternatives: states can litigate or seek congressional amendment, but cannot simply opt out. Resistance (0.72) is high because fossil-locked states and coal operators have mounted sustained legal and political opposition. The measurement series tracks the Clean Power Plan era, repeal, West Virginia v. EPA, and attempted revival, using a single shared grid.
 *
 * PERSPECTIVAL GAP:
 *   The EPA and clean-energy states experience the constraint as necessary coordination to solve a collective-action emission problem that markets and state-by-state regulation fail to address. Coal operators and fossil-locked states experience the same structure as asymmetric extraction that destroys asset value and tax bases without compensating transfer. The engine computes this divergence from the structural data: the same legal text generates low directionality for beneficiaries (subsidized compliance pathways) and high directionality for trapped, identity-locked fossil jurisdictions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiariesârenewable_energy_developers and clean_energy_statesâsit near the full-beneficiary end: the constraint subsidizes their market entry and turns their resource endowments into compliance assets. Victimsâcoal_operators and fossil_locked_statesâsit near the full-target end: the constraint extracts through forced asset stranding and jurisdictional restructuring. The EPA is agenda_setter but not financial beneficiary; its directionality is structurally distinct from both. Federal_judiciary sits at analytical distance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâuncontrolled power-sector emissions causing interstate and global harmsâremains live in scientific assessment, so mandatrophy is not a simple case of outlived function. However, the facility-constraint reading argues that the systemic mechanism is disproportionate: the coordination function (decarbonization) could be achieved with less extraction (facility-level improvements). The mandatrophy question here is whether the systemic scale is necessary coordination or coordination-captureâwhether the 'best system' label is used to legitimate a broader resource reallocation than the emission problem demands. The temporal measurements show extraction spiking during the Clean Power Plan, collapsing under repeal, and reviving under subsequent rulemaking, suggesting the constraint's extraction level is politically contingent rather than technologically fixed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    facility_vs_systemic_empirical_delta,
    'Does grid-wide generation shifting achieve materially greater emission reductions than facility-level heat-rate improvements and carbon capture?',
    'Comparative modeling of sector-wide emissions under facility-only versus systemic regulatory scenarios, controlling for fuel prices and demand growth.',
    'If facility-level measures achieve comparable reductions, the systemic reading''s coordination benefit is smaller than claimed and the extraction from fossil-locked states is disproportionate, pushing classification toward snare. If systemic measures are uniquely necessary, the tangled_rope framing is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(facility_vs_systemic_empirical_delta, empirical, 'Whether the systemic scale is empirically necessary for the emission goal.').

omega_variable(
    west_virginia_foreclosure_scope,
    'Does West Virginia v. EPA foreclose the systemic reading as a matter of statutory interpretation, or merely require a clearer congressional authorization?',
    'Subsequent circuit and Supreme Court review of EPA rules that adopt systemic elements under revised statutory bases.',
    'If courts consistently strike systemic elements, the constraint''s suppression requirement drops and its effective extraction is dampened by legal exit. If courts allow systemic measures under clearer authorization, the constraint''s extraction remains structurally viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(west_virginia_foreclosure_scope, conceptual, 'Legal scope of the systemic reading after West Virginia v. EPA.').

omega_variable(
    state_identity_lock,
    'Are fossil-locked states constrained by pure economic infrastructure, or by political identity fusion that makes compliance costlier than necessary?',
    'Comparative analysis of similarly fossil-dependent states that have pursued diversification versus those that have doubled down on extraction; measure whether identity-based rhetoric predicts compliance cost premiums.',
    'If identity-locked, effective suppression is higher than structural measures suggestâstates carry the constraint internally even when federal pressure relaxes. This would raise the derived directionality for fossil-locked states and amplify effective extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_identity_lock, preference, 'Identity fusion versus structural trap in fossil-locked state resistance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__systemic_transformation_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t0, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(caa__tr_t2, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2, 0.25).
narrative_ontology:measurement(caa__tr_t4, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(caa__tr_t6, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 6, 0.5).
narrative_ontology:measurement(caa__tr_t8, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 8, 0.55).
narrative_ontology:measurement(caa__tr_t10, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(caa__be_t0, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(caa__be_t2, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(caa__be_t4, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 4, 0.6).
narrative_ontology:measurement(caa__be_t6, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 6, 0.45).
narrative_ontology:measurement(caa__be_t8, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(caa__be_t10, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 10, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t0, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(caa__su_t2, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2, 0.6).
narrative_ontology:measurement(caa__su_t4, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 4, 0.45).
narrative_ontology:measurement(caa__su_t6, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 6, 0.25).
narrative_ontology:measurement(caa__su_t8, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 8, 0.2).
narrative_ontology:measurement(caa__su_t10, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__systemic_transformation_reading, resource_allocation).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation__facility_constraint_reading).

% DUAL FORMULATION NOTE:
% This constraint is the systemic-transformation reading of the CAA Section 111(d) 'best system' kernel. The facility_constraint_reading instantiates the same statutory text under a narrower facility-boundary interpretation. They are sibling readings in a constraint family linked by shared statutory source.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
