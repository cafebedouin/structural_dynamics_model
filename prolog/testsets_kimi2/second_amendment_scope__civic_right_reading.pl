% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__civic_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__civic_right_reading, []).

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
 *   constraint_id: second_amendment_scope__civic_right_reading
 *   human_readable: Second Amendment Civic Right Reading (Militia-Conditioned Individual Right)
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint story instantiates the civic_right_reading of the
 *   contested kernel second_amendment_scope. Under this reading, the Second
 *   Amendment protects an individual right to keep and bear arms, but only
 *   for those participating in or eligible for government-regulated militia
 *   service. The kernel is the fixed constitutional text; this reading
 *   imposes a service-based gate on the right, creating a structural
 *   distinction between militia-eligible beneficiaries and the disarmed
 *   civilian population. The reading sits in direct tension with the
 *   individual_right_reading (unconnected individual right) and the
 *   collective_right_reading (state authority only). The claim/metric
 *   independence is observed: the reading is claimed as a coordination
 *   mechanism for republican self-defense (tangled_rope), while the metrics
 *   acknowledge moderate extraction through state gatekeeping.
 *
 * KEY AGENTS:
 *   - militia_eligible_individuals: Primary beneficiary (moderate/constrained) â receive the right conditioned on service
 *   - disarmed_civilian_population: Primary payer (powerless/trapped) â excluded from the right by militia gate
 *   - federal_and_state_governments: Agenda setter (institutional/analytical) â defines eligibility and enforces condition
 *   - constitutional_scholars_civic_tradition: Analytical observer â maintains the interpretive framework
 *   - individual_rights_advocates: Excluded voice â opposes the militia nexus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, 0.45).
domain_priors:suppression_score(second_amendment_scope__civic_right_reading, 0.5).
domain_priors:theater_ratio(second_amendment_scope__civic_right_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__civic_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__civic_right_reading, "Second Amendment Civic Right Reading (Militia-Conditioned Individual Right)").
narrative_ontology:topic_domain(second_amendment_scope__civic_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__civic_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__civic_right_reading, '1f81c7f6-2376-48cb-a54b-5cdcd62887e9').
narrative_ontology:cs_kernel_codification('1f81c7f6-2376-48cb-a54b-5cdcd62887e9', fixed_text).
narrative_ontology:cs_authority_grounding('1f81c7f6-2376-48cb-a54b-5cdcd62887e9', lineage).
narrative_ontology:cs_interpretation_layer_present('1f81c7f6-2376-48cb-a54b-5cdcd62887e9').
narrative_ontology:cs_reading_relation('1f81c7f6-2376-48cb-a54b-5cdcd62887e9', second_amendment_scope__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('1f81c7f6-2376-48cb-a54b-5cdcd62887e9', second_amendment_scope__collective_right_reading, forecloses).
narrative_ontology:cs_axiom('1f81c7f6-2376-48cb-a54b-5cdcd62887e9', foundational, arms_bearing_tied_to_militia_service).
narrative_ontology:cs_axiom_status(arms_bearing_tied_to_militia_service, holdable).
narrative_ontology:cs_axiom_grounding('1f81c7f6-2376-48cb-a54b-5cdcd62887e9', arms_bearing_tied_to_militia_service, conventional).
narrative_ontology:cs_axiom('1f81c7f6-2376-48cb-a54b-5cdcd62887e9', secondary, militia_eligibility_as_constitutional_gate).
narrative_ontology:cs_axiom_status(militia_eligibility_as_constitutional_gate, holdable).
narrative_ontology:cs_axiom_grounding('1f81c7f6-2376-48cb-a54b-5cdcd62887e9', militia_eligibility_as_constitutional_gate, conventional).
narrative_ontology:cs_reference_frame('1f81c7f6-2376-48cb-a54b-5cdcd62887e9', civic_republican_militia_framework).
narrative_ontology:cs_drift_state('1f81c7f6-2376-48cb-a54b-5cdcd62887e9', post_heller_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('1f81c7f6-2376-48cb-a54b-5cdcd62887e9', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__civic_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, militia_eligible_individuals).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, disarmed_civilian_population).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, civic_republicanism).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, well_regulated_militia_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess and carry firearms by virtue of meeting government-defined militia eligibility criteria and accepting civic service obligations. Their constitutional protection is contingent on continued enrollment or readiness; withdrawal or disqualification removes the legal shield for their arms-bearing.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, militia_eligible_individuals, beneficiary,
    moderate, biographical, constrained, national).

% Civilians who fail to meet militia eligibility standards or decline service are denied the constitutional right to possess arms. They bear the costs of disarmament and dependency on state or professional security, with no legal pathway to acquire the right outside the militia gate.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, disarmed_civilian_population, payer,
    powerless, biographical, trapped, national).

% Define militia eligibility, regulate enrollment, and enforce the statutory framework that ties firearms possession to militia status. They administer the gatekeeping mechanism and adjust the boundaries of who qualifies as part of the well-regulated militia.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, federal_and_state_governments, agenda_setter,
    institutional, generational, analytical, national).

% Analyze and advocate for the civic-right reading based on founding-era republican political thought. They produce the interpretive tradition that connects the Second Amendment text to a duty-based, service-conditioned individual right.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, constitutional_scholars_civic_tradition, observer,
    analytical, civilizational, analytical, national).

% Argue for an unconditional individual right to firearms unconnected to militia service. Under the civic-right regime, their preferred interpretation is structurally ruled out and they are excluded from the constitutional bargain that distributes the right.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, individual_rights_advocates, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__civic_right_reading, federal_and_state_governments).
narrative_ontology:fixing_cost_class(second_amendment_scope__civic_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes armed civic defense by conditioning firearms possession on participation in a government-regulated militia, solving the collective-action problem of republican self-defense without relying on a permanent standing army.
% TRANSFER_FUNCTION: Moves the legal entitlement to bear arms from the general population to a state-circumscribed class of militia-eligible citizens, under regulatory authority that defines eligibility and service conditions.
% ABSENT_VOICES: Individual-rights advocates and non-eligible civilians who would claim an unconditional right to self-defense are excluded from the constitutional distribution; their objections are ruled out by the militia-condition framework.
% DISAPPEARANCE_RATIONALE: If the militia-condition vanished, the statutory architecture distinguishing eligible and ineligible possessors would collapse. Millions of currently disarmed civilians would gain access to arms, the state's gatekeeping monopoly over the militia would dissolve, and the civic-republican coordination mechanism would be replaced by either unregulated individual possession or a different regulatory scheme.
% FOUNDING_PROBLEM: The founding generation's concern that a professional standing army threatens republican liberty, and the need for a disciplined, armed citizenry available for collective defense without centralized military bureaucracy.
% FOUNDING_PROBLEM_CORROBORATION: Civic republican historians and constitutional originalists attest to the founding problem from outside the direct beneficiary set; modern military historians and individual-rights jurists contest that militia service remains a live or sufficient justification, noting the rise of the National Guard and professional military.
narrative_ontology:disappearance_verdict(second_amendment_scope__civic_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__civic_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__civic_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_scope__civic_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__civic_right_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__civic_right_reading_tests).
:- end_tests(second_amendment_scope__civic_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45 at present) because the militia condition genuinely coordinates civic defense but simultaneously empowers the state to define eligibility, creating a gate that excludes large portions of the population. Suppression (0.50) reflects the active enforcement of militia standards and prohibition on non-militia possession. Theater ratio (0.45) captures the decay of genuine militia coordination into rhetorical justification for regulatory control as militias became obsolete. Resistance (0.60) is substantial because the excluded population and individual-rights coalitions actively contest the reading. Accessibility collapse (0.50) indicates that while black-market alternatives exist, legal pathways to arms possession outside the militia framework are closed.
 *
 * PERSPECTIVAL GAP:
 *   The militia-eligible beneficiary seat experiences the constraint as a protected liberty with civic duties; the disarmed payer seat experiences it as a deprivation justified by an obsolete military framework. The government agenda-setter experiences it as a legitimate regulatory tool; the individual-rights advocate experiences it as a nullification of the constitutional guarantee. The engine computes these divergences from the structural data without reconciling them.
 *
 * DIRECTIONALITY LOGIC:
 *   Militia-eligible individuals are structural beneficiaries (low d): the constraint subsidizes their arms-bearing by constitutionalizing it. The disarmed civilian population are structural targets (high d): the constraint extracts their liberty and channels it into state-defined militia channels. The government sits near symmetric but with enforcement power; individual-rights advocates are excluded observers with high resistance.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy mislabeling because it carries both a live coordination function (republican self-defense) and identifiable asymmetric extraction (state-defined eligibility excludes non-participants). If the militia coordination function were dead and only extraction remained, it would drift toward snare or piton; the temporal measurements show theater_ratio rising as coordination decayed, signaling this risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    civic_right_kernel_position,
    'As the civic_right_reading of kernel second_amendment_scope, does the militia-service condition represent a genuine constitutional coordination mechanism for republican defense, or a constructed gate that enables selective disarmament by the state?',
    'Compare historical militia participation rates, regulatory impact, and the distribution of firearms rights under this reading against the individual_right_reading in jurisdictions with divergent doctrinal regimes.',
    'If the condition is a constructed gate, the constraint is a tangled rope with high extraction; if genuine coordination, it trends toward rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_right_kernel_position, conceptual, 'Kernel reading ambiguity for civic right').

omega_variable(
    sibling_individual_right_foreclosure,
    'Does the civic_right_reading''s core premiseâthat the right is conditioned on militia participationâlogically foreclose the individual_right_reading''s claim of an unconnected individual right within a single constitutional framework?',
    'Jurisprudential analysis of whether a single adjudicative authority can consistently hold both that the right requires militia service and that it does not.',
    'If foreclosed, the kernel generates zero-sum judicial politics; if the readings can coexist, the kernel permits doctrinal pluralism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_individual_right_foreclosure, conceptual, 'Structural relationship to individual right sibling').

omega_variable(
    militia_gatekeeping_asymmetry,
    'Does government control of militia eligibility criteria function as neutral administrative classification or as a historically selective mechanism of social control?',
    'Historical analysis of eligibility standards across jurisdictions for patterns of racial, class, or political exclusion masked as military fitness.',
    'If eligibility was systematically manipulated, the coordination story is cover for extraction, and the constraint migrates toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_gatekeeping_asymmetry, empirical, 'Whether militia eligibility is neutrally administered').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__civic_right_reading, 0, 233).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__civic_right_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(seco_tr_t50, second_amendment_scope__civic_right_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement(seco_tr_t100, second_amendment_scope__civic_right_reading, theater_ratio, 100, 0.25).
narrative_ontology:measurement(seco_tr_t150, second_amendment_scope__civic_right_reading, theater_ratio, 150, 0.4).
narrative_ontology:measurement(seco_tr_t200, second_amendment_scope__civic_right_reading, theater_ratio, 200, 0.5).
narrative_ontology:measurement(seco_tr_t233, second_amendment_scope__civic_right_reading, theater_ratio, 233, 0.45).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__civic_right_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(seco_be_t50, second_amendment_scope__civic_right_reading, base_extractiveness, 50, 0.2).
narrative_ontology:measurement(seco_be_t100, second_amendment_scope__civic_right_reading, base_extractiveness, 100, 0.35).
narrative_ontology:measurement(seco_be_t150, second_amendment_scope__civic_right_reading, base_extractiveness, 150, 0.5).
narrative_ontology:measurement(seco_be_t200, second_amendment_scope__civic_right_reading, base_extractiveness, 200, 0.55).
narrative_ontology:measurement(seco_be_t233, second_amendment_scope__civic_right_reading, base_extractiveness, 233, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__civic_right_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(seco_su_t50, second_amendment_scope__civic_right_reading, suppression_requirement, 50, 0.3).
narrative_ontology:measurement(seco_su_t100, second_amendment_scope__civic_right_reading, suppression_requirement, 100, 0.45).
narrative_ontology:measurement(seco_su_t150, second_amendment_scope__civic_right_reading, suppression_requirement, 150, 0.55).
narrative_ontology:measurement(seco_su_t200, second_amendment_scope__civic_right_reading, suppression_requirement, 200, 0.65).
narrative_ontology:measurement(seco_su_t233, second_amendment_scope__civic_right_reading, suppression_requirement, 233, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__collective_right_reading).

% DUAL FORMULATION NOTE:
% This story is one member of the second_amendment_scope constraint family. It decomposes the contested kernel into three structurally distinct constraints: civic_right_reading (individual right conditioned on militia service), individual_right_reading (unconnected individual right), and collective_right_reading (state authority over militias). Each has a distinct epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
