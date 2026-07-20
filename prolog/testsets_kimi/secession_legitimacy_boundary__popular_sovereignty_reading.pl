% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__popular_sovereignty_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__popular_sovereignty_reading
 *   human_readable: Popular Sovereignty Reading of Secession Legitimacy Boundary
 *   domain: political/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint instantiates the popular sovereignty reading of the
 *   secession legitimacy boundary kernel: the claim that a democratic
 *   majority within a province holds ultimate sovereignty and that a
 *   referendum result is self-legitimating, requiring no federal approval. It
 *   is a tangled rope because it coordinates the provincial population around
 *   a clear decision rule for self-determination while simultaneously
 *   extracting compliance from federal authority, internal minorities, and
 *   Indigenous treaty holders who do not consent. The constraint is actively
 *   enforced because federal states and excluded minorities typically resist
 *   unilateral secession, forcing the provincial majority to defend the
 *   legitimacy claim politically and sometimes coercively.
 *
 * KEY AGENTS:
 *   - Provincial secessionist majority (organized/mobile): primary beneficiary â receives self-determination mandate
 *   - Secessionist political leadership (powerful/mobile): agenda setter and secondary beneficiary â captures state apparatus
 *   - Federal authority (institutional/constrained): primary payer â loses territory and resource jurisdiction
 *   - Provincial minorities (moderate/constrained): payer â involuntarily transferred to new state
 *   - Indigenous treaty holders (organized/constrained): excluded â treaty rights overridden by majority will
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, 0.68).
domain_priors:suppression_score(secession_legitimacy_boundary__popular_sovereignty_reading, 0.6).
domain_priors:theater_ratio(secession_legitimacy_boundary__popular_sovereignty_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__popular_sovereignty_reading, "Popular Sovereignty Reading of Secession Legitimacy Boundary").
narrative_ontology:topic_domain(secession_legitimacy_boundary__popular_sovereignty_reading, "political/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__popular_sovereignty_reading, '31d7ff08-53ad-4794-b07e-b48da2f97333').
narrative_ontology:cs_kernel_codification('31d7ff08-53ad-4794-b07e-b48da2f97333', distributed).
narrative_ontology:cs_authority_grounding('31d7ff08-53ad-4794-b07e-b48da2f97333', distributed).
narrative_ontology:cs_reading_relation('31d7ff08-53ad-4794-b07e-b48da2f97333', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('31d7ff08-53ad-4794-b07e-b48da2f97333', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('31d7ff08-53ad-4794-b07e-b48da2f97333', secession_legitimacy_boundary__treaty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('31d7ff08-53ad-4794-b07e-b48da2f97333', foundational, provincial_majority_sovereignty_supreme).
narrative_ontology:cs_axiom_status(provincial_majority_sovereignty_supreme, holdable).
narrative_ontology:cs_axiom_grounding('31d7ff08-53ad-4794-b07e-b48da2f97333', provincial_majority_sovereignty_supreme, deontological).
narrative_ontology:cs_axiom('31d7ff08-53ad-4794-b07e-b48da2f97333', foundational, referendum_self_legitimating).
narrative_ontology:cs_axiom_status(referendum_self_legitimating, holdable).
narrative_ontology:cs_axiom_grounding('31d7ff08-53ad-4794-b07e-b48da2f97333', referendum_self_legitimating, conventional).
narrative_ontology:cs_reference_frame('31d7ff08-53ad-4794-b07e-b48da2f97333', popular_sovereignty_supreme).
narrative_ontology:cs_drift_state('31d7ff08-53ad-4794-b07e-b48da2f97333', contemporary_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('31d7ff08-53ad-4794-b07e-b48da2f97333', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_secessionist_majority).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, secessionist_political_leadership).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, federal_authority).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_minorities).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constitutes the democratic majority within provincial boundaries and provides the mandate for secession through referendum. Receives self-determination and policy control over territory and resources, but the benefit is diffuse across the population rather than concentrated in any one seat.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_secessionist_majority, beneficiary,
    organized, generational, mobile, regional).

% Organizes the referendum campaign and institutionalizes the popular sovereignty principle. Sets the agenda for the secession process and stands to capture the apparatus of the new state, including resource revenues and territorial authority.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, secessionist_political_leadership, agenda_setter,
    powerful, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__popular_sovereignty_reading, secessionist_political_leadership, beneficiary).

% Holds constitutional and territorial authority over the province. Bears the loss of tax base, resource jurisdiction, and territorial integrity if secession succeeds. Must either contest the referendum's legitimacy or negotiate under duress, both costly outcomes.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_authority, payer,
    institutional, civilizational, constrained, national).

% Resides within the provincial boundaries and opposes secession or belongs to groups outvoted by the majority. Bears the cost of involuntary transfer into a new political entity, including potential citizenship change, economic dislocation, and cultural marginalization.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_minorities, payer,
    moderate, biographical, constrained, regional).

% Holds treaty rights with the federal crown that predate provincial boundaries. Under the popular sovereignty reading, their consent is not required for secession, structurally excluding them from the legitimacy calculus and exposing treaty rights to unilateral alteration by the new provincial state.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_holders, excluded,
    organized, civilizational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__popular_sovereignty_reading, secessionist_political_leadership).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__popular_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective action problem of political self-determination by providing a democratic decision procedure that translates a population's desire for independence into a binding, recognizable mandate without requiring civil war or informal rebellion.
% TRANSFER_FUNCTION: Moves territorial authority, resource control, and institutional legitimacy from the federal state and internal minorities to the provincial majority and its political leadership, bypassing federal constitutional amendment processes.
% ABSENT_VOICES: Indigenous treaty holders, who hold pre-constitutional rights that this reading overrides; federal constitutionalists who assert the indissoluble unity of the federation; and persistent provincial minorities whose opposition is overridden by majority vote and who are not given a separate veto or exit pathway.
% DISAPPEARANCE_RATIONALE: Without the self-legitimating referendum principle, provincial independence movements would lack a recognized unilateral pathway; secession would require federal negotiation or constitutional reform, fundamentally rearranging the resource and authority flows between federal and provincial governments.
% FOUNDING_PROBLEM: How can a sub-state population achieve political independence when the federal constitution provides no exit mechanism and the central state refuses to negotiate?
% FOUNDING_PROBLEM_CORROBORATION: Independence movements and constitutional scholars outside the beneficiary camp attest that federal systems often lack secession clauses, creating a genuine democratic deficit. Federal courts and international lawyers outside the victim camp attest that constitutional integrity and territorial integrity norms provide alternative resolution frameworks; the Supreme Court of Canada's Quebec Secession Reference is a corroborating source from outside the beneficiary set.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__popular_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the majority unilaterally claims authority over territory and resources held by the federal state and minority groups, bypassing constitutional processes. Suppression is substantial (0.60) because the claim must be actively defended against federal legal and political resistance, and against minority non-compliance. Theater ratio (0.40) reflects that while the referendum expresses genuine democratic coordination, an increasing share of the discourse is performative legitimacy-seeking that obscures the asymmetric imposition on non-consenting parties. Accessibility collapse (0.55) is moderate: alternatives such as federal legal recourse or internal partition remain theoretically available but are politically blocked by the majority's legitimacy claim. Resistance (0.70) is high because federal authority, minorities, and Indigenous nations actively contest the reading.
 *
 * PERSPECTIVAL GAP:
 *   The provincial majority and its leadership experience the constraint as coordination â a legitimate democratic expression of self-determination. The federal state and internal minorities experience it as extraction â a unilateral seizure of authority and resources. The engine computes this divergence from the structural asymmetry in beneficiary/victim declarations and exit options: beneficiaries are mobile and organized, while victims are constrained or excluded.
 *
 * DIRECTIONALITY LOGIC:
 *   The provincial secessionist majority and secessionist political leadership are structural beneficiaries (low directionality): the constraint subsidizes their claim to sovereignty and resource control. The federal authority, provincial minorities, and Indigenous treaty holders are structural targets (high directionality): the constraint extracts territorial jurisdiction from the federal state, political status from minorities, and treaty rights from Indigenous nations. The leadership is an agenda setter with mobile exit options, amplifying its beneficiary position; Indigenous treaty holders are excluded with constrained exit, placing them nearest the full-target end.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as pure coordination (rope) by requiring named victims and active enforcement, which captures the coercion necessary to override federal and minority opposition. It also prevents mislabeling as pure extraction (snare) by acknowledging the genuine coordination function of a democratic referendum in solving the collective action problem of self-determination. The tangled rope classification is warranted only because both features are present simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the secession legitimacy boundary prevails when federal and provincial authorities collide: popular sovereignty, constitutional supremacy, grievance threshold, or treaty primacy?',
    'Comparative constitutional analysis and case studies of secession crises to determine which reading prevails in negotiated outcomes.',
    'If constitutional or treaty readings prevail, this constraint''s effective extraction collapses because its legitimacy claim is rejected by the authoritative interpreter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural location of disagreement within the secession legitimacy kernel').

omega_variable(
    treaty_succession_ambiguity,
    'Does unilateral provincial secession under popular sovereignty automatically transfer treaty obligations from the federal crown to the new provincial state without Indigenous treaty holder consent?',
    'Analysis of treaty succession in comparable secession contexts and rulings from Indigenous land tribunals.',
    'If treaties are extinguished, extraction from Indigenous treaty holders is severe and the reading leans toward snare; if treaties survive, the coordination function is more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_succession_ambiguity, empirical, 'Whether Indigenous treaty rights survive unilateral provincial secession').

omega_variable(
    minority_exit_structural,
    'Are provincial minorities who oppose secession structurally trapped in the new state, or do they retain viable exit through relocation, partition, or federal protection?',
    'Post-referendum mobility data and legal frameworks for minority protection and citizenship choice in comparable cases.',
    'If exit is structurally blocked, effective extraction is higher than the base measure suggests; if exit is available, the cost to minorities is mitigated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_exit_structural, empirical, 'Structural exit options for provincial anti-secession minorities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__popular_sovereignty_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sece_tr_t6, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(sece_tr_t12, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(sece_tr_t18, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 18, 0.35).
narrative_ontology:measurement(sece_tr_t24, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(sece_tr_t30, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sece_be_t6, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(sece_be_t12, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(sece_be_t18, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 18, 0.6).
narrative_ontology:measurement(sece_be_t24, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(sece_be_t30, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sece_su_t6, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(sece_su_t12, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(sece_su_t18, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement(sece_su_t24, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(sece_su_t30, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__popular_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the secession_legitimacy_boundary kernel. It is decomposed from the natural-language concept of secession legitimacy into four structurally distinct constraints because the source of legitimacy (popular sovereignty vs. constitutional text vs. injustice threshold vs. treaty rights) produces different epsilon values, beneficiary sets, and failure modes. This file isolates the popular sovereignty reading; siblings handle constitutional impossibility, grievance threshold, and treaty primacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
