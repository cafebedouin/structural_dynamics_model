% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__boundary_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__boundary_maintenance_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__boundary_maintenance_reading
 *   human_readable: Catastrophe-Memory Mourning Ritual as Group Boundary Enforcement
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   A historically persecuted community maintains a shared mourning-practice
 *   commemorating a collective catastrophe. This story isolates ONE
 *   structural reading of that practice among four sibling readings sharing a
 *   common catastrophe-memory kernel: here, the ritual functions primarily as
 *   a boundary-maintenance mechanism — its liturgical calendar and
 *   participation standards operate to sort in-group from out-group,
 *   sincere-enough from insufficiently observant, and to allocate social
 *   capital accordingly. As diaspora conditions stabilize and acute
 *   persecution recedes in many locations while communal enforcement of
 *   observance standards has, if anything, become more codified, the
 *   boundary-enforcement function increasingly outruns its founding threat,
 *   producing rising extraction on non-conforming and mixed-affiliation
 *   members even as the coordination good (identity transmission, mutual aid
 *   network) remains genuinely valuable to core members.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, 0.52).
domain_priors:suppression_score(catastrophe_memory_kernel__boundary_maintenance_reading, 0.58).
domain_priors:theater_ratio(catastrophe_memory_kernel__boundary_maintenance_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__boundary_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__boundary_maintenance_reading, "Catastrophe-Memory Mourning Ritual as Group Boundary Enforcement").
narrative_ontology:topic_domain(catastrophe_memory_kernel__boundary_maintenance_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__boundary_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__boundary_maintenance_reading, 'd30f39d4-fe53-411a-9c66-d0dcc186391c').
narrative_ontology:cs_kernel_codification('d30f39d4-fe53-411a-9c66-d0dcc186391c', distributed).
narrative_ontology:cs_authority_grounding('d30f39d4-fe53-411a-9c66-d0dcc186391c', practice).
narrative_ontology:cs_interpretation_layer_present('d30f39d4-fe53-411a-9c66-d0dcc186391c').
narrative_ontology:cs_reading_relation('d30f39d4-fe53-411a-9c66-d0dcc186391c', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('d30f39d4-fe53-411a-9c66-d0dcc186391c', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('d30f39d4-fe53-411a-9c66-d0dcc186391c', catastrophe_memory_kernel__trauma_encoding_reading, influences).
narrative_ontology:cs_axiom('d30f39d4-fe53-411a-9c66-d0dcc186391c', foundational, group_boundary_maintenance_is_the_ritual_core_function).
narrative_ontology:cs_axiom_status(group_boundary_maintenance_is_the_ritual_core_function, holdable).
narrative_ontology:cs_axiom_grounding('d30f39d4-fe53-411a-9c66-d0dcc186391c', group_boundary_maintenance_is_the_ritual_core_function, conventional).
narrative_ontology:cs_axiom('d30f39d4-fe53-411a-9c66-d0dcc186391c', secondary, conformity_cost_to_deviant_members_is_legitimate_price_of_cohesion).
narrative_ontology:cs_axiom_status(conformity_cost_to_deviant_members_is_legitimate_price_of_cohesion, holdable).
narrative_ontology:cs_axiom_grounding('d30f39d4-fe53-411a-9c66-d0dcc186391c', conformity_cost_to_deviant_members_is_legitimate_price_of_cohesion, instrumental).
narrative_ontology:cs_reference_frame('d30f39d4-fe53-411a-9c66-d0dcc186391c', persecution_era_survival_solidarity).
narrative_ontology:cs_drift_state('d30f39d4-fe53-411a-9c66-d0dcc186391c', contemporary_diaspora_stabilization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d30f39d4-fe53-411a-9c66-d0dcc186391c', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_cohesion_stakeholders).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, communal_leadership).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, boundary_deviant_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, intermarried_and_mixed_families).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_neighbors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the calendar, liturgy, and required participation standards for the shared mourning-practice; determines who counts as properly observant and who is treated as marginal. Draws communal authority and continued institutional relevance from being the recognized custodian of the memory-ritual.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, communal_leadership, agenda_setter,
    institutional, generational, arbitrage, national).

% Core observant members whose social capital, marriage prospects, and mutual-aid access depend on visible participation in the shared mourning-practice. The ritual supplies them with a durable in-group network and clear membership signal; exit would mean losing that network.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_cohesion_stakeholders, beneficiary,
    organized, generational, constrained, national).

% Individuals who question the practice, participate partially, or want to mourn in personally meaningful but non-conforming ways. They are subject to informal sanction — exclusion from communal events, gossip, reduced marriage or business prospects — for insufficient conformity. Their identity is bound up in the same community that punishes their deviation, making exit costly in ways beyond the ritual itself.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, boundary_deviant_members, payer,
    moderate, biographical, identity_locked, local).

% Families with out-group members face the ritual's boundary-drawing function directly: the shared mourning-practice codes who fully belongs, and mixed households are frequently treated as ambiguous or excluded participants regardless of personal grief or sincerity. They cannot renegotiate the ritual's terms individually.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, intermarried_and_mixed_families, payer,
    powerless, biographical, trapped, local).

% Neighbors and historical adjacent communities who are structurally positioned as the ritual's implicit counter-example — the group the catastrophe was done by or the group from which the in-group must remain distinct. They have no voice in how the ritual constructs them and bear the social cost of continued suspicion or distancing encoded into commemorative practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_neighbors, excluded,
    powerless, biographical, trapped, local).

% Study the ritual's boundary-drawing function comparatively, documenting how mourning-practice standardization tracks in-group/out-group demarcation across historically persecuted communities. Their analysis can either support communal legitimacy claims or expose the boundary-enforcement function, depending on audience.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__boundary_maintenance_reading, communal_leadership).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__boundary_maintenance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The shared mourning-practice provides a genuine coordination good: a common calendar and liturgy that lets a dispersed or historically threatened community recognize, locate, and mutually support its members, and transmit a coherent identity across generations without a central enforcing state.
% TRANSFER_FUNCTION: The ritual moves social capital, marriage eligibility, mutual-aid access, and communal standing toward conforming, visibly observant members, and moves the cost of exclusion, informal sanction, and ambiguous belonging onto non-conforming members, mixed families, and constructed out-groups.
% ABSENT_VOICES: Boundary-deviant members and mixed families are present but structurally outvoted in defining what counts as adequate observance; out-group neighbors, who are effectively encoded as the ritual's necessary contrast class, have no standing at all in communal deliberation over how the practice defines them.
% DISAPPEARANCE_RATIONALE: Communal leadership and core observant members would say the world rearranges catastrophically — identity transmission collapses, intermarriage and assimilation accelerate, historical memory of the catastrophe fades. Boundary-deviant members and mixed families would say their exclusion ends and the world becomes more livable for them specifically, while the broader community's cohesion mechanism migrates to some substitute practice. The verdict genuinely differs by seat.
% FOUNDING_PROBLEM: A historically persecuted or catastrophically diminished community needed a durable, low-infrastructure mechanism to keep dispersed survivors and their descendants recognizably bound together against pressure toward assimilation and forgetting.
% FOUNDING_PROBLEM_CORROBORATION: Communal leadership and core members attest the assimilation threat remains fully live and the ritual is still necessary for survival. Comparative religion scholars and some boundary-deviant members attest, from outside the direct beneficiary group, that the acute persecution conditions that originally motivated the practice have substantially eased in many diaspora contexts, and that the boundary-enforcement function has increasingly outrun the founding threat — though scholars also note the threat has not vanished everywhere, so the corroboration is genuinely mixed rather than one-sided.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__boundary_maintenance_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__boundary_maintenance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__boundary_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as moderate (0.52) rather than high: the ritual genuinely does coordinate a real mutual-aid and identity-transmission network for core members, and boundary costs, while real, are informal sanction and exclusion rather than violent coercion. Suppression (0.58) reflects that non-conforming members and mixed families face real informal social force (gossip, exclusion, marriage-market penalties) to conform, layered onto identity-lock rather than purely external barriers. Theater ratio is moderate-low and rising (0.12 to 0.28) as some communities increasingly perform observance-standard enforcement for internal political purposes beyond its coordination function, while the underlying founding-threat justification weakens. All three tracked metrics share one time grid across the full interval.
 *
 * PERSPECTIVAL GAP:
 *   From communal leadership's seat, the practice looks like pure coordination — a necessary mechanism for group survival and continuity. From a boundary-deviant member's seat, the identical practice looks like an extraction structure using shared grief as the enforcement lever for conformity. The engine's per-seat computation should reflect this divergence directly from the declared power/exit/beneficiary structure, not from any narrative reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Communal leadership and core in-group members sit near the beneficiary end: they receive social capital, network access, and continued institutional legitimacy through the ritual's operation. Boundary-deviant members and mixed families sit near the target end: the same structure that provides coordination for others imposes conformity costs and exclusion risk on them, and their exit options are constrained by identity-lock (deviant members) or outright trappedness (mixed families with no unilateral renegotiation power). Out-group neighbors are excluded from the constraint's internal accounting entirely — they are the structural contrast class the boundary work requires, bearing costs without any voice in how the ritual constructs them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — communal survival against assimilation pressure amid active persecution — was genuinely live at founding and remains partially live in some contexts today, which is why founding_problem_status is authored as contested rather than flatly dead. The boundary-maintenance function, however, appears to have outgrown even a diminished founding threat in many diaspora contexts, which is exactly the mismatch (status=contested, verdict=contested) the mandatrophy consumer is built to flag rather than resolve outright.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_function_vs_coordination_function_weight,
    'Is the boundary-enforcement effect of the mourning-practice the ritual''s primary structural function, or an incidental byproduct of a genuinely coordination-first practice?',
    'Comparative ethnographic study of communities where the practice is retained but formal boundary-sanctions (marriage restriction, informal shunning) have been relaxed — if cohesion and transmission persist without the sanction apparatus, boundary-enforcement is severable from coordination.',
    'If severable, the extractive component identified here is a removable overlay on a rope-like coordination core; if inseverable, boundary enforcement is intrinsic to how the ritual achieves cohesion at all, supporting the tangled_rope claim more strongly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_function_vs_coordination_function_weight, conceptual, 'Whether boundary-enforcement is severable from the ritual''s coordination function.').

omega_variable(
    reading_selection_under_determination,
    'Given that the same ritual practice supports at least four structurally distinct readings (boundary-maintenance, symbol-continuity, survival-competence, trauma-encoding) with different beneficiary/victim structures, what determines which reading is analytically or morally primary for a given community at a given time?',
    'No single resolution mechanism exists; different observers (community insiders, comparative religionists, mixed-family members) will weight the readings differently based on their structural position. This is documented as an omega rather than resolved because the readings coexist as live framings rather than converging on one true description.',
    'The choice of reading materially changes classification (this reading computes as tangled_rope; sibling readings may compute as rope or scaffold) even though the underlying practice is unchanged — which is precisely why the ε-invariance principle requires separate stories rather than one story with a selectable observable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_under_determination, conceptual, 'Under-determination among four structurally distinct readings of one ritual practice.').

omega_variable(
    assimilation_threat_current_severity,
    'In contemporary diaspora contexts, how severe is the actual assimilation/identity-loss threat the boundary-maintenance function claims to guard against?',
    'Demographic and sociological studies of intermarriage rates, religious retention rates, and community persistence with versus without strict boundary enforcement, across multiple diaspora locations and time periods.',
    'If the threat is substantially diminished in most contexts, the boundary-enforcement extraction is running well ahead of any live coordination need, supporting a mandatrophy reading; if the threat remains severe in specific contexts, the extraction may track a genuinely live founding problem there.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assimilation_threat_current_severity, empirical, 'Empirical severity of the assimilation threat the boundary function is justified against.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__boundary_maintenance_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cata_tr_t8, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cata_be_t8, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 24, 0.49).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 32, 0.51).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cata_su_t8, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(cata_su_t16, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(cata_su_t24, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 24, 0.51).
narrative_ontology:measurement(cata_su_t32, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__boundary_maintenance_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__boundary_maintenance_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the catastrophe_memory_kernel, each instantiating a structurally distinct constraint over the same underlying mourning-practice. boundary_maintenance_reading isolates the in-group/out-group sorting function (victims: individual autonomy, out-group relations). symbol_continuity_reading isolates identity-preservation across generational time. survival_competence_reading isolates transmission of adaptive persecution-survival capacity. trauma_encoding_reading isolates the intergenerational-warning-system function. Each carries its own ε, beneficiary/victim set, and classification; they are linked here via affects_constraints rather than merged into one multi-valued story, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
