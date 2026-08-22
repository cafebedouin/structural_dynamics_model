% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__messianic_deferral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__messianic_deferral, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: kodashim_commandment_status__messianic_deferral
 *   human_readable: Kodashim as Suspended-but-Live Commandment Pending Messianic Restoration
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   This story instantiates the messianic-deferral reading of the Kodashim
 *   (sacrificial law) commitment kernel: the commandment to bring offerings
 *   is treated as temporally suspended by the absence of the Temple and
 *   altar, but not obsolete — it remains binding in a dormant state, and its
 *   detailed study is justified as maintaining readiness for a future
 *   restoration rather than as fulfillment in itself. This is structurally
 *   distinct from the study-as-performance reading (where study IS the
 *   present fulfillment, collapsing the future-contingency structure
 *   entirely) and from the performance-only reading (where the commandment is
 *   simply inert until physically restorable, with no obligation-bearing
 *   status in the interim). Only the messianic-deferral reading generates the
 *   specific extraction pattern analyzed here: scholarly and communal
 *   resources committed to readiness-for-a-contingency, subordinating
 *   present-generation needs to a future that may never arrive.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, 0.42).
domain_priors:suppression_score(kodashim_commandment_status__messianic_deferral, 0.38).
domain_priors:theater_ratio(kodashim_commandment_status__messianic_deferral, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, extractiveness, 0.42).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__messianic_deferral, scaffold).
narrative_ontology:human_readable(kodashim_commandment_status__messianic_deferral, "Kodashim as Suspended-but-Live Commandment Pending Messianic Restoration").
narrative_ontology:topic_domain(kodashim_commandment_status__messianic_deferral, "religious/halakhic").

domain_priors:requires_active_enforcement(kodashim_commandment_status__messianic_deferral).
narrative_ontology:has_sunset_clause(kodashim_commandment_status__messianic_deferral).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__messianic_deferral, '0afb48d8-4bf2-42b6-a559-08744bcbf955').
narrative_ontology:cs_kernel_codification('0afb48d8-4bf2-42b6-a559-08744bcbf955', fixed_text).
narrative_ontology:cs_authority_grounding('0afb48d8-4bf2-42b6-a559-08744bcbf955', lineage).
narrative_ontology:cs_interpretation_layer_present('0afb48d8-4bf2-42b6-a559-08744bcbf955').
narrative_ontology:cs_reading_relation('0afb48d8-4bf2-42b6-a559-08744bcbf955', kodashim_commandment_status__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('0afb48d8-4bf2-42b6-a559-08744bcbf955', kodashim_commandment_status__performance_only, influences).
narrative_ontology:cs_axiom('0afb48d8-4bf2-42b6-a559-08744bcbf955', foundational, commandment_persists_through_temporal_suspension).
narrative_ontology:cs_axiom_status(commandment_persists_through_temporal_suspension, holdable).
narrative_ontology:cs_axiom_grounding('0afb48d8-4bf2-42b6-a559-08744bcbf955', commandment_persists_through_temporal_suspension, deontological).
narrative_ontology:cs_axiom('0afb48d8-4bf2-42b6-a559-08744bcbf955', foundational, study_functions_as_preparatory_not_terminal_fulfillment).
narrative_ontology:cs_axiom_status(study_functions_as_preparatory_not_terminal_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('0afb48d8-4bf2-42b6-a559-08744bcbf955', study_functions_as_preparatory_not_terminal_fulfillment, conventional).
narrative_ontology:cs_reference_frame('0afb48d8-4bf2-42b6-a559-08744bcbf955', temple_era_commanded_obligation).
narrative_ontology:cs_drift_state('0afb48d8-4bf2-42b6-a559-08744bcbf955', post_temple_extended_diaspora, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0afb48d8-4bf2-42b6-a559-08744bcbf955', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, kodashim_study_institutions).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, messianic_restorationist_authorities).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, future_generation_of_restored_temple_service).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, present_generation_practical_needs).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, students_directed_away_from_applicable_law).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, communities_prioritizing_immediate_ethical_or_social_torah).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbinic decisors and yeshiva leadership who frame Kodashim study as maintenance of a suspended-not-abolished commandment. They set curricular priority, direct communal resources toward sacrificial law study, and derive institutional authority and continuity from the doctrine that restoration is pending and preparation is presently obligatory.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, messianic_restorationist_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Yeshivot and study halls whose curricula, funding, and prestige depend on Kodashim remaining a live, obligatory subject of study rather than a historical curiosity. Their institutional standing is directly tied to the doctrine that the commandment is dormant but binding.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, kodashim_study_institutions, beneficiary,
    organized, generational, constrained, global).

% A not-yet-existing beneficiary class: the priests and community who would, upon restoration, draw on generations of accumulated study to perform the service correctly. Named for completeness; this class cannot presently act, object, or exit.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, future_generation_of_restored_temple_service, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(kodashim_commandment_status__messianic_deferral, future_generation_of_restored_temple_service).

% Students whose limited years of intensive study time are allocated to sacrificial procedure — laws with no present application — rather than to areas of Torah with direct bearing on their lived circumstances (civil law, ethics, contemporary halakhic problems). Leaving the track means abandoning standing within the institution that credentials them.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, students_directed_away_from_applicable_law, payer,
    powerless, biographical, constrained, local).

% Communities and individuals whose immediate religious, ethical, and social needs compete for the same finite pool of scholarly attention, communal funding, and institutional prestige that is instead directed toward maintaining readiness for a commandment with no current object of performance.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, present_generation_practical_needs, payer,
    moderate, biographical, constrained, national).

% Movements and congregations that would rather see communal scholarly capital directed at pressing ethical, social-justice, or civil-law questions. They can exit into non-Kodashim-centered institutions, but doing so costs them standing within traditions that treat suspended-commandment study as a marker of seriousness.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, communities_prioritizing_immediate_ethical_or_social_torah, payer,
    moderate, biographical, mobile, national).

% Academic and cross-denominational scholars who trace how the messianic-deferral reading emerged historically (post-Temple rabbinic literature) and compare it against the performance-only and study-as-performance readings without personally benefiting from any of the three.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, comparative_halakhic_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves detailed technical knowledge of sacrificial procedure across generations so that, should restoration occur, the community is not required to reconstruct the law from scratch — a genuine intergenerational knowledge-continuity problem.
% TRANSFER_FUNCTION: Moves scholarly time, institutional prestige, and communal funding away from present-applicable areas of law and ethics and toward the maintenance of readiness for a contingency (Temple restoration) that has not occurred for approximately two millennia.
% ABSENT_VOICES: Present-generation communities with urgent practical or ethical needs are rarely positioned to argue directly against Kodashim's curricular centrality without appearing to devalue Torah study itself or concede ground to the performance-only reading, which the tradition treats as more radical; their objection is structurally muted by the framing of the debate itself.
% DISAPPEARANCE_RATIONALE: If the messianic-deferral reading vanished overnight, institutions built around continuous Kodashim study would lose their doctrinal justification and would either fold into the study-as-performance reading (no loss of activity, changed rationale) or curricular time would genuinely reallocate to applicable law. Restorationist authorities dispute this, holding that abandoning the deferral doctrine itself constitutes a theological rupture regardless of what students then study.
% FOUNDING_PROBLEM: After the Temple's destruction, the rabbinic community faced the problem of how to relate to a body of divinely commanded law that had become materially unperformable — whether to declare it abolished, transmute study into a substitute performance, or hold it in formal suspension pending a hoped-for restoration.
% FOUNDING_PROBLEM_CORROBORATION: Restorationist authorities and Kodashim study institutions attest the founding problem is still live because restoration remains theologically anticipated. Comparative halakhic scholars, writing from outside the benefiting institutions, note that the deferral doctrine functions historically to preserve institutional and curricular continuity independent of any change in the probability of restoration, and that similar deferral doctrines appear in other traditions facing analogous ritual-obsolescence problems.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__messianic_deferral, contested).
narrative_ontology:founding_problem_status(kodashim_commandment_status__messianic_deferral, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__messianic_deferral, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_commandment_status__messianic_deferral, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__messianic_deferral, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__messianic_deferral_tests).
:- end_tests(kodashim_commandment_status__messianic_deferral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) and rises slowly across the interval: the primary cost is opportunity cost, not coercive extraction — communal and personal study-time diverted from applicable law and present ethical concerns, compounding gradually as institutional investment in Kodashim curricula deepened over centuries. Suppression is present but not severe (0.38): dissenting communities can and do reallocate emphasis, but doing so costs standing within traditions that treat sustained Kodashim engagement as a marker of seriousness, which functions as soft suppression rather than hard barriers. Theater ratio is moderate (0.30): the study genuinely transmits technical knowledge (not pure performance), but a meaningful fraction of curricular emphasis functions to signal doctrinal seriousness about eventual restoration rather than to advance any practically deployable skill.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, sustained Kodashim study is straightforward continuity-preservation for an eternally valid commandment — a scaffold whose 'sunset' is restoration itself. From the payer seats (students, present-need communities), the same activity reads as an opportunity-cost transfer sustained by doctrinal framing that makes withdrawal look like religious laxity. The engine should register this as genuine seat divergence rather than as one seat being simply mistaken.
 *
 * DIRECTIONALITY LOGIC:
 *   Restorationist authorities and study institutions sit at the beneficiary end: they derive continuing institutional authority, curricular centrality, and doctrinal coherence from treating the commandment as suspended-but-live. Present-generation communities and students directed toward Kodashim over applicable law sit at the target end: their scarce study-years and communal resources are the transfer, moving from present application to future contingency preparedness. Students carry the sharpest version of this because their exit options are constrained by credentialing dependency on the very institutions that benefit from the doctrine.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold framing is doing real work here: the messianic-deferral reading declares its own sunset clause (restoration of the Temple) rather than claiming permanent present-tense obligation like the study-as-performance reading, or complete present inertness like the performance-only reading. This prevents the constraint from being mislabeled as pure snare (there is a genuine, stated transitional logic and a real coordination function — knowledge continuity) while also preventing it from being waved through as costless coordination (the sunset condition has not obtained for two millennia, and the deferred cost has been borne continuously by every intervening generation of students and communities).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_deferral_reading_identity,
    'Is the correct account of Kodashim''s status the messianic-deferral reading (suspended-but-binding, study-as-readiness) rather than study-as-performance (study IS present fulfillment) or performance-only (commandment simply inert without an altar)?',
    'No empirical resolution mechanism exists; this is a live doctrinal dispute internal to rabbinic tradition, resolved (if at all) by which authorities and communities a given questioner treats as binding. Different halakhic communities and denominations hold different readings simultaneously and have done so continuously since the post-Temple period.',
    'Under study_as_performance, there is no future-contingency subordination of present needs — the extraction pattern analyzed here largely dissolves because study is the terminus, not preparation. Under performance_only, the commandment carries no live obligation at all and Kodashim study becomes elective historical/legal scholarship rather than commandment-fulfillment-adjacent activity, changing both the beneficiary structure and the coordination-function claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(messianic_deferral_reading_identity, conceptual, 'Which reading of the Kodashim kernel is authoritative — messianic deferral, study-as-performance, or performance-only.').

omega_variable(
    restoration_probability_and_extraction_magnitude,
    'Does the subjective or communal probability assigned to eventual Temple restoration affect how the opportunity-cost extraction is perceived by those bearing it?',
    'Comparative study of communities holding varying restoration expectations (from near-term messianic anticipation to purely eschatological/indefinite deferral) and their corresponding curricular allocation to Kodashim relative to applicable law.',
    'If perceived probability of restoration correlates with willingness to bear the opportunity cost, the extraction is partly consent-based (communities choosing the cost in proportion to their credence) rather than purely doctrinally imposed; if allocation is uniform regardless of stated credence, the doctrinal framing is doing more independent work than genuine anticipation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_probability_and_extraction_magnitude, empirical, 'Whether restoration-credence tracks or is decoupled from the resource allocation the doctrine produces.').

omega_variable(
    present_generation_subordination_ambiguity,
    'Is the subordination of present-generation practical and ethical needs to messianic-preparation study better understood as genuine intergenerational investment (analogous to preserving any endangered technical knowledge) or as institutional rent-seeking dressed in eschatological language?',
    'Track whether institutional emphasis on Kodashim correlates more strongly with funding/prestige incentives internal to yeshiva structures than with independent measures of restoration anticipation or communal demand for the knowledge.',
    'If institutional incentives predict curricular emphasis better than eschatological credence does, the coordination story (knowledge preservation) is more cover than function, pushing the classification toward tangled_rope; if anticipation and demand predict emphasis, the scaffold/coordination reading is better supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(present_generation_subordination_ambiguity, conceptual, 'Whether present-cost subordination reflects genuine future-oriented coordination or institutional self-interest wearing doctrinal cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__messianic_deferral, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__messianic_deferral, theater_ratio, 0, 0.18).
narrative_ontology:measurement(koda_tr_t300, kodashim_commandment_status__messianic_deferral, theater_ratio, 300, 0.2).
narrative_ontology:measurement(koda_tr_t700, kodashim_commandment_status__messianic_deferral, theater_ratio, 700, 0.24).
narrative_ontology:measurement(koda_tr_t1100, kodashim_commandment_status__messianic_deferral, theater_ratio, 1100, 0.27).
narrative_ontology:measurement(koda_tr_t1500, kodashim_commandment_status__messianic_deferral, theater_ratio, 1500, 0.29).
narrative_ontology:measurement(koda_tr_t1900, kodashim_commandment_status__messianic_deferral, theater_ratio, 1900, 0.3).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__messianic_deferral, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(koda_be_t300, kodashim_commandment_status__messianic_deferral, base_extractiveness, 300, 0.33).
narrative_ontology:measurement(koda_be_t700, kodashim_commandment_status__messianic_deferral, base_extractiveness, 700, 0.37).
narrative_ontology:measurement(koda_be_t1100, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1100, 0.4).
narrative_ontology:measurement(koda_be_t1500, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1500, 0.41).
narrative_ontology:measurement(koda_be_t1900, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1900, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__messianic_deferral, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(koda_su_t300, kodashim_commandment_status__messianic_deferral, suppression_requirement, 300, 0.32).
narrative_ontology:measurement(koda_su_t700, kodashim_commandment_status__messianic_deferral, suppression_requirement, 700, 0.34).
narrative_ontology:measurement(koda_su_t1100, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1100, 0.36).
narrative_ontology:measurement(koda_su_t1500, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1500, 0.37).
narrative_ontology:measurement(koda_su_t1900, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1900, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__messianic_deferral, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__messianic_deferral, 0.1).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__study_as_performance).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__performance_only).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the kodashim_commandment_status kernel. study_as_performance collapses the future-contingency structure by treating study itself as present fulfillment (near-zero opportunity-cost extraction, since nothing is being deferred). performance_only treats the commandment as simply inert absent a Temple, removing any live obligation and any coordination claim for study (extraction approaches zero from a different direction — there is no ongoing transfer because there is no ongoing obligation). messianic_deferral (this story) sits between them: obligation persists, performance does not, and study is justified instrumentally by a future condition — producing the moderate, slowly-rising extraction profile authored here. Each story authors its own epsilon under the shared-referent rule (the standing Kodashim arrangement, as this reading's own lights read it); values are not averaged or reconciled across the three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
