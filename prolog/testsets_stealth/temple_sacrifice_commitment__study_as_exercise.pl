% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__study_as_exercise, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: temple_sacrifice_commitment__study_as_exercise
 *   human_readable: Study-as-Performance Equivalence for Sacrificial Law
 *   domain: religious/halakhic/commitment-system
 *
 * SUMMARY:
 *   In the post-destruction rabbinic tradition, the teaching that engagement
 *   with the sacrificial laws constitutes performance of the sacrificial
 *   commandments turns a materially impossible service into a continuously
 *   practicable one. The academies keep the sacrificial orders central to the
 *   curriculum; the daily liturgy embeds recitation of the offerings; every
 *   learner, from academy student to lay congregant, occupies the command
 *   through study. On this reading's own terms the arrangement has no victim
 *   set: the cost (time given to study) and the benefit (covenantal credit,
 *   competence, continuity) land in the same hands, and the beneficiary is
 *   the studying community itself. This file instantiates the
 *   study_as_exercise reading of the temple_sacrifice_commitment kernel; the
 *   sibling readings are separate constraint files linked through the network
 *   block, and the committer contest is carried in the omega variables and
 *   kernel_context rather than inside this constraint's classification. KEY
 *   AGENTS (by structural relationship): - rabbinic_academy_leadership:
 *   Agenda-setting seat (institutional/identity_locked) — authors and
 *   transmits the equivalence teaching, sets curriculum, defines what counts
 *   as fulfillment - yeshiva_students: Participant seat
 *   (moderate/identity_locked) — bears the study time, receives formation and
 *   credited fulfillment - communal_learners: Participant seat
 *   (moderate/constrained) — recites the sacrificial passages in liturgy,
 *   sustains the practice - displaced_priestly_households: Excluded seat
 *   (moderate/trapped) — hereditary custodians of the ended material service,
 *   no seat in the fulfillment decision - academic_historians_of_judaism:
 *   Analytical observer (analytical/analytical) — traces the teaching's
 *   emergence and function from outside the tradition's warrant
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__study_as_exercise, 0.1).
domain_priors:suppression_score(temple_sacrifice_commitment__study_as_exercise, 0.15).
domain_priors:theater_ratio(temple_sacrifice_commitment__study_as_exercise, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, extractiveness, 0.1).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__study_as_exercise, rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__study_as_exercise, "Study-as-Performance Equivalence for Sacrificial Law").
narrative_ontology:topic_domain(temple_sacrifice_commitment__study_as_exercise, "religious/halakhic/commitment-system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__study_as_exercise, '22275865-c3d1-430b-a69b-a672f4dbd597').
narrative_ontology:cs_kernel_codification('22275865-c3d1-430b-a69b-a672f4dbd597', fixed_text).
narrative_ontology:cs_authority_grounding('22275865-c3d1-430b-a69b-a672f4dbd597', lineage).
narrative_ontology:cs_interpretation_layer_present('22275865-c3d1-430b-a69b-a672f4dbd597').
narrative_ontology:cs_reading_relation('22275865-c3d1-430b-a69b-a672f4dbd597', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('22275865-c3d1-430b-a69b-a672f4dbd597', temple_sacrifice_commitment__hybrid_preparatory, forecloses).
narrative_ontology:cs_axiom('22275865-c3d1-430b-a69b-a672f4dbd597', foundational, study_constitutes_offering_performance).
narrative_ontology:cs_axiom_status(study_constitutes_offering_performance, holdable).
narrative_ontology:cs_axiom_grounding('22275865-c3d1-430b-a69b-a672f4dbd597', study_constitutes_offering_performance, conventional).
narrative_ontology:cs_axiom('22275865-c3d1-430b-a69b-a672f4dbd597', foundational, discharge_requires_no_material_conditions).
narrative_ontology:cs_axiom_status(discharge_requires_no_material_conditions, holdable).
narrative_ontology:cs_axiom_grounding('22275865-c3d1-430b-a69b-a672f4dbd597', discharge_requires_no_material_conditions, deontological).
narrative_ontology:cs_reference_frame('22275865-c3d1-430b-a69b-a672f4dbd597', continuously_binding_command_fulfilled_through_study).
narrative_ontology:cs_drift_state('22275865-c3d1-430b-a69b-a672f4dbd597', contemporary_post_destruction_generations, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('22275865-c3d1-430b-a69b-a672f4dbd597', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, rabbinic_academy_leadership).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, yeshiva_students).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, communal_learners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__study_as_exercise, yeshiva_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The heads of the academies author, transmit, and adjudicate the teaching that engagement with the sacrificial texts discharges the sacrificial commandments. They set the curriculum that keeps the large sacrificial orders central to advanced learning, ordain successors, and answer communal questions about how the command is fulfilled without the altar. Their standing rests on being the authorities who define what counts as fulfillment; stepping outside that role would mean ceasing to be what they are.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, rabbinic_academy_leadership, agenda_setter,
    institutional, generational, identity_locked, continental).

% Spend their days mastering the sacrificial tractates, bearing the full time cost of the practice while receiving its credited benefit: formation in the tradition, communal standing, and the covenantal credit the teaching assigns to such study. Leaving the study hall would mean leaving the community and identity formed around it, not merely changing an activity.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, yeshiva_students, beneficiary,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__study_as_exercise, yeshiva_students, payer).

% Recite the sacrificial passages embedded in the daily liturgy and follow the sacrificial laws in their own measure of learning. They receive the continuity the practice provides and bear a modest time cost folded into existing prayer and study habits; their participation is shaped by what the synagogues and schools around them make available.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, communal_learners, beneficiary,
    moderate, biographical, constrained, regional).

% Descendants of the priestly families whose hereditary office was the material performance of the service. Their altar work ended with the destruction and was never restored; the teaching that any Israelite's study fulfills the command assigns the discharged obligation to a practice open to everyone, leaving their ancestral prerogative with no living function beyond residual honors. They inherit the loss and hold no seat where the terms of fulfillment are decided.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, displaced_priestly_households, excluded,
    moderate, generational, trapped, regional).

% Study the post-destruction adaptation from outside the tradition's own warrant, tracing how the equivalence teaching emerged in the talmudic sources and how it sustained communal practice across the diaspora centuries. They take no side in the tradition's internal question and bear none of its costs.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, academic_historians_of_judaism, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__study_as_exercise, rabbinic_academy_leadership).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__study_as_exercise, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a commanded practice operative when its material instrument is gone: by designating textual engagement as the command's fulfillment, it gives every generation and every locale a continuous, portable way to observe the obligation, preserves the sacrificial legal corpus as living curriculum rather than museum piece, and synchronizes communal identity around a shared daily practice.
% TRANSFER_FUNCTION: Moves time and attention from learners into the sacrificial corpus, and moves covenantal credit, learned competence, and communal continuity back to the learners and the community; doctrinal authority and institutional perpetuation accrue to the academies that define the terms of fulfillment. No material goods move from anyone into anyone else's exclusive account.
% ABSENT_VOICES: The priestly families whose office the teaching renders functionless would object that fulfillment has been reassigned away from its appointed custodians; within the tradition's own canon, the prophetic register that weighs deed over rite would press on whether textual occupation honors the command's full demand. Neither holds a seat: the priesthood's office ended in 70 CE, and halakhic adjudication subordinates the prophetic critique to settled interpretation.
% DISAPPEARANCE_RATIONALE: If the teaching that study fulfills the command vanished overnight, communities would face an undischarged obligation with no sanctioned outlet: the daily liturgy's sacrificial passages would lose their warrant, the sacrificial orders would drop out of the curriculum, and the covenant's continuity practice would collapse into either confessed lapse or improvised substitutes — a rearrangement of liturgy, curriculum, and communal self-understanding across the whole observing world.
% FOUNDING_PROBLEM: After the destruction of the Temple in 70 CE, the covenant's central commanded service became materially impossible; the community faced the lapse of a binding obligation and needed a way to keep the command operative rather than abandoned.
% FOUNDING_PROBLEM_CORROBORATION: The material condition is attested from outside the benefiting parties: the Temple Mount has remained under non-Jewish custodianship since antiquity, and Roman imperial records together with the archaeological record corroborate the destruction and the non-restoration. The site's continuous status independently establishes that the material conditions for the service remain absent; no party inside the arrangement is needed to establish the fact.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__study_as_exercise, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__study_as_exercise, 0.1, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__study_as_exercise_tests).
:- end_tests(temple_sacrifice_commitment__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits near the identity-coordination floor (0.10): the arrangement's cost is study time borne by the same people who receive its credit, and no seat collects material rent from another's participation; the small surplus that does accrue lands on the academy seat as doctrinal authority and institutional perpetuation, which is why the receipt surface names that seat rather than reporting diffuse gains. Suppression is low (0.15): the teaching competes interpretively with prayer-centered and restoration-centered framings rather than coercing them out of existence, and communal enforcement of study norms is soft and identity-carried. Theater is low (0.12): the study produces real textual competence and real liturgical practice, not performative maintenance of an admitted dead form. Accessibility collapse is moderate-low (0.30): rival accounts of fulfillment remain live inside the tradition, so understanding the teaching does not close the option space. Resistance is low (0.15): once established in the talmudic sources the teaching met little organized opposition. The claimed type is rope on structural grounds — a genuine collective-action problem (keeping a commanded practice operative without its material instrument), net-beneficiary participants, no victim set, no suppressed exits — authored independently of these metric values; the engine computes per-seat classifications from the structural data. Measurement points span the consolidation arc from the destruction generation (t=0) through the stabilized Geonic settlement (t=60); both tracked series are nearly flat, matching a stable coordination arrangement with no enforcement ratchet, which is why no suppression_requirement series is authored.
 *
 * PERSPECTIVAL GAP:
 *   The academy seat experiences the arrangement as sacred continuity it stewards and defines; the student seat experiences cost and reward fused in formation; the lay learner experiences it as liturgical habit; the excluded priestly seat experiences dispossession of an inherited function; the historian seat sees an adaptive institutional solution to catastrophic loss. Same arrangement, five structural positions — the engine computes the divergence from power, exit, and directional placement, not from the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to real recipients: academy leadership receives doctrinal authority and institutional perpetuation (slightly above the pure beneficiary pole, since it also directs others' study time); students and lay learners receive credit and continuity while bearing their own time cost, placing them near symmetric with a beneficiary lean. No victim group is declared because no seat bears costs transferred to another's account — the closest candidate, the priestly households, lost their function to history rather than to a transfer this arrangement operates, and they are seated as excluded rather than as victims. Suppression is authored as a raw structural property and is not scaled by power or scope; extractiveness alone rides the directionality and scope scaling.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — material impossibility of the commanded service — remains live, so no mandate has outlived its function and no mandatrophy declaration is made. The classification guards both mislabelings: against reading devotion as covert extraction (there is no victim set and no suppressed exit to point to), and against reading the arrangement as inertial performance (the study is the function on this reading's terms, not a stand-in for one). The arrangement's obsolescence risk is conditional and external: if the material conditions returned, this reading's premise (fulfillment through study in the absence of material conditions) would face direct pressure, and the hybrid_preparatory sibling — which builds the restoration transition into its own structure — anticipates precisely that contingency. This file does not encode that transition; it classifies the arrangement as it stands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This file instantiates one reading (study_as_exercise) of the kernel temple_sacrifice_commitment; which reading a community holds determines the entire structural profile — where exactly is the disagreement located?',
    'Curricular and liturgical framing discloses the held reading: whether teachers present sacrificial study as full fulfillment now, as suspended maintenance pending restoration, as archival preservation, or as a transformed instantiation alongside prayer.',
    'Under performance_only, epsilon rises sharply and the practice reads as maintenance of a defunct form (high theater, inertial character); under hybrid_preparatory the arrangement takes on a transitional character keyed to a restoration event; under symbolic_transformation the referent arrangement itself changes to a prayer-and-study instantiation. Each sibling is a separate constraint file with its own epsilon over the same standing arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer structure: one reading of a contested kernel; sibling files carry the alternative structural profiles.').

omega_variable(
    intrinsic_value_vs_self_perpetuation,
    'Is the value of sacrificial study intrinsic to the commitment (as this reading holds) or partly an institutional self-perpetuation mechanism that keeps the academies indispensable?',
    'Compare communities and periods where the sacrificial orders are de-emphasized in favor of other corpora: if covenant fidelity and communal continuity hold without heavy sacrificial-curriculum investment, the intrinsic-value account strengthens; if fidelity tracks curricular investment, the perpetuation account gains.',
    'If perpetuation dominates, measured extractiveness rises above the identity-coordination floor and pressure toward a hybrid coordination/extraction profile appears despite the absence of a victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intrinsic_value_vs_self_perpetuation, empirical, 'Whether the practice''s value is intrinsic to the commitment or institutionally self-serving.').

omega_variable(
    priestly_displacement_character,
    'Does the universalization of fulfillment (any Israelite''s study counts) constitute benign democratization of the command, or erasure of a hereditary counter-authority whose exclusion shapes the arrangement?',
    'Survey the retained priestly prerogatives in post-destruction law (dues, firstborn redemption, blessing rites) and the sources'' treatment of priestly claims about the service; assess whether the study-teaching displaced a live competing claim or filled a vacancy no one could occupy.',
    'If erasure of a suppressed alternative, the suppression estimate rises and an excluded-party dimension enters the classification; if vacancy-filling, the current low suppression stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priestly_displacement_character, conceptual, 'Character of the priestly office''s displacement by universalized study-fulfillment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__study_as_exercise, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0, 0.06).
narrative_ontology:measurement(temp_tr_t10, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 10, 0.08).
narrative_ontology:measurement(temp_tr_t20, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 20, 0.09).
narrative_ontology:measurement(temp_tr_t30, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 30, 0.1).
narrative_ontology:measurement(temp_tr_t40, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 40, 0.11).
narrative_ontology:measurement(temp_tr_t50, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 50, 0.12).
narrative_ontology:measurement(temp_tr_t60, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 60, 0.12).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 0, 0.07).
narrative_ontology:measurement(temp_be_t10, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 10, 0.08).
narrative_ontology:measurement(temp_be_t20, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 20, 0.09).
narrative_ontology:measurement(temp_be_t30, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 30, 0.09).
narrative_ontology:measurement(temp_be_t40, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 40, 0.1).
narrative_ontology:measurement(temp_be_t50, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 50, 0.1).
narrative_ontology:measurement(temp_be_t60, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 60, 0.1).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_commitment__study_as_exercise, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__study_as_exercise, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% The colloquial question 'what fulfills the sacrificial commandments after the Temple' decomposes into four structurally distinct constraints sharing one kernel: study-as-full-performance (this file, near-zero epsilon, no victim set), study-as-archival-preservation (performance_only, high theater, inertial character), study-as-suspended-maintenance (hybrid_preparatory, transitional character keyed to restoration), and authorized-transformation (symbolic_transformation, a different referent arrangement in which prayer and study are the new instantiation rather than occupation of the original command). All four author epsilon over the same standing arrangement as referent; the upstream talmudic sources are cited as evidence by each downstream reading, which is why the family edges run from this file to all three siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
