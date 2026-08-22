% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__study_as_exercise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__study_as_exercise_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: sacrifice_obligation_kernel__study_as_exercise_reading
 *   human_readable: Study-as-Exercise Reading of the Sacrificial Obligation
 *   domain: religious law / halakhic authority / commitment-system dynamics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the sacrifice_obligation_kernel:
 *   that study of the laws of offerings constitutes genuine exercise of the
 *   mitzvah, so the obligation is fully occupied through intellectual
 *   engagement under present conditions. The standing arrangement under
 *   contest — the referent for epsilon — is the study-based occupation of the
 *   obligation as this reading itself assesses it: a legitimate
 *   transformation, not a suspension and not a mask over an unmet duty. KEY
 *   AGENTS (by structural relationship): rabbinic_authority: agenda-setting
 *   interpreter (institutional/identity_locked) — administers the framework
 *   defining fulfillment and collects deference and support; torah_scholars:
 *   practicing beneficiaries (moderate/identity_locked) — fulfill the
 *   commandment through vocation; lay_observing_community: participating
 *   beneficiaries with material-support costs (organized/constrained);
 *   temple_restoration_advocates: excluded dissenting voice
 *   (organized/constrained) — would redirect communal energy toward restored
 *   performance; halakhic_analyst_observer: analytical seat. The claim and
 *   the metrics are independent authored facts: the reading is CLAIMED as
 *   rope and the metrics are authored as what the arrangement descriptively
 *   looks like from this reading's own lights — near-zero extraction, low
 *   suppression, low theater. Sibling readings are separate constraints
 *   (linked via network.affects_constraints), not folded into this one; per
 *   the epsilon-invariance principle each carries its own epsilon and victim
 *   structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__study_as_exercise_reading, 0.05).
domain_priors:suppression_score(sacrifice_obligation_kernel__study_as_exercise_reading, 0.12).
domain_priors:theater_ratio(sacrifice_obligation_kernel__study_as_exercise_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__study_as_exercise_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__study_as_exercise_reading, "Study-as-Exercise Reading of the Sacrificial Obligation").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__study_as_exercise_reading, "religious law / halakhic authority / commitment-system dynamics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__study_as_exercise_reading, '1d210768-4d38-4a81-9663-14d1734e3a72').
narrative_ontology:cs_kernel_codification('1d210768-4d38-4a81-9663-14d1734e3a72', fixed_text).
narrative_ontology:cs_authority_grounding('1d210768-4d38-4a81-9663-14d1734e3a72', lineage).
narrative_ontology:cs_interpretation_layer_present('1d210768-4d38-4a81-9663-14d1734e3a72').
narrative_ontology:cs_reading_relation('1d210768-4d38-4a81-9663-14d1734e3a72', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d210768-4d38-4a81-9663-14d1734e3a72', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d210768-4d38-4a81-9663-14d1734e3a72', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('1d210768-4d38-4a81-9663-14d1734e3a72', foundational, study_constitutes_genuine_fulfillment).
narrative_ontology:cs_axiom_status(study_constitutes_genuine_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('1d210768-4d38-4a81-9663-14d1734e3a72', study_constitutes_genuine_fulfillment, theological).
narrative_ontology:cs_axiom('1d210768-4d38-4a81-9663-14d1734e3a72', foundational, authorized_transformation_of_exercise_form).
narrative_ontology:cs_axiom_status(authorized_transformation_of_exercise_form, holdable).
narrative_ontology:cs_axiom_grounding('1d210768-4d38-4a81-9663-14d1734e3a72', authorized_transformation_of_exercise_form, conventional).
narrative_ontology:cs_reference_frame('1d210768-4d38-4a81-9663-14d1734e3a72', sinaitic_obligation_in_force).
narrative_ontology:cs_drift_state('1d210768-4d38-4a81-9663-14d1734e3a72', contemporary_diaspora_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1d210768-4d38-4a81-9663-14d1734e3a72', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, torah_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, lay_observing_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__study_as_exercise_reading, lay_observing_community).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, study_sacrifice_equivalence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the interpretive framework that determines what counts as fulfilling the sacrificial commandments: setting curricula, adjudicating questions of observance, and teaching that engagement with the laws of offerings discharges the obligation under present conditions. Communal deference, institutional support, and the prerogative of defining fulfillment flow toward its academies and courts. Its standing is inseparable from the framework it administers; stepping outside it would dissolve the very authority it exercises.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority, beneficiary).

% Devote their working lives to mastering the portions of the tradition that treat offerings. Through this study they fulfill the commandment as they understand it, earn communal standing and often livelihood in teaching roles, and reproduce the interpretive tradition for the next generation. Leaving the practice would forfeit both their vocation and the fulfillment their study secures.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, torah_scholars, beneficiary,
    moderate, biographical, identity_locked, global).

% Recites the passages concerning offerings in the daily liturgy, supports the academies materially, and relies on rabbinic adjudication for what its observance requires. Members receive continuity with the commandment and a recognized path of observance without the Temple; the support they provide sustains the institutions that define that path. Individual exit from the community is possible but carries real social and familial cost.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, lay_observing_community, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__study_as_exercise_reading, lay_observing_community, payer).

% Hold that the commandment's full exercise awaits physical restoration and prepare accordingly — cataloguing vessels, training candidates for priestly service, drafting liturgies for renewed offerings. They regard the primacy of study as diverting communal energy and resources away from preparation, and press their case at the margins of the academies that set the curriculum. Their proposals rarely reach the bodies that decide what observance requires.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, temple_restoration_advocates, excluded,
    organized, civilizational, constrained, regional).

% Studies the tradition from outside its adjudicating structure — tracing how the identification of study with offering entered the literature, how it is taught, and how alternative accounts of the obligation fare in the textual record. Neither collects from the arrangement nor bears its costs; observes and reports.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, halakhic_analyst_observer, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__study_as_exercise_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common, authoritative answer to how a Temple-dependent commandment remains exercisable when its site, altar, and priestly service do not exist: the obligation is occupied through a shared intellectual practice that keeps the law intelligible, transmissible, and connected to communal life.
% TRANSFER_FUNCTION: Moves interpretive deference, communal attention, and material support from the lay observing community toward the academies and courts that teach the laws of offerings and adjudicate fulfillment; participants receive in return a recognized path of observance and continuity with the commandment.
% ABSENT_VOICES: Performance-only and messianic-suspension advocates would object that study displaces practical readiness for restored service; voices outside the rabbinic framework (Karaite successors, secular critics) would contest the premise that authorized interpretation can transform a biblical obligation's exercise-form. They sit at the margins of the academies, in separate movements, or outside the tradition entirely — present enough to register as resistance, absent from the agenda-setting seats.
% DISAPPEARANCE_RATIONALE: If the study-as-exercise arrangement vanished overnight, the community would face an unoccupied commandment: it would reorganize around another reading (restoration preparation, messianic waiting) or drop the obligation from lived practice. The daily liturgy's offering passages, academy curricula, and rabbinic adjudication of observance would all rearrange; the institutions funded to teach these laws would lose their function.
% FOUNDING_PROBLEM: The destruction of the Second Temple left a biblically legislated sacrificial system without its site, altar, or functioning priesthood; the tradition needed an account of how the obligation stands when its prescribed performance is impossible.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the current beneficiary set: the talmudic discussions that articulate the problem predate the modern rabbinate they authorize; medieval Karaite polemics — hostile witnesses — engage the same problem of an unperformable sacrificial law; and academic historiography of post-destruction Judaism independently documents the transformation. No corroborating source attests that the problem is dead.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__study_as_exercise_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__study_as_exercise_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__study_as_exercise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, 0.05, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).
:- end_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near-zero (0.05) because, by this reading's own lights, the studier receives exactly what the arrangement promises — fulfillment — and no transfer of burden occurs; the small residual reflects the community's dependence on rabbinic adjudication of what counts. Suppression is low (0.12): no enforcement machinery compels adherence to this reading, sibling readings remain intellectually and socially available, and the modest value registers the social friction of holding minority views inside the tradition. Theater is low (0.10): the study is the function itself, not a performance standing in for one; the slight rise across the interval tracks the receding practical applicability of the material as generations passed. Accessibility_collapse is low (0.20): understanding this reading does not eliminate the alternatives — the sibling readings stay live, which is precisely what the coexistence relations record. Resistance is moderate-low (0.30): restoration advocacy, Karaite-descended objection, and secular detachment press against the arrangement without threatening it. Both temporal series run on one shared grid (t=0,10,20,30,40,50) with every tracked metric authored at every point; a suppression_requirement series is deliberately omitted because the enforcement picture is static — no machinery was built up or allowed to decay — so the scalar captures it.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the rabbinic seat the arrangement is stewardship: an inherited framework faithfully administered, with deference that is earned interpretive authority. From the scholar's seat it is fulfillment and vocation fused — exit would cost both. From the community's seat it is continuity purchased with material support and reliance on adjudication it does not control. From the restoration advocate's seat the same arrangement is displacement: energy and resources flowing to study that their project believes belongs to preparation. The engine derives these divergences from the structural data (power, exit, role); this commentary does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared and drive the derivation: rabbinic_authority (agenda-setter and collector, institutional power, identity-locked exit) sits nearest the beneficiary end; torah_scholars likewise; lay_observing_community sits near-symmetric — genuine participatory benefit offset by the material-support flow recorded in its secondary payer role. No victims are declared because this reading authorizes the transformation: nobody bears the obligation as a burden under it. The excluded restoration advocates fall outside the beneficiary/victim sets; their opposition registers in the resistance metric rather than in directionality, which encodes cost-bearing, not dissent. No directionality overrides are used: the derivation from declarations and exit options matches the structural relationships, and adding overrides would second-guess data the chain already reads correctly. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled downstream.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Against extraction-mislabeling: the interpretive monopoly could be read as definitional rent, but the transfers feeding it are voluntary expressions of the practice itself, the founding problem (an unperformable commanded system) is corroborated as live by hostile and external witnesses, and the extractiveness series declines rather than accumulates across the interval — no rent-layering signature. Against coordination-overclaiming: the omegas hold open the two ways this could degrade — a residual unfulfilled obligation persisting beneath the study practice, or demonstrable rents from the fulfillment-adjudication monopoly — either of which would move the arrangement toward a hybrid profile. The arrangement is not a piton: its function is live, its theater ratio is low, and its administrator could not cheaply discard it because discarding it would dissolve the administrator's own authority — which is why fixing_cost is prohibitive while the gains concentrate in a named seat rather than diffusing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates one reading (study_as_exercise) of the sacrifice_obligation_kernel; which reading actually governs a given community''s practice, and how does the classification change under each sibling?',
    'Survey liturgy, school curricula, and responsa across communities to determine which reading each operates under, then classify each community''s arrangement as its own constraint.',
    'Under messianic_suspension the arrangement carries a readiness-maintenance function with a different beneficiary structure; under performance_only a persistent unfulfilled obligation appears; under symbolic_archive the halakhic claim drops entirely. Effective extraction and victim sets differ per reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the sacrificial-obligation kernel governs actual practice.').

omega_variable(
    residual_unfulfilled_obligation,
    'Does the tradition treat the sacrificial obligation as fully discharged by study, or does a residual performance-obligation persist beneath the study practice (petitionary liturgy for restoration suggests the latter)?',
    'Analysis of whether authorities describe the obligation as fulfilled, accommodated, or merely maintained — e.g., treatment of the additional-service petitions and of vows concerning offerings.',
    'If a residual obligation persists, the arrangement partly manages an unmet duty rather than exhausting it, and the near-zero extraction assessment would need revision upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_unfulfilled_obligation, conceptual, 'Whether study exhausts the obligation or overlays a persistent remainder.').

omega_variable(
    interpretive_monopoly_rent,
    'Is the rabbinic prerogative to define what counts as fulfillment a legitimate coordination authority, or does it yield definitional rent?',
    'Compare communities with more distributed interpretive authority; test whether fulfillment-adjudication produces material or status returns above the cost of maintaining the interpretive function.',
    'Demonstrable rents would push the arrangement toward a hybrid coordination/extraction profile despite the absence of a coerced victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_monopoly_rent, empirical, 'Whether the interpretive monopoly generates rents.').

omega_variable(
    sibling_relation_hybrid_coherence,
    'Hybrid time-indexed positions (study fulfills now, performance resumes at restoration) are coherent and widely held; does their coherence confirm that no sibling reading is logically eliminated, or does some pairing admit no single framework?',
    'Test each sibling pairing for a coherent combined position; a pairing with no coherent hybrid would convert the relation between those readings to logical exclusion.',
    'Relations authored here as coexistence would shift to mutual logical exclusion for any pairing lacking a coherent hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_relation_hybrid_coherence, conceptual, 'Whether hybrid positions keep all sibling readings mutually compatible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__study_as_exercise_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0, 0.07).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t10, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement_basis(sacr_tr_t10, observed).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement_basis(sacr_tr_t20, observed).
narrative_ontology:measurement(sacr_tr_t30, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement_basis(sacr_tr_t30, observed).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement_basis(sacr_tr_t40, observed).
narrative_ontology:measurement(sacr_tr_t50, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement_basis(sacr_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 0, 0.09).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t10, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 10, 0.08).
narrative_ontology:measurement_basis(sacr_be_t10, observed).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 20, 0.07).
narrative_ontology:measurement_basis(sacr_be_t20, observed).
narrative_ontology:measurement(sacr_be_t30, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 30, 0.065).
narrative_ontology:measurement_basis(sacr_be_t30, observed).
narrative_ontology:measurement(sacr_be_t40, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 40, 0.055).
narrative_ontology:measurement_basis(sacr_be_t40, observed).
narrative_ontology:measurement(sacr_be_t50, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 50, 0.05).
narrative_ontology:measurement_basis(sacr_be_t50, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__study_as_exercise_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__study_as_exercise_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the sacrificial obligation.' The single label conflates four structurally distinct claims about one kernel: performance_only (obligation requires physical performance; study preparatory), messianic_suspension (obligation divinely suspended, not transformed, until restoration), study_as_exercise (this file: obligation genuinely occupied through study), and symbolic_archive (cultural-historical preservation making no halakhic claim). Each reading has its own epsilon, beneficiary/victim structure, and classification; this reading authors near-zero extraction over the standing study-based arrangement as assessed by its own lights. The upstream talmudic equivalence doctrine feeds this reading and is cited by the siblings in their contests over it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
