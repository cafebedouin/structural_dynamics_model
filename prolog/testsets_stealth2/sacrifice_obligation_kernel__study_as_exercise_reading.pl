% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__study_as_exercise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
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
 *   domain: religious law/halakhic authority/commitment systems
 *
 * SUMMARY:
 *   A covenantal community recognizes binding sacrificial commandments whose
 *   lawful site of performance no longer exists. Under the arrangement
 *   authored here, sustained study of the sacrificial legal corpus is the
 *   community's recognized form of observing those commandments: the duty is
 *   exercised, not waived, and its validity is certified by the interpreting
 *   institution that transmits the corpus. Participation is voluntary,
 *   internally motivated, and globally distributed through academies and
 *   individual study; the arrangement requires no enforcement machinery
 *   because compliance is the practice itself. The claim/metric gap is
 *   deliberate and small here: the constraint is CLAIMED as rope (pure
 *   coordination of observance around a transformed duty) while the authored
 *   metrics record near-zero extraction with a gentle historical drift toward
 *   institutional advantage — the engine measures whatever divergence exists.
 *
 * KEY AGENTS:
 *   - rabbinic_authority: agenda-setting interpreter and principal beneficiary (institutional power, identity-locked exit) — adjudicates what counts as fulfillment and collects interpretive centrality
 *   - torah_students: primary participants and beneficiaries (moderate power, mobile exit) — occupy the duty through sustained study
 *   - yeshiva_institutions: secondary beneficiaries (organized power, constrained exit) — curricula, enrollment, and funding center on the sacrificial corpus
 *   - lay_observant_community: beneficiaries (organized power, mobile exit) — gain an accessible fulfillable form of the duty and fund the study enterprise
 *   - karaite_tradition_communities: excluded voice (organized power, identity-locked exit) — deny interpretive mediation of fulfillment and stand outside the rabbinic conversation
 *   - halakhic_academic_observers: analytical observer (analytical power, analytical exit) — trace the reading's development and institutional entanglement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__study_as_exercise_reading, 0.08).
domain_priors:suppression_score(sacrifice_obligation_kernel__study_as_exercise_reading, 0.15).
domain_priors:theater_ratio(sacrifice_obligation_kernel__study_as_exercise_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__study_as_exercise_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__study_as_exercise_reading, "Study-as-Exercise Reading of the Sacrificial Obligation").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__study_as_exercise_reading, "religious law/halakhic authority/commitment systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__study_as_exercise_reading, '6c030fa0-860f-420b-b089-ca8446e80f7f').
narrative_ontology:cs_kernel_codification('6c030fa0-860f-420b-b089-ca8446e80f7f', fixed_text).
narrative_ontology:cs_authority_grounding('6c030fa0-860f-420b-b089-ca8446e80f7f', lineage).
narrative_ontology:cs_interpretation_layer_present('6c030fa0-860f-420b-b089-ca8446e80f7f').
narrative_ontology:cs_reading_relation('6c030fa0-860f-420b-b089-ca8446e80f7f', sacrifice_obligation_kernel__performance_only_reading, forecloses).
narrative_ontology:cs_reading_relation('6c030fa0-860f-420b-b089-ca8446e80f7f', sacrifice_obligation_kernel__messianic_suspension_reading, forecloses).
narrative_ontology:cs_reading_relation('6c030fa0-860f-420b-b089-ca8446e80f7f', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('6c030fa0-860f-420b-b089-ca8446e80f7f', foundational, study_constitutes_mitzvah_exercise).
narrative_ontology:cs_axiom_status(study_constitutes_mitzvah_exercise, holdable).
narrative_ontology:cs_axiom_grounding('6c030fa0-860f-420b-b089-ca8446e80f7f', study_constitutes_mitzvah_exercise, conventional).
narrative_ontology:cs_axiom('6c030fa0-860f-420b-b089-ca8446e80f7f', foundational, intellectual_engagement_satisfies_duty_substance).
narrative_ontology:cs_axiom_status(intellectual_engagement_satisfies_duty_substance, holdable).
narrative_ontology:cs_axiom_grounding('6c030fa0-860f-420b-b089-ca8446e80f7f', intellectual_engagement_satisfies_duty_substance, deontological).
narrative_ontology:cs_reference_frame('6c030fa0-860f-420b-b089-ca8446e80f7f', study_as_operative_mitzvah_form).
narrative_ontology:cs_drift_state('6c030fa0-860f-420b-b089-ca8446e80f7f', contemporary_curricular_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('6c030fa0-860f-420b-b089-ca8446e80f7f', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, torah_students).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, yeshiva_institutions).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, lay_observant_community).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, study_equals_sacrifice_principle).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, oral_law_interpretive_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues rulings on whether and how the sacrificial commandments bind under present conditions, certifies that engagement with the sacrificial corpus discharges the duty, and trains the decisors who carry that certification forward. Its standing rests on being the reference point for fulfillment questions; stepping outside that role would unravel the tradition's self-understanding, so departure is not a realistic option even where reinterpretation is doctrinally open.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority, beneficiary).

% Devote study hours to the sacrificial tractates as a recognized form of observing the commandment. The reward is framed as the observance itself; they can redirect study to other corpora or adopt different framings of the duty without institutional penalty.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, torah_students, beneficiary,
    moderate, biographical, mobile, global).

% Organize multi-year curricula around the sacrificial code, employ teachers whose expertise is concentrated there, and attract support partly on the promise of transmitting this body of law. Reorienting curricula away from it would be costly and would strand specialized faculty.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, yeshiva_institutions, beneficiary,
    organized, generational, constrained, global).

% Members observe the commandment by supporting and occasionally engaging with the sacrificial corpus, gaining a sense of ongoing fidelity to a duty whose original setting is gone. They sustain the academies financially and can shift allegiance between institutions or framings with little friction.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, lay_observant_community, beneficiary,
    organized, generational, mobile, global).

% Hold that scripture alone binds and that no interpreting institution can confer fulfillment-validity; from their position the certification of study-as-observance is precisely what they reject. They stand outside the rabbinic conversation by long-standing mutual exclusion, and their own identity is built on that refusal, so entry is not available to them.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, karaite_tradition_communities, excluded,
    organized, generational, identity_locked, regional).

% Scholars of religious law document how the reading arose, spread, and became institutionally embedded; they take no position inside the practice and bear none of its costs or benefits.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, halakhic_academic_observers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__study_as_exercise_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared, teachable answer to how a binding commandment remains observable when its prescribed site of performance is unavailable: one standardized form of observance (engagement with the legal corpus), one reference tradition for validity questions, and continuity of obligation-consciousness across generations.
% TRANSFER_FUNCTION: Moves study-time and intellectual labor from community members into the sacrificial corpus; moves deference on fulfillment-validity questions toward the interpreting institution; moves tuition and donations from the lay community to the academies that transmit the corpus.
% ABSENT_VOICES: Traditions that reject interpretive mediation altogether (e.g., Karaite communities) would object that no institution can confer fulfillment-validity and that the duty's terms must be read from scripture alone; they are absent because the rabbinic framework's boundaries exclude them, and their objection is registered only from outside.
% DISAPPEARANCE_RATIONALE: Overnight removal would leave the community without its working answer to an obligation it continues to recognize: obligation-consciousness would reorganize around a different account of the duty's present status, academy curricula would lose a load-bearing component, and the interpreting institution would lose a major field of jurisdiction — the practice world rearranges around whichever successor account wins.
% FOUNDING_PROBLEM: After the destruction of the Temple removed the lawful site of sacrificial performance, a community that understood the sacrificial commandments as binding faced an unresolved practical question: is the duty void, held in abeyance, or exercisable in another form?
% FOUNDING_PROBLEM_CORROBORATION: The problem's reality is corroborated from outside the benefiting parties: the shared scriptural text states the duty without conditioning it on any particular era, and rival traditions — including anti-rabbinic ones — acknowledge that some answer is required even while denying the rabbinic one. Corroboration of THIS solution from outside the benefiting parties is limited; the strongest external signal is that competing accounts address the same problem, which confirms the problem rather than the solution.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__study_as_exercise_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__study_as_exercise_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__study_as_exercise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, 0.08, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored near zero (0.08) because the activity the reading prescribes is the observance itself rather than a levy on participants: the cost of study is the point of the practice, and the only residual is the advantage flowing to whoever certifies fulfillment. Suppression is low (0.15): participation is internally motivated, no enforcement machinery defends the reading, and individuals remain free to adopt other accounts of the duty — the small residual reflects ordinary social conformity inside adopting communities. Theater is low (0.15): most engagement with the corpus is substantively tied to the fulfillment claim, with a modest share of curricular routine detached from it. Accessibility collapse is low-moderate (0.25): accepting the reading does not close off rival accounts, which remain live options for individuals. Resistance is low (0.18): objections come from outside the adopting communities or from performance-primacy voices, without organized opposition inside. The claim (rope) and the metrics are authored independently: the metrics describe operation as observed; the claim states the structure taken to be true. The measurement series share one grid; suppression_requirement is intentionally not tracked because the enforcement picture is static — nothing builds up or erodes.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the interpreting institution's seat the arrangement is the tradition's own living answer — near-full subsidy, since it both runs the arrangement and draws standing from it. From the student seat it is opportunity: a fulfillable duty at the price of effort they would call the observance itself. From the excluded anti-mediation seat the identical structure reads as an unauthorized concentration of fulfillment-power — same facts, opposite valence. The engine computes this divergence from power, exit, and role data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   No victim set is declared: nobody bears the arrangement as a cost imposed for another's benefit. All declared beneficiaries derive directionality near the beneficiary pole — the interpreting institution (identity-locked exit anchors it near d≈0 despite its agenda-setting role, since its position depends on the arrangement it administers), students and lay community (mobile exit, genuine gain), academies (constrained exit but real curricular benefit). Global spatial scope modestly amplifies verification difficulty in the engine's arithmetic, but with base epsilon at 0.08 the scaled result stays near the coordination floor. The residual gain flow lands on the interpreting institution, which is why gain_flow names that seat even though its directionality remains beneficiary-side.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a binding duty without its performance site — remains live, so the arrangement has not outlived its mandate and mandatrophy is not resolved. The drift watchpoints are curricular routinization (theater_ratio series) and mediation-advantage accumulation (base_extractiveness series); both rise gently and stay low. If the occupation turns out to be conditional on performance-impossibility (see omega conditional_vs_permanent_occupation), sunset logic attaches and the arrangement becomes transitional support whose justification expires at restoration — the classification apparatus would then distinguish the transitional phase from any post-restoration residue rather than letting inertia masquerade as function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (study_as_exercise_reading) of the sacrifice_obligation_kernel; which reading a community adopts determines the entire structural profile — what would each sibling reading change?',
    'Evaluate each sibling reading as its own constraint story and compare computed types, epsilon, and victim sets across the family; cross-reading adoption patterns (which readings gain adherence under which conditions) indicate which instantiation is stabilizing.',
    'If performance_only_reading were authoritative, an unfulfilled-obligee victim set appears and epsilon rises sharply; if messianic_suspension_reading, the arrangement becomes readiness-maintenance with a deferred obligation; if symbolic_archive_reading, the halakhic claim dissolves and the activity reduces to heritage preservation with no fulfillment-validity left to adjudicate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one-of-four readings of the sacrificial-obligation kernel.').

omega_variable(
    conditional_vs_permanent_occupation,
    'Is study''s occupation of the obligation asserted as permanently equivalent to performance, or only valid while performance is impossible (an implicit transitional provision)?',
    'Close reading of the reading''s own authorities: do they ground the equivalence in the duty''s intrinsic intelligibility (permanent) or expressly sub modo, pending restored performance conditions (conditional)?',
    'If conditional, the arrangement is transitional support carrying implicit sunset logic and scaffold-type classification becomes available upon restoration; if permanent, it is steady-state coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditional_vs_permanent_occupation, conceptual, 'Whether the study-occupation is permanent equivalence or implicit transitionality.').

omega_variable(
    interpretive_mediation_residual,
    'Does the interpretive monopoly on fulfillment-validity generate real mediation rents (deference, institutional funding justified by mediation), or is adjudication simply the coordination cost of maintaining a shared standard?',
    'Compare fulfillment-practice across communities with plural contemporaneous decisors: if validity-questions fragment without loss of practice coherence, monopoly rent is negligible; if communities concentrate deference and funding around certification seats beyond coordination need, a rent residual exists.',
    'A demonstrated rent residual would raise the institutional seat''s effective extraction and could move the computed type toward a hybrid coordination/extraction profile despite the reading''s benign core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_mediation_residual, empirical, 'Size of the extraction residual attributable to interpretive monopoly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__study_as_exercise_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement(sacr_tr_t6, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 6, 0.08).
narrative_ontology:measurement(sacr_tr_t12, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(sacr_tr_t18, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 18, 0.11).
narrative_ontology:measurement(sacr_tr_t24, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement(sacr_tr_t30, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(sacr_be_t6, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 6, 0.04).
narrative_ontology:measurement(sacr_be_t12, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 12, 0.05).
narrative_ontology:measurement(sacr_be_t18, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 18, 0.06).
narrative_ontology:measurement(sacr_be_t24, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 24, 0.07).
narrative_ontology:measurement(sacr_be_t30, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 30, 0.08).

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
% The colloquial label 'the sacrificial obligation after the destruction of the Temple' covers at least four structurally distinct claims — that study discharges the duty (this file), that performance alone ever discharges it, that the duty is suspended pending restoration, and that the corpus is heritage without halakhic force. Per the epsilon-invariance principle these are separate constraints with separate epsilon values, beneficiary/victim structures, and failure modes, linked here as one family: this member carries the lowest epsilon and the only no-victim profile; the sibling files carry the contested and potentially extractive profiles. Edges run from this file to each sibling because the talmudic equivalence teaching embedded here is the common source text each sibling rereads.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
