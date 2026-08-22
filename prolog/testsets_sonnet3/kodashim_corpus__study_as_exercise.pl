% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__study_as_exercise, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: kodashim_corpus__study_as_exercise
 *   human_readable: Talmudic Study of Kodashim as Living Fulfillment of the Sacrificial Mitzvah
 *   domain: religious/legal-hermeneutic
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the kodashim_corpus kernel — the
 *   standing commitment to how the biblical sacrificial legislation (Seder
 *   Kodashim) remains religiously operative after the physical cessation of
 *   Temple sacrifice in 70 CE. Under the study_as_exercise reading, sustained
 *   rabbinic study of the sacrificial tractates is not preparation for a
 *   future restored performance, nor a memorial of a superseded practice, but
 *   IS itself, presently and completely, the occupation of the kernel — the
 *   mitzvah is fulfilled through the act of engaged study. This produces a
 *   near-zero-extraction, victimless, coordination-only constraint: a genuine
 *   rope. This is deliberately narrower than 'the kodashim corpus' as a
 *   colloquial label — the sibling readings (performance_only: kernel as
 *   dormant blueprint awaiting restoration; substitution_archive: kernel as
 *   memorial record of a superseded practice) are structurally distinct
 *   constraints with their own ε and are NOT folded into this file, per the
 *   ε-invariance principle.
 *
 * KEY AGENTS:
 *   - torah_scholars: Primary agenda-setter and beneficiary (institutional/arbitrage) — occupy the kernel through study; no cost borne
 *   - yeshiva_study_communities: Institutional beneficiary (organized/mobile) — organizes curricula around this reading with no enforcement dependency
 *   - jewish_people_collectively: Diffuse collective beneficiary (moderate/constrained) — benefits vicariously per traditional merit doctrine
 *   - temple_restorationists: Excluded voice — holds a sibling reading (performance_only) not incorporated here
 *   - comparative_religion_scholars: Analytical observer — documents the doctrine's historical development without adjudicating between readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__study_as_exercise, 0.03).
domain_priors:suppression_score(kodashim_corpus__study_as_exercise, 0.06).
domain_priors:theater_ratio(kodashim_corpus__study_as_exercise, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, extractiveness, 0.03).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, suppression_requirement, 0.06).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__study_as_exercise, rope).
narrative_ontology:human_readable(kodashim_corpus__study_as_exercise, "Talmudic Study of Kodashim as Living Fulfillment of the Sacrificial Mitzvah").
narrative_ontology:topic_domain(kodashim_corpus__study_as_exercise, "religious/legal-hermeneutic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__study_as_exercise, '9402768d-a0b3-4a49-8dfa-423474594c48').
narrative_ontology:cs_kernel_codification('9402768d-a0b3-4a49-8dfa-423474594c48', fixed_text).
narrative_ontology:cs_authority_grounding('9402768d-a0b3-4a49-8dfa-423474594c48', practice).
narrative_ontology:cs_interpretation_layer_present('9402768d-a0b3-4a49-8dfa-423474594c48').
narrative_ontology:cs_reading_relation('9402768d-a0b3-4a49-8dfa-423474594c48', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('9402768d-a0b3-4a49-8dfa-423474594c48', kodashim_corpus__substitution_archive, coexists_with).
narrative_ontology:cs_axiom('9402768d-a0b3-4a49-8dfa-423474594c48', foundational, study_constitutes_complete_performance).
narrative_ontology:cs_axiom_status(study_constitutes_complete_performance, holdable).
narrative_ontology:cs_axiom_grounding('9402768d-a0b3-4a49-8dfa-423474594c48', study_constitutes_complete_performance, theological).
narrative_ontology:cs_axiom('9402768d-a0b3-4a49-8dfa-423474594c48', secondary, intellectual_engagement_maintains_cosmic_order).
narrative_ontology:cs_axiom_status(intellectual_engagement_maintains_cosmic_order, holdable).
narrative_ontology:cs_axiom_grounding('9402768d-a0b3-4a49-8dfa-423474594c48', intellectual_engagement_maintains_cosmic_order, theological).
narrative_ontology:cs_reference_frame('9402768d-a0b3-4a49-8dfa-423474594c48', temple_era_sacrificial_performance).
narrative_ontology:cs_drift_state('9402768d-a0b3-4a49-8dfa-423474594c48', post_destruction_rabbinic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9402768d-a0b3-4a49-8dfa-423474594c48', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__study_as_exercise, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, torah_scholars).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, yeshiva_study_communities).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, jewish_people_collectively).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, torah_study_equals_sacrificial_performance).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, continuous_engagement_maintains_cosmic_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Study the tractates of Kodashim (Zevachim, Menachot, and related sacrificial law) as an ongoing intellectual-spiritual discipline. They hold, following talmudic and later halakhic sources (e.g. the principle 'ha'osek betorat chatat ke'ilu hikriv chatat'), that the act of study itself occupies the place the sacrifice once occupied. Their engagement is voluntary, self-directed, and requires no temple, no priesthood, and no physical object — the study hall itself is the site of fulfillment. Exit is not meaningfully constrained: nothing compels study, and those who do not study are not deprived of anything the constraint holds itself out as providing.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, torah_scholars, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__study_as_exercise, torah_scholars, beneficiary).

% Institutions (yeshivot, batei midrash) organize curricula around Seder Kodashim as part of the full cycle of Talmudic study (e.g. the Daf Yomi cycle). They receive social and institutional coherence from a shared interpretive practice that requires no external validation or physical infrastructure to sustain itself. They can adopt, de-emphasize, or restructure their study cycles without threat of sanction from any enforcing body.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, yeshiva_study_communities, beneficiary,
    organized, generational, mobile, global).

% Under this reading, the community as a whole benefits from the ongoing occupation of the sacrificial kernel through study, understood as maintaining cosmic/covenantal order in the absence of the Temple. Individuals who do not personally study still benefit vicariously through the community's continuous engagement, per traditional accounts of collective merit; their exit option is constrained only in the sense that they cannot personally opt out of being members of the benefiting collective, not in the sense of being coerced into study.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, jewish_people_collectively, beneficiary,
    moderate, civilizational, constrained, global).

% Hold the sibling performance_only reading: that the sacrificial mitzvah remains unperformed and the corpus is a dormant blueprint awaiting a rebuilt Temple. From their view, study_as_exercise risks defusing the eschatological urgency of restoration by declaring the kernel already satisfied through intellectual engagement. They are not consulted by this reading's proponents and their objection does not register within the study_as_exercise framework, which treats the question of physical restoration as orthogonal to whether study currently fulfills the mitzvah.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, temple_restorationists, excluded,
    moderate, civilizational, constrained, global).

% Study the historical development of the 'study as sacrifice' doctrine (tracing its roots to Hoshea 14:3 as read rabbinically, and its elaboration in Talmud Menachot 110a and later authorities) as a case of religious innovation substituting textual practice for ritual practice after the loss of a physical cultic site. They document but do not adjudicate between the three readings.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__study_as_exercise, diffuse).
narrative_ontology:fixing_cost_class(kodashim_corpus__study_as_exercise, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared interpretive practice — sustained collective study of sacrificial law — that lets a textually-organized religious community maintain continuity of religious obligation and meaning after the physical infrastructure (the Temple, the priesthood, the altar) that the original commandments presupposed ceased to exist.
% TRANSFER_FUNCTION: Nothing is extracted or transferred from any party to another. Time and attention are voluntarily invested by scholars and communities in exchange for the good the practice itself declares itself to provide (fulfillment of the mitzvah, communal merit, continuity of meaning) — there is no payer class distinct from the beneficiary class.
% ABSENT_VOICES: Temple restorationists (holding the performance_only reading) would object that declaring study a complete fulfillment risks removing the felt urgency of physical restoration; they are not structurally excluded from Jewish communal life, but their view is a different reading of the same kernel and is not incorporated into this reading's internal logic.
% DISAPPEARANCE_RATIONALE: If communal study of Kodashim vanished, proponents of this reading hold the world would meaningfully rearrange (a mode of maintaining cosmic/covenantal order would be lost); proponents of the substitution_archive reading would say little rearranges structurally since, on their view, prayer already carries the substituted function and study is supplementary; the verdict is genuinely contested between readings of the same kernel, not resolvable from within this story alone.
% FOUNDING_PROBLEM: After the destruction of the Second Temple (70 CE), the physical performance of korbanot (sacrifices) became impossible, leaving open the question of how the biblical commandments concerning sacrifice remain religiously operative for a people without a Temple.
% FOUNDING_PROBLEM_CORROBORATION: The scholars and communities who hold this reading attest that the founding problem (how to fulfill an unperformable mitzvah) is actively and continuously resolved by study itself, citing classical sources (Talmud Menachot 110a, Vayikra Rabbah 7:3) as external-to-the-present-moment corroboration predating any contemporary institutional interest. Comparative religion scholars, an analytical seat outside the community of practice, corroborate that the doctrine is textually attested across centuries and not a recent innovation, though they do not corroborate its theological truth-claim, only its historical continuity as a living practice.
narrative_ontology:disappearance_verdict(kodashim_corpus__study_as_exercise, contested).
narrative_ontology:founding_problem_status(kodashim_corpus__study_as_exercise, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__study_as_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_corpus__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__study_as_exercise, 0.03, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__study_as_exercise_tests).
:- end_tests(kodashim_corpus__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.03) because under this reading nothing is extracted from anyone — no payer class exists, no coercive enforcement is required, and no one is deprived of anything by either participating or not participating in the study practice. Suppression is likewise near zero (0.06): no one is compelled to study, and the practice's legitimacy does not depend on suppressing alternatives (the sibling readings coexist openly in the same tradition). Theater ratio is low (0.08) and rises only marginally over the interval — the practice has genuine intellectual and spiritual content and is not, on its own account, primarily performative, though some contemporary drift toward routinized ('doing the daf') rather than deeply engaged study is documented at later time points. Accessibility collapse is moderate (0.15): the study_as_exercise doctrine has become sufficiently normative within traditional Ashkenazi/Sephardi rabbinic culture that few practitioners actively entertain the sibling readings as live alternatives for their own practice, even though all three remain textually available. Resistance is very low (0.05) — this reading meets essentially no organized opposition; disagreement, where it exists, takes the form of a different reading of the same kernel (performance_only, substitution_archive) rather than resistance to this one.
 *
 * DIRECTIONALITY LOGIC:
 *   Every named party in this reading is a beneficiary or a neutral observer; there is no victim group and therefore no beneficiary/victim asymmetry to compute. Torah scholars sit at the beneficiary end of directionality because the constraint's entire operation subsidizes their spiritual and intellectual life at no cost extracted from any other party. Yeshiva communities and the collective similarly sit near the beneficiary end. Temple restorationists are marked excluded rather than payer — they are not harmed by this reading, they simply hold a different reading of the same underlying kernel, which is a Rule-4 sibling relationship, not a directionality relationship within this story.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy almost by construction: since the founding problem (how to maintain the sacrificial mitzvah without a Temple) and the proposed solution (occupation-through-study) are asserted by this reading to be perpetually and simultaneously coextensive — the mitzvah IS the studying, not a proxy awaiting the real thing — there is no drift target for the mandate to outlive. The founding_problem_status is authored as contested rather than resolved precisely because sibling readings (performance_only) hold the founding problem to be still fully live and unresolved by study, while this reading holds it perpetually resolved by the act of engagement itself. That contest is exactly what Rule 1/Rule 2 route to the omega variables and cs_structure.reading_relations rather than blending into this story's own metrics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_complete_fulfillment_or_partial_placeholder,
    'Does textual study genuinely and completely occupy the sacrificial kernel (this reading), or does it only partially stand in for an obligation that remains formally unfulfilled pending restoration (the performance_only reading)?',
    'No empirical resolution is available — this is a live doctrinal dispute within rabbinic literature itself, addressed differently across sources (compare Talmud Menachot 110a''s strong claim with more restorationist-leaning halakhic authorities). Resolution, if any, would come from internal legal-theological argument, not external evidence.',
    'If the sibling performance_only reading is correct, this story''s near-zero extractiveness and victimless rope classification would not apply to the underlying kernel as a whole — a further story (already declared as a sibling) would carry the unresolved-longing structure instead. Each story keeps its own ε per the decomposition principle; this omega documents why they diverge rather than attempting to average them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_as_complete_fulfillment_or_partial_placeholder, conceptual, 'Whether study is a complete or partial occupation of the sacrificial kernel — the central contest between this reading and its performance_only sibling.').

omega_variable(
    study_versus_prayer_as_functional_substitute,
    'Is the operative substitute for sacrifice study itself (this reading) or prayer, with study functioning as a secondary/documentary practice (the substitution_archive reading)?',
    'Historical-liturgical analysis of which practice (Amidah/prayer service timed to correspond to sacrificial times, versus dedicated Kodashim study) was and is treated by practicing communities as bearing the primary weight of religious obligation; textual analysis of the relevant halakhic sources on their own terms.',
    'If prayer is the true functional substitute and study is memorial/archival, this reading''s beneficiary structure would shift — the coordination function claimed here (cosmic order maintained through study) would be more accurately located in prayer practice, and this story''s claimed rope classification would need re-examination against the substitution_archive sibling''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_versus_prayer_as_functional_substitute, conceptual, 'Whether study or prayer bears the substituted function for sacrifice — the central contest between this reading and its substitution_archive sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__study_as_exercise, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__study_as_exercise, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(koda_tr_t0, projected).
narrative_ontology:measurement(koda_tr_t300, kodashim_corpus__study_as_exercise, theater_ratio, 300, 0.06).
narrative_ontology:measurement_basis(koda_tr_t300, projected).
narrative_ontology:measurement(koda_tr_t700, kodashim_corpus__study_as_exercise, theater_ratio, 700, 0.07).
narrative_ontology:measurement_basis(koda_tr_t700, projected).
narrative_ontology:measurement(koda_tr_t1100, kodashim_corpus__study_as_exercise, theater_ratio, 1100, 0.07).
narrative_ontology:measurement_basis(koda_tr_t1100, projected).
narrative_ontology:measurement(koda_tr_t1500, kodashim_corpus__study_as_exercise, theater_ratio, 1500, 0.08).
narrative_ontology:measurement_basis(koda_tr_t1500, observed).
narrative_ontology:measurement(koda_tr_t1900, kodashim_corpus__study_as_exercise, theater_ratio, 1900, 0.08).
narrative_ontology:measurement_basis(koda_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__study_as_exercise, base_extractiveness, 0, 0.02).
narrative_ontology:measurement_basis(koda_be_t0, projected).
narrative_ontology:measurement(koda_be_t300, kodashim_corpus__study_as_exercise, base_extractiveness, 300, 0.02).
narrative_ontology:measurement_basis(koda_be_t300, projected).
narrative_ontology:measurement(koda_be_t700, kodashim_corpus__study_as_exercise, base_extractiveness, 700, 0.03).
narrative_ontology:measurement_basis(koda_be_t700, projected).
narrative_ontology:measurement(koda_be_t1100, kodashim_corpus__study_as_exercise, base_extractiveness, 1100, 0.03).
narrative_ontology:measurement_basis(koda_be_t1100, projected).
narrative_ontology:measurement(koda_be_t1500, kodashim_corpus__study_as_exercise, base_extractiveness, 1500, 0.03).
narrative_ontology:measurement_basis(koda_be_t1500, observed).
narrative_ontology:measurement(koda_be_t1900, kodashim_corpus__study_as_exercise, base_extractiveness, 1900, 0.03).
narrative_ontology:measurement_basis(koda_be_t1900, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_corpus__study_as_exercise, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__study_as_exercise, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_corpus__study_as_exercise, 0.08).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposed from the colloquial label 'the Kodashim corpus' per the ε-invariance principle: performance_only (dormant blueprint awaiting restoration — expected non-trivial suppression around unresolved longing), study_as_exercise (this story — near-zero extraction, complete present fulfillment through study, rope), and substitution_archive (memorial record, prayer as the true functional substitute — expected near-mountain/documentary function). Each carries its own ε, its own beneficiary/victim structure, and its own claimed_type; they are linked via affects_constraints rather than merged, since measuring 'the Kodashim corpus' by different observables (is it occupied? is it dormant? is it memorial?) yields incompatible ε values — the classic signal that one label covers multiple constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
