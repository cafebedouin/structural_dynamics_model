% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__study_as_performance, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: sacrifice_obligation_continuity__study_as_performance
 *   human_readable: Sacrifice Obligation Continuity via Textual Study
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   In halakhic Judaism, the obligation to engage with sacrifice law persists
 *   despite the Temple's destruction. This reading—the study-as-performance
 *   reading—interprets the obligation as fulfilled through textual
 *   engagement: mastery of the corpus (Mishnah Zevaim, Talmudic discussions,
 *   halakhic codes), contemplative engagement with its details, and community
 *   transmission of the tradition. Study is not preparation for future Temple
 *   restoration; it is the fulfillment of the obligation itself, in its
 *   current, binding form. The reading sustains a living tradition that keeps
 *   sacrifice law at the center of normative Jewish practice through
 *   intellectual engagement rather than ritual performance. The claim is rope
 *   (genuine coordination: study maintains the tradition's continuity and the
 *   community's engagement with it); the metrics are low because the reading
 *   generates minimal extractiveness (study is accessible, participation is
 *   voluntary within the framework, and no beneficiary is substantially
 *   enriching themselves at another's cost).
 *
 * KEY AGENTS:
 *   - Halakhic interpreters: Define and authenticate what counts as legitimate study of sacrifice law; maintain the authority to declare study fulfillment; sustain the interpretive tradition.
 *   - Study practitioners: Engage in the practice; fulfill the obligation through textual and contemplative engagement; constitute the active community for whom the reading holds.
 *   - Textual tradition community: Preserves and transmits the corpus; sustains the reading's institutional force across generations; ensures study practice remains normative.
 *   - Performance-reading adherents (excluded): Reject study as fulfillment; insist on restoration and physical performance; represent the sibling reading that forecloses this one.
 *   - Messianic-suspension holders (excluded): Hold a third reading—obligation is suspended, not fulfilled or violated; study is preparation, not fulfillment.
 *   - Archival-preservation readers (excluded): Read sacrifice law as binding only during Temple era; study is cultural memory, not legal obligation.
 *   - Textual scholars (observer): Analyze the reading's historical emergence, textual grounding, institutional effects, and relationship to sibling readings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__study_as_performance, 0.18).
domain_priors:suppression_score(sacrifice_obligation_continuity__study_as_performance, 0.12).
domain_priors:theater_ratio(sacrifice_obligation_continuity__study_as_performance, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, extractiveness, 0.18).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__study_as_performance, "Sacrifice Obligation Continuity via Textual Study").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__study_as_performance, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__study_as_performance, '5d381d2b-ae9c-45bc-befd-facaa2f38a7d').
narrative_ontology:cs_kernel_codification('5d381d2b-ae9c-45bc-befd-facaa2f38a7d', distributed).
narrative_ontology:cs_authority_grounding('5d381d2b-ae9c-45bc-befd-facaa2f38a7d', lineage).
narrative_ontology:cs_interpretation_layer_present('5d381d2b-ae9c-45bc-befd-facaa2f38a7d').
narrative_ontology:cs_reading_relation('5d381d2b-ae9c-45bc-befd-facaa2f38a7d', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('5d381d2b-ae9c-45bc-befd-facaa2f38a7d', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('5d381d2b-ae9c-45bc-befd-facaa2f38a7d', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_axiom('5d381d2b-ae9c-45bc-befd-facaa2f38a7d', foundational, study_fulfills_obligation).
narrative_ontology:cs_axiom_status(study_fulfills_obligation, holdable).
narrative_ontology:cs_axiom_grounding('5d381d2b-ae9c-45bc-befd-facaa2f38a7d', study_fulfills_obligation, deontological).
narrative_ontology:cs_axiom('5d381d2b-ae9c-45bc-befd-facaa2f38a7d', foundational, obligation_persists_post_temple).
narrative_ontology:cs_axiom_status(obligation_persists_post_temple, holdable).
narrative_ontology:cs_axiom_grounding('5d381d2b-ae9c-45bc-befd-facaa2f38a7d', obligation_persists_post_temple, conventional).
narrative_ontology:cs_reference_frame('5d381d2b-ae9c-45bc-befd-facaa2f38a7d', rabbinic_textual_obligation_authority).
narrative_ontology:cs_drift_state('5d381d2b-ae9c-45bc-befd-facaa2f38a7d', contemporary_pluralistic_judaism, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('5d381d2b-ae9c-45bc-befd-facaa2f38a7d', '2026-06-11T00:00:00Z').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, halakhic_interpreters).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, study_practitioners).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, textual_tradition_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbinic scholars and legal authorities who interpret and codify the halakhic position that study of sacrifice law fulfills the commandment. They maintain the textual corpus, teach, publish commentaries, and authenticate what counts as legitimate study. Their authority rests on mastery of textual tradition and interpretive precedent.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, halakhic_interpreters, agenda_setter,
    organized, generational, identity_locked, global).

% Individuals who engage in structured study of sacrifice law (Mishnah Zevaim, halakhic codes, responsa). They fulfill the obligation through this engagement without physical performance. Study is accessible (texts available, no special ritual objects required), and the practice integrates into daily religious life and identity.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, study_practitioners, beneficiary,
    moderate, biographical, identity_locked, global).

% The broader halakhic community and scholarly tradition that preserves, interprets, and transmits the corpus of sacrifice law. The reading sustains the tradition's living continuity; study ensures the knowledge does not atrophy, and the obligation's binding force keeps study institutionalized.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, textual_tradition_community, beneficiary,
    organized, civilizational, identity_locked, global).

% Those who hold that study alone does NOT fulfill the obligation; performance (or preparation for future performance) is required for obligation satisfaction. They would reject the study-as-fulfillment framing as inauthentic substitution. They are not in the conversation authorizing this reading but would have the strongest objection to it.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, performance_reading_adherents, excluded,
    organized, generational, identity_locked, global).

% Those who hold that the obligation is suspended (neither fulfilled nor violated) pending messianic restoration. Study is preparation and readiness maintenance, not fulfillment. They represent an alternative reading of the same kernel that rejects both 'study fulfills' and 'study cannot fulfill' in favor of a third position.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, messianic_suspension_holders, excluded,
    organized, generational, identity_locked, global).

% Those who read sacrifice law as binding only while the Temple stood; study now is preservation of cultural memory, not legal obligation. They reject the normative force this reading assigns to study, seeing it as historical rather than prescriptive.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, archival_preservation_readers, excluded,
    organized, generational, identity_locked, global).

% Academic historians of law, comparative religionists, and textual analysts who study the constraint itself: how the reading emerged, its textual groundings, its relationship to other readings, and its institutional effects.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, textual_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__study_as_performance, textual_tradition_community).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__study_as_performance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the living halakhic obligation to engage with sacrifice law despite the absence of a Temple and physical performance. The reading coordinates a community around textual study as a legitimate, continuous form of obligation fulfillment, keeping the tradition's corpus alive and its interpretive work active across generations.
% TRANSFER_FUNCTION: Transfers the obligation from physical ritual performance to textual-intellectual engagement. What flows is authority (the right to claim fulfillment through study), continuity (the tradition persists as living law rather than historical artifact), and identity (community members become obligated participants in the study practice).
% ABSENT_VOICES: Performance-reading adherents, messianic-suspension holders, and archival-preservation readers are structurally excluded from this reading's framework. They would argue that study cannot substitute for performance, that the obligation is suspended rather than transformed, or that it no longer binds. Their objections are not absent from Jewish legal discourse—they are present but formally rejected by the reading's own commitments.
% DISAPPEARANCE_RATIONALE: If this reading (study fulfills the obligation) disappeared and only the performance-only reading remained, the obligation would become inactive and atrophied for the vast majority of Jews. The textual tradition would lose its normative force; study of sacrifice law would become antiquarian rather than obligatory. If both this reading and performance-reading disappeared together, the obligation would be understood as suspended or abrogated, reshaping how the community relates to its past. The constraint's existence keeps study practice institutionalized and normatively binding.
% FOUNDING_PROBLEM: After the destruction of the Second Temple (70 CE), physical sacrifice became impossible, but halakhic authorities faced a theological and legal problem: what happens to the commandment to engage in sacrifice? Is it suspended, abrogated, transferred, or maintained in some other form? Early rabbinic authorities developed the reading that study of sacrifice law itself constitutes fulfillment of the obligation, allowing the practice to continue in textual form.
% FOUNDING_PROBLEM_CORROBORATION: Talmudic texts (Megillah 31b, Berakhot 32b), medieval halakhic authorities (Maimonides, Mishneh Torah Hilchot Temidim 4:11), and early modern responsa cite this reading. Modern halakhic scholars (including Rav Moshe Feinstein, Rav Aharon Lichtenstein) affirm the reading's binding force. Academic historians of Jewish law (Shaye Cohen, Christine Hayes) have documented the historical emergence and rabbinic consensus on this position, corroborating from outside the halakhic system itself that the founding problem (maintaining the obligation post-Temple) is still actively addressed by this reading.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__study_as_performance, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__study_as_performance_tests).
:- end_tests(sacrifice_obligation_continuity__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18 at interval end) because the reading generates minimal asymmetric transfer: study is accessible (texts widely available), participation is voluntary within the framework of commitment, and no identified beneficiary is collecting rents from the arrangement. Suppression is very low (0.12) because alternatives are not foreclosed—a practitioner can reject the reading and adopt performance-only or messianic-suspension without institutional barrier (though with theological cost). Theater is minimal (0.08) because study genuinely fulfills the obligation in this reading's own terms; performative justification is not required. Accessibility collapse is moderate (0.35) because while study texts are available, the cognitive and temporal burden creates a filtering effect—not everyone can or will engage at depth. Resistance is low (0.22) because the reading has deep rabbinic consensus and institutional support; countervailing readings (performance-only, messianic-suspension) exist but are minority positions. The measurement series track a small rise in extractiveness and theater over 2000 years, suggesting gradual intensification of institutional control over what counts as 'legitimate' study and some drift toward justification labor, but from a very low baseline. This is measured from the study-as-performance reading's internal reference frame, not from the performance-reading frame.
 *
 * PERSPECTIVAL GAP:
 *   From the halakhic-interpreter seat, the arrangement is genuine coordination: study fulfills the obligation, keeps the tradition alive, and allows the community to honor its commitments without Temple. From an excluded performance-reading seat, the same arrangement is a theological substitution that evacuates the obligation of its force—study is rationalization, not fulfillment. From an archival-preservation seat, the obligation itself is historical, and the reading treats it as still binding, which is a false transformation. The engine computes these divergences from the structural data: for the study-practitioners, d is near beneficiary (they receive fulfillment-through-study as a benefit, low resistance to exit via identity_locked fusion); for performance-reading adherents, d is near target (the reading forecloses their core understanding, suppressing their voice). The directionality differs because the reading's legitimacy claim is contested at the kernel level.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic interpreters are beneficiaries: they sustain authority, maintain tradition continuity, and receive institutional prestige. Study practitioners are beneficiaries: they access a fulfillment practice available to them. Textual tradition community is beneficiary: it remains living and binding. Performance-reading adherents (excluded) are targets in the weak sense that the reading forecloses their core understanding, but they are not victims (they are not trapped, they hold an alternative reading with institutional adherents). Archival-preservation readers are targets in the same weak sense. The reading has no victim set because within its own frame, no party bears costs without consent or benefit. This is a ropelike situation: genuine coordination (obligation fulfillment, tradition continuity) with no hidden extraction. The low extractiveness and absence of victims reflect this.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (what happens to the obligation post-Temple) is live: rabbinic authorities still engage with it, Jews still feel the obligation binding, and study practice remains institutionalized. The reading prevents mandatrophy by transforming rather than abandoning the obligation. Absent this reading, the obligation would either atrophy (fall silent, forgotten) or require active suppression (become a dead requirement in law books). The study-as-performance reading keeps it active and integrated into practice. This is not mandatrophy resolution; it is mandatrophy prevention—the reading sustains the obligation's normative force by reframing it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_authenticity_vs_substitute,
    'Does study of sacrifice law constitute genuine fulfillment of the obligation, or is it a theologically necessary but ultimately inauthentic substitute for Temple performance?',
    'No empirical resolution possible. Conceptual resolution through theological debate: does the obligation''s binding force rest on the specific form (physical performance) or on the underlying purpose (engagement with divine commandment)? Different halakhic traditions have reached different answers.',
    'If study is genuine fulfillment, the reading is stable and rope-classified. If study is substitute, the obligation''s normative force degrades over time, the reading approaches piton (maintained by tradition inertia rather than intrinsic function), and extractiveness rises as justification labor increases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_authenticity_vs_substitute, conceptual, 'Whether study fulfills the obligation authentically or substitutes for it.').

omega_variable(
    performance_reading_suppression,
    'Is the performance-reading (obligation requires physical performance) actively suppressed by institutional halakhic authority, or does it coexist as a minority position with legitimate space?',
    'Textual history: document whether performance-reading authorities are cited respectfully, debated, or silenced in later compilations. Institutional history: examine whether study-practicing communities have formally rejected performance-reading adherents or simply disagreed.',
    'If performance-reading is suppressed, suppression rises and extractiveness moderately rises (the reading must defend against alternatives). If coexistent, the reading''s low suppression score is appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_reading_suppression, empirical, 'Whether alternative readings are suppressed or coexistent.').

omega_variable(
    kernel_bounded_vs_open,
    'Is the kernel (sacrifice-obligation-post-Temple) fully resolved by these four readings, or are there unacknowledged framings not captured by any of them?',
    'Extensive textual and theoretical analysis of sources from Second Temple period to modern responsa. Check whether any major halakhic authority holds a position not captured by performance-only, study-as-performance, messianic-suspension, or archival-preservation.',
    'If kernel is fully bounded by the four readings, this reading''s relationship to its siblings is structurally complete. If unacknowledged framings exist, the reading''s scope is smaller than claimed, and the kernel description requires revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_bounded_vs_open, empirical, 'Whether the kernel is exhausted by the declared sibling readings.').

omega_variable(
    reading_institutional_capture,
    'Is the study-as-performance reading authorized because it is the strongest theological position, or because it became rabbinic consensus and institutional capture has obscured alternatives?',
    'Historical analysis: trace how the reading emerged in early Tannaitic period; document whether competing readings were rejected on merits or marginalized institutionally. Contemporary analysis: check whether modern practitioners encounter the reading as ''obviously true'' or as ''what I was taught.''',
    'If the reading achieved consensus through rigorous theological debate, it remains rope-classified. If it became dominant through institutional momentum, extractiveness may be underestimated and the reading approaches tangled_rope (coordination of tradition-maintenance + extraction of ideological authority from practitioners).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_institutional_capture, empirical, 'Whether the reading''s authority rests on theological strength or institutional capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__study_as_performance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0, 0.02).
narrative_ontology:measurement_basis(sacr_tr_t0, projected).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 500, 0.03).
narrative_ontology:measurement_basis(sacr_tr_t500, observed).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 1000, 0.05).
narrative_ontology:measurement_basis(sacr_tr_t1000, observed).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 1500, 0.07).
narrative_ontology:measurement_basis(sacr_tr_t1500, observed).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 2000, 0.08).
narrative_ontology:measurement_basis(sacr_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(sacr_be_t0, projected).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 500, 0.08).
narrative_ontology:measurement_basis(sacr_be_t500, observed).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 1000, 0.12).
narrative_ontology:measurement_basis(sacr_be_t1000, observed).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 1500, 0.16).
narrative_ontology:measurement_basis(sacr_be_t1500, observed).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 2000, 0.18).
narrative_ontology:measurement_basis(sacr_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(sacr_su_t0, projected).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 500, 0.09).
narrative_ontology:measurement_basis(sacr_su_t500, observed).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 1000, 0.1).
narrative_ontology:measurement_basis(sacr_su_t1000, observed).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 1500, 0.11).
narrative_ontology:measurement_basis(sacr_su_t1500, observed).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 2000, 0.12).
narrative_ontology:measurement_basis(sacr_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__study_as_performance, 0.1).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel sacrifice_obligation_continuity. The kernel asks: what happens to the halakhic obligation to engage with sacrifice law after Temple destruction? Four structurally distinct constraints, one per reading: study_as_performance (this file), performance_only (physical performance required), messianic_suspension (obligation suspended), archival_preservation (obligation obsolete). Each reading instantiates a different constraint with different ε, different beneficiary/victim structure, different classification. They are linked here to show family relationship; they are not sub-types of one constraint. The readings have different temporal horizons, different institutional adherents, and different implications for Jewish practice. The network edges represent that each reading's classification depends on understanding where it sits in the kernel's dispute space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
