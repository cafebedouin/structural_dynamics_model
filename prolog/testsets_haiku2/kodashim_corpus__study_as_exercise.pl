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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Kodashim Corpus as Living Interpretive Practice (Study-as-Exercise Reading)
 *   domain: religious/rabbinic/commitment_system
 *
 * SUMMARY:
 *   The Kodashim corpus (Orders in the Mishnah dealing with sacrificial law
 *   and temple practices) is a contested kernel in Rabbinic Judaism. Three
 *   substantively different readings instantiate three different constraints:
 *   (1) Study-as-Exercise (this reading): Kodashim is a live kernel; study of
 *   sacrifice law is itself the performance of the mitzvah. (2)
 *   Performance-Only: Kodashim is an archived blueprint awaiting messianic
 *   restoration; the mitzvah is suspended. (3) Substitution-Archive: Prayer
 *   and Torah study replaced sacrifice; Kodashim is memorial documentation of
 *   what was superseded. This story instantiates ONLY the study-as-exercise
 *   reading as a clean ε-invariant constraint: zero extractiveness, zero
 *   suppression, zero theater, pure coordination among scholars who
 *   voluntarily commit to the interpretive practice. The other readings are
 *   not included in this story; they are separate constraints, linked via
 *   network edges. This reading's claim is rope (pure coordination around
 *   shared practice) with no victim class—no one is deprived by this
 *   reading's dominance; the alternative readings coexist as legitimate
 *   positions held by different communities and scholars.
 *
 * KEY AGENTS:
 *   - Torah scholars (powerless individually, organized collectively): engage in interpretive study, understanding it as direct fulfillment of mitzvah
 *   - Rabbinic institutional authority (institutional power): maintains and transmits the study-as-exercise reading, trains scholars, preserves tradition
 *   - Performance-only adherents (organized power, excluded from this reading): hold alternative reading waiting for messianic restoration
 *   - Substitution-archive adherents (organized power, excluded from this reading): hold alternative reading treating Kodashim as memorial archive
 *   - Analytical observer: external examination of how contested kernel is instantiated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__study_as_exercise, 0.0).
domain_priors:suppression_score(kodashim_corpus__study_as_exercise, 0.0).
domain_priors:theater_ratio(kodashim_corpus__study_as_exercise, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, extractiveness, 0.0).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__study_as_exercise, rope).
narrative_ontology:human_readable(kodashim_corpus__study_as_exercise, "Kodashim Corpus as Living Interpretive Practice (Study-as-Exercise Reading)").
narrative_ontology:topic_domain(kodashim_corpus__study_as_exercise, "religious/rabbinic/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__study_as_exercise, '80276613-9a50-4f23-8be3-72e677f8ffc9').
narrative_ontology:cs_kernel_codification('80276613-9a50-4f23-8be3-72e677f8ffc9', fixed_text).
narrative_ontology:cs_authority_grounding('80276613-9a50-4f23-8be3-72e677f8ffc9', lineage).
narrative_ontology:cs_interpretation_layer_present('80276613-9a50-4f23-8be3-72e677f8ffc9').
narrative_ontology:cs_reading_relation('80276613-9a50-4f23-8be3-72e677f8ffc9', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('80276613-9a50-4f23-8be3-72e677f8ffc9', kodashim_corpus__substitution_archive, coexists_with).
narrative_ontology:cs_axiom('80276613-9a50-4f23-8be3-72e677f8ffc9', foundational, study_fulfills_mitzvah).
narrative_ontology:cs_axiom_status(study_fulfills_mitzvah, holdable).
narrative_ontology:cs_axiom_grounding('80276613-9a50-4f23-8be3-72e677f8ffc9', study_fulfills_mitzvah, deontological).
narrative_ontology:cs_axiom('80276613-9a50-4f23-8be3-72e677f8ffc9', foundational, kodashim_kernel_remains_operative).
narrative_ontology:cs_axiom_status(kodashim_kernel_remains_operative, holdable).
narrative_ontology:cs_axiom_grounding('80276613-9a50-4f23-8be3-72e677f8ffc9', kodashim_kernel_remains_operative, theological).
narrative_ontology:cs_reference_frame('80276613-9a50-4f23-8be3-72e677f8ffc9', post_temple_rabbinic_fulfillment).
narrative_ontology:cs_drift_state('80276613-9a50-4f23-8be3-72e677f8ffc9', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('80276613-9a50-4f23-8be3-72e677f8ffc9', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__study_as_exercise, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, torah_scholars).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, rabbinic_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, rabbinic_institutional_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in rigorous textual study of Kodashim. They understand their interpretive work as the actual fulfillment of the mitzvah—the commandment to study and understand the sacrificial system is itself the commanded act. Their benefit is the direct coordination of intellectual, spiritual, and cosmic maintenance through this engagement. No payment or coercion binds them; participation is voluntary and sustained by conviction that the study itself is the commanded exercise.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, torah_scholars, beneficiary,
    organized, generational, mobile, global).

% Maintains and transmits the interpretive tradition reading Kodashim as a live kernel. They set the curriculum, train scholars, and preserve the reading that study fulfills the mitzvah. They benefit from the continuation of the tradition and the spiritual authority that comes from being custodians of this knowledge. They do not extract material benefit; their authority rests on interpretive legitimacy and community acceptance of their reading.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, rabbinic_institutional_authority, agenda_setter,
    institutional, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__study_as_exercise, rabbinic_institutional_authority, beneficiary).

% Hold an alternative reading: that Kodashim is an archived blueprint awaiting messianic restoration, that the mitzvah is suspended, and that study is preparation/commemoration rather than direct fulfillment. They are not suppressed by the study-as-exercise reading; they maintain their position through separate communities of practice and textual authority.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, performance_only_community, excluded,
    organized, civilizational, mobile, global).

% Hold an alternative reading: that prayer and Torah study replaced the sacrificial system, that Kodashim documents a superseded covenant, and that study is memorial rather than occupancy of an operative kernel. Like the performance-only reading, this is not suppressed by the study-as-exercise reading but represents a genuinely different interpretation of what the corpus is.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, substitution_archive_community, excluded,
    organized, civilizational, mobile, global).

% Examines the constraint from outside: how a contested textual/theological kernel is instantiated as a coordination mechanism, and how the competing readings coexist without mutual suppression in contemporary Rabbinic Judaism.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__study_as_exercise, diffuse).
narrative_ontology:fixing_cost_class(kodashim_corpus__study_as_exercise, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates scholars and the rabbinic community around a shared interpretive practice: the continuous intellectual-spiritual engagement with Kodashim as a living kernel of Jewish law and theology. The coordination problem solved is: how do we maintain, transmit, and actualize a body of law whose original performance context (the temple sacrificial system) is unavailable? Answer: through interpretive study as itself the fulfillment of the commandment.
% TRANSFER_FUNCTION: No transfer occurs. This is pure coordination—the constraint channels intellectual and spiritual labor into shared practice but produces no extraction from one seat to another. Study flows to study; interpretation flows to interpretation; authority flows to those who maintain the tradition. No material or status is moved from one seat to another as a condition of participation.
% ABSENT_VOICES: Performance-only and substitution-archive readers are excluded from instantiating THIS reading's constraint, but they are not suppressed voices—they maintain their own competing readings and communities. A voice that would be genuinely absent: someone who would argue that the Kodashim corpus should be abandoned or that study of it is spiritually inert. Such a voice exists nowhere in the contemporary Rabbinic ecosystem; the constraint is not defended against it because no real advocate exists to exclude.
% DISAPPEARANCE_RATIONALE: If this constraint—the reading of Kodashim as a live kernel occupied through study—disappeared, the corpus would not vanish, the texts would not be burned, and scholars would not stop engaging with them. What would change is the MEANING of the engagement: study would become commemoration, archive-reading, or preparation for messianic restoration, rather than direct fulfillment. The world rearranges only if the alternative readings are wrong—if this reading is the true account of the kernel, then disappearing it leaves the kernel itself unchanged but orphaned of its proper interpretation.
% FOUNDING_PROBLEM: After the destruction of the Second Temple, the sacrificial system could no longer be performed. A core body of Rabbinic law (Kodashim) describes the sacrificial system in elaborate detail. The founding problem: how do we maintain the commandments regarding sacrifice when the performance context is gone? How do we honor and transmit the halakhic knowledge? How do we preserve the mitzvot that cannot be physically performed?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (post-Temple halakhic orphaning) is corroborated by historical fact: the Temple was destroyed in 70 CE, and the rabbis thereafter created a legal corpus on sacrifice despite its unavailability. Different readings contest what this corroboration means. The study-as-exercise reading is corroborated by Talmudic and medieval sources (Talmud Menachot 110a, Maimonides Mishneh Torah, responsa tradition) that explicitly state study of the halakhah fulfills the mitzvah. The performance-only reading is corroborated by waiting for messianic restoration. The substitution-archive reading is corroborated by the Rabbinic doctrine that prayer replaced sacrifice. No corroborating voice exists outside the Rabbinic tradition itself; all three readings originate within it. The corroboration comes from textual authority (Talmudic and Maimonidean precedent) internal to the tradition, not from external witnesses.
narrative_ontology:disappearance_verdict(kodashim_corpus__study_as_exercise, world_unchanged).
narrative_ontology:founding_problem_status(kodashim_corpus__study_as_exercise, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__study_as_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_corpus__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__study_as_exercise, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is zero (0.0) because this reading produces no extraction: no scholar is deprived, no material or status transfers from one seat to another, no coercion maintains the constraint. Study is voluntary; benefit and cost are symmetrically distributed (scholars spend intellectual effort, receive spiritual-intellectual fulfillment; rabbinic authority spends maintenance effort, receives authority and tradition-continuity). Theater is zero (0.0) because the constraint is all function and no performance: the study IS the commanded act; there is no gap between the functional core and theatrical maintenance. Suppression is zero (0.0) because no force is needed to hold this reading in place for those who hold it—it is held by conviction. Accessibility collapse is high (0.95) because once a scholar understands this reading (that study fulfills the mitzvah), the alternative of abandoning Kodashim becomes conceptually very costly—not because exit is blocked, but because the reading makes abandonment contradict one's own commitments. Resistance is near-zero (0.05) because this reading faces no organized resistance from within the communities that hold it; the alternative readings coexist peacefully in separate communities.
 *
 * PERSPECTIVAL GAP:
 *   All seats in this constraint should compute the same type because they occupy the same role: they are all coordinators or participants in the shared practice. Scholars and rabbinic authority both benefit from continuation of the tradition and both contribute to its maintenance. No seat collects extraction; no seat bears coercive burden. The engine should compute rope from every seat because the structure is symmetric: voluntary participation, shared benefit, no transfer from payer to beneficiary. The only perspectival gap is BETWEEN this reading and the alternative readings (performance-only, substitution-archive), which is why those are separate constraint stories.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is near-zero (d ≈ 0.0) for all seats because all are structural beneficiaries of the reading's continuance. Torah scholars benefit from having a coherent interpretation that makes their study meaningful. Rabbinic authority benefits from maintaining the tradition. No seat is a target (d ≈ 1.0) because no seat bears extraction. The constraint is not enforced against resistance; it is held by conviction. The exclusion of performance-only and substitution-archive adherents is NOT a suppression—those readers maintain their own communities and readings; they are excluded only from THIS reading's instantiation, not from scholarly or religious practice itself.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not present in this reading because the founding problem (what to do about sacrifice law post-Temple) remains actively engaged. The study-as-exercise reading directly addresses the founding problem by making study itself the fulfillment. The constraint could only become mandatrophic if scholars stopped finding meaning in this reading and abandoned Kodashim study—but that abandonment would be a change in the constraint's structure, not theater masking decline. The alternative readings (performance-only, substitution-archive) do engage with mandatrophy questions: if the performance-only reading is correct, then the constraint is theater (waiting for messianic restoration); if substitution-archive is correct, then the constraint is piton (the mitzvah is dead but the text is preserved for memory). This reading does NOT present mandatrophy because it denies the premises that would generate it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_live_vs_archived,
    'Is the Kodashim corpus a live operative kernel (the study-as-exercise reading claims yes) or an archived blueprint awaiting messianic restoration (performance-only) or a memorial to what was superseded (substitution-archive)?',
    'This is a conceptual/theological omega, not empirically resolvable. Resolution would require accepting or rejecting the theological premises: does the rabbinical doctrine that ''study fulfills the mitzvah'' hold valid authority? Or does messianic restoration reserve the true fulfillment for future performance? Or does post-Temple theology treat the sacrificial system as genuinely superseded by prayer?',
    'If the kernel is live (study-as-exercise is true), this reading is rope with zero extractiveness. If the kernel is archived (performance-only is true), this reading becomes theater/piton—study is preparation, not fulfillment. If the kernel is memorial (substitution-archive is true), study is commemoration of what was, not occupation of what is.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_live_vs_archived, conceptual, 'Whether Kodashim is a live operative kernel, archived blueprint, or memorial—the reading-differentiating question at the theological level.').

omega_variable(
    study_fulfillment_axiom_grounding,
    'On what authority rests the claim that study of sacrifice law fulfills the mitzvah? Is it grounded in Talmudic precedent, medieval jurisprudential innovation, or contemporary reinterpretation?',
    'Textual and historical scholarship examining the sources: does Talmud Menachot 110a actually support this reading, or is it a medieval or modern extrapolation? What is the genealogy of the claim in Jewish legal and theological literature?',
    'If the claim is grounded in early Talmudic authority (Menachot 110a and earlier sources), the axiom is foundational and holds robust authority. If it emerges later (medieval or modern), the axiom''s status is ''secondary'' rather than foundational, and alternative readings might claim equal or greater precedent. This affects whether the study-as-exercise reading is the dominant reading or one among several live options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_fulfillment_axiom_grounding, empirical, 'The historical/textual grounding of the ''study fulfills the mitzvah'' axiom in rabbinic sources.').

omega_variable(
    coexistence_vs_hierarchy,
    'Do the three readings (study-as-exercise, performance-only, substitution-archive) genuinely coexist as equally legitimate positions, or does one reading hold hierarchical authority that subordinates the others?',
    'Examination of contemporary Rabbinic community practice: do different Yeshivot, movements, and scholars accept multiple readings, or is one reading treated as the normative standard? Is dissent welcomed or marginalized?',
    'If the readings coexist, the constraint is rope-like for all readers (pure coordination within each reading''s community). If one reading subordinates the others, those other readings experience the dominant reading as an imposed structure, shifting toward snare for the subordinated readers. This affects whether the overall kernel-system is a pure coordination or a hierarchical extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coexistence_vs_hierarchy, empirical, 'Whether the three readings coexist as legitimate alternatives or whether one dominates, subordinating the others.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__study_as_exercise, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__study_as_exercise, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_corpus__study_as_exercise, 0.05).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% The Kodashim corpus is a contested kernel instantiated by three structurally distinct constraint stories: (1) study-as-exercise (this story) — zero extractiveness, rope, study fulfills mitzvah; (2) performance-only — theater/piton candidate, study is preparation for messianic restoration; (3) substitution-archive — piton, study is memorial to superseded covenant. Each story has its own epsilon, beneficiary structure, and computed type. The three readings coexist as live positions within contemporary Rabbinic Judaism. They are linked through this network because each reading influences the legitimacy conditions and interpretive context of the others, even though they remain separate constraint instantiations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
