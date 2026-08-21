% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__uncreated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__uncreated_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: quran_ontological_status__uncreated_reading
 *   human_readable: Qur'an as Uncreated Eternal Divine Speech (Uncreated Reading)
 *   domain: islamic_theology/philosophy_of_language/political_authority
 *
 * SUMMARY:
 *   This constraint instantiates the 'uncreated_reading' of the
 *   'quran_ontological_status' kernel. It posits the Qur'an as coeternal with
 *   God, an uncreated divine attribute, rather than a created artifact. This
 *   reading maximizes prophetic authority, privileges literalist
 *   hermeneutics, and treats textual meaning as fixed divine fact. From this
 *   perspective, the constraint functions as a permanent mountain, providing
 *   an immutable foundation for Islamic theology and law. The low
 *   extractiveness reflects its self-conception as divine truth, while high
 *   suppression reflects its active defense against alternative theological
 *   positions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, 0.1).
domain_priors:suppression_score(quran_ontological_status__uncreated_reading, 0.85).
domain_priors:theater_ratio(quran_ontological_status__uncreated_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__uncreated_reading, mountain).
narrative_ontology:human_readable(quran_ontological_status__uncreated_reading, "Qur'an as Uncreated Eternal Divine Speech (Uncreated Reading)").
narrative_ontology:topic_domain(quran_ontological_status__uncreated_reading, "islamic_theology/philosophy_of_language/political_authority").

domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__uncreated_reading, '884755a8-3820-4391-8dc3-06ce3ec56782').
narrative_ontology:cs_kernel_codification('884755a8-3820-4391-8dc3-06ce3ec56782', fixed_text).
narrative_ontology:cs_authority_grounding('884755a8-3820-4391-8dc3-06ce3ec56782', lineage).
narrative_ontology:cs_interpretation_layer_present('884755a8-3820-4391-8dc3-06ce3ec56782').
narrative_ontology:cs_reading_relation('884755a8-3820-4391-8dc3-06ce3ec56782', quran_ontological_status__created_reading, forecloses).
narrative_ontology:cs_reading_relation('884755a8-3820-4391-8dc3-06ce3ec56782', quran_ontological_status__state_enforced_creation_reading, forecloses).
narrative_ontology:cs_axiom('884755a8-3820-4391-8dc3-06ce3ec56782', foundational, quran_coeternal_with_god).
narrative_ontology:cs_axiom_status(quran_coeternal_with_god, holdable).
narrative_ontology:cs_axiom_grounding('884755a8-3820-4391-8dc3-06ce3ec56782', quran_coeternal_with_god, deontological).
narrative_ontology:cs_axiom('884755a8-3820-4391-8dc3-06ce3ec56782', secondary, divine_speech_immutable).
narrative_ontology:cs_axiom_status(divine_speech_immutable, holdable).
narrative_ontology:cs_axiom_grounding('884755a8-3820-4391-8dc3-06ce3ec56782', divine_speech_immutable, deontological).
narrative_ontology:cs_reference_frame('884755a8-3820-4391-8dc3-06ce3ec56782', classical_sunni_orthodoxy).
narrative_ontology:cs_drift_state('884755a8-3820-4391-8dc3-06ce3ec56782', contemporary_theological_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('884755a8-3820-4391-8dc3-06ce3ec56782', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__uncreated_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, traditional_jurists).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, literalist_communities).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, anti_rationalist_schools).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, rational_theologians).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, metaphorical_interpreters).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, reform_movements).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, divine_immutability).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, prophetic_infallibility).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, textual_inerrancy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As custodians of the sacred text and its traditional interpretation, they derive immense authority and legitimacy from the belief that the Qur'an is uncreated divine speech. This position solidifies their role in adjudicating legal and theological matters, and they actively defend this doctrine.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, traditional_jurists, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, traditional_jurists, beneficiary).

% These communities find certainty and a clear framework for life in the uncreated nature of the Qur'an, which supports a literalist hermeneutic. Their identity is often deeply intertwined with this theological stance, providing a stable worldview.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, literalist_communities, beneficiary,
    organized, generational, identity_locked, global).

% For schools emphasizing revelation over human reason, the uncreated Qur'an serves as the ultimate, unquestionable source of truth, reinforcing their epistemological framework and providing a bulwark against philosophical challenges.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, anti_rationalist_schools, beneficiary,
    organized, generational, identity_locked, global).

% Historically, rationalist theologians (like the Mu'tazilites) argued for the Qur'an's createdness to preserve divine unity and allow for human free will. This reading suppresses their theological positions, often leading to marginalization or persecution.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, rational_theologians, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, rational_theologians, excluded).

% Those who seek metaphorical or allegorical interpretations of the Qur'an find their methods challenged and often delegitimized by the uncreated doctrine, which favors a fixed, literal meaning. Their interpretive flexibility is curtailed.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, metaphorical_interpreters, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, metaphorical_interpreters, excluded).

% Movements advocating for reinterpretation or contextualization of Islamic law and ethics often face significant resistance from the uncreated doctrine, which treats the text as immutable and its traditional interpretations as divinely sanctioned, limiting their ability to enact change.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, reform_movements, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, reform_movements, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__uncreated_reading, traditional_jurists).
narrative_ontology:fixing_cost_class(quran_ontological_status__uncreated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed, immutable, and universally authoritative source of divine law and moral guidance, coordinating belief and practice across diverse Muslim communities by establishing a singular, unquestionable textual referent.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority and legitimacy from human reason or contextual understanding to the divinely revealed, uncreated text, thereby empowering traditional custodians of that text and its established meanings.
% ABSENT_VOICES: Rationalist theologians, philosophical schools, and reformist thinkers who argue for a created Qur'an or more flexible hermeneutics are often excluded from mainstream discourse or actively suppressed, their arguments deemed heterodox or dangerous to the foundational belief.
% DISAPPEARANCE_RATIONALE: If the belief in the Qur'an's uncreated nature vanished, the entire edifice of traditional Islamic theology, jurisprudence, and political authority would undergo a profound reordering. The basis for prophetic infallibility, textual inerrancy, and the authority of traditional scholars would be fundamentally challenged, leading to a radical shift in hermeneutics and the structure of religious authority.
% FOUNDING_PROBLEM: To establish the absolute, unquestionable authority of the Qur'an as the direct, eternal word of God, thereby unifying the nascent Muslim community under a singular divine mandate and providing an immutable source of law and ethics.
% FOUNDING_PROBLEM_CORROBORATION: Traditional Islamic scholars and the vast majority of Sunni Muslim communities attest that the problem of establishing and maintaining divine authority and textual immutability remains live and central to their faith. This is corroborated by centuries of theological consensus and the continued emphasis on this doctrine in religious education and discourse.
narrative_ontology:disappearance_verdict(quran_ontological_status__uncreated_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__uncreated_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__uncreated_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(quran_ontological_status__uncreated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__uncreated_reading, 0.1, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__uncreated_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, ExtMetricName, E),
    domain_priors:suppression_score(quran_ontological_status__uncreated_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(quran_ontological_status__uncreated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is set to a very low value (0.1) because, from the perspective of this reading, the Qur'an as uncreated divine speech is an ontological truth, not a human construct designed for extraction. It is the source of all legitimate authority, not a mechanism for rent-seeking. However, `suppression` is very high (0.85) because this doctrine actively forecloses and delegitimizes alternative theological positions (e.g., the createdness of the Qur'an) and interpretive methodologies (e.g., metaphorical readings). `Accessibility_collapse` is high (0.9) as it claims ultimate truth, leaving little room for alternatives. `Resistance` is moderate (0.4) reflecting historical and ongoing challenges to this doctrine. `Theater_ratio` is low (0.1) as the belief is deeply held and foundational, not performative. The measurement series show relative stability, reflecting the claim of eternal truth, with slight fluctuations in suppression as the doctrine defends itself against challenges over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of its beneficiaries (traditional jurists, literalist communities), this constraint is a foundational, immutable truth (a mountain) that provides order and divine guidance. From the perspective of its victims (rational theologians, reform movements), it functions as a powerful, suppressive force that limits intellectual inquiry and adaptation, effectively trapping them within a rigid interpretive framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional jurists, literalist communities, and anti-rationalist schools are structural beneficiaries; they gain authority, certainty, and a reinforced worldview from this doctrine. Rational theologians, metaphorical interpreters, and reform movements are targets/victims, as their positions are suppressed and delegitimized by this foundational claim. The constraint subsidizes the former by providing an immutable source of authority, while extracting from the latter by limiting their interpretive and theological freedom.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_theological_construct,
    'Is the uncreated nature of the Qur''an an ontological truth (a genuine mountain) or a theological construct that benefits specific institutional and interpretive groups (a false summit)?',
    'Analysis of the historical development of the doctrine, its political implications, and the material benefits accrued by its proponents versus its challengers. If its persistence correlates strongly with institutional power and suppression of alternatives, it leans towards a construct.',
    'If a construct, the constraint would reclassify from Mountain to a more extractive type (e.g., Tangled Rope or Snare), reflecting its function in maintaining power structures rather than merely stating a divine fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_vs_theological_construct, conceptual, 'Ambiguity between genuine ontological truth and a power-serving theological construct.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (institutional power, legal enforcement) or internalized (theological conviction, social pressure)?',
    'Post-exit suppression trajectory: if theological positions challenging the uncreated Qur''an persist and gain traction in contexts free from institutional enforcement, it suggests a higher degree of internalized suppression within traditional contexts.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them even in the absence of overt coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for theological dissent.').

omega_variable(
    kernel_framing_underdetermination,
    'Does framing the Qur''an''s ontological status as ''uncreated'' (this reading) accurately capture the core commitment, or would a ''created'' framing (sibling reading) offer a more coherent or less extractive account of divine speech?',
    'Comparative analysis of the logical coherence and ethical implications of both the ''uncreated'' and ''created'' framings, particularly regarding divine justice, human free will, and the possibility of textual interpretation and reform. This is a conceptual choice, not an empirical one.',
    'Adopting a ''created'' framing would fundamentally alter the constraint''s base properties, likely increasing its perceived extractiveness (from the perspective of those who benefit from the ''uncreated'' reading) and decreasing its suppression, leading to a different classification (e.g., Rope or Scaffold for a more flexible, adaptable text).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Under-determination of the kernel''s core framing between uncreated and created readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__uncreated_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__uncreated_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qura_tr_t300, quran_ontological_status__uncreated_reading, theater_ratio, 300, 0.1).
narrative_ontology:measurement(qura_tr_t600, quran_ontological_status__uncreated_reading, theater_ratio, 600, 0.1).
narrative_ontology:measurement(qura_tr_t900, quran_ontological_status__uncreated_reading, theater_ratio, 900, 0.1).
narrative_ontology:measurement(qura_tr_t1200, quran_ontological_status__uncreated_reading, theater_ratio, 1200, 0.1).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__uncreated_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(qura_be_t300, quran_ontological_status__uncreated_reading, base_extractiveness, 300, 0.1).
narrative_ontology:measurement(qura_be_t600, quran_ontological_status__uncreated_reading, base_extractiveness, 600, 0.1).
narrative_ontology:measurement(qura_be_t900, quran_ontological_status__uncreated_reading, base_extractiveness, 900, 0.1).
narrative_ontology:measurement(qura_be_t1200, quran_ontological_status__uncreated_reading, base_extractiveness, 1200, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__uncreated_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(qura_su_t300, quran_ontological_status__uncreated_reading, suppression_requirement, 300, 0.85).
narrative_ontology:measurement(qura_su_t600, quran_ontological_status__uncreated_reading, suppression_requirement, 600, 0.85).
narrative_ontology:measurement(qura_su_t900, quran_ontological_status__uncreated_reading, suppression_requirement, 900, 0.8).
narrative_ontology:measurement(qura_su_t1200, quran_ontological_status__uncreated_reading, suppression_requirement, 1200, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__uncreated_reading, identity_coordination).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__state_enforced_creation_reading).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, prophetic_infallibility_doctrine).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, islamic_legal_hermeneutics).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'quran_ontological_status' kernel. Its 'uncreated' premise directly contradicts the 'created' and 'state_enforced_creation' sibling readings, leading to distinct structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
