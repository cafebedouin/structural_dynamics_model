% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__uncreated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Qur'an as Uncreated Eternal Divine Speech (kalÄm AllÄh qadÄ«m)
 *   domain: theological/political
 *
 * SUMMARY:
 *   This constraint instantiates the 'uncreated reading' of the
 *   quran_ontological_status kernel: the doctrine that the Qur'an is kalÄm
 *   AllÄh qadÄ«m, eternal uncreated divine speech coeternal with God.
 *   Structurally, this reading treats revelation as a permanent ontic
 *   mountain, maximizing prophetic authority and privileging literalist
 *   hermeneutics. It generates a commitment system in which textual meaning
 *   is fixed divine fact rather than contingent artifact. Key agents include
 *   traditional jurists who administer the interpretive tradition, literalist
 *   and anti-rationalist communities who receive identity and epistemic
 *   certainty, and rational theologians, metaphorical interpreters, and
 *   reform movements who bear the costs of hermeneutic foreclosure. The
 *   doctrine is claimed as mountain (natural divine law) but carries
 *   identifiable beneficiaries and victims, triggering False Summit Mountain
 *   evaluation.
 *
 * KEY AGENTS:
 *   - Traditional jurists (institutional/agenda-setter): Guard access to the fixed text and collect interpretive authority.
 *   - Literalist communities (organized/beneficiary): Receive group cohesion and epistemic certainty from eternal textuality.
 *   - Anti-rationalist schools (organized/beneficiary): Vindicated by a doctrine that subordinates reason to text.
 *   - Rational theologians (moderate/payer): Bear exclusion from orthodox institutions and hermeneutic constraints.
 *   - Metaphorical interpreters (moderate/payer): Excluded by literalist fixity requirements.
 *   - Reform movements (moderate/payer): Blocked from contextual re-readings by eternal uncreated status.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, 0.65).
domain_priors:suppression_score(quran_ontological_status__uncreated_reading, 0.78).
domain_priors:theater_ratio(quran_ontological_status__uncreated_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__uncreated_reading, mountain).
narrative_ontology:human_readable(quran_ontological_status__uncreated_reading, "Qur'an as Uncreated Eternal Divine Speech (kalÄm AllÄh qadÄ«m)").
narrative_ontology:topic_domain(quran_ontological_status__uncreated_reading, "theological/political").

domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__uncreated_reading, 'a36b1f32-efec-41c2-9ba6-7022b90e7c74').
narrative_ontology:cs_kernel_codification('a36b1f32-efec-41c2-9ba6-7022b90e7c74', fixed_text).
narrative_ontology:cs_authority_grounding('a36b1f32-efec-41c2-9ba6-7022b90e7c74', lineage).
narrative_ontology:cs_interpretation_layer_present('a36b1f32-efec-41c2-9ba6-7022b90e7c74').
narrative_ontology:cs_reading_relation('a36b1f32-efec-41c2-9ba6-7022b90e7c74', quran_ontological_status__created_reading, forecloses).
narrative_ontology:cs_reading_relation('a36b1f32-efec-41c2-9ba6-7022b90e7c74', quran_ontological_status__state_enforced_creation_reading, forecloses).
narrative_ontology:cs_axiom('a36b1f32-efec-41c2-9ba6-7022b90e7c74', foundational, quran_eternal_and_uncreated).
narrative_ontology:cs_axiom_status(quran_eternal_and_uncreated, holdable).
narrative_ontology:cs_axiom_grounding('a36b1f32-efec-41c2-9ba6-7022b90e7c74', quran_eternal_and_uncreated, theological).
narrative_ontology:cs_axiom('a36b1f32-efec-41c2-9ba6-7022b90e7c74', foundational, literal_textual_meaning_as_fixed_divine_fact).
narrative_ontology:cs_axiom_status(literal_textual_meaning_as_fixed_divine_fact, holdable).
narrative_ontology:cs_axiom_grounding('a36b1f32-efec-41c2-9ba6-7022b90e7c74', literal_textual_meaning_as_fixed_divine_fact, theological).
narrative_ontology:cs_reference_frame('a36b1f32-efec-41c2-9ba6-7022b90e7c74', eternal_uncreated_divine_speech).
narrative_ontology:cs_drift_state('a36b1f32-efec-41c2-9ba6-7022b90e7c74', contemporary_modernity, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a36b1f32-efec-41c2-9ba6-7022b90e7c74', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__uncreated_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, traditional_jurists).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, literalist_communities).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, anti_rationalist_schools).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, rational_theologians).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, metaphorical_interpreters).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, reform_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Guardians of the interpretive tradition that treats the Qur'an as eternal, uncreated divine speech. Their institutional authority derives from exclusive access to a fixed divine text whose meaning is not contingent on historical context. They set the boundaries of orthodox hermeneutics and collect religious legitimacy and social deference from this role.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, traditional_jurists, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, traditional_jurists, beneficiary).

% Communities whose religious identity is constituted by literal adherence to an eternal, uncreated scripture. They receive epistemic certainty and group cohesion from the doctrine, and experience alternative readings as threats to identity rather than interpretive options.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, literalist_communities, beneficiary,
    organized, generational, identity_locked, global).

% Theological schools that reject rationalist speculation in favor of textual literalism. The uncreated doctrine vindicates their epistemic framework by elevating the text above human reason, granting them standing in intra-religious debates.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, anti_rationalist_schools, beneficiary,
    organized, generational, identity_locked, global).

% Theologians who employ reason and speculative inquiry. The uncreated doctrine constrains their hermeneutic range, classifies their methods as suspect, and historically exposed them to charges of heresy or exclusion from institutional teaching positions.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, rational_theologians, payer,
    moderate, biographical, constrained, global).

% Interpreters who employ figurative, contextual, or historically situated readings of revelation. Their interpretive practice is treated as a denial of the text's eternal fixity and divine authorship, limiting their access to orthodox platforms and educational institutions.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, metaphorical_interpreters, payer,
    moderate, biographical, constrained, global).

% Movements seeking to reinterpret scripture in response to modern political, ethical, or scientific contexts. The eternal uncreated status of the text is invoked to block any re-reading that would adapt divine speech to new conditions, forcing reformists into heterodox margins.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, reform_movements, payer,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__uncreated_reading, traditional_jurists).
narrative_ontology:fixing_cost_class(quran_ontological_status__uncreated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the global Islamic community around a single, immutable, transcendent textual referent, eliminating epistemic ambiguity about divine will and standardizing religious legitimacy across diverse populations.
% TRANSFER_FUNCTION: Moves interpretive authority and communal religious legitimacy from rational theologians, metaphorical interpreters, and reformists to traditional jurists and literalist communities by fixing textual meaning as eternal divine fact.
% ABSENT_VOICES: Historical-critical scholars and modernist Islamic thinkers who treat the Qur'an as historically situated literature are structurally excluded from legitimate theological discourse; their absence is enforced by the doctrine's classification of such approaches as heretical denial of divine transcendence.
% DISAPPEARANCE_RATIONALE: If the uncreated doctrine vanished, the epistemic foundation of classical fiqh, literalist aqida, and the authority of traditional scholars would collapse; Islamic jurisprudence and political theology would reorganize around contingent, created, or historically situated revelation, and the current beneficiary structure would lose its grounding.
% FOUNDING_PROBLEM: The early Abbasid state enforced the doctrine of the Qur'an's createdness through inquisition (mihna), subordinating revelation to caliphal authority and rationalist theology; the uncreated reading was constructed to safeguard divine speech from state capture and rationalist reduction.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians and independent scholars outside the beneficiary set corroborate that the mihna ended centuries ago and the state no longer imposes createdness; they do not corroborate that active state coercion against revelation persists. Traditional jurists assert the problem is live, but this attestation comes from within the benefiting party.
narrative_ontology:disappearance_verdict(quran_ontological_status__uncreated_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__uncreated_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__uncreated_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_ontological_status__uncreated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__uncreated_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__uncreated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

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
 *   Extractiveness (0.65) is authored as moderately-high because the constraint transfers substantial interpretive authority and communal legitimacy from alternative readers to traditional jurists and literalist communities. Suppression (0.78) is high because the doctrine actively excludes rational, metaphorical, and reformist readings as heretical. Theater_ratio (0.45) reflects significant performative maintenance of orthodoxy alongside genuine theological commitment. Accessibility_collapse (0.82) is high because, within the doctrinal framework, alternatives collapse completelyâthe created or metaphorical readings are not just wrong but theologically impossible. Resistance (0.55) is moderate because historical and contemporary rationalist and reformist movements have actively contested the doctrine, though often from heterodox margins. The claim/metric divergence is intentional: the constraint claims mountain status while the metrics describe an actively maintained, extractive structure with concentrated beneficiaries.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (jurists, literalists) experience the constraint as divine gift and epistemic foundationâa mountain securing certainty. The payer seats (rationalists, reformists) experience the same structure as an extractive gate that captures interpretive possibility and delegates authority to a specific clerical class. The engine computes this divergence from the same structural facts; the authored claim does not adjudicate between the experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional jurists are structural beneficiaries (low d) because the constraint subsidizes their authority by making them the necessary mediators of an eternal, fixed text. Literalist and anti-rationalist communities are also beneficiaries (low-to-moderate d) because the constraint vindicates their identity and epistemic framework. Rational theologians, metaphorical interpreters, and reform movements are targets (high d) because the constraint extracts from them by foreclosing their hermeneutic options and, historically, their institutional standing. The derivation is straightforward from beneficiary/victim declarations paired with constrained or identity-locked exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâresisting the Abbasid mihna's state-imposed creatednessâis dead. The state no longer enforces createdness, and the uncreated doctrine has not faced serious state challenge for centuries. Yet the arrangement persists and has accrued extraction over time: what began as defensive theology became an authority structure that concentrates interpretive power. The R5 mismatch (founding_problem_status=dead + disappearance_verdict=world_rearranges) flags this as a zombie constraint. The mandatrophy is not resolved; the doctrine has outlived its defensive function and now operates as a structural extractor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_divinity,
    'Is the Qur''an''s uncreatedness a genuine ontological mountain (a divine fact coeternal with God) or a constructed commitment system that benefits identifiable juridical parties by freezing interpretive authority?',
    'Comparative theological analysis across Abrahamic traditions regarding the ontological status of scripture, combined with sociological study of authority distribution before and after the doctrine''s institutionalization.',
    'If constructed, the constraint is a false summit (tangled_rope or snare) whose mountain claim naturalizes human authority structures; if genuine, the beneficiaries are incidental and the extraction is epiphenomenal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_divinity, conceptual, 'Whether the uncreated doctrine is a natural-law fact or a constructed authority mechanism.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of rational and metaphorical theology structural (institutional exclusion, censorship) or internalized (theological identity fusion that makes heterodox thought unthinkable for believers)?',
    'Post-exit belief trajectory: observe whether rational theologians who exit the institutional sphere continue to experience epistemic suppression or recover cognitive autonomy.',
    'If internalized, effective suppression exceeds structural measures and the constraint operates as identity_coordination with high identity-locked extraction; if purely structural, removal of institutional barriers would liberate interpretive alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of alternative hermeneutics.').

omega_variable(
    literalist_fixity_vs_historical_textuality,
    'Does treating a specific historical Arabic text as eternal uncreated speech genuinely preserve divine transcendence, or does it anthropomorphize a linguistic artifact into the divine essence?',
    'Philosophical analysis of the relationship between language, eternity, and transcendence; theological assessment of whether the doctrine resolves or replicates the anthropomorphism it was meant to avoid.',
    'If the doctrine replicates anthropomorphism, its mountain claim is internally unstable and its extraction is revealed as maintenance of a specific textual community''s boundaries rather than preservation of divine otherness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(literalist_fixity_vs_historical_textuality, conceptual, 'Internal coherence of the uncreated doctrine regarding divine transcendence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__uncreated_reading, 0, 130).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quru_tr_t0, quran_ontological_status__uncreated_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(quru_tr_t20, quran_ontological_status__uncreated_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(quru_tr_t40, quran_ontological_status__uncreated_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(quru_tr_t60, quran_ontological_status__uncreated_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(quru_tr_t80, quran_ontological_status__uncreated_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(quru_tr_t100, quran_ontological_status__uncreated_reading, theater_ratio, 100, 0.43).
narrative_ontology:measurement(quru_tr_t130, quran_ontological_status__uncreated_reading, theater_ratio, 130, 0.45).

% Extraction over time
narrative_ontology:measurement(quru_be_t0, quran_ontological_status__uncreated_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(quru_be_t20, quran_ontological_status__uncreated_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(quru_be_t40, quran_ontological_status__uncreated_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(quru_be_t60, quran_ontological_status__uncreated_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(quru_be_t80, quran_ontological_status__uncreated_reading, base_extractiveness, 80, 0.62).
narrative_ontology:measurement(quru_be_t100, quran_ontological_status__uncreated_reading, base_extractiveness, 100, 0.64).
narrative_ontology:measurement(quru_be_t130, quran_ontological_status__uncreated_reading, base_extractiveness, 130, 0.65).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(quran_ontological_status__uncreated_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__uncreated_reading, identity_coordination).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the quran_ontological_status kernel. The uncreated reading (eternal divine speech), created reading (makhlÅ«q), and state_enforced_creation reading (created plus mihna) are structurally distinct constraints with different epsilon values, beneficiary/victim structures, and historical enforcement patterns. They form a constraint family linked by shared kernel origin.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
