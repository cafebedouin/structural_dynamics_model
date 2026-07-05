% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__created_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__created_reading, []).

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
 *   constraint_id: quran_ontological_status__created_reading
 *   human_readable: Createdness of the Qur'an (khalq al-Qur'an) — Mu'tazilite/rationalist reading
 *   domain: religious/philosophical/political
 *
 * SUMMARY:
 *   This story models the createdness (khalq al-Qur'an) reading of the
 *   contested kernel over the ontological status of the Qur'an, considered
 *   independently of state enforcement (the mihna is modeled separately as
 *   state_enforced_creation_reading). On this reading, God's speech is a
 *   temporal, produced act — the Qur'an is makhlūq — precisely in order to
 *   preserve God's essence as absolutely transcendent and free of any
 *   coeternal reality. The doctrine functions as a genuine coordination
 *   artifact within rationalist theology: it resolves an internal tension
 *   between divine unity and the existence of a communicable, recited text,
 *   and it does so through argument and persuasion rather than coercion. But
 *   the same doctrine relocates interpretive authority away from
 *   traditionalist jurists and literalist communities toward rationalist
 *   theologians and philosophically inclined political authorities, producing
 *   an asymmetric shift in whose expertise counts even without any
 *   inquisitorial machinery attached.
 *
 * KEY AGENTS:
 *   - rationalist_theologians: agenda_setter/beneficiary (organized/mobile) — supply the interpretive apparatus and gain hermeneutic authority
 *   - philosophical_schools: beneficiary (moderate/mobile) — gain room for allegorical reinterpretation of scripture
 *   - reform_movements: beneficiary (moderate/constrained) — inherit hermeneutic flexibility as a later resource
 *   - caliphal_interpretive_authority: beneficiary/agenda_setter (institutional/arbitrage) — gains warrant for state-aligned interpretive primacy
 *   - traditionalist_jurists: payer (organized/constrained) — lose the textual-fixity ground of their authority
 *   - literalist_communities: payer (powerless/identity_locked) — lose unmediated devotional access to divine speech
 *   - hadith_transmitters: payer (moderate/constrained) — their transmission-based expertise is devalued
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__created_reading, 0.42).
domain_priors:suppression_score(quran_ontological_status__created_reading, 0.28).
domain_priors:theater_ratio(quran_ontological_status__created_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__created_reading, rope).
narrative_ontology:human_readable(quran_ontological_status__created_reading, "Createdness of the Qur'an (khalq al-Qur'an) — Mu'tazilite/rationalist reading").
narrative_ontology:topic_domain(quran_ontological_status__created_reading, "religious/philosophical/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__created_reading, '2bf300af-803c-4665-aac5-c357f4dd58b4').
narrative_ontology:cs_kernel_codification('2bf300af-803c-4665-aac5-c357f4dd58b4', distributed).
narrative_ontology:cs_authority_grounding('2bf300af-803c-4665-aac5-c357f4dd58b4', expertise).
narrative_ontology:cs_interpretation_layer_present('2bf300af-803c-4665-aac5-c357f4dd58b4').
narrative_ontology:cs_reading_relation('2bf300af-803c-4665-aac5-c357f4dd58b4', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('2bf300af-803c-4665-aac5-c357f4dd58b4', quran_ontological_status__state_enforced_creation_reading, influences).
narrative_ontology:cs_axiom('2bf300af-803c-4665-aac5-c357f4dd58b4', foundational, divine_speech_is_temporally_produced_act).
narrative_ontology:cs_axiom_status(divine_speech_is_temporally_produced_act, holdable).
narrative_ontology:cs_axiom_grounding('2bf300af-803c-4665-aac5-c357f4dd58b4', divine_speech_is_temporally_produced_act, deontological).
narrative_ontology:cs_axiom('2bf300af-803c-4665-aac5-c357f4dd58b4', secondary, textual_meaning_requires_rational_mediation).
narrative_ontology:cs_axiom_status(textual_meaning_requires_rational_mediation, holdable).
narrative_ontology:cs_axiom_grounding('2bf300af-803c-4665-aac5-c357f4dd58b4', textual_meaning_requires_rational_mediation, instrumental).
narrative_ontology:cs_reference_frame('2bf300af-803c-4665-aac5-c357f4dd58b4', kalam_rationalist_tawhid_framework).
narrative_ontology:cs_drift_state('2bf300af-803c-4665-aac5-c357f4dd58b4', post_ashari_synthesis_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('2bf300af-803c-4665-aac5-c357f4dd58b4', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__created_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, rationalist_theologians).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, philosophical_schools).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, reform_movements).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, caliphal_interpretive_authority).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, traditionalist_jurists).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, literalist_communities).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, hadith_transmitters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Kalam-trained scholars (Mu'tazila and later rationalist currents) argue that God's speech, being uttered/produced, is a temporal act and therefore created, since only God's essence is eternal. They set the interpretive agenda by supplying the philosophical apparatus that judges determine what the text 'really' means, and they gain hermeneutic authority whenever textual meaning is treated as requiring rational mediation rather than being self-evidently fixed.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, rationalist_theologians, agenda_setter,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__created_reading, rationalist_theologians, beneficiary).

% Falsafa-adjacent thinkers benefit from a doctrine that opens scripture to allegorical and rational reinterpretation, allowing Greek-influenced metaphysics to coexist with revelation without contradiction. Their intellectual programs depend on the text being treated as a produced artifact amenable to interpretation rather than a coeternal, self-interpreting divine attribute.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, philosophical_schools, beneficiary,
    moderate, civilizational, mobile, regional).

% Later reformist and modernist religious movements draw on the createdness doctrine to argue for historically contextualized, reinterpretable scripture, supporting arguments for legal and social reform grounded in changed circumstance rather than fixed textual mandate. They inherit this reading's hermeneutic flexibility as a resource, though most live under political orders where the doctrine is not officially favored.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, reform_movements, beneficiary,
    moderate, generational, constrained, regional).

% Where political authorities favor rationalist theology, a created Qur'an supports claims that the caliph (or state-aligned scholars) possesses legitimate interpretive authority over scripture, since a produced text requires an authoritative interpreter rather than standing as self-sufficient uncreated speech. This seat benefits from the doctrine's logic even apart from any enforcement machinery.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, caliphal_interpretive_authority, beneficiary,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__created_reading, caliphal_interpretive_authority, agenda_setter).

% Hanbali and traditionist jurists ground their entire authority in the claim that the Qur'an's wording is itself uncreated divine speech, transmitted without human mediation, which is what makes their literal jurisprudential method (as opposed to rationalist kalam) authoritative. The created reading strips the textual fixity their authority rests on, demoting their expertise relative to rationalist theologians even where no state coercion is present; their exit is constrained because their entire professional and communal standing is built on the opposing premise.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, traditionalist_jurists, payer,
    organized, generational, constrained, regional).

% Ordinary believers whose devotional and communal identity is built around reciting and hearing unmediated divine speech experience the created reading as displacing God's direct presence in the text with a philosophical abstraction they cannot access. Their exit is identity-locked: rejecting the doctrine's implications would require reconceiving what recitation and revelation mean to them personally, not merely changing an institutional affiliation.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, literalist_communities, payer,
    powerless, biographical, identity_locked, local).

% Scholars whose social capital rests on faithfully transmitting and preserving the wording of revelation and prophetic tradition find their function devalued when meaning is understood to require rational adjudication rather than faithful transmission; their professional path depends on textual fixity remaining paramount.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, hadith_transmitters, payer,
    moderate, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The created reading solves a genuine theological coordination problem: how to affirm God's absolute transcendence and freedom from any co-eternal reality (avoiding what rationalists see as an implicit ditheism of an uncreated, coeternal speech-attribute) while still permitting revelation to function as a communicable, temporally situated, humanly receivable text.
% TRANSFER_FUNCTION: The doctrine transfers interpretive authority from those whose expertise is grounded in preserving and transmitting fixed textual wording (traditionalist jurists, hadith transmitters, literalist communities) to those whose expertise is grounded in rational/philosophical mediation of meaning (rationalist theologians, philosophical schools, and political authorities aligned with them).
% ABSENT_VOICES: Ordinary reciters and devotional communities whose relationship to the Qur'an is experiential rather than doctrinal are rarely represented in the kalam debate itself; their objection — that the doctrine estranges them from a text they experience as directly and intimately divine — is preserved mainly through traditionalist advocacy on their behalf rather than their own direct participation in the theological argument.
% DISAPPEARANCE_RATIONALE: If the created reading vanished as a live theological position, rationalist theology would lose its principal textual warrant for treating scripture as interpretively open, philosophical schools would face renewed pressure to reconcile revelation with reason through other doctrinal routes, and traditionalist jurists would face one less major rival claim to interpretive authority — the balance of hermeneutic power within Islamic theology would shift measurably toward textual literalism.
% FOUNDING_PROBLEM: Early kalam theologians confronted an apparent tension: affirming God's absolute oneness and freedom from any coeternal reality while explaining how God's speech could be heard, recited, written, and transmitted in human language and time.
% FOUNDING_PROBLEM_CORROBORATION: Rationalist theologians and philosophical historians attest the tension is a genuine and still-debated problem in Islamic theology proper; traditionalist scholars, from outside the rationalist camp, attest that the 'problem' is itself a philosophical importation onto a matter they hold as already settled by revelation's self-attestation, and that the doctrine functions primarily to relocate authority rather than resolve any problem intrinsic to the text.
narrative_ontology:disappearance_verdict(quran_ontological_status__created_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__created_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__created_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_ontological_status__created_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__created_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__created_reading_tests).
:- end_tests(quran_ontological_status__created_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42 at interval end) because the doctrine does redistribute real interpretive authority and social standing away from traditionalist actors toward rationalist ones, but this happens through argument, persuasion, and shifting institutional favor rather than through coercive suppression of alternatives — hence suppression is authored low-moderate (0.28), reflecting genuine intellectual contest rather than enforced compliance (contrast sharply with the state_enforced_creation_reading sibling, where suppression would be far higher). Theater ratio is low (0.2) because the doctrinal apparatus is substantively engaged by both sides, not performative. Resistance is authored moderately high (0.55) because traditionalist jurists mount serious, sustained doctrinal counter-argument (culminating historically in Ash'ari and Hanbali responses) rather than acquiescing.
 *
 * PERSPECTIVAL GAP:
 *   From the rationalist theologian's seat, the createdness doctrine is a rope: it solves a real coordination problem (reconciling divine transcendence with communicable revelation) with minimal coercive overhead, and its adoption is voluntary intellectual persuasion. From the traditionalist jurist's seat, the same doctrine looks like an encroachment that delegitimizes an entire interpretive tradition by philosophical fiat — coordination for one party, extraction of standing for another, through the identical structural mechanism. The engine's per-seat computation should register this divergence without either seat's view overriding the story-level classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (rationalist theologians, philosophical schools, reform movements, caliphal interpretive authority) receive low directionality because the doctrine directly expands their interpretive jurisdiction and legitimizes their expertise. Victims (traditionalist jurists, literalist communities, hadith transmitters) receive high directionality because their authority and identity are structurally undercut by the same doctrinal move, even though no one directly coerces them within this reading — the extraction here is reputational and jurisdictional, not physically coercive.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling divine unity with a communicable revelation) remains genuinely contested rather than resolved or abandoned — both readings of the kernel continue to be held by live communities of scholars centuries later. This is not a case of an arrangement whose function has died while the arrangement persists by inertia; both this reading and its sibling readings are actively defended on their own theological merits, which is why founding_problem_status is authored as contested rather than dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    createdness_coordination_or_status_transfer,
    'Is the createdness doctrine best read as a genuine solution to a real theological coordination problem (reconciling divine unity with communicable revelation), or as a status-transfer mechanism whose philosophical framing is cover for relocating interpretive authority to rationalist theologians and state-aligned scholars?',
    'Comparative doctrinal history: does the doctrine''s content and internal argumentative structure track the stated theological problem closely (supporting coordination), or does its adoption correlate more strongly with which faction held political/interpretive power at the time (supporting status-transfer)?',
    'If genuinely coordination-dominant, the rope classification is well-supported; if status-transfer dominant even absent state coercion, the constraint drifts toward tangled_rope despite the absence of the mihna''s enforcement machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(createdness_coordination_or_status_transfer, conceptual, 'Whether createdness functions primarily as theological coordination or authority relocation.').

omega_variable(
    reading_independent_of_enforcement,
    'Can the createdness doctrine be evaluated independently of its historical entanglement with the mihna, or does the historical record make the doctrine and its state-enforced imposition inseparable in practice?',
    'Examine pre-mihna and post-mihna advocacy of createdness by scholars operating without state backing (e.g., in regions or periods where Mu''tazilite theology was a minority position rather than state orthodoxy) to see whether the doctrine''s structural profile (extractiveness, suppression) holds steady absent enforcement.',
    'If the doctrine''s extraction profile is essentially unchanged whether or not state enforcement is present, this reading''s decomposition from state_enforced_creation_reading is well-founded; if extraction was negligible absent enforcement, this story''s ε may be overstated and should be revised downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_independent_of_enforcement, empirical, 'Whether the created reading''s structural profile is genuinely separable from its historical state-enforced instantiation.').

omega_variable(
    divine_transcendence_framing_stability,
    'Does locating God''s essence categorically above the temporal text genuinely preserve divine transcendence in a way traditionalists would recognize as coherent, or does it introduce a philosophical framework (essence/attribute distinctions imported from kalam) that traditionalists reject as an illegitimate premise rather than merely a rival conclusion?',
    'Close comparison of Mu''tazilite/Ash''ari essence-attribute frameworks against traditionalist (Hanbali, Zahiri) rejections of the framework itself, prior to any conclusion about createdness.',
    'If traditionalists reject the entire philosophical framework rather than merely disagreeing within it, the two readings may not be commensurable within a single theological system, strengthening a forecloses relation rather than coexists_with between this reading and uncreated_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_transcendence_framing_stability, conceptual, 'Whether the readings share enough common philosophical ground to be genuinely coexisting positions within one tradition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__created_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__created_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qura_tr_t20, quran_ontological_status__created_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(qura_tr_t40, quran_ontological_status__created_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(qura_tr_t60, quran_ontological_status__created_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(qura_tr_t80, quran_ontological_status__created_reading, theater_ratio, 80, 0.19).
narrative_ontology:measurement(qura_tr_t100, quran_ontological_status__created_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__created_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(qura_be_t20, quran_ontological_status__created_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(qura_be_t40, quran_ontological_status__created_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(qura_be_t60, quran_ontological_status__created_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement(qura_be_t80, quran_ontological_status__created_reading, base_extractiveness, 80, 0.42).
narrative_ontology:measurement(qura_be_t100, quran_ontological_status__created_reading, base_extractiveness, 100, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(quran_ontological_status__created_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__created_reading, identity_coordination).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% This story, uncreated_reading, and state_enforced_creation_reading form a three-member constraint family reading the single kernel quran_ontological_status. created_reading (this story) models the doctrine's theological/authority logic without state coercion — rope-leaning, moderate extraction via status transfer, low suppression. uncreated_reading models the traditionalist position where revelation itself is a coeternal ontic constraint — expected mountain-leaning with minimal extraction (though traditionalist authority interests may warrant an FSM check there too). state_enforced_creation_reading models the same createdness content but with the historical mihna's coercive apparatus layered on — expected tangled_rope or snare, with much higher suppression and identifiable victims among persecuted jurists (notably Ahmad ibn Hanbal). All three share ε-invariance discipline: each has a distinct, stable extraction profile rather than one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
