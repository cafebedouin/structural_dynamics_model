% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__created_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Qur'an as Created Divine Speech (Mu'tazilite/Rationalist Reading)
 *   domain: Islamic Theology / Philosophy of Language / Political Authority
 *
 * SUMMARY:
 *   This story authors ONE reading of a contested kernel — the question of
 *   the ontological status of the Qur'an, one of the most consequential
 *   doctrinal disputes in Islamic intellectual history. This reading (the
 *   Mu'tazilite/rationalist created-speech position, makhluq) holds that the
 *   Qur'an, as composed of temporal Arabic words, letters, and discrete
 *   recitations, is a created act of divine speech rather than a coeternal,
 *   uncreated attribute of God's essence. On this reading, the coordination
 *   function is real: it resolves the metaphysical tension between affirming
 *   God spoke authoritatively and affirming that nothing shares eternity with
 *   God's essence. The reading is authored here purely as a THEOLOGICAL
 *   position — it does NOT include the historical state-enforced mihna
 *   (Abbasid inquisition under al-Ma'mun and successors), which is a
 *   structurally distinct constraint (state_enforced_creation_reading) where
 *   the same doctrine becomes coercively imposed. Nor does this story address
 *   the uncreated_reading's own claim (that the Qur'an is kalam Allah qadim,
 *   a coeternal attribute), which is authored as its own separate constraint.
 *   The ε value here (0.42) reflects this reading's OWN standing arrangement
 *   — theological argument reallocating interpretive authority among elite
 *   religious specialists — assessed on its own terms, not the enforcement
 *   apparatus that a sibling reading layers on top of it.
 *
 * KEY AGENTS:
 *   - rationalist_theologians: Primary beneficiary (organized/mobile) — gains hermeneutic authority
 *   - reform_movements: Secondary beneficiary (moderate/constrained) — gains reinterpretive warrant
 *   - philosophical_schools: Beneficiary (organized/mobile) — metaphysical consistency with transcendence
 *   - traditionalist_jurists: Primary target (institutional/trapped) — loses textual-fixity-based authority
 *   - literalist_communities: Primary target (powerless/identity_locked) — loses devotional unmediated-word status
 *   - comparative_theologians: Analytical observer — sees the full structural dispute
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
narrative_ontology:human_readable(quran_ontological_status__created_reading, "Qur'an as Created Divine Speech (Mu'tazilite/Rationalist Reading)").
narrative_ontology:topic_domain(quran_ontological_status__created_reading, "Islamic Theology / Philosophy of Language / Political Authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__created_reading, '927088f1-ecef-47ff-8a72-d91710119d5f').
narrative_ontology:cs_kernel_codification('927088f1-ecef-47ff-8a72-d91710119d5f', fixed_text).
narrative_ontology:cs_authority_grounding('927088f1-ecef-47ff-8a72-d91710119d5f', practice).
narrative_ontology:cs_interpretation_layer_present('927088f1-ecef-47ff-8a72-d91710119d5f').
narrative_ontology:cs_reading_relation('927088f1-ecef-47ff-8a72-d91710119d5f', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('927088f1-ecef-47ff-8a72-d91710119d5f', quran_ontological_status__state_enforced_creation_reading, influences).
narrative_ontology:cs_axiom('927088f1-ecef-47ff-8a72-d91710119d5f', foundational, divine_essence_incomposite_transcendence).
narrative_ontology:cs_axiom_status(divine_essence_incomposite_transcendence, holdable).
narrative_ontology:cs_axiom_grounding('927088f1-ecef-47ff-8a72-d91710119d5f', divine_essence_incomposite_transcendence, deontological).
narrative_ontology:cs_axiom('927088f1-ecef-47ff-8a72-d91710119d5f', secondary, temporal_linguistic_features_entail_createdness).
narrative_ontology:cs_axiom_status(temporal_linguistic_features_entail_createdness, holdable).
narrative_ontology:cs_axiom_grounding('927088f1-ecef-47ff-8a72-d91710119d5f', temporal_linguistic_features_entail_createdness, conventional).
narrative_ontology:cs_reference_frame('927088f1-ecef-47ff-8a72-d91710119d5f', mutazilite_rationalist_kalam_consensus).
narrative_ontology:cs_drift_state('927088f1-ecef-47ff-8a72-d91710119d5f', post_ashari_consolidation, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('927088f1-ecef-47ff-8a72-d91710119d5f', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__created_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, rationalist_theologians).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, reform_movements).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, philosophical_schools).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, traditionalist_jurists).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, literalist_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Kalam scholars who argue that treating the Qur'an's letters, ink, and recited sound as eternal collapses divine unity (tawhid) into a form of associating a temporal artifact with God's essence. By classifying revelation as created speech, they gain the hermeneutic authority to allegorize, contextualize, and rationally adjudicate scriptural meaning against reason (aql), expanding their own interpretive jurisdiction relative to text-bound jurists.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, rationalist_theologians, beneficiary,
    organized, civilizational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__created_reading, rationalist_theologians, agenda_setter).

% Modernizing and reformist currents draw on the created-speech doctrine to argue that specific textual injunctions are historically situated products of a communicative act at a particular time and place, and can therefore be reinterpreted for changed circumstances without denying the Qur'an's divine origin. Their exit from literalist orthodoxy is easier when the ontological premise underwriting textual fixity is itself contested.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, reform_movements, beneficiary,
    moderate, generational, constrained, regional).

% Falsafa-adjacent and Mu'tazilite-descended schools benefit because a created Qur'an is compatible with their broader commitments to divine transcendence, causal reasoning, and the priority of reason in resolving apparent scriptural anthropomorphism. The doctrine removes an obstacle (a coeternal second uncreated thing alongside God) that their metaphysics treats as incoherent.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, philosophical_schools, beneficiary,
    organized, civilizational, mobile, continental).

% Hanbali-descended and traditionalist (later Ash'ari-consolidated) jurists derive much of their authority from the claim that the Qur'an's wording is itself the fixed, unmediated, eternal word of God, which makes their literal transmission and textual custody the primary site of religious authority. If revelation is created, their gatekeeping function over an unchanging text is structurally weakened in favor of rational adjudicators; they cannot simply exit this doctrinal dispute because their institutional legitimacy is built on the opposing premise.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, traditionalist_jurists, payer,
    institutional, civilizational, trapped, continental).

% Ordinary believers whose devotional and communal identity is built around reciting, memorizing, and venerating the Qur'an as directly and eternally God's own uncreated word experience the created-speech doctrine as a demotion of the text they organize their lives around. Their attachment is not primarily doctrinal-technical but devotional and identity-constituting, making exit from the felt sense of unmediated divine presence in the text costly regardless of the argument's logical force.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, literalist_communities, payer,
    powerless, biographical, identity_locked, local).

% Caliphal authorities who at various points sponsored or persecuted this doctrine (most famously during the mihna) are not part of THIS reading's structure — the doctrine as authored here is a theological position independent of state enforcement. Their coercive instrumentalization of the doctrine is a separate constraint (state_enforced_creation_reading) and is deliberately excluded from this story's scope.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, historical_state_authorities, excluded,
    institutional, generational, analytical, continental).

% Scholars of comparative religion and historical theology who study the created/uncreated dispute as a structural episode in how communities negotiate the relationship between transcendence and textual authority, without themselves holding devotional stakes in the outcome.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, comparative_theologians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a metaphysically consistent way to affirm both that God spoke authoritatively to humanity AND that God's essence remains absolutely transcendent, unlike any created thing — solving the coordination problem of how a temporal, composed text (words, letters, sounds, a specific Arabic idiom) can be reconciled with a strict doctrine of divine unity and non-composite essence.
% TRANSFER_FUNCTION: Moves interpretive authority away from those whose legitimacy rests on custodianship of a fixed, unmediated textual object (traditionalist jurists, literalist transmission communities) toward those whose legitimacy rests on rational and contextual interpretation (kalam theologians, reformist and philosophical schools).
% ABSENT_VOICES: Ordinary devotional reciters and memorizers of the Qur'an, whose lived relationship to the text as directly divine is reclassified by elite theological argument they did not participate in constructing; their objection would be that the doctrine, whatever its logical merits, alters what the text means to hold in their hands and voice.
% DISAPPEARANCE_RATIONALE: If the created-speech doctrine vanished as a live theological position, rational theology would lose one of its principal levers for claiming interpretive authority over literalist custodianship, reform movements would lose a key metaphysical warrant for contextualized reinterpretation, and the entire historical dispute (and its downstream jurisprudential consequences) would need a different resolution — the doctrinal landscape does depend on this reading existing as an option.
% FOUNDING_PROBLEM: How to preserve absolute divine transcendence and non-composite unity (tawhid) against the apparent implication that an eternal, uncreated Qur'an existing 'alongside' God's essence introduces a second eternal thing, when the text itself displays temporal, contextual, and linguistically composite features (specific Arabic words, occasions of revelation, historical address).
% FOUNDING_PROBLEM_CORROBORATION: Historians of Islamic theology (outside both the Mu'tazilite and traditionalist camps) attest that the metaphysical problem of composite eternality was a genuine and independently identifiable puzzle in classical theology, not merely retrojected; contemporary comparative theologians corroborate that structurally analogous problems (the status of a scripture believed literally dictated versus divinely inspired) recur across traditions, suggesting the founding problem is real rather than a rationalization invented solely to serve the beneficiaries named above.
narrative_ontology:disappearance_verdict(quran_ontological_status__created_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__created_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__created_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.42) is moderate: this reading redistributes interpretive authority rather than material resources, but the redistribution is real — traditionalist jurists lose a genuine institutional lever, not merely a debating point. Suppression is comparatively low (0.28) because THIS reading, absent state enforcement, persists through argument, scholarly reputation, and doctrinal debate rather than coercion; theater ratio is low-moderate (0.2) because most of the activity is substantive doctrinal argument, though some performative point-scoring in polemical exchanges exists. Accessibility collapse is moderate (0.35): the rational argument for createdness does foreclose some naive literalist readings once understood, but multiple coherent theological positions on the kernel remain genuinely available (this is precisely why it is a kernel with live sibling readings, not a mountain). Resistance is fairly high (0.55) because traditionalist and literalist communities have historically mounted, and continue to mount, serious intellectual and communal resistance to this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Rationalist theologians and philosophical schools sit near the beneficiary end: they gain interpretive jurisdiction and metaphysical coherence without bearing offsetting costs. Traditionalist jurists sit near the target end: their institutional authority is structurally premised on the opposing (uncreated) doctrine, and this reading's success directly erodes that premise — they are also power-institutional but exit-trapped because abandoning the underlying claim would dissolve their own authority basis. Literalist communities are powerless and identity-locked: their relationship to the text is devotional/pre-theoretical, so the doctrinal shift is experienced as loss of felt divine presence, not as a move in an argument they can simply exit by finding a better one.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling temporal scripture with non-composite divine transcendence) remains live in the sense that no side of the kernel dispute has achieved universal doctrinal closure — Ash'ari, Mu'tazili, and various reformist/traditionalist positions continue to coexist across the Muslim world. This reading is not mandatrophic: its coordination function (transcendence-preservation) is still doing real work for the communities that hold it, not merely persisting as institutional inertia. Classifying it as rope rather than mountain or snare prevents two errors: treating a contested metaphysical claim as settled natural fact (the mountain error the uncreated_reading would risk if unexamined), and treating it as pure extraction when it in fact solves a real and recognized theological problem for its adherents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    createdness_vs_transcendence_entailment,
    'Does affirming the Qur''an''s createdness necessarily follow from a strict doctrine of divine non-composite transcendence, or is the uncreated position equally compatible with transcendence via a different account of divine attributes (as Ash''ari theology later argued)?',
    'This is not empirically resolvable — it depends on which account of divine attributes and predication is adopted, a matter internal to competing theological/philosophical frameworks with no external arbiter.',
    'If the entailment is not necessary, this reading''s claim to be the unique metaphysically consistent solution weakens, and the kernel remains genuinely open between readings rather than trending toward this one as more coherent — supporting the coexists_with classification for the uncreated sibling rather than a forecloses relation in either direction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(createdness_vs_transcendence_entailment, conceptual, 'Whether created-speech is metaphysically entailed by transcendence or merely one compatible option among others.').

omega_variable(
    authority_reallocation_intent_vs_effect,
    'Did rationalist theologians advance the created-speech doctrine primarily because of its internal theological coherence, or was the doctrine''s attractiveness to this camp partly a function of the interpretive authority it would confer on them relative to traditionalist jurists?',
    'Historical-intellectual analysis of the doctrine''s development timeline relative to institutional competition between kalam theologians and traditionalist jurists in the 8th-9th centuries; examination of whether the doctrine''s proponents changed their institutional practices/claims to authority concurrently with advancing it.',
    'If authority reallocation was a significant motivating factor rather than a mere side effect, the beneficiary declarations here understate the doctrine''s function as an interpretive-authority claim dressed in metaphysical argument — raising the effective extractiveness of this reading beyond what a purely theological reading would suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_reallocation_intent_vs_effect, empirical, 'Whether the doctrine''s beneficiary structure reflects incidental theological coherence or motivated authority-seeking.').

omega_variable(
    kernel_framing_under_determination,
    'Is the correct unit of analysis ''the ontological status of the Qur''an'' as a single contested doctrinal question, or should it be split further — e.g., separating the metaphysical claim (created vs. uncreated) from the epistemic-authority claim (who gets to interpret) as two distinct kernels that happen to correlate historically?',
    'Could be resolved by identifying historical or contemporary cases where the two claims decouple — e.g., a traditionalist jurist who accepts createdness but retains textual-literalist interpretive authority, or a rationalist theologian who holds uncreated status but argues for contextual reinterpretation anyway. Presence of such decoupled cases would support treating them as separable kernels.',
    'If decoupled cases are common, this story''s declared victim/beneficiary structure (tied tightly to the metaphysical claim) would need revision — the authority reallocation might be better modeled as an independent constraint influenced by, but not identical to, the ontological-status kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether ontological status and interpretive authority are one kernel or two correlated kernels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__created_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__created_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qura_tr_t50, quran_ontological_status__created_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement(qura_tr_t100, quran_ontological_status__created_reading, theater_ratio, 100, 0.15).
narrative_ontology:measurement(qura_tr_t150, quran_ontological_status__created_reading, theater_ratio, 150, 0.18).
narrative_ontology:measurement(qura_tr_t200, quran_ontological_status__created_reading, theater_ratio, 200, 0.2).
narrative_ontology:measurement(qura_tr_t250, quran_ontological_status__created_reading, theater_ratio, 250, 0.2).
narrative_ontology:measurement(qura_tr_t300, quran_ontological_status__created_reading, theater_ratio, 300, 0.2).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__created_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(qura_be_t50, quran_ontological_status__created_reading, base_extractiveness, 50, 0.34).
narrative_ontology:measurement(qura_be_t100, quran_ontological_status__created_reading, base_extractiveness, 100, 0.4).
narrative_ontology:measurement(qura_be_t150, quran_ontological_status__created_reading, base_extractiveness, 150, 0.42).
narrative_ontology:measurement(qura_be_t200, quran_ontological_status__created_reading, base_extractiveness, 200, 0.42).
narrative_ontology:measurement(qura_be_t250, quran_ontological_status__created_reading, base_extractiveness, 250, 0.42).
narrative_ontology:measurement(qura_be_t300, quran_ontological_status__created_reading, base_extractiveness, 300, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(quran_ontological_status__created_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__created_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_ontological_status__created_reading, 0.1).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the kernel quran_ontological_status. The uncreated_reading shares the same textual object but authors the opposite metaphysical claim (kalam Allah qadim) with a correspondingly different beneficiary/victim structure (favoring traditionalist jurists and literalist communities rather than rationalist theologians). The state_enforced_creation_reading shares THIS reading's theological content but adds coercive state enforcement (the mihna), which raises its ε substantially above this story's — the decomposition follows the ε-invariance principle: same doctrinal core, structurally distinct enforcement context, therefore a separate constraint rather than a single story with a variable enforcement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
