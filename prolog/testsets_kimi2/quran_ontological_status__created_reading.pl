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
 *   constraint_id: quran_ontological_status__created_reading
 *   human_readable: Qur'an as Created Divine Speech (Mu'tazilite Rational Theology Reading)
 *   domain: theological/political/philosophical
 *
 * SUMMARY:
 *   This constraint story instantiates the created_reading of the
 *   quran_ontological_status kernel. The standing arrangement under contest
 *   is the doctrinal commitment that the Qur'an is created divine speech
 *   (makhlÅ«q) and that God's essence transcends all temporal artifacts,
 *   including revelation. Under this reading, revelation functions as a
 *   coordination artifact (rope-like) rather than an unchangeable natural law
 *   (mountain), preserving divine transcendence (tanzÄ«h) and granting
 *   hermeneutic authority to rational theology. The arrangement is contested
 *   by the uncreated_reading (eternal divine speech) and is structurally
 *   distinct from the state_enforced_creation_reading, which adds caliphal
 *   inquisition (mihna) to this doctrinal base. The created reading extracts
 *   from traditionalist jurists and literalist communities by destabilizing
 *   the textual fixity their authority and identity require, while benefiting
 *   rationalist theologians, philosophical schools, and reform movements
 *   through interpretive flexibility.
 *
 * KEY AGENTS:
 *   - rationalist_theologians: Agenda-setter and primary beneficiary (organized/constrained) â develops the doctrine and captures hermeneutic authority
 *   - traditionalist_jurists: Primary payer (powerful/constrained) â loses methodological authority from textual fixity erosion
 *   - literalist_communities: Secondary payer (powerless/identity_locked) â experiences ontological rupture in unmediated divine relationship
 *   - philosophical_schools: Secondary beneficiary (moderate/mobile) â gains metaphysical compatibility
 *   - reform_movements: Secondary beneficiary (moderate/constrained) â gains adaptive interpretive license
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__created_reading, 0.48).
domain_priors:suppression_score(quran_ontological_status__created_reading, 0.52).
domain_priors:theater_ratio(quran_ontological_status__created_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__created_reading, tangled_rope).
narrative_ontology:human_readable(quran_ontological_status__created_reading, "Qur'an as Created Divine Speech (Mu'tazilite Rational Theology Reading)").
narrative_ontology:topic_domain(quran_ontological_status__created_reading, "theological/political/philosophical").

domain_priors:requires_active_enforcement(quran_ontological_status__created_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__created_reading, 'a3c42849-e1e3-471d-b577-5071145fd7d1').
narrative_ontology:cs_kernel_codification('a3c42849-e1e3-471d-b577-5071145fd7d1', fixed_text).
narrative_ontology:cs_authority_grounding('a3c42849-e1e3-471d-b577-5071145fd7d1', expertise).
narrative_ontology:cs_interpretation_layer_present('a3c42849-e1e3-471d-b577-5071145fd7d1').
narrative_ontology:cs_reading_relation('a3c42849-e1e3-471d-b577-5071145fd7d1', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('a3c42849-e1e3-471d-b577-5071145fd7d1', quran_ontological_status__state_enforced_creation_reading, influences).
narrative_ontology:cs_axiom('a3c42849-e1e3-471d-b577-5071145fd7d1', foundational, divine_speech_is_created).
narrative_ontology:cs_axiom_status(divine_speech_is_created, holdable).
narrative_ontology:cs_axiom_grounding('a3c42849-e1e3-471d-b577-5071145fd7d1', divine_speech_is_created, deontological).
narrative_ontology:cs_axiom('a3c42849-e1e3-471d-b577-5071145fd7d1', foundational, absolute_divine_transcendence_requires_temporal_separation).
narrative_ontology:cs_axiom_status(absolute_divine_transcendence_requires_temporal_separation, holdable).
narrative_ontology:cs_axiom_grounding('a3c42849-e1e3-471d-b577-5071145fd7d1', absolute_divine_transcendence_requires_temporal_separation, deontological).
narrative_ontology:cs_reference_frame('a3c42849-e1e3-471d-b577-5071145fd7d1', absolute_tawhid_and_rational_theology).
narrative_ontology:cs_drift_state('a3c42849-e1e3-471d-b577-5071145fd7d1', post_mihna_sunni_orthodoxy, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('a3c42849-e1e3-471d-b577-5071145fd7d1', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__created_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, rationalist_theologians).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, philosophical_schools).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, reform_movements).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, traditionalist_jurists).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, literalist_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop, teach, and defend the doctrine that the Qur'an is created speech (makhlÅ«q), using dialectical theology (kalÄm) to preserve God's absolute transcendence (tanzÄ«h). They staff theological schools, adjudicate interpretive questions, and set the boundaries of acceptable theological method. Their institutional authority depends on the hermeneutic flexibility that createdness provides; if the text were eternal and fixed, rational speculation on divine attributes would be curtailed. Exit from this doctrinal position means abandoning the rationalist theological framework and its accumulated scholarly lineage.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, rationalist_theologians, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__created_reading, rationalist_theologians, beneficiary).

% Benefit from a theological framework that locates God outside temporal and material reality, making revelation compatible with Neoplatonic and Aristotelian metaphysics. They do not administer the doctrine but rely on its conclusions to prevent direct conflict between philosophy and scripture. Their exit is mobile because they can shift to allegorical or independent philosophical methods without institutional dependence on this specific theological verdict.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, philosophical_schools, beneficiary,
    moderate, civilizational, mobile, continental).

% Use the createdness doctrine to justify adaptive reinterpretation of legal and ethical verses in response to changing social conditions. The interpretive flexibility granted by a non-eternal text allows them to argue for reform without accusing the revelation of error. Their exit is constrained because reform within the tradition requires a theological license that the uncreated reading would structurally deny them.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, reform_movements, beneficiary,
    moderate, generational, constrained, global).

% Derive methodological authority from the premise that the Qur'anic text is eternal, fixed, and immediately normative. The createdness doctrine undermines the foundation of literalist jurisprudence by introducing a layer of rationalist mediation between the text and the legal ruling. They resist through traditionalist jurisprudence (fiqh) and hadith scholarship, but within institutions committed to createdness their authority is structurally subordinated to theological dialectic rather than textual transmission.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, traditionalist_jurists, payer,
    powerful, generational, constrained, global).

% Understand their religious identity as direct, unmediated submission to the literal word of God. The claim that the Qur'an is created introduces a temporal gap between God and speech that they experience as a rupture in their relational bond with the divine. Their identity is fused with the idea of immediate divine presence in the text; exit would require reconstructing their self-concept outside the frame of unmediated revelation, which they experience as existential rather than merely intellectual.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, literalist_communities, payer,
    powerless, biographical, identity_locked, local).

% Analyze the theological dispute as a historical and structural phenomenon, tracing how ontological commitments about the text distribute hermeneutic authority among competing scholarly factions. They neither benefit from nor pay the constraint, but document its effects on Islamic intellectual history.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, neutral_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__created_reading, rationalist_theologians).
narrative_ontology:fixing_cost_class(quran_ontological_status__created_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves divine absolute transcendence (tanzÄ«h) and unity (tawá¸¥Ä«d) by preventing identification of God with any temporal artifact; enables rational theology to reconcile revelation with philosophical inquiry; provides hermeneutic flexibility so that legal and ethical verses can be interpreted in light of changing contexts without impugning divine perfection.
% TRANSFER_FUNCTION: Moves hermeneutic authority and interpretive legitimacy from traditionalist jurists and literalist communities to rationalist theologians, philosophical schools, and reformers by making the text historically and ontologically contingent rather than eternally fixed.
% ABSENT_VOICES: Traditionalist jurists and literalist communities who hold the uncreated reading are formally present in the broader Islamic public sphere but are structurally excluded from rationalist-dominated theological institutions; their objections are recorded as heresy or intellectual naivety rather than as live alternatives within the rationalist framework.
% DISAPPEARANCE_RATIONALE: If the createdness doctrine vanished from the rationalist framework, divine speech would revert to uncreated eternal status in these institutions, textual meaning would fix around literalist jurisprudence, traditionalist jurists would reclaim methodological authority, and rationalist theology would face the unresolved problem of divine attributes being temporal. The scholarly landscape would reorganize around textual fixity.
% FOUNDING_PROBLEM: How to maintain God's absolute transcendence and unity (tawá¸¥Ä«d) while affirming the reality and authority of Qur'anic revelation, without making God subject to time, change, or materiality.
% FOUNDING_PROBLEM_CORROBORATION: Philosophical schools outside Islamic theology (Neoplatonic, Aristotelian) attest to the general problem of divine temporality; however, traditionalist jurists deny the problem was ever genuine and assert that the Mu'tazilite framing generated a pseudo-problem through Hellenistic premises. No party entirely outside the theological dispute corroborates the specific Mu'tazilite formulation of the problem â corroboration is split along sectarian lines.
narrative_ontology:disappearance_verdict(quran_ontological_status__created_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__created_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__created_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_ontological_status__created_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__created_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__created_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__created_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_ontological_status__created_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is moderate because the doctrine transfers significant hermeneutic authority from traditionalists to rationalists, but the transfer is intellectual and institutional rather than material. Suppression (0.52) reflects the active theological and institutional enforcement required to maintain createdness against the structurally dominant uncreated reading in Sunni Islam. Theater_ratio (0.40) reflects that while the transcendence argument is philosophically genuine, a substantial share of maintenance activity is performative reassertion of a minority position. Accessibility_collapse (0.45) is moderate: the uncreated alternative remains intellectually accessible and historically dominant, but within rationalist institutions it is practically excluded. Resistance (0.58) is high because traditionalist jurists and literalist communities mount sustained theological and social opposition. The temporal series run on one shared time grid; they show extraction and suppression rising during the doctrine's early institutionalization and then stabilizing as the reading settles into a minority but persistent rationalist niche.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (rationalist theologians) experiences the constraint as necessary coordination to preserve monotheism from anthropomorphism and textual reification. The payer seats (traditionalist jurists, literalist communities) experience the same constraint as an artificial theological apparatus that dissolves the immediacy of revelation. The gap is not reducible to mere disagreement; it is structural because the constraint distributes hermeneutic authority asymmetrically. The traditionalist jurist and the rationalist theologian may share the same physical or institutional space, but the constraint assigns them different epistemic statuses.
 *
 * DIRECTIONALITY LOGIC:
 *   Rationalist theologians are structural beneficiaries (low d): the constraint subsidizes their hermeneutic authority and preserves their theological framework. Philosophical schools and reform movements are secondary beneficiaries (low-moderate d): they collect coordination benefits without administering the constraint. Traditionalist jurists are structural targets (high d): the constraint extracts their methodological authority by making the text historically contingent rather than eternally fixed. Literalist communities are full targets (high d, amplified by identity_locked exit): their self-concept is constituted through unmediated divine speech, so the createdness doctrine extracts not just authority but ontological security. The engine will compute divergent per-seat classifications: beneficiary seats will experience coordination; payer seats will experience extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The created reading resists mandatrophy mislabeling because its coordination function is genuine and distinct from its extraction vector. It genuinely solves the theological problem of divine transcendence (a real coordination problem in monotheist metaphysics) while simultaneously extracting from traditionalists by subordinating textual literalism to rational interpretation. If we classified it as pure rope, we would erase the victims; if as pure snare, we would erase the transcendence-coordination it provides. Tangled_rope captures the hybrid structure. Decomposing this doctrinal reading from the state_enforced_creation_reading sibling prevents conflating doctrinal coordination with caliphal coercion, which would otherwise inflate suppression and theater inappropriately for the theological position alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer,
    'How does the created_reading of quran_ontological_status differ structurally from the uncreated_reading and state_enforced_creation_reading siblings?',
    'Comparative analysis of the three constraint stories in the kernel family; identifying which structural elements (enforcement mode, beneficiary set, extraction vector) change across readings.',
    'Clarifies whether the createdness doctrine is structurally separable from its enforcement mechanism; if separable, this reading stands as an independent tangled_rope rather than a snare disguised as theology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer, conceptual, 'Structural relationship between sibling readings in the Qur''an ontological status kernel.').

omega_variable(
    enforcement_mode_ambiguity,
    'Does the createdness reading persist through rational demonstration alone, or through active theological institutional enforcement?',
    'Historical analysis of curriculum control, appointment to teaching posts, and boundary-policing in institutions operating under this reading.',
    'If pure rational demonstration, extractiveness and suppression should be lower, possibly shifting computed type toward rope; if institutional enforcement is required, tangled_rope remains appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mode_ambiguity, empirical, 'Whether doctrinal persistence is intellectual or institutional.').

omega_variable(
    hermeneutic_substitution,
    'Does the createdness doctrine enable genuine interpretive pluralism, or does it substitute rationalist interpretive monopoly for traditionalist textual monopoly?',
    'Analysis of interpretive outcomes: range of acceptable readings under Mu''tazilite versus Athari hermeneutics.',
    'If substitution, the coordination function is thinner than claimed, pushing type toward snare; if genuine pluralism, coordination is thicker, supporting tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hermeneutic_substitution, conceptual, 'Whether hermeneutic flexibility is real or nominal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__created_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__created_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(qura_tr_t50, quran_ontological_status__created_reading, theater_ratio, 50, 0.24).
narrative_ontology:measurement(qura_tr_t100, quran_ontological_status__created_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement(qura_tr_t150, quran_ontological_status__created_reading, theater_ratio, 150, 0.35).
narrative_ontology:measurement(qura_tr_t200, quran_ontological_status__created_reading, theater_ratio, 200, 0.38).
narrative_ontology:measurement(qura_tr_t250, quran_ontological_status__created_reading, theater_ratio, 250, 0.4).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__created_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qura_be_t50, quran_ontological_status__created_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement(qura_be_t100, quran_ontological_status__created_reading, base_extractiveness, 100, 0.5).
narrative_ontology:measurement(qura_be_t150, quran_ontological_status__created_reading, base_extractiveness, 150, 0.52).
narrative_ontology:measurement(qura_be_t200, quran_ontological_status__created_reading, base_extractiveness, 200, 0.5).
narrative_ontology:measurement(qura_be_t250, quran_ontological_status__created_reading, base_extractiveness, 250, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__created_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(qura_su_t50, quran_ontological_status__created_reading, suppression_requirement, 50, 0.48).
narrative_ontology:measurement(qura_su_t100, quran_ontological_status__created_reading, suppression_requirement, 100, 0.55).
narrative_ontology:measurement(qura_su_t150, quran_ontological_status__created_reading, suppression_requirement, 150, 0.56).
narrative_ontology:measurement(qura_su_t200, quran_ontological_status__created_reading, suppression_requirement, 200, 0.54).
narrative_ontology:measurement(qura_su_t250, quran_ontological_status__created_reading, suppression_requirement, 250, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__created_reading, identity_coordination).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% The quran_ontological_status kernel decomposes into three structurally distinct constraints. The doctrinal createdness reading (this story) provides the theological content that the state_enforced_creation_reading enforces through caliphal coercion. The uncreated_reading is the structural negation of this constraint, classifying revelation as mountain rather than rope. Each story carries a different epsilon, beneficiary/victim structure, and enforcement mode.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
