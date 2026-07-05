% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__uncreated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: The Qur'an as Uncreated Eternal Divine Speech (Kalām Allāh Qadīm)
 *   domain: Islamic Theology / Philosophy of Language / Political Authority
 *
 * SUMMARY:
 *   This story instantiates the UNCREATED reading of the contested kernel
 *   over the ontological status of the Qur'an: kalām Allāh qadīm — the
 *   doctrine that the Qur'an as divine speech is eternal and coeternal with
 *   God, not a created-in-time artifact. This reading became Sunni orthodoxy
 *   after the ninth-century mihna (the Abbasid inquisition that had briefly
 *   enforced the opposite, Mu'tazilite created-Qur'an position by state
 *   coercion) was reversed and Ash'ari theology consolidated. The reading
 *   treats revelation as entering constraint-space as something close to a
 *   permanent mountain — not chosen, not negotiable, not a policy — while its
 *   actual operation confers durable interpretive authority on traditionalist
 *   and literalist institutions and forecloses rationalist and reformist
 *   reading strategies. Two sibling constraints exist and are NOT modeled
 *   here: `created_reading` (the Mu'tazilite position that the Qur'an is a
 *   created, temporal artifact, preserving strict divine transcendence) and
 *   `state_enforced_creation_reading` (the historically specific mihna period
 *   in which the created-Qur'an position was imposed by state coercion via
 *   inquisition). Each sibling has its own epsilon, its own
 *   beneficiary/victim structure, and its own classification; this file
 *   addresses only the uncreated reading as it has operated as settled Sunni
 *   orthodoxy since roughly the tenth century.
 *
 * KEY AGENTS:
 *   - traditionalist_hadith_scholars: institutional beneficiaries whose authority as literal transmitters is maximized by an eternal, fixed text
 *   - literalist_jurist_class: institutional beneficiaries deriving law directly from an unchanging text
 *   - ashari_orthodoxy_establishment: institutional agenda-setter that codified and enforces the doctrine as creedal boundary
 *   - mutazilite_rational_theologians: trapped payers, historically purged from mainstream institutional legitimacy
 *   - metaphorical_interpreters and philosophically_trained_scholars: constrained payers whose interpretive methods are foreclosed
 *   - textual_reform_movements: powerless contemporary payers using the doctrine's coercive weight against them in reform debates
 *   - lay_believing_community: dual beneficiary/payer receiving certainty at the cost of interpretive rigidity
 *   - comparative_religious_scholars: analytical observers documenting the doctrine's political formation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, 0.58).
domain_priors:suppression_score(quran_ontological_status__uncreated_reading, 0.72).
domain_priors:theater_ratio(quran_ontological_status__uncreated_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__uncreated_reading, mountain).
narrative_ontology:human_readable(quran_ontological_status__uncreated_reading, "The Qur'an as Uncreated Eternal Divine Speech (Kalām Allāh Qadīm)").
narrative_ontology:topic_domain(quran_ontological_status__uncreated_reading, "Islamic Theology / Philosophy of Language / Political Authority").

domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__uncreated_reading, 'e2753c49-15b5-4b71-afe2-498a7b680209').
narrative_ontology:cs_kernel_codification('e2753c49-15b5-4b71-afe2-498a7b680209', fixed_text).
narrative_ontology:cs_authority_grounding('e2753c49-15b5-4b71-afe2-498a7b680209', lineage).
narrative_ontology:cs_interpretation_layer_present('e2753c49-15b5-4b71-afe2-498a7b680209').
narrative_ontology:cs_reading_relation('e2753c49-15b5-4b71-afe2-498a7b680209', quran_ontological_status__created_reading, forecloses).
narrative_ontology:cs_reading_relation('e2753c49-15b5-4b71-afe2-498a7b680209', quran_ontological_status__state_enforced_creation_reading, influences).
narrative_ontology:cs_axiom('e2753c49-15b5-4b71-afe2-498a7b680209', foundational, divine_speech_is_eternal_attribute).
narrative_ontology:cs_axiom_status(divine_speech_is_eternal_attribute, holdable).
narrative_ontology:cs_axiom_grounding('e2753c49-15b5-4b71-afe2-498a7b680209', divine_speech_is_eternal_attribute, theological).
narrative_ontology:cs_axiom('e2753c49-15b5-4b71-afe2-498a7b680209', secondary, textual_wording_shares_eternality_with_meaning).
narrative_ontology:cs_axiom_status(textual_wording_shares_eternality_with_meaning, holdable).
narrative_ontology:cs_axiom_grounding('e2753c49-15b5-4b71-afe2-498a7b680209', textual_wording_shares_eternality_with_meaning, theological).
narrative_ontology:cs_reference_frame('e2753c49-15b5-4b71-afe2-498a7b680209', ashari_post_mihna_consolidation).
narrative_ontology:cs_drift_state('e2753c49-15b5-4b71-afe2-498a7b680209', contemporary_reformist_challenge, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e2753c49-15b5-4b71-afe2-498a7b680209', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__uncreated_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, traditionalist_hadith_scholars).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, literalist_jurist_class).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, anti_rationalist_theological_schools).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, ashari_orthodoxy_establishment).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, mutazilite_rational_theologians).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, metaphorical_interpreters).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, textual_reform_movements).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, philosophically_trained_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, lay_believing_community).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, lay_believing_community).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, divine_speech_attribute_doctrine).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, quranic_textual_immutability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold that the Qur'an's wording, sound, and letters are themselves the eternal, uncreated attribute of God (or at minimum that the meaning is uncreated), which makes their transmitted readings of the text unfalsifiable by any rational or historical method. Their authority as narrators and preservers of the literal text is maximized precisely because the text cannot be treated as a contingent, time-bound human artifact open to reinterpretation.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, traditionalist_hadith_scholars, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, traditionalist_hadith_scholars, agenda_setter).

% Derive legal rulings (fiqh) directly from the plain wording of an eternal, unchanging text. Uncreated status forecloses arguments that the text's legal content is historically situated and therefore revisable, which entrenches their interpretive monopoly and the durability of rulings issued under it across centuries.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, literalist_jurist_class, beneficiary,
    institutional, civilizational, arbitrage, global).

% Post-mihna Sunni orthodoxy codified the uncreated doctrine as a creedal test, administering it through educational institutions, fatwa councils, and social exclusion of dissenters. They set and enforce the boundary of acceptable belief, converting a theological claim into a marker of communal membership.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, ashari_orthodoxy_establishment, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Argue that an eternal, uncreated Qur'an alongside an eternal God threatens strict monotheism (tawhid) by positing a second eternal entity, and that createdness better preserves divine transcendence. After the mihna's reversal and Ash'ari consolidation, holding this position exposed a scholar to charges of heresy, loss of teaching posts, and social excommunication; exit into open advocacy is effectively closed within mainstream institutions.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, mutazilite_rational_theologians, payer,
    moderate, generational, trapped, regional).

% Philosophically-inclined exegetes (falsafa-influenced, some Sufi metaphysicians) who read anthropomorphic or difficult verses figuratively bear the cost of a doctrine that treats the literal wording as itself the eternal divine attribute, since allegorical readings can be framed as denying an essential attribute of God rather than as legitimate interpretive method.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, metaphorical_interpreters, payer,
    moderate, generational, constrained, regional).

% Modern reformist and modernist Muslim thinkers seeking to historicize revelation (situate verses in seventh-century Arabian context to permit contemporary reinterpretation, especially on gender, slavery, and penal law) find the uncreated doctrine used against them as a bar: if the text's meaning is coeternal with God rather than a contingent human-historical artifact, contextualizing it can be characterized as denying the faith itself.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, textual_reform_movements, payer,
    powerless, biographical, constrained, national).

% Scholars trained in Hellenistic-influenced philosophy (falsafa) who attempted to reconcile revelation with philosophical accounts of divine simplicity and causation found the uncreated-speech doctrine, once creedally fixed, foreclosed lines of inquiry that questioned multiplicity of eternal attributes; several prominent thinkers faced condemnation (takfīr) partly on these grounds.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, philosophically_trained_scholars, payer,
    moderate, generational, constrained, regional).

% Ordinary believers receive a psychologically stabilizing certainty: the words they recite are literally and eternally God's own speech, unmediated by human or historical contingency. They also inherit the doctrine's rigidities — legal and social norms grounded in the literal text are correspondingly harder to revise even where local custom or changed circumstance would favor flexibility.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, lay_believing_community, beneficiary,
    powerless, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, lay_believing_community, payer).

% Study the doctrine's historical formation (the mihna, the Ash'ari-Mu'tazilite dispute, parallels with Christian Logos theology and Jewish Torah-eternity debates) without a stake in its truth, documenting how a contested theological claim was institutionally settled and enforced across the Sunni world.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, comparative_religious_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__uncreated_reading, diffuse).
narrative_ontology:fixing_cost_class(quran_ontological_status__uncreated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a single, stable referent for Islamic law, theology, and communal identity: if the Qur'an's wording is eternal and uncreated, its content cannot be relativized by changing historical or political circumstance, giving the community a shared, non-negotiable textual anchor across vast time and geography.
% TRANSFER_FUNCTION: Moves interpretive authority and social legitimacy toward those who control transmission and literal exegesis of the fixed text (hadith transmitters, literalist jurists, creedal enforcers) and away from those whose methods require treating the text as historically situated or metaphorically read (rationalist theologians, philosophers, reformers).
% ABSENT_VOICES: Mu'tazilite theologians and their intellectual descendants were largely purged from mainstream Sunni institutional memory after the mihna's reversal and Ash'ari consolidation; contemporary historical-critical Qur'an scholars operating outside state-sanctioned religious authority are excluded from doctrinal deliberation in most Muslim-majority states.
% DISAPPEARANCE_RATIONALE: If uncreated status were abandoned as settled doctrine, the entire edifice of literalist jurisprudence resting on textual immutability would lose its metaphysical warrant; legal reform movements, historical-critical exegesis, and rationalist theology would gain doctrinal legitimacy they currently must fight for, and clerical authority currently grounded in stewardship of an eternal text would need a different legitimating basis.
% FOUNDING_PROBLEM: The ninth-century mihna (inquisition) under Caliph al-Ma'mun attempted to impose the Mu'tazilite created-Qur'an position by state force; the traditionalist/Ash'ari counter-reaction that produced the uncreated doctrine's creedal fixation was built to resist state coercion of theology and to secure scriptural authority against both rationalist reinterpretation and caliphal overreach.
% FOUNDING_PROBLEM_CORROBORATION: Sunni traditionalist scholarship attests the doctrine remains necessary to guard against theological relativism and state manipulation of scripture. Independent historians of Islamic thought (including scholars working outside any confessional commitment to the doctrine's truth) corroborate that the creedal fixation also functioned, and continues to function, as an institutional mechanism excluding rationalist and reformist readings from mainstream legitimacy long after the specific mihna crisis ended — a status the traditionalist beneficiaries themselves do not generally frame as extraction.
narrative_ontology:disappearance_verdict(quran_ontological_status__uncreated_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__uncreated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__uncreated_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_ontological_status__uncreated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__uncreated_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at a moderate-high 0.58 — not the near-zero of a genuine mountain, because the doctrine's actual social operation transfers real interpretive and institutional authority toward specific scholarly classes rather than merely describing a metaphysical fact with no distributive consequence. Suppression is authored higher (0.72) reflecting the doctrine's history as an enforced creedal boundary (the reversal of the mihna did not eliminate coercive enforcement of orthodoxy — it inverted which position was coerced) and its continuing use to foreclose reformist and rationalist readings via charges of unbelief. Accessibility collapse is high (0.71): once the doctrine is accepted as settled orthodoxy, alternative rational-theological framings become nearly unavailable within mainstream institutions, though not zero — Mu'tazilite thought persisted in some strands (Zaydism, parts of Shi'a theology) and modern reformists continue to contest it. Resistance is moderate-high (0.62), reflecting persistent Mu'tazilite-descended and modernist rational theology that never fully disappeared. Theater ratio is modest (0.28): most of the doctrine's operation is substantive (real legal and creedal consequences follow from it), though a portion of contemporary invocation of the doctrine functions more as identity-marking boundary policing than live theological argument.
 *
 * PERSPECTIVAL GAP:
 *   From the traditionalist/Ash'ari agenda-setting seat, the doctrine is Mountain: a metaphysical fact about God's nature and speech, discovered through revelation and theological necessity, not constructed for anyone's benefit. From the Mu'tazilite-descended and reformist payer seats, the same doctrine computes closer to Tangled Rope or Snare: it does perform a real coordination function (textual stability across a vast, diverse community) but that coordination is inseparable from an enforced asymmetry that silences a specific class of theological and legal argument. The engine's structural data — beneficiary/victim declarations, enforcement history, accessibility collapse — is what should produce this divergence; the claimed_type of mountain here documents the traditionalist community's own honest self-understanding, not a prediction of what the engine will compute.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditionalist hadith scholars, literalist jurists, and the Ash'ari establishment sit near the beneficiary end: the doctrine's fixed-text ontology directly grounds and stabilizes their institutional authority, and they possess arbitrage-grade exit (they can move fluidly among institutions that share the doctrine, with no structural cost). Mu'tazilite theologians, metaphorical interpreters, and philosophically-trained scholars sit near the target end: the doctrine's operation directly forecloses their methods and, historically, exposed them to charges of heresy; their exit options are trapped or constrained because leaving mainstream institutional theology means losing scholarly legitimacy entirely, not relocating to an equally legitimate alternative. Textual reform movements are powerless payers with constrained exit — they operate within contemporary nation-states where the doctrine (or its descendants) still carries real legal and social force. The lay community occupies a genuinely dual position: real psychological and communal benefit from textual certainty, but identity-locked exit, since abandoning the doctrine is experienced as abandoning the faith itself rather than as a revisable policy choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (resisting mihna-era state coercion of theology, and securing scripture against both caliphal overreach and unconstrained rationalist relativization) was live and urgent in the ninth century. Whether it remains live today is genuinely contested: traditionalist scholarship holds the underlying danger (state or elite manipulation of scripture, theological relativism) is perennial and the doctrine's protective function still operates; independent historians and reformist theologians hold that the specific mihna crisis ended over a millennium ago and that the doctrine now persists primarily as an inertial institutional boundary-marker rather than as an active defense against any present coercive threat. This is exactly the mismatch pattern the R5 interview is built to surface: founding_problem_status is authored as contested rather than resolved in either direction, and the corroboration explicitly notes that non-beneficiary historians read the doctrine's contemporary function differently from its traditionalist beneficiaries — without treating that reading as dispositive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_orthodoxy,
    'Is the uncreated-Qur''an doctrine a genuine metaphysical discovery about the nature of divine speech (a Mountain, true independent of any human institution), or is it a theological position that became orthodoxy through a specific ninth-century political and institutional victory (the reversal of the mihna and Ash''ari consolidation), and which now persists partly because it benefits identifiable scholarly and clerical classes?',
    'This is not resolvable by empirical inquiry in the way a physical claim would be — it is a first-order theological question internal to Islamic thought. What CAN be examined empirically is the doctrine''s historical trajectory: whether its acceptance tracks theological argument alone, or tracks political power (caliphal patronage, institutional consolidation, exclusion of rival scholarly networks) at the moments of its key consolidations.',
    'If the doctrine is a genuine mountain, the declared beneficiaries are incidental — people who happen to be well-positioned relative to a true metaphysical fact, not people who constructed or maintain it for advantage. If it is substantially a constructed orthodoxy, the beneficiary structure indicates a false summit: a claim presented as necessary metaphysical fact that in operation confers concentrated, defensible institutional advantage on specific scholarly classes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_orthodoxy, conceptual, 'Whether the uncreated doctrine is discovered metaphysical fact or politically consolidated orthodoxy — the central FSM-relevant ambiguity for this story.').

omega_variable(
    kernel_reading_selection_pressure,
    'Given that all three readings of the quran_ontological_status kernel (uncreated, created, state_enforced_creation) are structurally coherent theological positions, what historical and institutional factors explain why the uncreated reading became dominant rather than the created reading, given that the mihna initially enforced the opposite?',
    'Historical analysis of the mihna''s reversal under al-Mutawakkil, the rise of Ash''ari kalam as a middle path between pure traditionalism and Mu''tazilite rationalism, and the institutional/patronage networks that consolidated around each position in the ninth-eleventh centuries.',
    'If the uncreated reading''s dominance is substantially explained by contingent political victory rather than superior theological argument, this strengthens the case that the beneficiary structure named in this story reflects genuine capture rather than incidental alignment with truth. This omega documents where the reading''s disagreement with its siblings is located, per the committer-frame Rule 2.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, empirical, 'Historical explanation for why the uncreated reading defeated the created reading despite the mihna''s initial enforcement of the reverse.').

omega_variable(
    sibling_reading_structural_delta,
    'What would change structurally if the created_reading (Mu''tazilite) were adopted as the operative constraint instead of this one?',
    'Comparative analysis of the created_reading constraint file: under createdness, revelation is a temporal artifact, which reopens historicist and rationalist interpretive methods as theologically legitimate rather than heretical, redistributing interpretive authority toward rational theologians and away from literalist transmitters — approximately inverting this story''s beneficiary/victim structure.',
    'Confirms the two readings are genuinely distinct constraints (different epsilon, different beneficiaries, different victims) rather than one constraint viewed from two angles, satisfying the epsilon-invariance decomposition test.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Documents the structural delta to the created_reading sibling, per Rule 2''s routing requirement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__uncreated_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__uncreated_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(qura_tr_t0, observed).
narrative_ontology:measurement(qura_tr_t200, quran_ontological_status__uncreated_reading, theater_ratio, 200, 0.2).
narrative_ontology:measurement_basis(qura_tr_t200, observed).
narrative_ontology:measurement(qura_tr_t400, quran_ontological_status__uncreated_reading, theater_ratio, 400, 0.25).
narrative_ontology:measurement_basis(qura_tr_t400, observed).
narrative_ontology:measurement(qura_tr_t700, quran_ontological_status__uncreated_reading, theater_ratio, 700, 0.27).
narrative_ontology:measurement_basis(qura_tr_t700, observed).
narrative_ontology:measurement(qura_tr_t1000, quran_ontological_status__uncreated_reading, theater_ratio, 1000, 0.28).
narrative_ontology:measurement_basis(qura_tr_t1000, observed).
narrative_ontology:measurement(qura_tr_t1200, quran_ontological_status__uncreated_reading, theater_ratio, 1200, 0.28).
narrative_ontology:measurement_basis(qura_tr_t1200, observed).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__uncreated_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(qura_be_t0, observed).
narrative_ontology:measurement(qura_be_t200, quran_ontological_status__uncreated_reading, base_extractiveness, 200, 0.55).
narrative_ontology:measurement_basis(qura_be_t200, observed).
narrative_ontology:measurement(qura_be_t400, quran_ontological_status__uncreated_reading, base_extractiveness, 400, 0.6).
narrative_ontology:measurement_basis(qura_be_t400, observed).
narrative_ontology:measurement(qura_be_t700, quran_ontological_status__uncreated_reading, base_extractiveness, 700, 0.58).
narrative_ontology:measurement_basis(qura_be_t700, observed).
narrative_ontology:measurement(qura_be_t1000, quran_ontological_status__uncreated_reading, base_extractiveness, 1000, 0.57).
narrative_ontology:measurement_basis(qura_be_t1000, observed).
narrative_ontology:measurement(qura_be_t1200, quran_ontological_status__uncreated_reading, base_extractiveness, 1200, 0.58).
narrative_ontology:measurement_basis(qura_be_t1200, observed).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__uncreated_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(qura_su_t0, observed).
narrative_ontology:measurement(qura_su_t200, quran_ontological_status__uncreated_reading, suppression_requirement, 200, 0.85).
narrative_ontology:measurement_basis(qura_su_t200, observed).
narrative_ontology:measurement(qura_su_t400, quran_ontological_status__uncreated_reading, suppression_requirement, 400, 0.78).
narrative_ontology:measurement_basis(qura_su_t400, observed).
narrative_ontology:measurement(qura_su_t700, quran_ontological_status__uncreated_reading, suppression_requirement, 700, 0.7).
narrative_ontology:measurement_basis(qura_su_t700, observed).
narrative_ontology:measurement(qura_su_t1000, quran_ontological_status__uncreated_reading, suppression_requirement, 1000, 0.72).
narrative_ontology:measurement_basis(qura_su_t1000, observed).
narrative_ontology:measurement(qura_su_t1200, quran_ontological_status__uncreated_reading, suppression_requirement, 1200, 0.72).
narrative_ontology:measurement_basis(qura_su_t1200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__uncreated_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_ontological_status__uncreated_reading, 0.08).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% This constraint forms one of three linked readings of the kernel quran_ontological_status. created_reading models the Mu'tazilite position (Qur'an as temporal artifact, preserving strict divine transcendence) as a distinct constraint with an inverted beneficiary/victim structure. state_enforced_creation_reading models the historically bounded mihna episode in which the created position was itself imposed by state coercion — a constraint whose own claimed_type and metrics differ sharply from this one despite sharing surface content with created_reading, because state enforcement adds a coercive-apparatus dimension absent from the doctrinal position alone. All three should be read together as a constraint family; none is a complete account of 'the Islamic doctrine of Qur'anic ontology' in isolation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
