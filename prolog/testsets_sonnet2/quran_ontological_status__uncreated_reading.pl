% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__uncreated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Doctrine of Qur'anic Uncreatedness (Kalām Allāh Qadīm)
 *   domain: religious/theological/political
 *
 * SUMMARY:
 *   This story instantiates the UNCREATED reading of the contested kalām
 *   Allāh kernel: the Qur'an as eternal, uncreated divine speech, coeternal
 *   with God's essence. This is the reading that prevailed in mainstream
 *   Sunni orthodoxy following the mihna (833–848 CE), when the Abbasid
 *   state's attempted enforcement of the rival created-Qur'an doctrine
 *   collapsed under traditionalist resistance led by Ahmad ibn Hanbal. The
 *   reading treats revelation as an ontic mountain — permanent, unchangeable,
 *   existing independent of any human or state act — which maximizes
 *   prophetic and textual authority and privileges literalist hermeneutics.
 *   It is deliberately generated as one clean, ε-invariant reading among
 *   three: the sibling `created_reading` (Muʿtazilite doctrine, no state
 *   enforcement) and `state_enforced_creation_reading` (the same created
 *   doctrine but coercively imposed via inquisition) are separate constraint
 *   files with their own ε values and stakeholder sets, linked here via
 *   network.affects_constraints. This file does not average across or
 *   reference their internal contest — only the structural pressure this
 *   reading exerts on them.
 *
 * KEY AGENTS:
 *   - traditionalist_jurists: institutional beneficiaries whose legal authority is grounded in the text's fixed, eternal status
 *   - literalist_hadith_scholars: organized beneficiaries whose transmission-based method is validated by textual permanence
 *   - rationalist_theologians: trapped payers whose tawhid-based objections were marginalized after the mihna's reversal
 *   - textual_reform_movements: powerless payers foreclosed from contextual reinterpretation arguments
 *   - state_religious_authorities: institutional agenda-setters who enforce the doctrine as settled orthodoxy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, 0.58).
domain_priors:suppression_score(quran_ontological_status__uncreated_reading, 0.62).
domain_priors:theater_ratio(quran_ontological_status__uncreated_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__uncreated_reading, tangled_rope).
narrative_ontology:human_readable(quran_ontological_status__uncreated_reading, "Doctrine of Qur'anic Uncreatedness (Kalām Allāh Qadīm)").
narrative_ontology:topic_domain(quran_ontological_status__uncreated_reading, "religious/theological/political").

domain_priors:requires_active_enforcement(quran_ontological_status__uncreated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__uncreated_reading, 'b6a44bc6-f76b-4bc9-83ae-0842bff805a3').
narrative_ontology:cs_kernel_codification('b6a44bc6-f76b-4bc9-83ae-0842bff805a3', formalized).
narrative_ontology:cs_authority_grounding('b6a44bc6-f76b-4bc9-83ae-0842bff805a3', lineage).
narrative_ontology:cs_interpretation_layer_present('b6a44bc6-f76b-4bc9-83ae-0842bff805a3').
narrative_ontology:cs_reading_relation('b6a44bc6-f76b-4bc9-83ae-0842bff805a3', quran_ontological_status__created_reading, forecloses).
narrative_ontology:cs_reading_relation('b6a44bc6-f76b-4bc9-83ae-0842bff805a3', quran_ontological_status__state_enforced_creation_reading, forecloses).
narrative_ontology:cs_axiom('b6a44bc6-f76b-4bc9-83ae-0842bff805a3', foundational, divine_speech_is_eternal_attribute_not_creation).
narrative_ontology:cs_axiom_status(divine_speech_is_eternal_attribute_not_creation, holdable).
narrative_ontology:cs_axiom_grounding('b6a44bc6-f76b-4bc9-83ae-0842bff805a3', divine_speech_is_eternal_attribute_not_creation, theological).
narrative_ontology:cs_axiom('b6a44bc6-f76b-4bc9-83ae-0842bff805a3', secondary, textual_meaning_is_fixed_divine_fact).
narrative_ontology:cs_axiom_status(textual_meaning_is_fixed_divine_fact, holdable).
narrative_ontology:cs_axiom_grounding('b6a44bc6-f76b-4bc9-83ae-0842bff805a3', textual_meaning_is_fixed_divine_fact, deontological).
narrative_ontology:cs_reference_frame('b6a44bc6-f76b-4bc9-83ae-0842bff805a3', post_mihna_traditionalist_consolidation).
narrative_ontology:cs_drift_state('b6a44bc6-f76b-4bc9-83ae-0842bff805a3', contemporary_reformist_challenge, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b6a44bc6-f76b-4bc9-83ae-0842bff805a3', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__uncreated_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, traditionalist_jurists).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, literalist_hadith_scholars).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, anti_rationalist_theological_schools).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, rationalist_theologians).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, metaphorical_interpreters).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, textual_reform_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, lay_believers).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, divine_speech_coeternality_doctrine).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, textual_literalism_as_orthodoxy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer legal and doctrinal authority premised on the Qur'an's text being fixed, eternal, divine fact rather than a historically situated artifact. Their interpretive method (literal transmission, isnad-based authority) is validated precisely because the text they transmit is treated as ontologically unchanging. They control seminaries, fatwa councils, and religious courts that enforce this reading as orthodoxy.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, traditionalist_jurists, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, traditionalist_jurists, agenda_setter).

% Their scholarly authority rests on close textual transmission rather than rational speculation. An uncreated, eternal Qur'an makes their method (memorization, chain-of-transmission verification) the only legitimate route to divine truth, elevating their status relative to rationalist competitors and insulating their tradition from philosophical challenge.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, literalist_hadith_scholars, beneficiary,
    organized, civilizational, identity_locked, global).

% Ashʿarite and Hanbalite-aligned schools that built doctrinal and institutional identity around opposing Muʿtazilite rationalism. The uncreated-Qur'an doctrine is their signature victory; it structures curricula, creeds, and communal boundaries. Their institutional survival is bound to the doctrine remaining unquestioned.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, anti_rationalist_theological_schools, beneficiary,
    organized, civilizational, identity_locked, regional).

% Argue (following Muʿtazilite kalām) that an eternal, uncreated Qur'an compromises divine unity (tawhid) by positing a second eternal entity alongside God. Historically persecuted (post-mihna reversal), they now face exclusion from mainstream religious institutions, accusations of heresy, and loss of teaching posts when they advance this position. Their exit is blocked by the doctrine's entrenchment as communal orthodoxy.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, rationalist_theologians, payer,
    moderate, biographical, trapped, regional).

% Scholars who read scriptural anthropomorphisms and difficult passages figuratively (ta'wil) find their method delegitimized once the text itself is treated as eternally, literally fixed divine speech — literal reading becomes the default posture toward an uncreated text. They can publish in restricted academic circles but are excluded from mainstream doctrinal authority.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, metaphorical_interpreters, payer,
    moderate, biographical, constrained, regional).

% Modern reformist movements seeking to read Qur'anic legal injunctions as historically contextual (subject to reinterpretation for contemporary conditions) confront a doctrine that treats the text's meaning as fixed divine fact independent of historical circumstance. Reform proposals are met with accusations of denying the Qur'an's eternal nature, foreclosing legislative and social reform arguments grounded in textual flexibility.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, textual_reform_movements, payer,
    powerless, generational, trapped, national).

% Experience the doctrine as devotional comfort — the Qur'an as literally God's own eternal word offers certainty and unmediated access to divine truth. Benefit psychologically and communally from doctrinal stability, though they bear no direct role in enforcing or contesting it.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, lay_believers, beneficiary,
    powerless, biographical, identity_locked, global).

% In states with established religious authority (ministries of religious affairs, state muftis), enforce the uncreated reading as the settled orthodoxy, using it to deny licensure or platform to dissenting scholars. Historically this enforcement direction reversed after the mihna period, when the uncreated reading itself became the enforced state position rather than the mu'tazilite created reading.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, state_religious_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__uncreated_reading, traditionalist_jurists).
narrative_ontology:fixing_cost_class(quran_ontological_status__uncreated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, shared referent for legal and theological reasoning across the Muslim world: if the Qur'an's wording and meaning are fixed eternal divine fact, jurists across centuries and regions can build a coherent, cumulative legal and theological edifice on a single unchanging textual foundation rather than a contested, revisable one.
% TRANSFER_FUNCTION: Moves interpretive authority from those who would ground meaning in reason, historical context, or philosophical argument (rationalist theologians, reformers) to those whose authority derives from transmission and literal fidelity to a fixed text (traditionalist jurists, hadith scholars). It also transfers hermeneutic flexibility away from later generations and communities seeking reform toward the fixed authority of early textual transmission.
% ABSENT_VOICES: Muʿtazilite-descended rationalist schools were historically present and even briefly dominant (during the mihna) but were subsequently marginalized after the doctrine's consolidation under Ahmad ibn Hanbal's resistance and later Ashʿarite synthesis; their arguments about divine unity and the incoherence of positing eternal attributes alongside God's essence are preserved mostly in refutation literature written by their opponents, meaning their strongest form is often not transmitted in mainstream sources.
% DISAPPEARANCE_RATIONALE: If the uncreated-Qur'an doctrine were abandoned by mainstream Sunni orthodoxy overnight, the entire hermeneutic architecture of literalist jurisprudence would lose its metaphysical grounding: legal reasoning could shift toward treating textual meaning as historically situated and revisable, rationalist and reformist positions would gain institutional legitimacy, and the boundary between orthodox and heterodox theological schools (built substantially around this doctrine) would need to be redrawn.
% FOUNDING_PROBLEM: The doctrine was consolidated to resolve a theological crisis: how can God's speech to humanity be genuinely divine and authoritative without being either (a) a created, contingent thing subordinate to God (as Muʿtazilites argued, threatening its absolute authority) or (b) a threat to divine unity by positing something eternal alongside God (the rationalist objection). It also emerged from and hardened during a specific political conflict — the mihna (inquisition) under al-Ma'mun, al-Mu'tasim, and al-Wathiq, when the state attempted to enforce the created-Qur'an position and traditionalists led by Ahmad ibn Hanbal resisted, eventually prevailing under al-Mutawakkil.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist and Ashʿarite sources attest the doctrine resolves the tension by locating divine speech in God's eternal attribute of Kalām rather than as a temporal creation, and treat this as settled since the ninth-century consolidation. Historians of the mihna period (including scholars outside the traditionalist camp, and modern academic historians of early Islamic theology) corroborate that the doctrine's victory was substantially a political outcome of the mihna's failure and subsequent backlash, not solely a resolution on theological merits — meaning the 'problem' as framed by winners may not describe what actually settled the dispute.
narrative_ontology:disappearance_verdict(quran_ontological_status__uncreated_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__uncreated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__uncreated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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

:- end_tests(quran_ontological_status__uncreated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) rather than extreme because the doctrine genuinely does perform a coordination function — providing legal and theological continuity across a global, temporally extended community — even as it forecloses rationalist and reformist alternatives. Suppression (0.62) reflects real institutional enforcement (excommunication, denial of teaching posts, historical imprisonment during the mihna's aftermath for creationist dissenters) though it operates through communal and institutional exclusion more than direct state violence in most periods. Accessibility collapse is high (0.72): once a community accepts the ontological premise that the Qur'an is literally God's eternal speech, the space for treating the text as a revisable human-mediated artifact essentially disappears from within that framework — this is a genuine, deep collapse, not merely social pressure. Resistance (0.55) reflects the persistence of rationalist and reformist currents throughout Islamic intellectual history despite marginalization, from classical Muʿtazilite remnants and Ismaili philosophy to modern reformist theology.
 *
 * PERSPECTIVAL GAP:
 *   From the traditionalist jurist and hadith-scholar seats, this doctrine functions as a coordination mechanism securing continuity and preventing arbitrary reinterpretation — closer to a rope or even a mountain, since revelation's eternality is experienced as simply true, not chosen. From the rationalist theologian and reform-movement seats, the same doctrine operates as an enforced foreclosure mechanism that transfers interpretive authority away from reason and context toward literalist transmission — closer to a tangled rope or snare, since real coordination benefit (textual stability) coexists with asymmetric extraction (their exclusion from legitimate religious discourse). The engine should compute divergent per-seat types from this same structural data; the claimed_type (tangled_rope) reflects the authoring seat's judgment that both a genuine coordination function AND asymmetric extraction are present.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (traditionalist jurists, hadith scholars, anti-rationalist schools) get low d because their institutional authority and epistemic method are validated and even constituted by the doctrine — they collect status, legitimacy, and continued relevance from its truth being fixed. Victims (rationalist theologians, metaphorical interpreters, reform movements) get high d because the doctrine's premises directly foreclose the interpretive moves their positions require — they bear the cost of exclusion, marginalization, and historically, persecution. Lay believers sit near the beneficiary end for devotional/psychological reasons despite bearing no institutional power, hence their exit_options are identity_locked rather than trapped — their stake is in belief-identity, not institutional position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (resolving the ontological status of revealed speech in a way compatible with divine unity and prophetic authority) has arguably already been settled by orthodox consensus for over a millennium, yet the doctrine continues to actively foreclose rationalist and reformist theological work — this is not classic mandatrophy (mandate outliving function with no one left to serve) because the coordination function (communal doctrinal stability, legal continuity) remains genuinely live for the beneficiary communities. Classifying this as tangled_rope rather than snare or pure mountain prevents two errors: treating it as pure extraction would erase the real coordination value traditionalist communities derive from doctrinal stability; treating it as an unchallengeable natural mountain would erase the identifiable beneficiaries and the doctrine's traceable political-historical origin in the mihna conflict, and this is precisely the FSM (false summit) risk this reading must guard against by NOT claiming mountain status despite revelation being framed internally as ontologically necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    eternal_speech_vs_divine_unity,
    'Does positing an eternal, uncreated Qur''an alongside God''s eternal essence violate strict divine unity (tawhid), as the Muʿtazilite rationalists argued, or is Kalām properly understood as an eternal divine attribute rather than a second eternal entity, as Ashʿarite theology maintains?',
    'This is not empirically resolvable — it depends on which framework of divine attributes (Ashʿarite attribute-realism vs. Muʿtazilite attribute-nominalism) one accepts as theologically prior. Resolution would require adjudicating between incommensurable metaphysical frameworks, which the historical record shows was settled politically (mihna outcome) rather than philosophically.',
    'If the rationalist critique is granted force, the uncreated doctrine''s claim to resolve rather than merely displace the theological problem is undermined, weakening its ''mountain-like'' self-presentation and supporting a tangled_rope or even snare reading. If the Ashʿarite attribute framework is granted, the doctrine appears as a more successfully mountain-like resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(eternal_speech_vs_divine_unity, conceptual, 'Whether the uncreated doctrine coherently preserves divine unity or merely relocates the problem.').

omega_variable(
    theological_resolution_vs_political_victory,
    'Did the uncreated-Qur''an doctrine become orthodox because it resolved the underlying theological problem better than the created-Qur''an alternative, or because Ahmad ibn Hanbal''s faction won a political and social conflict (the mihna) against state-imposed Muʿtazilism, with doctrinal victory following political victory rather than the reverse?',
    'Historical analysis of the mihna''s actual dynamics — who held power before, during, and after; whether theological argument or political attrition (public unrest, elite defection, succession politics under al-Mutawakkil) drove the reversal; comparison with academic historiography of the period outside the traditionalist self-narrative.',
    'If political victory drove doctrinal consolidation, the doctrine''s self-presentation as simply recognizing an eternal truth (mountain framing) is significantly undermined, and the tangled_rope/snare structural reading (an extractive settlement dressed as natural fact) gains substantial support. If theological argument was genuinely decisive independent of political outcome, the doctrine''s claim to naturalness is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_resolution_vs_political_victory, empirical, 'Whether doctrinal consolidation tracked theological merit or political conflict outcome.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the correct unit of analysis ''the Qur''an''s ontological status'' (a metaphysical claim about the text) or ''the authority to determine hermeneutic method'' (a political-institutional claim about who gets to interpret)? These two framings could produce different classifications: the metaphysical framing looks more mountain-like (God''s nature is not a human choice), while the institutional framing looks more clearly tangled_rope/snare (a specific communities'' interpretive monopoly).',
    'Track whether debates about the doctrine, historically and today, are conducted primarily in metaphysical vocabulary (divine attributes, tawhid) or in institutional/political vocabulary (who may issue fatwas, whose exegesis is authoritative). If institutional vocabulary dominates in practice despite metaphysical vocabulary in theory, the institutional framing is doing the real work.',
    'Choosing the metaphysical framing would push this reading toward mountain (following the sibling created_reading''s parallel metaphysical framing); choosing the institutional framing supports the tangled_rope claimed_type authored here. This story adopts the institutional framing because the schema''s stakeholder/beneficiary structure requires observable, real-world actors, and the historical record (mihna) shows the doctrine''s consolidation was substantially about institutional authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether to frame this reading as a metaphysical claim (mountain-leaning) or an institutional authority claim (tangled_rope-leaning); this story adopts the institutional framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__uncreated_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__uncreated_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(qura_tr_t0, observed).
narrative_ontology:measurement(qura_tr_t200, quran_ontological_status__uncreated_reading, theater_ratio, 200, 0.2).
narrative_ontology:measurement_basis(qura_tr_t200, observed).
narrative_ontology:measurement(qura_tr_t400, quran_ontological_status__uncreated_reading, theater_ratio, 400, 0.22).
narrative_ontology:measurement_basis(qura_tr_t400, observed).
narrative_ontology:measurement(qura_tr_t600, quran_ontological_status__uncreated_reading, theater_ratio, 600, 0.24).
narrative_ontology:measurement_basis(qura_tr_t600, observed).
narrative_ontology:measurement(qura_tr_t900, quran_ontological_status__uncreated_reading, theater_ratio, 900, 0.26).
narrative_ontology:measurement_basis(qura_tr_t900, observed).
narrative_ontology:measurement(qura_tr_t1200, quran_ontological_status__uncreated_reading, theater_ratio, 1200, 0.28).
narrative_ontology:measurement_basis(qura_tr_t1200, observed).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__uncreated_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(qura_be_t0, observed).
narrative_ontology:measurement(qura_be_t200, quran_ontological_status__uncreated_reading, base_extractiveness, 200, 0.52).
narrative_ontology:measurement_basis(qura_be_t200, observed).
narrative_ontology:measurement(qura_be_t400, quran_ontological_status__uncreated_reading, base_extractiveness, 400, 0.55).
narrative_ontology:measurement_basis(qura_be_t400, observed).
narrative_ontology:measurement(qura_be_t600, quran_ontological_status__uncreated_reading, base_extractiveness, 600, 0.56).
narrative_ontology:measurement_basis(qura_be_t600, observed).
narrative_ontology:measurement(qura_be_t900, quran_ontological_status__uncreated_reading, base_extractiveness, 900, 0.57).
narrative_ontology:measurement_basis(qura_be_t900, observed).
narrative_ontology:measurement(qura_be_t1200, quran_ontological_status__uncreated_reading, base_extractiveness, 1200, 0.58).
narrative_ontology:measurement_basis(qura_be_t1200, observed).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__uncreated_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(qura_su_t0, observed).
narrative_ontology:measurement(qura_su_t200, quran_ontological_status__uncreated_reading, suppression_requirement, 200, 0.85).
narrative_ontology:measurement_basis(qura_su_t200, observed).
narrative_ontology:measurement(qura_su_t400, quran_ontological_status__uncreated_reading, suppression_requirement, 400, 0.7).
narrative_ontology:measurement_basis(qura_su_t400, observed).
narrative_ontology:measurement(qura_su_t600, quran_ontological_status__uncreated_reading, suppression_requirement, 600, 0.6).
narrative_ontology:measurement_basis(qura_su_t600, observed).
narrative_ontology:measurement(qura_su_t900, quran_ontological_status__uncreated_reading, suppression_requirement, 900, 0.58).
narrative_ontology:measurement_basis(qura_su_t900, observed).
narrative_ontology:measurement(qura_su_t1200, quran_ontological_status__uncreated_reading, suppression_requirement, 1200, 0.62).
narrative_ontology:measurement_basis(qura_su_t1200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__uncreated_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_ontological_status__uncreated_reading, 0.1).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the natural-language concept 'the ontological status of the Qur'an' (the kalām Allāh kernel), per the ε-invariance principle. uncreated_reading (this file) represents the historically victorious Sunni orthodox position with ε=0.58. created_reading represents the Muʿtazilite doctrinal position absent state coercion (expected lower suppression, different beneficiary/victim structure — rationalist theologians as beneficiaries). state_enforced_creation_reading represents the same created doctrine but coercively imposed via the mihna inquisition (expected higher suppression and extraction, since state violence is directly implicated). The three stories share a kernel but are not the same constraint: their ε values, stakeholder sets, and enforcement structures differ substantially. This story exerts downstream influence on both siblings by structurally defining the 'default' orthodox position against which the created readings are read as heterodox or historically defeated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
