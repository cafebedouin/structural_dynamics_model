% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__uncreated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Qur'an as Uncreated Eternal Divine Speech (kalām Allāh qadīm)
 *   domain: theological/political
 *
 * SUMMARY:
 *   The uncreated reading of the Qur'an's ontological status holds that
 *   revelation is eternal divine speech (kalām Allāh qadīm), coeternal with
 *   God's essence, and therefore fixed, immutable, and determinate in
 *   meaning. This reading emerged in early Islamic theology as a counter to
 *   Mu'tazilite rationalism (which treated the Qur'an as created in time) and
 *   became institutionalized within Sunni orthodoxy from the Ash'arite and
 *   Maturidite schools onward. The constraint operates as a permanent
 *   theological fact that forecloses certain interpretive moves: treating the
 *   text as contingent, subject to historical-critical method, or malleable
 *   to accommodate reform. Beneficiaries include traditional juristic
 *   schools, literalist communities, and state authorities who leverage the
 *   doctrine for institutional power. Payers include rational theologians,
 *   reform movements, and modernists who require hermeneutical flexibility.
 *   This story instantiates the uncreated reading as ONE specific constraint;
 *   the created reading and state-enforced-creation reading are separate
 *   constraint files in the same kernel family (to be authored as companion
 *   stories).
 *
 * KEY AGENTS:
 *   - traditional_jurists: institutional beneficiaries whose legal methodologies depend on fixed textual meaning (d near 0.0)
 *   - literalist_communities: organized beneficiaries whose interpretive practice is secured by the constraint (d near 0.1)
 *   - anti_rationalist_schools: institutional agenda-setters that defend and transmit the doctrine (d near 0.05)
 *   - rational_theologians: moderate-power payers whose philosophical position is marginalized (d near 0.8)
 *   - reform_movements: moderate-power payers requiring textual flexibility for social reinterpretation (d near 0.75)
 *   - state_authorities: institutional agenda-setters leveraging the doctrine for orthodoxy enforcement (d near 0.2)
 *   - philosophical_materialists: excluded observers (d undefined; not a seat in the theological conversation)
 *   - academic analysts: observer seat (d near 0.5)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, 0.68).
domain_priors:suppression_score(quran_ontological_status__uncreated_reading, 0.71).
domain_priors:theater_ratio(quran_ontological_status__uncreated_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__uncreated_reading, mountain).
narrative_ontology:human_readable(quran_ontological_status__uncreated_reading, "Qur'an as Uncreated Eternal Divine Speech (kalām Allāh qadīm)").
narrative_ontology:topic_domain(quran_ontological_status__uncreated_reading, "theological/political").

domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__uncreated_reading, '8ce2e71e-48c3-43e9-9079-2c31f05532e1').
narrative_ontology:cs_kernel_codification('8ce2e71e-48c3-43e9-9079-2c31f05532e1', fixed_text).
narrative_ontology:cs_authority_grounding('8ce2e71e-48c3-43e9-9079-2c31f05532e1', lineage).
narrative_ontology:cs_interpretation_layer_present('8ce2e71e-48c3-43e9-9079-2c31f05532e1').
narrative_ontology:cs_reading_relation('8ce2e71e-48c3-43e9-9079-2c31f05532e1', quran_ontological_status__created_reading, forecloses).
narrative_ontology:cs_reading_relation('8ce2e71e-48c3-43e9-9079-2c31f05532e1', quran_ontological_status__state_enforced_creation_reading, coexists_with).
narrative_ontology:cs_axiom('8ce2e71e-48c3-43e9-9079-2c31f05532e1', foundational, revelation_ontologically_uncreated).
narrative_ontology:cs_axiom_status(revelation_ontologically_uncreated, holdable).
narrative_ontology:cs_axiom_grounding('8ce2e71e-48c3-43e9-9079-2c31f05532e1', revelation_ontologically_uncreated, deontological).
narrative_ontology:cs_axiom('8ce2e71e-48c3-43e9-9079-2c31f05532e1', secondary, textual_meaning_divinely_fixed).
narrative_ontology:cs_axiom_status(textual_meaning_divinely_fixed, holdable).
narrative_ontology:cs_axiom_grounding('8ce2e71e-48c3-43e9-9079-2c31f05532e1', textual_meaning_divinely_fixed, deontological).
narrative_ontology:cs_reference_frame('8ce2e71e-48c3-43e9-9079-2c31f05532e1', eternal_divine_speech_framework).
narrative_ontology:cs_drift_state('8ce2e71e-48c3-43e9-9079-2c31f05532e1', contemporary_pluralist_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8ce2e71e-48c3-43e9-9079-2c31f05532e1', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__uncreated_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, traditional_jurists).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, literalist_communities).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, anti_rationalist_schools).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, state_authorities).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, rational_theologians).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, reform_movements).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, divine_immutability_doctrine).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, prophetic_authority_maximalism).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, textual_meaning_fixity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Classical Islamic legal scholars who derive jurisprudential authority from treating revelation as eternal, uncreated divine utterance. Their method depends on textual meaning being fixed and determinable from the Qur'an itself. They interpret the constraint as a natural theological fact that licenses their interpretive autonomy from caliphal interference.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, traditional_jurists, beneficiary,
    institutional, generational, identity_locked, continental).

% Communities of Qur'an reciters, memorizers, and exegetes who maintain that every word and letter is divine, uncreated, and inviolable. The constraint secures their interpretive position and defends their practice against rationalist critique or state pressure to treat the text as historically contingent or malleable.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, literalist_communities, beneficiary,
    organized, generational, identity_locked, global).

% Theological and jurisprudential schools (Ash'arites, Maturidites, traditionalist Hanbalis) that treat uncreated speech as the foundation for refuting Mu'tazilite rationalism. They actively defend and transmit this doctrine through fatwa, teaching, and institutional practice. Institutionally organized within religious hierarchies and educational networks.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, anti_rationalist_schools, beneficiary,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, anti_rationalist_schools, agenda_setter).

% Mu'tazilite and Ash'ari rationalist thinkers who argue the Qur'an is created in time and belongs to the contingent order. They bear the cost of defending their position against institutional and theological pressure; their interpretive framework is marginalized within dominant institutional Islam; their careers are constrained by opposition from established jurists and state-backed orthodox authorities.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, rational_theologians, payer,
    moderate, generational, constrained, continental).

% Modern reform movements and reinterpretive schools seeking textual flexibility to accommodate historical-critical method, scientific findings, or social change. They require treating the Qur'an as a contingent artifact authored in a specific historical moment to justify hermeneutical innovation. The uncreated doctrine blocks this move; they are positioned as heterodox and face institutional resistance.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, reform_movements, payer,
    moderate, biographical, constrained, national).

% Political authorities that leverage the uncreated doctrine to establish caliphal authority over textual interpretation. The constraint provides theological cover for enforcing orthodoxy: if the Qur'an is uncreated divine speech, then the state's role as protector of the faith includes suppressing heterodox readings. They benefit from the constraint's closure of interpretive space.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, state_authorities, agenda_setter,
    institutional, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, state_authorities, beneficiary).

% Philosophers (medieval falsāfa tradition, modern secular thinkers) who would treat all texts, including the Qur'an, as human linguistic artifacts subject to naturalistic analysis. They are structurally excluded from the theological conversation by the uncreated doctrine, which asserts the Qur'an's transcendence from the natural order of creation.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, philosophical_materialists, excluded,
    moderate, generational, trapped, global).

% Contemporary academic analysts and historiographers studying the theological debates. They observe how the ontological status of the Qur'an structures interpretive authority and political legitimacy within Islamic institutional hierarchies.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, islamic_legal_scholars, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__uncreated_reading, traditional_jurists).
narrative_ontology:fixing_cost_class(quran_ontological_status__uncreated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes the Qur'an as a stable, immutable reference point for legal, theological, and moral reasoning. By treating revelation as uncreated and eternal, the constraint solves the coordination problem of how a diverse community can appeal to a shared textual authority without that authority being subject to revision, reinterpretation, or degradation over time.
% TRANSFER_FUNCTION: Transfers hermeneutical authority from rational argumentation and historical contingency to traditional juristic schools and literalist interpreters. Those who can claim to be transmitting unchanging divine speech gain institutional and epistemic power; those who treat the text as created, contingent, or subject to reinterpretation lose standing to adjudicate meaning within orthodox contexts.
% ABSENT_VOICES: Philosophers of language and scientific empiricists are structurally excluded — they would argue for treating the Qur'an as a historical artifact subject to textual criticism and naturalistic explanation. Reform movements and modernist thinkers who need textual flexibility are marginalized and face institutional suppression when they voice their position. Women's liberation movements seeking to reinterpret misogynistic verses are similarly excluded from legitimate interpretive conversation.
% DISAPPEARANCE_RATIONALE: If the uncreated doctrine were suddenly abandoned and the Qur'an treated as created divine speech (or as a human artifact), the entire structure of Islamic jurisprudence, theological education, and institutional authority would require reorganization. Literalist legal schools would lose their textual foundation; rationalist approaches would gain legitimacy; state authorities would lose theological cover for enforcing orthodoxy; reform movements would gain hermeneutical space for innovation. The legal, theological, and political systems that ride on this constraint would have to reconstitute themselves.
% FOUNDING_PROBLEM: After the Prophet's death, the Muslim community faced the problem of preserving the integrity of divine revelation in the absence of new prophecy. How could believers ensure that the Qur'an remained authoritative, uncorrupted, and universally binding across generations and territories? How could the community resist both rational reductionism (treating revelation as subject to philosophical critique) and sectarian fragmentation (allowing each group to reinterpret the text according to local needs)?
% FOUNDING_PROBLEM_CORROBORATION: Traditional jurists and anti-rationalist schools attest the founding problem remains live: the need to preserve textual immutability against rationalist erosion and modernist reinterpretation. Rational theologians and reform scholars attest the founding problem has been superseded: the community now requires flexibility to accommodate historical understanding and evolving social ethics. Modern academic historians document that the founding problem WAS acute in the 2nd–3rd centuries AH but that the 'solution' (ontological uncreatedness) was one option among several, not a discovered natural fact; the doctrine was institutionalized through state enforcement and scholastic dominance, not through inevitable theological truth.
narrative_ontology:disappearance_verdict(quran_ontological_status__uncreated_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__uncreated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__uncreated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_ontological_status__uncreated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__uncreated_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68) because the constraint excludes rational theological and reform interpretations from legitimate institutional discourse; it transfers hermeneutical authority from those who would reinterpret to those who defend literalism. The extraction is not violent rent collection (like a marketplace commission) but rather institutional closure of interpretive space — a constraint on what can be meaningfully said within orthodox Islamic institutions. Suppression is higher (0.71) because maintaining the doctrine requires active institutional enforcement: refutations of Mu'tazilites, inquisitions (mihna), fatwa condemnations, educational control, and state power. Theater is low (0.22) because the constraint genuinely performs a coordination function (textual stability) alongside its extraction function; the functional component is real, not purely performative. Accessibility collapse is very high (0.88) because, once the uncreated doctrine is asserted, alternatives (treating the Qur'an as created, historically contingent, or malleable) appear to collapse into obvious falsehood within traditional juristic discourse — the doctrine forecloses interpretive space nearly completely from within its own framework. Resistance is low (0.42) because the doctrine has been institutionalized through centuries of Sunni scholarship and state backing; overt challenge is rare, though covert reinterpretation persists. The measurement trajectory shows rising extractiveness and suppression over the interval (0–12), modeling increasing institutional rigidity and reduced tolerance for heterodoxy as the doctrine consolidates through education and state enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the traditional jurist seat: the uncreated doctrine is a natural theological fact, a mountain, that enables stable legal reasoning and protects the community from rationalist corruption. From the rational theologian seat: the uncreated doctrine is an institutional constraint, a snare, that suppresses legitimate philosophical inquiry and freezes hermeneutics in literalism. From the reform movement seat: the constraint is a snare that prevents social adaptation and gender-justice reinterpretation. From the state authority seat: the constraint is a rope that coordinates orthodoxy and justifies suppression of heterodoxy. From the academic observer seat: the constraint is a tangled rope that genuinely solves a coordination problem (textual stability) while extracting hermeneutical power from those who require flexibility. The engine computes all these classifications per-seat from the structural data; the authored claim (mountain) is deliberate divergence from what some seats would compute (snare, tangled rope). This divergence is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional jurists and literalist communities are structural beneficiaries (near d=0.0) because the constraint's operation directly enables their authority: textual meaning is determinate, textual authority is maximized, and their role as interpreters of unchanging divine speech is secured. Anti-rationalist schools are also beneficiaries but also agenda-setters (low d, high power) because they actively maintain and defend the doctrine; state authorities are similarly dual-positioned (beneficiary/agenda-setter) because they leverage the constraint for orthodoxy enforcement. Rational theologians and reform movements are targets (high d, near 0.75–0.8) because they must defend their alternative readings against institutional pressure and suffer exclusion from orthodox discourse; their exit options are constrained (they remain within Islamic intellectual tradition but at marginalized status) and their identity is locked (being a theologian or reformer means remaining committed to their interpretive framework even when institutionally isolated). The asymmetry is stark: those who benefit from fixed meaning gain institutional power; those who require flexibility lose standing. Philosophical materialists are excluded entirely (d undefined) — they occupy no seat in the theological conversation because the uncreated doctrine treats the Qur'an as ontologically transcendent from material/natural language analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving textual authority and preventing rationalist erosion) was acute in the 2nd–3rd centuries AH during the Mihna period and Mu'tazilite rise. By the 5th–6th centuries AH, the uncreated doctrine had been institutionalized within Sunni orthodoxy; Ash'arite and Maturidite theology consolidated it as established fact; the rationalist threat had been contained (though not eliminated). In contemporary contexts, the founding problem's status is contested. Traditional institutions attest it remains live (threat from modernism, reform Islam, scientific biblical criticism). Reform movements attest the problem is obsolete or inverted: the constraint NOW prevents the community from responding to legitimate social-justice reinterpretations. Academic analysis suggests the founding problem WAS real but that its 'solution' (ontological uncreatedness) was one option among several; the doctrine persisted because it served the institutional interests of traditional jurists and state authorities, not because it was the only coherent answer. A mandatrophy flag would apply IF the constraint's enforcement costs (suppression, theater, institutional rigidity) now exceed the benefit of the coordination it provides. This is contested: beneficiaries (traditional jurists, literalists) argue the benefit (textual stability, legal certainty) justifies the cost; payers (reformers, modernists) argue the cost (interpretive restriction, social stagnation) is excessive. No neutral metric resolves the dispute; the classification divergence documents the disagreement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is the uncreated status of the Qur''an a discovered property of reality (a natural theological fact), or a constructed doctrine strategically institutionalized to consolidate orthodox authority against rationalist challenge?',
    'Historical-genealogical analysis: if the doctrine emerged gradually from sectarian dispute and institutional power struggles (Mihna period, Ash''arite/Maturidite consolidation) rather than from revelation or logical necessity, it is constructed. Textual evidence: if the Qur''an itself does not explicitly assert its own uncreatedness, the doctrine is a deduction from other premises, not a foundational fact.',
    'If natural/inevitable: the constraint is a genuine mountain (revelation is eternally coercive). If constructed: the constraint is a false summit (beneficiaries include traditional jurists and anti-rationalist schools; the doctrine exists because those parties have institutional power to enforce it). False-summit reclassification would subject the constraint to FSM override.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, conceptual, 'Whether uncreated status is discovered or institutionalized').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is suppression of rational theology and reform interpretation driven by external institutional coercion (inquisition, fatwa condemnation, state punishment) or by internalized identity fusion (rational theologians and reformers have internalized the belief that their own positions are heretical)?',
    'Post-exit suppression trajectory: if rational theologians and reformers in jurisdictions where state enforcement has relaxed (modern secular nation-states, academic freedom contexts) abandon their positions, suppression is internalized. If they flourish and organize when enforcement lapses, suppression is structural. Historical comparison: medieval Baghdadi suppression vs. contemporary pluralist contexts.',
    'If structural: suppression is an active enforcement cost that would dissolve with institutional change. If internalized: suppression persists even after external barriers are removed; the constraint''s effective suppressive force is higher than the institutional measure suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of heterodox theology is structural or internalized').

omega_variable(
    coordination_vs_extraction_boundary,
    'Is the uncreated doctrine necessary for solving the coordination problem of maintaining textual authority? Or does the coordination problem admit multiple solutions (created but immutable-in-practice, historically-situated-but-divinely-inspired, etc.), and the uncreated doctrine is chosen precisely because it ALSO maximizes literal interpretive authority and minimizes hermeneutical flexibility?',
    'Comparative institutional analysis: are there other theological traditions or Islamic communities that maintain textual authority and legal coherence WITHOUT the uncreated doctrine (Ismaili Shi''ism, Ithna Ashari Shi''ism, reform movements)? If so, the coordination function is separable from the ontological claim.',
    'If inseparable: a substantial portion of the measured extraction (0.68) is the necessary cost of coordination. If separable: the extraction 0.68 includes a pure monopoly-rent component from doctrine-enforced interpretive restriction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether uncreated doctrine is structurally necessary for textual coordination').

omega_variable(
    kernel_reading_contested_premise,
    'This constraint is one reading of the kernel ''Qur''an ontological status''. The uncreated reading claims revelation is eternal, coeternal with God''s essence, and therefore fixed in meaning. The created reading claims the Qur''an is a divine speech-act in time, subject to God''s power to create or not create. The state-enforced-creation reading asserts both the created doctrine AND state coercion of this orthodoxy. Which reading correctly describes the Qur''an''s true ontological status?',
    'This question is irreducible: it concerns a metaphysical claim (the ontic status of revelation) that admits no empirical test and no logical demonstration acceptable to all parties. The resolution is constitutively dependent on which religious authority framework (traditional jurisprudence, rational theology, state-backed orthodoxy, Shi''ite imamate theology) one acknowledges. No neutral epistemic vantage point exists.',
    'The classification of this constraint is reading-indexed: from the uncreated reading''s own lights, the Qur''an is a mountain (unchangeable, coeternal with God). From the created reading''s lights, the Qur''an is a contingent artifact whose status is itself contestable. From the state-enforced reading''s lights, both readings are subordinate to state determination. The engine computes a per-seat classification; this omega documents that the seat itself (the reading chosen) determines the type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contested_premise, preference, 'Irreducible theological premise about revelation''s ontic status').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__uncreated_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__uncreated_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(qura_tr_t0, projected).
narrative_ontology:measurement(qura_tr_t3, quran_ontological_status__uncreated_reading, theater_ratio, 3, 0.12).
narrative_ontology:measurement_basis(qura_tr_t3, projected).
narrative_ontology:measurement(qura_tr_t6, quran_ontological_status__uncreated_reading, theater_ratio, 6, 0.16).
narrative_ontology:measurement_basis(qura_tr_t6, observed).
narrative_ontology:measurement(qura_tr_t9, quran_ontological_status__uncreated_reading, theater_ratio, 9, 0.19).
narrative_ontology:measurement_basis(qura_tr_t9, observed).
narrative_ontology:measurement(qura_tr_t12, quran_ontological_status__uncreated_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement_basis(qura_tr_t12, observed).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__uncreated_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(qura_be_t0, projected).
narrative_ontology:measurement(qura_be_t3, quran_ontological_status__uncreated_reading, base_extractiveness, 3, 0.58).
narrative_ontology:measurement_basis(qura_be_t3, projected).
narrative_ontology:measurement(qura_be_t6, quran_ontological_status__uncreated_reading, base_extractiveness, 6, 0.63).
narrative_ontology:measurement_basis(qura_be_t6, observed).
narrative_ontology:measurement(qura_be_t9, quran_ontological_status__uncreated_reading, base_extractiveness, 9, 0.66).
narrative_ontology:measurement_basis(qura_be_t9, observed).
narrative_ontology:measurement(qura_be_t12, quran_ontological_status__uncreated_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement_basis(qura_be_t12, observed).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__uncreated_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(qura_su_t0, projected).
narrative_ontology:measurement(qura_su_t3, quran_ontological_status__uncreated_reading, suppression_requirement, 3, 0.61).
narrative_ontology:measurement_basis(qura_su_t3, projected).
narrative_ontology:measurement(qura_su_t6, quran_ontological_status__uncreated_reading, suppression_requirement, 6, 0.66).
narrative_ontology:measurement_basis(qura_su_t6, observed).
narrative_ontology:measurement(qura_su_t9, quran_ontological_status__uncreated_reading, suppression_requirement, 9, 0.69).
narrative_ontology:measurement_basis(qura_su_t9, observed).
narrative_ontology:measurement(qura_su_t12, quran_ontological_status__uncreated_reading, suppression_requirement, 12, 0.71).
narrative_ontology:measurement_basis(qura_su_t12, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__uncreated_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_ontological_status__uncreated_reading, 0.12).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-member kernel family. The kernel is the Qur'an's ontological status; the readings are: (1) uncreated_reading (this file) — revelation is eternal, coeternal with God, fixed in meaning. (2) created_reading — revelation is created in time, contingent on God's power, subject to reinterpretation. (3) state_enforced_creation_reading — Mu'tazilite doctrine (created) is enforced by state authority (mihna) as orthodoxy. Each reading is a separate constraint with its own ε, stakeholder structure, and classification. They are linked via network.affects_constraints because each reading's institutional success constrains the others. The upstream reading (uncreated) is most established; the created reading is contestative; the state-enforced reading is historical (the Mihna ended ~233 AH). Each sibling story links to this one and to the other sibling(s), forming a family network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_ontological_status__uncreated_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
