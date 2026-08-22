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
 *   human_readable: The Uncreated Qur'an Settlement: Kalam Allah Qadim as Standing Sunni Orthodoxy
 *   domain: islamic_theology/philosophy_of_language/political_authority
 *
 * SUMMARY:
 *   This file instantiates ONE reading — the uncreated reading — of the
 *   contested kernel quran_ontological_status. Historical surface: between
 *   833 and 848 the Abbasid state coerced affirmation that the Qur'an is
 *   created (the mihna, authored separately as the sibling
 *   state_enforced_creation_reading); the reversal under al-Mutawakkil made
 *   the opposite doctrine the standing settlement of Sunni Islam: the Qur'an
 *   is the uncreated eternal speech of God (kalam Allah qadim), an ontic
 *   reality coeternal with the divine essence, whose meaning is fixed divine
 *   fact rather than contingent artifact. Authored as this reading's
 *   constraint, the doctrine's self-presentation is the maximal mountain
 *   claim — a constraint that predates all enforcement, collects from no one,
 *   and would hold even if no one defended it. The structural record shows
 *   otherwise-shaped facts: named classes collect from the settlement (the
 *   jurist establishment whose judicial and teaching authority it
 *   constitutes, the literalist traditionist communities whose belonging it
 *   marks, the Ash'ari-Maturidi schools that hold its chairs), and named
 *   classes pay (the Mu'tazila purged from office, the non-literal
 *   interpreters whose method was priced into suspicion, the reformers who
 *   need the text to bend). The claim/metric gap is deliberate and is the
 *   measurement: claimed_type records the reading's own structural claim; the
 *   metrics record the settlement's enforced, asymmetrically costly operation
 *   — the false-summit signature the engine is positioned to detect. Per Rule
 *   1 the contest is not described inside the constraint; the sibling
 *   readings are separate files linked in network.affects_constraints.
 *
 * KEY AGENTS:
 *   - sunni_jurist_establishment: Agenda-setter and collector (institutional/identity_locked) — administers the settlement; its authority is constituted by it
 *   - caliphal_and_successor_states: Agenda-setter (institutional/arbitrage) — enforced the settlement, flipped doctrinal enforcement when legitimacy calculations changed
 *   - literalist_traditionist_communities: Beneficiary (organized/identity_locked) — popular base whose belonging is constituted by the affirmation
 *   - ashari_maturidi_schools: Beneficiary (institutional/identity_locked) — articulate the doctrine and hold its chairs
 *   - mutazila_rationalist_theologians: Primary target (organized then marginalized/constrained) — purged from office, schools defunded, method criminalized
 *   - metaphorical_interpreters: Secondary target (moderate/constrained) — non-literal reading priced upward into suspicion
 *   - textual_flexibility_reformers: Target (moderate/constrained) — every departure from apparent meaning carries rising risk
 *   - modern_academic_historiography: Analytical observer (analytical/analytical) — sees the full structure from outside confessional commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, 0.7).
domain_priors:suppression_score(quran_ontological_status__uncreated_reading, 0.72).
domain_priors:theater_ratio(quran_ontological_status__uncreated_reading, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, theater_ratio, 0.54).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__uncreated_reading, mountain).
narrative_ontology:human_readable(quran_ontological_status__uncreated_reading, "The Uncreated Qur'an Settlement: Kalam Allah Qadim as Standing Sunni Orthodoxy").
narrative_ontology:topic_domain(quran_ontological_status__uncreated_reading, "islamic_theology/philosophy_of_language/political_authority").

domain_priors:requires_active_enforcement(quran_ontological_status__uncreated_reading).
domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__uncreated_reading, 'bd5de7fc-2b8f-45ac-82f9-d4889963adb6').
narrative_ontology:cs_kernel_codification('bd5de7fc-2b8f-45ac-82f9-d4889963adb6', fixed_text).
narrative_ontology:cs_authority_grounding('bd5de7fc-2b8f-45ac-82f9-d4889963adb6', lineage).
narrative_ontology:cs_interpretation_layer_present('bd5de7fc-2b8f-45ac-82f9-d4889963adb6').
narrative_ontology:cs_reading_relation('bd5de7fc-2b8f-45ac-82f9-d4889963adb6', quran_ontological_status__created_reading, forecloses).
narrative_ontology:cs_reading_relation('bd5de7fc-2b8f-45ac-82f9-d4889963adb6', quran_ontological_status__state_enforced_creation_reading, forecloses).
narrative_ontology:cs_axiom('bd5de7fc-2b8f-45ac-82f9-d4889963adb6', foundational, divine_speech_coeternal_with_essence).
narrative_ontology:cs_axiom_status(divine_speech_coeternal_with_essence, holdable).
narrative_ontology:cs_axiom_grounding('bd5de7fc-2b8f-45ac-82f9-d4889963adb6', divine_speech_coeternal_with_essence, theological).
narrative_ontology:cs_axiom('bd5de7fc-2b8f-45ac-82f9-d4889963adb6', secondary, interpreter_bounded_by_fixed_divine_meaning).
narrative_ontology:cs_axiom_status(interpreter_bounded_by_fixed_divine_meaning, holdable).
narrative_ontology:cs_axiom_grounding('bd5de7fc-2b8f-45ac-82f9-d4889963adb6', interpreter_bounded_by_fixed_divine_meaning, theological).
narrative_ontology:cs_reference_frame('bd5de7fc-2b8f-45ac-82f9-d4889963adb6', quran_as_coeternal_divine_attribute).
narrative_ontology:cs_drift_state('bd5de7fc-2b8f-45ac-82f9-d4889963adb6', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bd5de7fc-2b8f-45ac-82f9-d4889963adb6', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__uncreated_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, sunni_jurist_establishment).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, literalist_traditionist_communities).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, ashari_maturidi_schools).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, mutazila_rationalist_theologians).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, metaphorical_interpreters).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, textual_flexibility_reformers).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, eternal_divine_speech_thesis).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, fixed_textual_meaning_doctrine).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, prophetic_transmission_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines orthodoxy, staffs the courts and the madrasas, and administers credal education across the Sunni lands. Its judicial and teaching authority rests on the text being God's own eternal speech with fixed meaning; offices, chairs, and appointments flow through affirming and administering that settlement. Exit would mean forfeiting the authority its members' careers and standing are made of — the class has become its function as transmitters of uncreated speech.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, sunni_jurist_establishment, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, sunni_jurist_establishment, beneficiary).

% Reversed the mihna's created-text inquisition within fifteen years of imposing it, then enforced the uncreated settlement through patronage — endowing the orthodox schools' madrasas and purging remaining created-text holders from office. Doctrinal enforcement functioned as a legitimacy instrument: the dynasty that coerced creation under al-Ma'mun coerced its denial under al-Mutawakkil, and later states shifted patronage as calculations changed. Exit from any particular doctrine was cheap; commitment was not required.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, caliphal_and_successor_states, agenda_setter,
    institutional, generational, arbitrage, continental).

% The hadith-folk and Hanbali base whose identity fused with the uncreated doctrine when Ahmad ibn Hanbal refused the inquisition and was beaten for it. They supplied popular enforcement — Baghdad crowds harassed suspected rationalists for generations — and recite the doctrine as the boundary of belonging. Their membership in the orthodox community is constituted by the affirmation; leaving it would unmake who they are.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, literalist_traditionist_communities, beneficiary,
    organized, generational, identity_locked, continental).

% The theological schools that articulated uncreatedness using the rational method of their defeated opponents, winning state patronage and the madrasa chairs that came with it. Their teachers' posts, students, and endowments flow from administering the settlement they defend at the highest level of sophistication; the schools' institutional position and the doctrine's content have grown into each other.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, ashari_maturidi_schools, beneficiary,
    institutional, generational, identity_locked, continental).

% Held the judgeships and ran the inquisition before the reversal; after it they were purged from office, their schools lost patronage, and crowds attacked their books. They bear the settlement's costs directly — careers, institutions, and the public standing of rationalist method itself. Some migrated to Zaydi courts at the periphery; inside the Sunni lands their position became socially untenable, and they had no seat where orthodoxy was defined.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, mutazila_rationalist_theologians, payer,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, mutazila_rationalist_theologians, excluded).

% Practitioners of non-literal reading — those who take anthropomorphic verses and difficult legal texts as figurative. The settlement's fixed-meaning privilege raises the price of every non-literal reading; the emblem is al-Andalus, where Averroes was banished and philosophical books burned in the 1190s. The practice survives under persistent suspicion, its practitioners one accusation from trial.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, metaphorical_interpreters, payer,
    moderate, biographical, constrained, continental).

% Jurists and thinkers who need the fixed text to bend — novel cases, changed conditions, sciences the text does not name. Each departure from apparent meaning carries rising risk as the settlement hardens and the madrasa system standardizes. The class recurs across the whole life of the settlement and extends beyond this interval to the modernist movements the kernel's structural record names.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, textual_flexibility_reformers, payer,
    moderate, biographical, constrained, continental).

% Studies the inquisition and its reversal with no doctrinal stake in either answer, from archives and chronicles outside confessional control. It attests the founding crisis's political-institutional character and the settlement's consolidation arc. Its seat is analytical: it neither recites nor administers.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, modern_academic_historiography, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__uncreated_reading, sunni_jurist_establishment).
narrative_ontology:fixing_cost_class(quran_ontological_status__uncreated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a single answer to what the recited text is, stabilizing the Qur'an's authority as the foundation of law and creed; gives the community one shared credal boundary and one fixed textual reference for courts, schools, and prayer after a generation of state-coerced dispute over the text's status.
% TRANSFER_FUNCTION: Moves interpretive authority and institutional position from rationalist theologians and non-literal interpreters to the jurist-literalist establishment; moves public doctrinal loyalty — affirmation of uncreatedness — from the community's members to the establishment as the price of office, teaching standing, and orthodox belonging; the costs of interpretive flexibility fall on reform-minded scholars while the benefits of fixed meaning accrue to the holders of judicial and teaching authority.
% ABSENT_VOICES: The Mu'tazila were excluded by design — the post-848 reversal purged them from the judgeships and councils where orthodoxy was being defined, so the settlement's unanimity was produced partly by their absence from the room. Non-literal interpreters and flexibility-needing scholars had no seat in credal formulation; ordinary believers received the doctrine as settled recitation rather than participating in its determination. The consensus-provenance check should read this unanimity as partially manufactured by exclusion.
% DISAPPEARANCE_RATIONALE: The jurist establishment's authority, the madrasa curriculum's credal core, and the orthodox/deviant boundary all rest on the doctrine; if it vanished overnight, law's textual foundation would need re-grounding (the created-text alternative grounds authority in divine wisdom and communal benefit rather than eternal essence), the orthodox majority would lose its credal shibboleth, and the interpretive settlement channeling office and standing to the literalist class would dissolve into open hermeneutic competition.
% FOUNDING_PROBLEM: The mihna-era crisis: an inquisition coerced affirmation that the Qur'an is created, threatening the text's authority with contingency (a created text is an artifact whose authority must be grounded in something else) and splitting the community. The uncreated settlement was built to secure the text's authority against contingency, answer the inquisition's challenge, and restore communal unity under a fixed credal core.
% FOUNDING_PROBLEM_CORROBORATION: Extra-confessional academic historiography of the mihna attests the founding crisis was a specific ninth-century political-institutional conflict, now closed, whose settlement outlived its occasion; confessional traditionalist authorities attest the underlying problem — securing divine textual authority against rationalist reduction — as permanently live. No attestation from outside the beneficiary set supports the permanently-live reading as anything other than confessional commitment; the closure reading is the one with corroboration from outside all confessional parties. Both attestations are recorded; the status is authored contested because the dispute between them is live.
narrative_ontology:disappearance_verdict(quran_ontological_status__uncreated_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__uncreated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__uncreated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_ontological_status__uncreated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__uncreated_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.70: the settlement's costs concentrate on named seats — the purged rationalist theologians, the interpreters whose non-literal method carried rising risk (Averroes banished, books burned, 1190s), the reformers whose every departure from apparent meaning grew costlier as the madrasa system hardened — while its benefits diffuse across the orthodox majority. Suppression 0.72: within the Sunni mainstream the created alternative became socially impossible to hold, maintained by purges, popular enforcement, and patronage gatekeeping; the suppression_requirement series shows the active-machinery arc (spin-up after the reversal, institutional peak as the madrasa system absorbed enforcement, decline into normalization as alternatives died socially) while the structural scalar stays high because the closed alternative set persisted after the machinery relaxed (see omega suppression_structural_vs_internalized). Theater 0.54: by the interval's end, affirming uncreatedness functioned more as loyalty recitation than live theology for most holders — the series reaches the 0.5 Goodhart threshold around the 1195 point. Accessibility_collapse 0.75: within the reading's own framework, accepting coeternity collapses the created alternative almost completely — it reads not as a wrong answer but as an impossible one — yet the created reading persisted for centuries at the periphery (Buyid Baghdad, Zaydi Yemen), so the collapse is short of mountain-grade. Resistance 0.58: fierce early (the purged party had just run an inquisition of its own, and Buyid patronage kept Mu'tazilite kalam alive in Baghdad to roughly 1050), decaying to marginal as orthodoxy consolidated. All three series share one seven-point grid (848-1258) so no metric's end-state leaks into earlier rows. Claim and metrics are independent authored facts; the divergence is the data. Coordination type is authored identity_coordination — the settlement's operative function is boundary maintenance, the creed as membership marker — with the FNL gaming risk flagged: identity framing is precisely the cover under which asymmetric costs hide, and the type's complexity offset must not excuse the concentration of costs on the paying seats.
 *
 * PERSPECTIVAL GAP:
 *   From the jurist establishment's seat the doctrine is the ground of all authority — not an arrangement at all but the structure of reality — and the engine should compute a near-mountain experience there. From the purged Mu'tazilite seat the same structure is an enforced settlement that destroyed their schools and criminalized their method — a maximally costly experience. The caliphal seat diverges again: with arbitrage exit (the same dynasty coerced creation in 833 and coerced its denial in 848), doctrine was policy rather than commitment, and that seat's experience tracks legitimacy calculations, not ontology. The engine computes these per-seat divergences from the structural data; the authored mountain claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (sunni_jurist_establishment, literalist_traditionist_communities, ashari_maturidi_schools) drive low directionality — the settlement subsidizes these seats, and identity_locked exit deepens the capture (they cannot leave without dissolving the authority or belonging the settlement constitutes). Victim declarations (mutazila_rationalist_theologians, metaphorical_interpreters, textual_flexibility_reformers) drive high directionality — they bear the settlement's costs, with constrained rather than arbitrage exit (migration to Zaydi peripheries was possible at the price of institutional extinction inside the Sunni lands). The caliphal_and_successor_states seat is an agenda-setter without beneficiary declaration: it administered and enforced, collected legitimation, and exited doctrines cheaply — mid-range directionality by structure. Suppression is authored as a raw structural property and is not scaled by power or scope; extractiveness scales with directionality and continental scope — verification of orthodoxy across the dar al-Islam was hard, which amplifies effective extraction at the paying seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding crisis — the mihna-era question of whether a created text could ground law and creed, weaponized by an inquisition — is historically closed: its parties are dead, its enforcement episode ended in the 850s, and extra-confessional historiography attests the closure. Yet the arrangement persists, now carried by recitation and institutional inertia, with the theater series reaching the drift threshold before the interval's end. The R5 fields record founding_problem_status=contested over disappearance_verdict=world_rearranges: the parties dispute whether the underlying problem (securing divine textual authority) is permanently live or historically closed, while the arrangements demonstrably depend on the settlement. The classification prevents two opposite mislabels: it refuses the reading's self-presentation as a pure natural-law fixture (named classes collect from enforcement — the false-summit signature), and it refuses the opposite reduction to pure extraction (the settlement does solve a real coordination problem — a single fixed authoritative text for law, creed, and belonging — and the created alternative never demonstrated a cheaper solution inside the Sunni mainstream). The dead-or-contested founding problem over a world that rearranges is left to the consumer as the mismatch flag, with the confessional/conceptual dispute carried by the omegas rather than pre-empted here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontic_fact_vs_constructed_settlement,
    'Is the uncreated doctrine a fact about reality — making this a genuine fixture that would persist regardless of enforcement and collect from no one — or a ninth-century political-theological settlement that identifiable classes collect from?',
    'Not resolvable by data inside the framework: within the tradition''s axioms the doctrine is fact, while extra-confessional history shows a dated, enforced settlement with a reversal, purges, and patronage flows. The false-summit signature treats beneficiary presence as the operative test; framework-internal resolution would require the axioms themselves to be adjudicated.',
    'If constructed, the constraint classifies as a hybrid coordination/extraction arrangement with the jurist establishment as the collecting seat; if fact, the mountain claim stands and the enforcement history is defense of truth rather than maintenance of an arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontic_fact_vs_constructed_settlement, conceptual, 'Natural-law vs. constructed-settlement ambiguity of the uncreated doctrine (required for a mountain with declared beneficiaries).').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (purges, popular mobs, book-burning, patronage gatekeeping) or internalized (the doctrine as unquestionable background such that adherents cannot entertain the created alternative)?',
    'Post-enforcement trajectory: where enforcement machinery vanished (post-Mongol fragmentation, modern secular states), dissent re-emerged where suppression was structural and stayed absent where internalized; compare Zaydi peripheries and modern academic contexts against the Sunni mainstream.',
    'If substantially internalized, the constraint''s effective suppression exceeds the structural measure — adherents carry the closed alternative set after enforcement removal, and the falling suppression_requirement trajectory overstates the settlement''s liberalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism in the orthodox settlement.').

omega_variable(
    coordination_extraction_separability,
    'Is the settlement''s coordination function (a single fixed authoritative text for law and creed, a shared credal boundary) separable from its asymmetric costs (the purged rationalists, the priced-out non-literal interpreters)? Could textual authority be secured without the uncreated settlement''s enforcement history?',
    'Comparative test: communities holding the created doctrine (Zaydi Yemen, the Mu''tazila remnant under Buyid patronage) maintained textual authority and functioning legal systems without the settlement; if their texts functioned comparably, the functions are separable.',
    'If separable, the asymmetric costs are rent layered on a real coordination function; if inseparable, part of the measured cost is the price of the coordination itself and a hybrid classification would overstate extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether the settlement''s coordination and cost asymmetry are structurally separable.').

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the kernel quran_ontological_status — what structurally changes under the sibling readings, and where exactly is the disagreement located?',
    'The sibling created_reading flips the structure wholesale: under createdness the rationalist theologians are the collecting seats and the literalist establishment bears the cost of a contingent text. The disagreement is located in the ontological premise (the relation of divine speech to the divine essence), which no observable inside any party''s framework can settle. The state_enforced_creation_reading adds a state-enforcement bundle to the created premise.',
    'Cross-reading comparison is valid only seat-by-seat against the flipped structure; aggregating readings into one constraint would average away exactly the beneficiary/victim asymmetry the corpus exists to measure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame omega: one-reading indexicality of the quran_ontological_status kernel.').

omega_variable(
    theater_drift_interpretation,
    'Does the rising theater_ratio measure Goodhart drift (live theology decaying into loyalty recitation) or is creed recitation inherently performative, such that the metric misreads the settlement''s normal operation?',
    'Compare periods by the costliness of affirmation: when the doctrine was contested (848-950) affirmation was costly and informative; track whether affirmation''s informativeness declined in step with the theater series.',
    'If drift, the settlement''s functional core is decaying into shibboleth and post-interval inertial persistence risk rises; if inherent, the metric needs a doctrine-specific reading and the drift signal is spurious.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theater_drift_interpretation, empirical, 'Interpretation of the theater_ratio trajectory over the settlement''s classical arc.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__uncreated_reading, 848, 1258).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t848, quran_ontological_status__uncreated_reading, theater_ratio, 848, 0.18).
narrative_ontology:measurement_basis(qura_tr_t848, observed).
narrative_ontology:measurement(qura_tr_t915, quran_ontological_status__uncreated_reading, theater_ratio, 915, 0.26).
narrative_ontology:measurement_basis(qura_tr_t915, observed).
narrative_ontology:measurement(qura_tr_t985, quran_ontological_status__uncreated_reading, theater_ratio, 985, 0.33).
narrative_ontology:measurement_basis(qura_tr_t985, observed).
narrative_ontology:measurement(qura_tr_t1055, quran_ontological_status__uncreated_reading, theater_ratio, 1055, 0.4).
narrative_ontology:measurement_basis(qura_tr_t1055, observed).
narrative_ontology:measurement(qura_tr_t1125, quran_ontological_status__uncreated_reading, theater_ratio, 1125, 0.45).
narrative_ontology:measurement_basis(qura_tr_t1125, observed).
narrative_ontology:measurement(qura_tr_t1195, quran_ontological_status__uncreated_reading, theater_ratio, 1195, 0.5).
narrative_ontology:measurement_basis(qura_tr_t1195, observed).
narrative_ontology:measurement(qura_tr_t1258, quran_ontological_status__uncreated_reading, theater_ratio, 1258, 0.54).
narrative_ontology:measurement_basis(qura_tr_t1258, observed).

% Extraction over time
narrative_ontology:measurement(qura_be_t848, quran_ontological_status__uncreated_reading, base_extractiveness, 848, 0.52).
narrative_ontology:measurement_basis(qura_be_t848, observed).
narrative_ontology:measurement(qura_be_t915, quran_ontological_status__uncreated_reading, base_extractiveness, 915, 0.55).
narrative_ontology:measurement_basis(qura_be_t915, observed).
narrative_ontology:measurement(qura_be_t985, quran_ontological_status__uncreated_reading, base_extractiveness, 985, 0.58).
narrative_ontology:measurement_basis(qura_be_t985, observed).
narrative_ontology:measurement(qura_be_t1055, quran_ontological_status__uncreated_reading, base_extractiveness, 1055, 0.61).
narrative_ontology:measurement_basis(qura_be_t1055, observed).
narrative_ontology:measurement(qura_be_t1125, quran_ontological_status__uncreated_reading, base_extractiveness, 1125, 0.64).
narrative_ontology:measurement_basis(qura_be_t1125, observed).
narrative_ontology:measurement(qura_be_t1195, quran_ontological_status__uncreated_reading, base_extractiveness, 1195, 0.68).
narrative_ontology:measurement_basis(qura_be_t1195, observed).
narrative_ontology:measurement(qura_be_t1258, quran_ontological_status__uncreated_reading, base_extractiveness, 1258, 0.7).
narrative_ontology:measurement_basis(qura_be_t1258, observed).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t848, quran_ontological_status__uncreated_reading, suppression_requirement, 848, 0.62).
narrative_ontology:measurement_basis(qura_su_t848, observed).
narrative_ontology:measurement(qura_su_t915, quran_ontological_status__uncreated_reading, suppression_requirement, 915, 0.68).
narrative_ontology:measurement_basis(qura_su_t915, observed).
narrative_ontology:measurement(qura_su_t985, quran_ontological_status__uncreated_reading, suppression_requirement, 985, 0.72).
narrative_ontology:measurement_basis(qura_su_t985, observed).
narrative_ontology:measurement(qura_su_t1055, quran_ontological_status__uncreated_reading, suppression_requirement, 1055, 0.7).
narrative_ontology:measurement_basis(qura_su_t1055, observed).
narrative_ontology:measurement(qura_su_t1125, quran_ontological_status__uncreated_reading, suppression_requirement, 1125, 0.66).
narrative_ontology:measurement_basis(qura_su_t1125, observed).
narrative_ontology:measurement(qura_su_t1195, quran_ontological_status__uncreated_reading, suppression_requirement, 1195, 0.64).
narrative_ontology:measurement_basis(qura_su_t1195, observed).
narrative_ontology:measurement(qura_su_t1258, quran_ontological_status__uncreated_reading, suppression_requirement, 1258, 0.6).
narrative_ontology:measurement_basis(qura_su_t1258, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__uncreated_reading, identity_coordination).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Qur'an's ontological status' is one kernel with at least three structurally distinct readings; forcing them into one story would average away exactly the asymmetry the corpus measures. Under the created_reading the beneficiary/victim structure inverts — rationalist theologians become the collecting seats and the literalist establishment bears the cost of a contingent text — so the readings have different epsilon, different stakeholder surfaces, and different enforcement histories (the mihna belongs to the state_enforced sibling, not to this one). This file authors the uncreated settlement only; both siblings are linked here, and the uncreated settlement's victory is the structural event that marginalized the created reading inside Sunni lands.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
